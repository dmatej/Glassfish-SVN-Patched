/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.image.cache;

import java.util.Hashtable;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoderManager;

/**
 * Cache data structure
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/Cache.java#0 $) $Date: 10-nov-2005.19:05:58 $
 */
class Cache
{
  public Cache()
  {
    _cache = new ConcurrentHashMap<Object, CacheEntry>(_CACHE_SIZE);
  }

  public CacheEntry get(ImageContext context, Object key)
  {
    CacheEntry entry = _cache.get(key);
    if (entry == null)
      return null;

    TrinidadAgent agent = context.getAgent();
    //int encodings = ((Integer)agent.getCapability(
    //                                  AdfFacesAgent.CAP_IMAGE_ENCODINGS)).intValue();
    boolean capGif = false;
    Object capability =  agent.getCapability(TrinidadAgent.CAP_GIF_TYPE_IMAGE);
    if (capability == Boolean.TRUE)
       capGif = true;

    boolean capPng = false;
    capability = agent.getCapability(TrinidadAgent.CAP_PNG_TYPE_IMAGE);
    if (capability == Boolean.TRUE)
      capPng = true;

    if (_supportsEncoding(entry, capGif, capPng))
      return entry;

    while ((entry = entry.getNext()) != null)
    {
      if (_supportsEncoding(entry, capGif, capPng))
        return entry;
    }

    return null;
  }

  public void put(ImageContext context, Object key, CacheEntry entry)
  {
    String baseName = _getBaseName(entry.getImageURI());

    if (baseName != null)
      _names.put(baseName.toLowerCase(), Boolean.TRUE);

    synchronized (_cache)
    {
      // First check to see if we have already got an entry -
      // perhaps for some other encoding
      CacheEntry topEntry = _cache.get(key);
      if (topEntry == null)
      {
        _cache.put(key, entry);
      }
      else
      {
        // If we have already got an entry, add the new entry to
        // the list.  If the new entry is a miss/retry entry,
        // we explicitly add the new entry to the end of the list.
        // This allows lookups of existing images to be found.
        // Otherwise, just add the entry to the front of the list.
        if (_isMissRetryEntry(entry))
        {
          CacheEntry nextEntry = null;

          // Loop until there is no next entry
          while ((nextEntry = topEntry.getNext()) != null)
            topEntry = nextEntry;

          // When we get to the end, tack on the new entry
          // We reuse miss/retry entries - so never set the
          // next pointer for these entries - see 2510160.
          if (!_isMissRetryEntry(topEntry))
            topEntry.setNext(entry);
        }
        else
        {
          entry.setNext(topEntry);
          _cache.put(key, entry);
        }
      }
    }
  }

  public void remove(
    ImageContext context,
    Object       key,
    CacheEntry   entry)
  {
    synchronized (_cache)
    {
      CacheEntry topEntry = _cache.get(key);

      if (topEntry == entry)
      {
        _cache.remove(key);

        // If there is a next CacheEntry, put it back into the cache
        CacheEntry nextEntry = entry.getNext();
        if (nextEntry != null)
        {
          _cache.put(key, nextEntry);

          // What the heck, null out the reference to the next entry
          entry.setNext(null);
        }
      }
      else if (topEntry != null)
      {
        // If the entry isn't the first entry in the list, check
        // to see if the entry is somewhere in the middle of the
        // list - if so, remove it.
        CacheEntry nextEntry = null;
        while ((nextEntry = topEntry.getNext()) != null)
        {
          if (nextEntry == entry)
          {
            // Remove the entry from the list
            topEntry.setNext(entry.getNext());

            // What the heck, null out the reference to the next entry
            entry.setNext(null);
          }

          topEntry = nextEntry;
        }
      }
    }
  }

  public synchronized String getUniqueName(String baseName)
  {
    baseName = _filterName(baseName);

    String lowerBaseName = baseName.toLowerCase();

    // Try to find a unique name
    for (int i = 0; i < Integer.MAX_VALUE; i++)
    {
      String name = (i == 0) ? lowerBaseName : lowerBaseName + "-" + i;

      synchronized (_names)
      {
        if (!_names.containsKey(name))
        {
          // Add a placeholder into the inverse name cache to reserve
          // this name
          _names.put(name, Boolean.TRUE);

          return (i == 0) ? baseName : baseName + "-" + i;
        }
      }
    }

    // We should never get here - we should always get a unique name above!
    _LOG.severe("CANNOT_GET_UNIQUE_NAME");

    return null;
  }

  // Frees up the unique name for reuse.
  public synchronized void releaseUniqueName(String uniqueName)
  {
    _names.remove(uniqueName.toLowerCase());
  }

  // Filters the specified name to a valid file name.  We strip out
  // characters that we suspect might cause problems for the file
  // system.  For example, the name may be based on a label which
  // contains special characters (like '/').
  private String _filterName(String name)
  {
    StringBuffer buffer = null;
    int length = name.length();

    for (int i = 0; i < length; i++)
    {
      char c = name.charAt(i);

      if (_isValidCharacter(c))
      {
        if (buffer != null)
          buffer.append(c);
      }
      else
      {
        if (buffer == null)
        {
          buffer = new StringBuffer(length);
          buffer.append(name.substring(0, i));
        }

        buffer.append(_getValidCharacter(c));
      }
    }

    // If we have transformed the name, return the new name
    if (buffer != null)
      return buffer.toString();;

    return name;
  }

  // Tests whether the specified character can be used as part of
  // a file name (and URL).  We pretty much just allow ascii alphanumerics.
  private boolean _isValidCharacter(char c)
  {
    return
      ((c >= 0x41) && (c < 0x5b))  ||  // A-Z
      ((c >= 0x61) && (c < 0x7b))  ||  // a-z
      ((c >= 0x30) && (c < 0x3a))  ||  // 0-9
      (c == 0x5F)                  ||  // underscore
      (c == 0x2D);                     // dash
  }

  // Returns a valid character to use in place of the specified character
  // We just return an Ascii character, based on the hexadecimal
  // representation of the character.  This results in meaningless but
  // valid file names.
  private char _getValidCharacter(char c)
  {
    // Convert space to underline
    if (c == 0x20)
      return 0x5f;

    // Convert to hexidecimal string
    String hex = Integer.toHexString(c);

    // To keep names short, just pick off the last character - it's the
    // most variable digit.
    return hex.charAt(hex.length() - 1);
  }

  // Gets the base name from the uri.
  private String _getBaseName(String uri)
  {
    if (uri == null)
      return null;

    // The base name is everything from the first '/' to the last '.'.
    // Everything before the first '/' is the locale.  Everything
    // after the last '.' is the extension
    int startIndex = uri.indexOf('/');
    int endIndex = uri.indexOf('.');

    assert(endIndex != -1);

    return uri.substring((startIndex == -1) ? 0 : startIndex + 1, endIndex);
  }

  // Tests whether the specified entry uses a supported encoding.
  private boolean _supportsEncoding(
    CacheEntry entry,
    //int        encodings,
    boolean    capGif,
    boolean    capPng
    )
  {
    // Miss/retry entries are always supported.
    if (_isMissRetryEntry(entry))
      return true;

    String encoding = entry.getEncoding();

    return (((encoding == ImageEncoderManager.GIF_TYPE) && (capGif)) ||
               //((AdfFacesAgent.IMAGE_ENCODINGS_CAP_GIF & encodings) != 0)) ||
            ((encoding == ImageEncoderManager.PNG_TYPE) && (capPng)));
               //((AdfFacesAgent.IMAGE_ENCODINGS_CAP_PNG & encodings) != 0)));
  }

  // Test whether the entry is a special miss/retry entry.
  // We consider an entry with a null URI to be a miss/retry
  // entry.  This is a bit obscure, but what the heck...
  private static boolean _isMissRetryEntry(CacheEntry entry)
  {
    return (entry.getImageURI() == null);
  }

  // Hashtable of CacheKeys to CacheEntrys
  private ConcurrentHashMap<Object, CacheEntry> _cache;

  // List of base names.
  // =-=ags We use a Hashtable so that lookups are quick.  However,
  // this probably should just be a sorted list and do a binary
  // search, since we don't care about the value, just the key.
  // -= Simon Lessard =-
  // FIXME: Hashtale is synchronized (thus slow). If thread safety is not 
  //        required, HashMap would be better,
  private Hashtable<String, Boolean> _names = 
    new Hashtable<String, Boolean>(_NAMES_SIZE);

  private static final int _CACHE_SIZE = 101;

  // Default size for names cache
  private static final int _NAMES_SIZE = 101;

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(Cache.class);
}
