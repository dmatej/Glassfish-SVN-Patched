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
package org.apache.myfaces.trinidadinternal.util;

import java.io.Serializable;

import java.math.BigInteger;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * A simple LRU tokenized cache.  The cache is responsible for storing tokens,
 * but the storage of the values referred to by those tokens is not handled
 * by this class.  Instead, the user of this class has to provide a Map
 * instance to each call.
 * <p>
 * The design seems odd, but is intentional - this way, a session Map can be used
 * directly as the storage target for values, while the TokenCache simply maintains
 * the logic of which tokens should still be available.  Storing values
 * directly in the cache object (instead of directly on the session) causes
 * HttpSession failover difficulties.
 * <p>
 * TokenCache also supports the concept of "pinning", whereby one token
 * can be pinned to another.  The pinned token will not be removed from
 * the cache until all tokens that it is pinned to are also out of the
 * cache.
 */
public class TokenCache implements Serializable
{
  /**
   * Character guaranteed to not be used in tokens
   */
  static public final char SEPARATOR_CHAR = '.';

  /**
   * Gets a TokenCache from the session, creating it if needed.
   */
  @SuppressWarnings("unchecked")
  static public TokenCache getTokenCacheFromSession(
    ExternalContext extContext,
    String          cacheName,
    boolean         createIfNeeded,
    int             defaultSize)
  {
    Map<String, Object> sessionMap = extContext.getSessionMap();

    TokenCache cache = (TokenCache)sessionMap.get(cacheName);
    
    if (cache == null)
    {
      if (createIfNeeded)
      {
        Object session = extContext.getSession(true);
  
        // Synchronize on the session object to ensure that
        // we don't ever create two different caches
        synchronized (session)
        {
          cache = (TokenCache)sessionMap.get(cacheName);
          
          if (cache == null)
          {
            // create the TokenCache with the crytographically random seed
            cache = new TokenCache(defaultSize, _getSeed(), sessionMap, cacheName);
    
            sessionMap.put(cacheName, cache);
          }
          else
          {
            // make sure the existing cache has its own attached so it can dirty itself
            cache.reattachOwner(sessionMap);
          }
        }
      }
    }
    else
    {
      // make sure the existing cache has its own attached so it can dirty itself
      cache.reattachOwner(sessionMap);
    }

    return cache;
  }
    
  /**
   * Returns a cryptographically secure random number to use as the TokenCache seed
   */
  private static long _getSeed()
  {
    SecureRandom rng;
    
    try
    {
      // try SHA1 first
      rng = SecureRandom.getInstance("SHA1PRNG");
    }
    catch (NoSuchAlgorithmException e)
    {
      // SHA1 not present, so try the default (which could potentially not be
      // cryptographically secure)
      rng = new SecureRandom();
    }
    
    // use 48 bits for strength and fill them in
    byte[] randomBytes = new byte[6];
    rng.nextBytes(randomBytes);
    
    // convert to a long
    return new BigInteger(randomBytes).longValue();
  }


  /**
   * For serialization only
   */
  TokenCache()
  {
    this(_DEFAULT_SIZE, 0L, null, null);
  }   

  /**
   * Create a TokenCache that will store the last "size" entries.  This version should
   * not be used if the token cache is externally accessible (since the seed always 
   * starts at 0).  Use the
   * constructor with the long seed instead.
   */
  public TokenCache(int size)
  {
    this(size, 0L, null, null);
  }

 /**
  * Create a TokenCache that will store the last "size" entries,
  * and begins its tokens based on the seed (instead of always
  * starting at "0").
  * @patam owner      Optional Cache that stores the token cache
  * @param keyInOwner Optional Name under which this cache is stored in the owner
  */
  private TokenCache(int size, long seed, Map<String, Object> owner, String keyInOwner)
  {
    _cache      = new LRU(size);
    _pinned     = new ConcurrentHashMap<String, String>(size);
    _count      = new AtomicLong(seed);
    _owner      = owner;
    _keyInOwner = keyInOwner;
  } 

  /**
   * Reattaches the owner after Serialization since the owner might not be Serializable or it
   * might not be a good idea to serialize the owner.
   * @param owner
   * @throws NullPointerException if owner is null
   */
  public void reattachOwner(Map<String, Object> owner)
  {
    if (owner == null)
      throw new NullPointerException("Can't set owner to null");
      
    _owner = owner;
  }

  /**
   * Create a new token;  and use that token to store a value into
   * a target Map.  The least recently used values from the
   * cache may be removed.
   * @param value the value being added to the target store
   * @param targetStore the map used for storing the value
   * @return the token used to store the value
   */
  public <V> String addNewEntry(
      V value, 
      Map<String, V> targetStore)
  {
    return addNewEntry(value, targetStore, null);
  }
  
  /**
   * Create a new token;  and use that token to store a value into
   * a target Map.  The least recently used values from the
   * cache may be removed.
   * @param value the value being added to the target store
   * @param targetStore the map used for storing the value
   * @param pinnedToken a token, that if still in the cache,
   *    will not be freed until this current token is also freed
   * @return the token used to store the value
   */
  public <V> String addNewEntry(
      V value, 
      Map<String, V> targetStore,
      String pinnedToken)
  {
    String remove = null;
    String token = null;
    synchronized (this)
    {
      token = _getNextToken();

      // If there is a request to pin one token to another, 
      // store that:  the pinnedToken is the value
      if (pinnedToken != null)
        _pinned.put(token, pinnedToken);
      
      assert(_removed == null);
      // NOTE: this put() has a side-effect that can result
      // in _removed being non-null afterwards
      _cache.put(token, token);

      if(TokenCacheDebugUtils.debugTokenCache())
      {
        TokenCacheDebugUtils.startLog("Add New Entry");
        TokenCacheDebugUtils.addTokenToViewIdMap(token);

        if (pinnedToken != null)
        {
          TokenCacheDebugUtils.addToLog("\nPINNING " + 
                      TokenCacheDebugUtils.getTokenToViewIdString(token) + 
                      " to " + 
                      TokenCacheDebugUtils.getTokenToViewIdString(pinnedToken));  
        }
      }
      
      remove = _removed;
      _removed = null;
    }

    // This looks like "remove" must be null - given the
    // assert above.
    if (remove != null)
    {
      _removeTokenIfReady(targetStore, remove);        
    }
    
    targetStore.put(token, value);

    // our contents have changed, so mark ourselves as dirty in our owner
    _dirty();

    if(TokenCacheDebugUtils.debugTokenCache())
    {
      TokenCacheDebugUtils.logCacheInfo(targetStore, _pinned, "After Additions"); 
      _LOG.severe(TokenCacheDebugUtils.getLogString());
      
    }

    return token;
  }

  /**
   * Returns true if an entry is still available.  This
   * method has a side-effect:  by virtue of accessing the token,
   * it is now at the top of the most-recently-used list.
   */
  public boolean isAvailable(String token)
  {
    synchronized (this)
    {
      // If the token is in the LRU cache, then it's available
      if (_cache.get(token) != null)
        return true;
      
      // And if the token is a value in "pinned", then it's also available
      if (_pinned.containsValue(token))
        return true;
      
      return false;
    }
  }

  /**
   * Remove a token if is ready:  there are no pinned references to it.
   * Note that it will be absent from the LRUCache.
   */
  synchronized private <V> V _removeTokenIfReady(
      Map<String, V> targetStore, 
      String              token)
  {
    V removedValue;
    
    // See if it's pinned to something still in memory
    if (!_pinned.containsValue(token))
    {
      _LOG.finest("Removing token ''{0}''", token);
      // Remove it from the target store

      if (TokenCacheDebugUtils.debugTokenCache())
      {
        TokenCacheDebugUtils.removeTokenFromViewIdMap(token);
      }
      
      removedValue = targetStore.remove(token);
      // Now, see if that key was pinning anything else
      String wasPinned = _pinned.remove(token);
      if (wasPinned != null)
      {

        if (TokenCacheDebugUtils.debugTokenCache())
        {
          TokenCacheDebugUtils.addToLog("\nREMOVING pinning of token " + token + " to " + 
                      TokenCacheDebugUtils.getTokenToViewIdString(wasPinned));  
        }        
        
        // Yup, so see if we can remove that token
        _removeTokenIfReady(targetStore, wasPinned);
      }
    }
    else
    {
      if (TokenCacheDebugUtils.debugTokenCache())
      {
        TokenCacheDebugUtils.addToLog("\nNOT removing pinned token from target store " + 
                    TokenCacheDebugUtils.getTokenToViewIdString(token) );  
      }
      
      _LOG.finest("Not removing pinned token ''{0}''", token);
      // TODO: is this correct?  We're not really removing
      // the target value.
      removedValue = targetStore.get(token);
    }

    return removedValue;
  }


  /**
   * Removes a value from the cache.
   * @return previous value associated with the token, if any
   */
  public <V> V removeOldEntry(
      String token, 
      Map<String, V> targetStore)
  {
    V oldValue;
    
    synchronized (this)
    {
      if(TokenCacheDebugUtils.debugTokenCache())
      {
        TokenCacheDebugUtils.startLog("Remove Old Entry");
      }
      
      _LOG.finest("Removing token {0} from cache", token);
      _cache.remove(token);
      
      // TODO: should removing a value that is "pinned" take?
      // Or should it stay in memory?
      oldValue = _removeTokenIfReady(targetStore, token);
      
      if (TokenCacheDebugUtils.debugTokenCache())
      {
        TokenCacheDebugUtils.logCacheInfo(targetStore, _pinned, "After removing old entry:");
        _LOG.severe(TokenCacheDebugUtils.getLogString());
      }
    }

    // our contents have changed, so mark ourselves as dirty in our owner
    _dirty();
    
    return oldValue;
  }

  /**
   * Clear a cache, without resetting the token.
   */
  public <V> void clear(Map<String, V> targetStore)
  {
    synchronized (this)
    {
      for(String keyToRemove : _cache.keySet())
      {
        _LOG.finest("Clearing token {0} from cache", keyToRemove);
        targetStore.remove(keyToRemove);
      }

      _cache.clear();
    }

    // our contents have changed, so mark ourselves as dirty in our owner
    _dirty();
  }

  private String _getNextToken()
  {
    // atomically increment the value
    long nextToken = _count.incrementAndGet();
    
    // convert using base 36 because it is a fast efficient subset of base-64
    return Long.toString(nextToken, 36);
  }
  
  /**
   * Mark the cache as dirty in the owner
   */
  private void _dirty()
  {
    if (_keyInOwner != null)
    {
      _owner.put(_keyInOwner, this);
    }
  }

  private class LRU extends LRUCache<String, String>
  {
    public LRU(int maxSize)
    {
      super(maxSize);
    }

    @Override
    protected void removing(String key)
    {
      _removed = key;
    }

    private static final long serialVersionUID = 1L;
  }

  private final Map<String, String> _cache;
  
  // Map from String to String, where the keys represent tokens that are
  // stored, and the values are the tokens that are pinned.  This is 
  // an N->1 ratio:  the values may appear multiple times.
  private final Map<String, String> _pinned;
  
  // the current token value
  private final AtomicLong _count;

  private final String _keyInOwner;
  
  // Hack instance parameter used to communicate between the LRU cache's
  // removing() method, and the addNewEntry() method that may trigger it
  private transient String _removed;

  // owning cache
  private transient Map<String, Object> _owner;

  static private final int _DEFAULT_SIZE = 15;
  static private final long serialVersionUID = 1L;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TokenCache.class);
}
