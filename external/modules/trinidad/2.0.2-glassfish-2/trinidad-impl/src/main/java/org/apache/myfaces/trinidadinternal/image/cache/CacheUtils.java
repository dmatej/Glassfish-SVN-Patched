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

import java.awt.Color;

import java.util.Collections;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.util.LRUCache;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageTypeManager;

/**
 * Private utilites used by org.apache.myfaces.trinidadinternal.image.cache.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CacheUtils.java#0 $) $Date: 10-nov-2005.19:06:01 $
 */
class CacheUtils
{
  private CacheUtils() {}

  /**
   * Color used by all of our cache key impementations for the
   * transparent surrounding color.
   */
  public static final Color TRANSPARENT_COLOR = new Color(255, 0, 255, 0);

  public static int getReadingDirection(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    LocaleContext localeContext = context.getLocaleContext();
    int direction = LocaleUtils.getReadingDirection(localeContext);

    if (properties != null)
    {
      Object o = null;
      if ((o = properties.get(ImageConstants.DIRECTION_KEY)) != null)
        direction = ((Integer)o).intValue();
    }

    if (direction == LocaleUtils.DIRECTION_DEFAULT)
    {

      return LocaleUtils.getReadingDirectionForLocale(
                           localeContext.getTranslationLocale());
    }

    return direction;
  }

  // Gets a Boolean value
  public static boolean getBoolean(
    Map<Object, Object> properties,
    Object key,
    boolean defaultValue
    )
  {
    Boolean value = (Boolean)properties.get(key);
    if (value == null)
      return defaultValue;

    return value.booleanValue();
  }

  // Puts a Boolean value into a dictionary
  public static void putBoolean(
    Map<Object, Object> properties,
    Object key,
    boolean value
    )
  {
    properties.put(key, value ? Boolean.TRUE : Boolean.FALSE);
  }

  // Tests for TEXT_ANTIALIAS_KEY
  public static boolean isTextAntialiased(Map<Object, Object> properties)
  {
    return getBoolean(properties, ImageConstants.TEXT_ANTIALIAS_KEY, false);
  }

  // Returns a non-null ImageTypeManager
  public static ImageTypeManager getImageTypeManager(ImageContext context)
  {
    return ImageTypeManager.getDefaultImageTypeManager();
  }

  /**
   * Returns a shared FontProxy instance with the same properties as
   * the specific FontProxy instance.  This method is used to reduce
   * the number of FontProxy instances that are permanently stored in
   * FileSystemImageCache key objects.  Without FontProxy sharing, every
   * FontProxy instance variable in every CacheKey instance would hold on
   * to its own FontProxy.  Instead, we keep a cache of FontProxy instances
   * that can be shared across CacheKeys.
   */
  public static FontProxy getSharedFont(FontProxy font)
  {
    if (font == null)
      return null;

    FontProxy sharedFont = _sFontProxyCache.get(font);
    if (sharedFont != null)
      return sharedFont;

    // Create a new FontProxy instance and store it in the cache
    sharedFont = new FontProxy(font.getName(),
                               font.getStyle(),
                               font.getSize());

    _sFontProxyCache.put(sharedFont, sharedFont);

    return sharedFont;
  }

  /**
   * Convenience method for getting a shared FontProxy for a font
   * specified in a properties dictionary.
   */
  public static FontProxy getSharedFont(Map<Object, Object> properties, Object key)
  {
    return getSharedFont((FontProxy)properties.get(key));
  }

  /**
   * Utility method for producing a (psuedo) base 64 encoding
   * of an int value, suitable for inclusion in a file name.
   * This is used by NameProviders to include a semi-unique
   * identifier (ie. the base 64 encoded hash code) in the
   * image file names in order to avoid name collisions.
   */
  public static String base64Encode(int value)
  {
    StringBuffer buffer = new StringBuffer(6);

    // For now, let's just use the lower 3 bytes.  This increases
    // the possibility of collisions, but it allows us to go
    // from a 6 char to 4 char id.

    for (int i = 0; i < 4; i++)
      buffer.append(_BASE_64_CHARS[((value >> (6 * i)) & 0x3f)]);

    return buffer.toString();
  }

  private static final Map<FontProxy, FontProxy> _sFontProxyCache = 
    Collections.synchronizedMap(new LRUCache<FontProxy, FontProxy>(50));

  // Characters for base 64 encodings
  private static final char[] _BASE_64_CHARS =
  {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', // 0-9
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', // 10-19
    'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', // 20-29
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', // 30-39
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', // 40-49
    'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', // 50-59
    '8', '9', '_', '-'                                // 60-63
   };

}
