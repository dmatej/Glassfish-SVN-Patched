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
package org.apache.myfaces.trinidad.util;

/**
 * This class has been moved from bali share.
 * Class containing various integer utilities. It caches commonly
 * used Integer objects and String representations of Integers to avoid
 * expensive object creations.
 * <p>
 * IntegerUtils is used throughout most Bali projects, and clients are also
 * encouraged to use it for increased performance.
 * <p>
 * @since Inspector 0.2
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/IntegerUtils.java#0 $) $Date: 10-nov-2005.18:49:09 $
 */
public final class IntegerUtils
{
  private IntegerUtils()
  {
  }

  /**
   * @return a String value corresponding to the specified integer, possibly
   * creating a new String object if a cached one does not exist.
   */
  public static String getString(
    int intValue
    )
  {
    int cacheIndex = intValue - _START_INTEGER;

    if ((cacheIndex >= _NUM_CACHED) || (cacheIndex < 0))
    {
      return String.valueOf(intValue);
    }
    else
    {
      String cachedValue = _sCachedStrings[cacheIndex];

      if (cachedValue == null)
      {
        cachedValue = String.valueOf(intValue);

        _sCachedStrings[cacheIndex] = cachedValue;
      }

      return cachedValue;
    }
  }

  /**
   * @return a String value corresponding to the specified long, possibly
   * creating a new String object if a cached one does not exist.
   */
  public static String getString(long longValue)
  {
    if ((longValue >= _START_INTEGER) && (longValue <= _NUM_CACHED))
      return getString((int) longValue);
    return String.valueOf(longValue);
  }

  /**
   * @return a String value corresponding to the specified Integer, possibly
   * creating a new String object if a cached one does not exist.
   */
  public static String getString(
    Integer intObject
    )
  {
    return getString(intObject.intValue());
  }

  // index of first cached value
  private static final int _START_INTEGER = -10;

  // number of cached values
  private static final int _NUM_CACHED = 266;

  // cache of int Strings
  private static String[] _sCachedStrings = new String[_NUM_CACHED];
}