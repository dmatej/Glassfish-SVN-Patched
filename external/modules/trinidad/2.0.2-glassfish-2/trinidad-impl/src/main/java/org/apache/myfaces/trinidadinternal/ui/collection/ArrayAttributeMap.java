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
package org.apache.myfaces.trinidadinternal.ui.collection;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.myfaces.trinidad.util.ArrayMap;


import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * AttributeMap implementation that stores AttributeKey value pairs in
 * an array and locates the desired AttributeKey by making a linear scan.
 * <p>
 * Although extremely memory efficient and fast for the number of attributes
 * stored on most UINodes ( < 15), FlaggedAttributeMap is a better general
 * purpose AttributeMap implementation for use as UINode storage.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/ArrayAttributeMap.java#0 $) $Date: 10-nov-2005.18:56:56 $
 * @see FlaggedAttributeMap
 * @see IndexedAttributeMap
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ArrayAttributeMap implements AttributeMap
{
  public ArrayAttributeMap()
  {
    this(0);
  }

  public ArrayAttributeMap(
    int size
    )
  {
    this(size, 5);
  }

  public ArrayAttributeMap(
    int size,
    int increment
    )
  {
    if ((increment < 1) || (size < 0))
      throw new IllegalArgumentException();

    if (size > 0)
    {
      _keyValues = new Object[2 * size];
    }
    else
    {
      // shared empty array
      _keyValues = _EMPTY_KEY_VALUES;
    }

    _increment = increment;
  }


  public Object getAttribute(
    UIXRenderingContext context,
    AttributeKey     key
    )
  {
    return ArrayMap.getByIdentity(_keyValues, key);
  }

  public void setAttribute(
    AttributeKey key,
    Object       value
    )
  {
    if (value != null)
    {
      putAttribute(key, value);
    }
    else
    {
      removeAttribute(key);
    }
  }

  public Iterator<AttributeKey> attributeKeys(
    UIXRenderingContext context
    )
  {
    int size = size();

    if (size == 0)
      return null;
    ArrayList<AttributeKey> keyValuesList = new ArrayList<AttributeKey>();
    int i = (size-1)*2;
    while(i>=0)
    {
      keyValuesList.add((AttributeKey)_keyValues[i]);
      i=i-2;
    }
   return  keyValuesList.iterator();
  }

  public int size()
  {
    return _size;
  }


  /**
   * Clones the map.
   */
  @Override
  public Object clone()
  {
    try
    {
      ArrayAttributeMap am = (ArrayAttributeMap)super.clone();

      if (_keyValues != _EMPTY_KEY_VALUES)
      {
        am._keyValues = _keyValues.clone();
      }

      return am;
    }
    catch (CloneNotSupportedException cnse)
    {
      // Should never reach here
      throw new IllegalStateException();
    }
  }

  @Override
  public String toString()
  {
    int entryCount = size() * 2;

    Object[] keyValues = _keyValues;

    StringBuffer keyValuesBuffer = new StringBuffer(10 * entryCount);

    keyValuesBuffer.append(super.toString());
    keyValuesBuffer.append('[');

    for (int i = 0; i < entryCount; i += 2)
    {
      keyValuesBuffer.append(keyValues[i]);
      keyValuesBuffer.append('=');
      keyValuesBuffer.append(keyValues[i + 1]);
      keyValuesBuffer.append(',');
    }

    keyValuesBuffer.append(']');

    return keyValuesBuffer.toString();
  }

  protected void putAttribute(
    AttributeKey key,
    Object       value
    )
  {
    int size = size();

    int entryCount = size * 2;

    Object[] keyValues = _keyValues;

    for (int i = 0; i < entryCount; i += 2)
    {
      if (keyValues[i] == key)
      {
        keyValues[i + 1] = value;
        return;
      }
    }

    Object[] newKeyValues = keyValues;

    if (entryCount >= keyValues.length)
    {
      newKeyValues = new Object[entryCount + (2 * _increment)];

      if (entryCount != 0)
      {
        System.arraycopy(keyValues, 0, newKeyValues, 2, entryCount);
      }

      newKeyValues[0] = key;
      newKeyValues[1] = value;

      _keyValues = newKeyValues;
    }
    else
    {
      newKeyValues[entryCount]     = key;
      newKeyValues[entryCount + 1] = value;
    }

    _size++;
  }

  protected void removeAttribute(
    AttributeKey key
    )
  {
    int size = size();

    if (size > 0)
    {
      int entryCount = size * 2;

      Object[] keyValues = _keyValues;

      for (int i = 0; i < entryCount; i += 2)
      {
        if (keyValues[i] == key)
        {
          System.arraycopy(keyValues, i + 2, keyValues, i, entryCount - i - 2);

          keyValues[entryCount - 1] = null;
          keyValues[entryCount - 2] = null;

          _size--;

          return;
        }
      }
    }
  }

  // shared empty array value
  private static final Object[] _EMPTY_KEY_VALUES = new Object[0];


  private Object[] _keyValues;
  private int      _size;
  private int      _increment;
}
