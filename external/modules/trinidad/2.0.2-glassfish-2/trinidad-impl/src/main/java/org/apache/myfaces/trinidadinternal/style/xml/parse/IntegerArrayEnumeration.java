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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Enumeration;

/**
 * Dinky little class for enumerating an array of integers.
 * Note: This class assumes that the contents of the
 * array do not change during the enumeration - it avoids
 * making a copy of the array.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/IntegerArrayEnumeration.java#0 $) $Date: 10-nov-2005.18:58:09 $
 */
class IntegerArrayEnumeration implements Enumeration<Integer>
{
  /**
   * Creates an Enumeration for the specified array.
   * @param array The array to enumerate
   */
  public IntegerArrayEnumeration(int[] array)
  {
    _array = array;
  }

  /**
   * Implementation of Enumeration.hasMoreElements().
   */
  public boolean hasMoreElements()
  {
    if (_array == null)
      return false;

    return (_index < _array.length);
  }

  /**
   * Implementation of Enumeration.nextElement().
   */
  public Integer nextElement()
  {
    return _array[_index++];
  }

  private int[] _array;
  private int   _index;
}
