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
package org.apache.myfaces.trinidad.component;

import java.util.List;

abstract public class UIXSelectOrderTemplate extends UIXSelectMany
{
	
  /**
   * Compares two values, paying attention to the order of the elements.
   * @return true if the values are different
   */
  // TODO improve efficiency for the array case?
  @Override
  @SuppressWarnings("unchecked")
  protected boolean compareValues(Object previous, Object value)
  {
    int prevSize = __getSize(previous);
    int newSize = __getSize(value);

    // If the sizes are different, no need to bother with further work
    if (prevSize != newSize)
      return true;

    // If the sizes are the same, and they're empty, we're also done.
    if (prevSize == 0)
      return false;

    List<Object> prevList = (previous instanceof List)
                      ? (List<Object>) previous : __toList(previous);
    List<Object> newList = (value instanceof List)
                      ? (List<Object>) value : __toList(value);

    // Since List has explicit rules about how equals() works, we
    // can just use that method.
    return !prevList.equals(newList);
  }
}
