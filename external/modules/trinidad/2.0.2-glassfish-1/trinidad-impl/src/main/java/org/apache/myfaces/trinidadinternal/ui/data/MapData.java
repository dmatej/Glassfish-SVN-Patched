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
package org.apache.myfaces.trinidadinternal.ui.data;

import java.util.Iterator;
import java.util.HashMap;

import java.util.Map;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * The MapData interface provides a simple,
 * hashmap-like datasource.  The "select" strings
 * are defined as keys to string values;  DataSet
 * is not supported.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/MapData.java#0 $) $Date: 10-nov-2005.18:56:33 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MapData implements KeyedDataObject, MutableDataObject
{
  /**
   * Creates a MapData.
   */
  public MapData()
  {
    _table = new HashMap<Object, Object>(7);
  }

  /**
   * Creates a MapData, using the provided Map
   * for storage.
   */
  public MapData(Map<Object, Object> table)
  {
    _table = table;
  }


  /**
   * Creates a MapData initialized with a single
   * pair of data.
   */
  public MapData(Object select, Object data)
  {
    this();
    put(select, data);
  }


  /**
   * Adds a key/value pair to the MapData.
   */
  public void put(Object select, Object data)
  {
    _table.put(select, data);
  }


  /**
   * Returns the value registered with the select key.
   */
  public Object selectValue(UIXRenderingContext context, Object select)
  {
    if (select == null)
      return null;
    return  _table.get(select);
  }

  public void updateValue(
    UIXRenderingContext context,
    Object select,
    Object value)
  {
    put(select, value);
  }

  public Iterator<Object> keys(
    UIXRenderingContext context
    )
  {
    return _table.keySet().iterator();
  }

  private Map<Object, Object> _table;
}
