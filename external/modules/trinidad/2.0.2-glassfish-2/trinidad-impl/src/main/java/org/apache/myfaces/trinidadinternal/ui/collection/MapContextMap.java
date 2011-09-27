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

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/MapContextMap.java#0 $) $Date: 10-nov-2005.18:57:34 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class MapContextMap implements ContextMap
{
  public MapContextMap()
  {
    this(null);
  }

  public MapContextMap(
    Map<String, Object> map
    )
  {
    if (map == null)
      map = createDefaultMap();

    _map = map;
  }

  public Object get(
    UIXRenderingContext context,
    Object           key
    )
  {
    return _map.get(key);
  }

  public void set(
      String  key,
      Object  value
    )
  {
    if (value == null)
    {
      _map.remove(key);
    }
    else
    {
      _map.put(key, value);
    }
  }

  public Iterator<String> keys(
    UIXRenderingContext context
    )
  {  
    if(_map instanceof ArrayMap)
    {
      return ((ArrayMap<String, Object>) _map).keys();
    }
    return _map.keySet().iterator();
  }

  public int size()
  {
    return _map.size();
  }

  protected Map<String, Object> createDefaultMap()
  {
    return new HashMap<String, Object>(13);
  }

  private Map<String, Object> _map;
}
