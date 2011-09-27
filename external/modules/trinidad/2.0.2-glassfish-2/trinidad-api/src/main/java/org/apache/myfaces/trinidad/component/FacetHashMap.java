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

import javax.faces.component.UIComponent;

import java.util.Map;

import org.apache.myfaces.trinidad.util.ArrayMap;

/**
 * HashMap used for storing facets.
 *
 */
class FacetHashMap extends ArrayMap<String, UIComponent>
{
  public FacetHashMap(UIComponent parent)
  {
    super(0, 5);
    _parent = parent;
  }

  @Override
  public void clear()
  {
    for(UIComponent value : values())
    {
      value.setParent(null);
    }

    super.clear();
  }
  
  @Override
  public UIComponent put(String key, UIComponent value)
  {
    if ((key == null) || (value == null))
    {
      throw new NullPointerException();
    }

    UIComponent previous = super.get(key);
    if (previous != null)
    {
      previous.setParent(null);
    }
    
    if (value.getParent() != null)
    {
      ChildArrayList.__removeFromParent(value, -1);
    }
    
    // calling setParent triggers an addEvent, which might have listeners, 
    // so first put the component in the map, then set the parent
    UIComponent comp = super.put(key, value);
    value.setParent(_parent);
    return comp;
  }

  @Override
  public void putAll(Map<? extends String, ? extends UIComponent> map)
  {
    if (map == null)
    {
      throw new NullPointerException();
    }

    for(Map.Entry<? extends String, ? extends UIComponent> entry : map.entrySet())
    {
      put(entry.getKey(), entry.getValue());
    }
  }
  
  @Override
  public UIComponent remove(Object key)
  {
    UIComponent previous = super.remove(key);
    if (previous != null)
    {
      previous.setParent(null);
    }
    
    return (previous);
  }

  private UIComponent _parent;
}
