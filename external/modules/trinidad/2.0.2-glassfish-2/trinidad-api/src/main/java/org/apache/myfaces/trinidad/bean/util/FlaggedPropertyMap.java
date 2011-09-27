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
package org.apache.myfaces.trinidad.bean.util;

import java.util.AbstractMap;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.PropertyMap;


public class FlaggedPropertyMap extends AbstractMap<PropertyKey,Object>
                                implements PropertyMap
{
  @Override
  public boolean containsKey(
    Object key)
  {
    PropertyKey pKey = (PropertyKey) key;
    int index = pKey.getIndex();

    if (index >= 0 && index < 64)
    {
      // if we don't have the property, don't search the map
      return ((_flags & (1L << index)) == 0);
    }

    Map<PropertyKey, Object> map = getPropertyMap(false);
    return (map != null) ? map.containsKey(pKey) : false;
  }

  @Override
  public Object get(
    Object key)
  {
    PropertyKey pKey = (PropertyKey) key;
    int index = pKey.getIndex();

    if (index >= 0 && index < 64)
    {
      // if we don't have the property, don't search the map
      if ((_flags & (1L << index)) == 0)
      {
        return null;
      }
    }

    Map<PropertyKey, Object> map = getPropertyMap(false);
    return (map != null) ? map.get(key) : null;
  }

  @Override
  public Object put(
    PropertyKey key,
    Object      value)
  {
    int index = key.getIndex();

    if (index >= 0 && index < 64)
    {
      // set the property mask
      _flags |= (1L << index);
    }

    Map<PropertyKey, Object> map = getPropertyMap(true);
    return map.put(key, value);
  }

  @Override
  public Object remove(
    Object key)
  {
    PropertyKey pKey = (PropertyKey) key;
    int index = pKey.getIndex();

    if (index >= 0 && index < 64)
    {
      long propertyMask = (1L << index);

      // if we don't have the property, don't search the map
      if ((_flags & propertyMask) == 0)
      {
        return null;
      }

      // clear the property mask
      _flags &= ~propertyMask;
    }

    Map<PropertyKey, Object> map = getPropertyMap(false);
    return (map != null) ? map.remove(key) : null;
  }

  @Override
  public void putAll(Map<? extends PropertyKey, ? extends Object> t)
  {
    Iterator<? extends PropertyKey> iter = t.keySet().iterator();

    PropertyKey key;

    while(iter.hasNext())
    {
      key = iter.next();
      int index = key.getIndex();

      if (index >= 0 && index < 64)
      {
        long propertyMask = (1L << index);
        // set the property mask
        _flags |= propertyMask;
      }
    }

    Map<PropertyKey, Object> map = getPropertyMap(true);
    map.putAll(t);
  }

  @Override
  public Set<Map.Entry<PropertyKey, Object>> entrySet()
  {
    Map<PropertyKey, Object> map = getPropertyMap(false);
    if ((map == null) || map.isEmpty())
      return Collections.emptySet();

    return map.entrySet();
  }

  // TODO Optimize to take advantage of flags.
  @Override
  public Set<PropertyKey> keySet()
  {
    Map<PropertyKey, Object> map = getPropertyMap(false);
    if ((map == null) || map.isEmpty())
      return Collections.emptySet();

    return map.keySet();
  }

  @Override
  public Collection<Object> values()
  {
    Map<PropertyKey, Object> map = getPropertyMap(false);
    if ((map == null) || map.isEmpty())
      return Collections.emptySet();

    return map.values();
  }

  public void markInitialState()
  {
    PropertyMap map = getPropertyMap(false);
    if (map != null)
      map.markInitialState();
  }

  public void clearInitialState()
  {
    PropertyMap map = getPropertyMap(false);
    if (map != null)
      map.clearInitialState();
  }

  public boolean initialStateMarked()
  {
    PropertyMap map = getPropertyMap(false);

    if (map != null)
      return map.initialStateMarked();

    // TODO gcrawford - do something better?
    return false;
  }

  public Object saveState(FacesContext context)
  {
    PropertyMap map = getPropertyMap(false);
    if (map != null)
      return map.saveState(context);
    return null;
  }

  public void restoreState(
    FacesContext context,
    FacesBean.Type type,
    Object state)
  {
    // We have to go through ourselves to restore state so that the
    // flags will be set up correctly
    StateUtils.restoreState(this, context, type, state, getUseStateHolder());
  }

  public boolean getUseStateHolder()
  {
    return _useStateHolder;
  }

  public void setUseStateHolder(boolean useStateHolder)
  {
    _useStateHolder = useStateHolder;
  }
  
  /**
   * Sets the the FacesBean type used by this map's owner bean
   * @param type FacesBean type
   */
  public void setType(FacesBean.Type type)
  {
    _type = type;
  }
  
  /**
   * Retrieves FacesBean type used by this map's owner bean
   * @return FacesBean type
   */
  protected FacesBean.Type getType()
  {
    return _type;
  }

  protected PropertyMap getPropertyMap(
    boolean createIfNull)
  {
    PropertyMap map = _map;

    if (map == null && createIfNull)
    {
      map = _map = createMap();
    }

    return map;
  }

  protected PropertyMap createMap()
  {
    PropertyHashMap map = new PropertyHashMap();
    map.setUseStateHolder(getUseStateHolder());
    map.setType(_type);
    return map;
  }

  private PropertyMap    _map;
  private long           _flags;
  private boolean        _useStateHolder;
  private FacesBean.Type _type;
}
