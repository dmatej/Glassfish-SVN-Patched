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
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.util.CollectionUtils;

/**
 * Map implementation that exposes the properties of a FacesBean
 * as a Map.  This Map supports Iterator.remove(), treats
 * putting a null value as identical to removing the value,
 * but does not support null keys.  The keys may be either
 * Strings or {@link PropertyKey}s, but all Map.Entry objects
 * will return String keys.
 *
 */
public class ValueMap extends AbstractMap<String, Object>
{
  public ValueMap(FacesBean bean)
  {
    _bean = bean;
  }

  @Override
  public Object get(Object key)
  {
    if (key == null)
      throw new NullPointerException();

    PropertyKey propertyKey = _getPropertyKey(key);
    // Support attribute transparency for list-based
    // properties
    if (propertyKey.isList())
    {
      Class<?> type = propertyKey.getType();
      if (type.isArray())
        type = type.getComponentType();

      return _bean.getEntries(propertyKey, type);
    }
    else
    {
      Object val = _bean.getProperty(propertyKey);
      return (val != null) ? val : propertyKey.getDefault();
    }
  }

  @Override
  public Object put(String key, Object value)
  {
    if (key == null)
      throw new NullPointerException();

    return _putInternal(_getPropertyKey(key), value);
  }
  
  // TODO Should remove just remove values, or also remove bindings?
  @Override
  public Object remove(Object key)
  {
    PropertyKey propertyKey = _getPropertyKey(key);
    Object oldValue = _bean.getProperty(propertyKey);
    _bean.setProperty(propertyKey, null);

    return oldValue;
  }

  /**
   * Override for better performance
   * @param key
   * @return
   */
  @Override
  public boolean containsKey(Object key)
  {
    if (key == null)
      throw new NullPointerException();

    PropertyKey propertyKey = _getPropertyKey(key);
    
    if (_bean.keySet().contains(propertyKey))
      return true;
    else
      return _bean.bindingKeySet().contains(propertyKey);    
  }

  @Override
  public Set<Map.Entry<String, Object>> entrySet()
  {
    return CollectionUtils.overlappingCompositeSet(
             new MakeEntries(_bean.keySet()),
             new MakeEntries(_bean.bindingKeySet()));
  }
  
  private Object _putInternal(PropertyKey propertyKey, Object value)
  {
    assert propertyKey != null;

    Object oldValue = _bean.getProperty(propertyKey);
    _bean.setProperty(propertyKey, value);

    return oldValue;
  }

  private class MakeEntries extends AbstractSet<Map.Entry<String, Object>>
  {
    public MakeEntries(Set<PropertyKey> keys)
    {
      _keys = keys;
    }

    @Override
    public int size()
    {
      return _keys.size();
    }

    @Override
    public Iterator<Map.Entry<String, Object>> iterator()
    {
      final Iterator<PropertyKey> base = _keys.iterator();
      return new Iterator<Map.Entry<String, Object>>()
      {
        public boolean hasNext()
        {
          return base.hasNext();
        }

        public Map.Entry<String, Object> next()
        {
          _lastEntry = new EntryImpl(base.next());
          return _lastEntry;
        }

        public void remove()
        {
          if (_lastEntry == null)
            throw new IllegalStateException();
          base.remove();
          _lastEntry = null;
        }
        
        private EntryImpl _lastEntry;
      };
    }

    private final Set<PropertyKey> _keys;
  }

  private class EntryImpl implements Entry<String, Object>
  {
    public EntryImpl(PropertyKey key)
    {
      assert key != null;
      
      _key = key;
    }

    public String getKey()
    {
      return _key.getName();
    }

    public Object getValue()
    {
      return get(_key);
    }

    public Object setValue(Object value)
    {
      return _putInternal(_key, value);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if (!(o instanceof Map.Entry))
        return false;

      Map.Entry<Object, Object> e = (Map.Entry<Object, Object>)o;
      Object k1 = getKey();
      Object k2 = e.getKey();
      if (k1 == k2 || (k1 != null && k1.equals(k2)))
      {
        Object v1 = getValue();
        Object v2 = e.getValue();
        if (v1 == v2 || (v1 != null && v1.equals(v2))) 
          return true;
      }

      return false;
    }
    
    @Override
    public int hashCode()
    {
      Object value = getValue();
      return _key.hashCode() ^ (value==null ? 0 : value.hashCode());
    }

    private final PropertyKey _key;
  }

  private PropertyKey _getPropertyKey(Object key)
  {
    if (key instanceof String)
    {
      String keyString = (String) key;
      PropertyKey propertyKey = _bean.getType().findKey(keyString);
      if (propertyKey == null)
        propertyKey = PropertyKey.getDefaultPropertyKey(keyString);
      
      return propertyKey;
    }
    
    if (key instanceof PropertyKey)
    {
      return (PropertyKey) key;
    }
    
    throw new ClassCastException();
  }

  private FacesBean    _bean;
}
