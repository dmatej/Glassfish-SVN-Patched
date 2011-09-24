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

import java.io.Externalizable;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.io.ObjectOutput;
import java.io.IOException;
import java.io.ObjectInput;

/**
 * This class implements a Map whose values can themselves be used to obtain
 * the corresponding keys.
 * 
 * it is illegal for two keys to be paired with the same value. 
 * 
 */
final class ValueMap<K, V> extends AbstractMap<K, V> implements Externalizable 
{
  public ValueMap()
  {
    // noarg constructor needed by the Externalizable interface
    this(13);
  }

  public ValueMap(int initialCapacity)
  {
    _cache = new HashMap<K, V>(initialCapacity);
    _valueMap = new HashMap<V, K>(initialCapacity);
  }

  /**
   * Gets the value associated with the given key
   */
  @Override
  public V get(Object key)
  {
    return _cache.get(key);
  }
  
  /**
   * Gets the key associated with the given value
   */
  public K getKey(V value)
  {
    return _valueMap.get(value);
  }
  
  @Override
  public V put(K key, V value)
  {
    K oldKey = _valueMap.put(value, key);
    assert oldKey == null : "value:"+value+" is referenced by both key:"+key+
      " and key:"+oldKey;                          

    V old = _cache.put(key, value);
    assert old == null : "can't put the same key twice";
    return old;
  }
    
  /**
   * Removes the mapping for the specified key. Also
   * removes the corresponding value mapping.
   * @param key
   * @return
   */
  @Override
  public V remove(Object key) 
  {
    V value = _cache.remove(key);
    
    if (value != null)
    {
      K oldKey = _valueMap.remove(value);
      assert oldKey != null : "missing key in value map for value "+value;
    }
    return value;
  }
    
  @Override
  public void clear()
  {
    _cache.clear();
    _valueMap.clear();
  }
  
  @Override
  public int size()
  {
    return _cache.size();
  }
  
  @Override
  public Set<Map.Entry<K, V>> entrySet()
  {
    return Collections.unmodifiableSet(_cache.entrySet());
  }
  
  private static <K,V> Map<V, K> _invertMap(Map<K, V> cache)
  {
    Map<V, K> valueMap = new HashMap<V, K>(cache.size());
    for(Map.Entry<K, V> entry : cache.entrySet())
    {
      K old = valueMap.put(entry.getValue(), entry.getKey());
      assert old == null : "the value:"+entry.getValue()+
                           " was bound to both key:"+old+
                           " and key:"+entry.getKey();
    }
    
    return valueMap;
  }
  
  public void writeExternal(ObjectOutput out) throws IOException
  {
    if (_cache.isEmpty())
      out.writeObject(null);
    else
      out.writeObject(_cache);
  }

  @SuppressWarnings("unchecked")
  public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
  {
    Map<K, V> cache = (Map<K, V>) in.readObject();
    if (cache != null)
    {
      _cache = cache;
      _valueMap = _invertMap(_cache);
    }
  }

  private Map<K, V> _cache;
  private transient Map<V, K> _valueMap;
}
