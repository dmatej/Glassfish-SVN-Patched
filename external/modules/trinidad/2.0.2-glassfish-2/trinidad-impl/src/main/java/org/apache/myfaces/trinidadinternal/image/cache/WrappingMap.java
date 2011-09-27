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
package org.apache.myfaces.trinidadinternal.image.cache;

import java.util.Collection;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Map which wraps Map.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/WrappingMap.java#0 $) $Date: 10-nov-2005.19:06:16 $
 */
class WrappingMap<K, V> implements Map<K, V>
{
  public WrappingMap(Map<K, V> map)
  {
    _wrappedMap = map;
  }

  public int size()
  {
    return _wrappedMap.size();
  }

  public boolean isEmpty()
  {
    return _wrappedMap.isEmpty();
  }

  public Iterator<K> keys()
  {
    return _wrappedMap.keySet().iterator();
  }

  public Set<Map.Entry<K, V>> entrySet()
  {
    return _wrappedMap.entrySet();
  }

  public Set<K> keySet()
  {
    return _wrappedMap.keySet();
  }

  public void putAll(Map<? extends K, ? extends V> map)
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "PUTALL_OPERATION_NOT_SUPPORTED_FOR_WRAPPING"));
  }

  public Collection<V> values()
  {
    return _wrappedMap.values();
  }

  public boolean containsValue(Object value)
  {
    return _wrappedMap.containsValue(value);
  }

  public boolean containsKey(Object key)
  {
    return _wrappedMap.containsKey(key);
  }

  public void clear()
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "CLEAROPERATION"));
  }

  public Iterator<V> elements()
  {
    return _wrappedMap.values().iterator();
  }

  public V get(Object key)
  {
    return _wrappedMap.get(key);
  }

  public V put(K key, V value)
  {
    return _wrappedMap.put(key, value);
  }

  public V remove(Object key)
  {
    return _wrappedMap.remove(key);
  }

  protected Map<K, V> getWrappedMap()
  {
    return _wrappedMap;
  }

  private Map<K, V> _wrappedMap;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    WrappingMap.class);
}
