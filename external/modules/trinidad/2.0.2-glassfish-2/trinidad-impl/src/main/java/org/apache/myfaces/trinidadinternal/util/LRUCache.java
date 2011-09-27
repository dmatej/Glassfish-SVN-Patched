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
package org.apache.myfaces.trinidadinternal.util;

import java.util.LinkedHashMap;
import java.util.Map;


import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A VERY simple LRU cache.
 * <p>
 */
public class LRUCache<K, V> extends LinkedHashMap<K, V>
{
  public LRUCache(int maxSize)
  {
    super(16, 0.75f, true /*prioritize by access order*/);
    _maxSize = maxSize;
  }

  protected void removing(K key)
  {
  }

  @Override
  protected boolean removeEldestEntry(Map.Entry<K, V> eldest)
  {
    if (size() > _maxSize)
    {
      K key = eldest.getKey();
      removing(key);

      _LOG.finer("Discarding cached value for key {0}", key);

      return true;
    }
    return false;
  }

  private final int _maxSize;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(LRUCache.class);

  private static final long serialVersionUID = 1L;
}
