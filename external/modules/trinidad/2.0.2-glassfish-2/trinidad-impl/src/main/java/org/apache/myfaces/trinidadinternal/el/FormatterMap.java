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
package org.apache.myfaces.trinidadinternal.el;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidadinternal.share.util.FastMessageFormat;

/**
 * Map implementation that provides a functor for message formatting.
 * <p>
 */
public class FormatterMap extends AbstractMap<Object, Map<Object,String>>
{
  static public Map<Object, Map<Object,String>> sharedInstance()
  {
    return _INSTANCE;
  }

  private FormatterMap()
  {
  }

  @Override
  public Map<Object,String> get(Object key)
  {
    if (key == null)
      return Collections.emptyMap();

    return new FunctorMap(key);
  }

  @Override
  public Set<Map.Entry<Object, Map<Object,String>>> entrySet()
  {
    return Collections.emptySet();
  }

  static private final class FunctorMap extends AbstractMap<Object, String>
  {
    public FunctorMap(Object key)
    {
      // Assumes check against null above
      _format = new FastMessageFormat(key.toString());
    }

    @Override
    public String get(Object key)
    {
      return _format.format(new Object[]{key});
    }

    @Override
    public Set<Map.Entry<Object, String>> entrySet()
    {
      return Collections.emptySet();
    }
    
    private final FastMessageFormat _format;
  }

  
  static private final Map<Object, Map<Object,String>> _INSTANCE = new FormatterMap();
}
