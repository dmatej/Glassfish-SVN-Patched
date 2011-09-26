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
package org.apache.myfaces.trinidadinternal.context.external;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;

/**
 * Helper Map implementation for use with different modifiable
 * Attribute Maps.
 * This was origionally taken from MyFaces.
 *
 * @version $Revision$ $Date$
 */

abstract class ModifiableAbstractAttributeMap<K, V> extends AbstractAttributeMap<K, V>
{
  @Override
  public void clear()
  {
    final List<K> names = new ArrayList<K>();

    for (final Enumeration<K> e = getAttributeNames(); e.hasMoreElements();)
    {
      names.add(e.nextElement());
    }

    for (final K val : names)
    {
      removeAttribute(val);
    }
  }

  @Override
  public V put(final K key, final V value)
  {
    final V retval = getAttribute(key);
    setAttribute(key, value);
    return retval;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void putAll(final Map<? extends K, ? extends V> t)
  {

    for (final Map.Entry entry : t.entrySet())
    {
      setAttribute((K) entry.getKey(), (V) entry.getValue());
    }
  }

  @Override
  public V remove(final Object key)
  {
    final V retval = getAttribute(key);
    removeAttribute(key);
    return retval;
  }

  protected abstract void removeAttribute(Object arg0);

  protected abstract void setAttribute(K arg0, V arg1);
}
