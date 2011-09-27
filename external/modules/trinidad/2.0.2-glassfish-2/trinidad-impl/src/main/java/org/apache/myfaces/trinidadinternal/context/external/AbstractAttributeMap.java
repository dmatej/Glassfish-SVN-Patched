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

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * Helper Map implementation for use with different Attribute Maps.
 * This was origionally taken from MyFaces.
 *
 * @version $Revision$ $Date$
 */
abstract class AbstractAttributeMap<K, V> implements Map<K, V>
{
  public void clear()
  {
    throw new UnsupportedOperationException();
  }

  public boolean containsKey(final Object key)
  {
    return getAttribute(key) != null;
  }

  public boolean containsValue(final Object findValue)
  {
    if (findValue == null)
    {
      return false;
    }

    for (final Enumeration<K> e = getAttributeNames(); e.hasMoreElements();)
    {
      final Object value = getAttribute(e.nextElement());
      if (findValue.equals(value))
      {
        return true;
      }
    }

    return false;
  }

  public Set<Map.Entry<K, V>> entrySet()
  {
    return _entrySet != null ? _entrySet : (_entrySet = new EntrySet());
  }

  public V get(final Object key)
  {
    return getAttribute(key.toString());
  }

  public boolean isEmpty()
  {
    return !getAttributeNames().hasMoreElements();
  }

  public Set<K> keySet()
  {
    return _keySet != null ? _keySet : (_keySet = new KeySet());
  }

  public V put(final K key, final V value)
  {
    throw new UnsupportedOperationException();
  }

  public void putAll(final Map<? extends K, ? extends V> t)
  {
    throw new UnsupportedOperationException();
  }

  public V remove(final Object key)
  {
    throw new UnsupportedOperationException();
  }

  public int size()
  {
    int size = 0;
    for (final Enumeration<K> e = getAttributeNames(); e.hasMoreElements();)
    {
      size++;
      e.nextElement();
    }
    return size;
  }

  public Collection<V> values()
  {
    return _values != null ? _values : (_values = new Values());
  }

  abstract protected V getAttribute(Object key);

  abstract protected Enumeration<K> getAttributeNames();

  private Set<Map.Entry<K, V>> _entrySet;

  private Set<K>               _keySet;

  private Collection<V>        _values;

  private abstract class BaseAbstractIterator<T> implements Iterator<T>
  {
    public boolean hasNext()
    {
      return _e.hasMoreElements();
    }

    public void remove()
    {
      // remove() may cause ConcurrentModificationException.
      // We could throw an exception here, but not throwing an exception
      //   allows one call to remove() to succeed
      if (_currentKey == null)
      {
        throw new NoSuchElementException();
      }
      AbstractAttributeMap.this.remove(_currentKey);
    }

    protected void advance()
    {
      _currentKey = _e.nextElement();
    }

    protected K                    _currentKey;

    protected final Enumeration<K> _e = getAttributeNames();
  }

  private abstract class BaseAbstractSet<T> extends AbstractSet<T>
  {
    @Override
    public void clear()
    {
      AbstractAttributeMap.this.clear();
    }

    @Override
    public boolean isEmpty()
    {
      return AbstractAttributeMap.this.isEmpty();
    }

    @Override
    public int size()
    {
      return AbstractAttributeMap.this.size();
    }
  }

  /**
   * Not very efficient since it generates a new instance of <code>Entry</code>
   * for each element and still internaly uses the <code>KeyIterator</code>.
   * It is more efficient to use the <code>KeyIterator</code> directly.
   */
  private class EntryIterator extends BaseAbstractIterator<Map.Entry<K, V>>
  {
    public Map.Entry<K, V> next()
    {
      advance();
      // Must create new Entry every time--value of the entry must stay
      // linked to the same attribute name
      return new EntrySetEntry(_currentKey);
    }
  }

  private class EntrySet extends BaseAbstractSet<Map.Entry<K, V>>
  {
    @Override
    public boolean contains(final Object o)
    {
      if (!(o instanceof Entry))
      {
        return false;
      }

      final Entry<?, ?> entry = (Entry<?, ?>) o;
      final Object key = entry.getKey();
      final Object value = entry.getValue();
      if (key == null || value == null)
      {
        return false;
      }

      return value.equals(AbstractAttributeMap.this.get(key));
    }

    @Override
    public Iterator<Map.Entry<K, V>> iterator()
    {
      return new EntryIterator();
    }

    @Override
    public boolean remove(final Object o)
    {
      if (!(o instanceof Entry))
      {
        return false;
      }

      final Entry<?, ?> entry = (Entry<?, ?>) o;
      final Object key = entry.getKey();
      final Object value = entry.getValue();
      if (key == null || value == null || !value.equals(AbstractAttributeMap.this.get(key)))
      {
        return false;
      }

      return AbstractAttributeMap.this.remove(((Entry) o).getKey()) != null;
    }
  }

  private class EntrySetEntry implements Map.Entry<K, V>
  {
    public EntrySetEntry(final K currentKey)
    {
      _currentKey = currentKey;
    }

    public K getKey()
    {
      return _currentKey;
    }

    public V getValue()
    {
      return AbstractAttributeMap.this.get(_currentKey);
    }

    public V setValue(final V value)
    {
      return AbstractAttributeMap.this.put(_currentKey, value);
    }

    private final K _currentKey;
  }

  private class KeyIterator extends BaseAbstractIterator<K>
  {
    public K next()
    {
      advance();
      return _currentKey;
    }
  }

  private class KeySet extends BaseAbstractSet<K>
  {
    @Override
    public boolean contains(final Object o)
    {
      return AbstractAttributeMap.this.containsKey(o);
    }

    @Override
    public Iterator<K> iterator()
    {
      return new KeyIterator();
    }

    @Override
    public boolean remove(final Object o)
    {
      return AbstractAttributeMap.this.remove(o) != null;
    }

  }

  private class Values extends BaseAbstractSet<V>
  {
    @Override
    public boolean contains(final Object o)
    {
      return AbstractAttributeMap.this.containsValue(o);
    }

    @Override
    public Iterator<V> iterator()
    {
      return new ValuesIterator();
    }

    @Override
    public boolean remove(final Object o)
    {
      if (o == null)
      {
        return false;
      }

      for (final Iterator<V> it = iterator(); it.hasNext();)
      {
        if (o.equals(it.next()))
        {
          it.remove();
          return true;
        }
      }

      return false;
    }
  }

  private class ValuesIterator extends BaseAbstractIterator<V>
  {
    public V next()
    {
      advance();
      return AbstractAttributeMap.this.get(_currentKey);
    }
  }
}
