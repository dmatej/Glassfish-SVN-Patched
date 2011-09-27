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
package org.apache.myfaces.trinidadinternal.share.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import org.apache.myfaces.trinidad.util.ArrayMap;

/**
 * Implements a map between a namespace+key and a value.
 * This unsynchronized class is optimized for a small number of
 * namespaces.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/util/NamespaceMap.java#0 $) $Date: 10-nov-2005.18:59:24 $
 */
public class NamespaceMap implements Cloneable
{
  /**
   * Creates a NamespaceMap, using a default size
   * for the number of values per namespace.
   */
  public NamespaceMap()
  {
    this(11);
  }


  /**
   * Creates a NamespaceMap.
   * @param defaultSize the default size of each per-namespace
   *   storage.
   */
  public NamespaceMap(int defaultSize)
  {
    _defaultSize = defaultSize;
  }


  /**
   * associates a value with a namespace and a key
   * @param namespace namespace of the value
   * @param key the key to associate the value with
   * @param value the value to associate with the key.
   */
  public void put(String namespace, Object key, Object value)
  {
    __put(namespace, key, value);
  }

  /**
   * Returns the stored object.
   * @param namespace the namespace to search.
   * @param key the key to search the namespace for.
   * @return null if such a namespace/key does not exist. else returns the
   *  associated value.
   */
  @SuppressWarnings("unchecked")
  public Object get(String namespace, Object key)
  {
    Map<Object, Object> map = 
      (Map<Object, Object>) ArrayMap.get(_namespaces, namespace);
    
    if (map!=null)
      return map.get(key);

    return null;
  }

  /**
   * Removes a key from a namespace.
   * @param namespace the namespace to search.
   * @param key the key to search the namespace for.
   * @return the associated value, or null if the namespace/key does not exist.
   */
  @SuppressWarnings("unchecked")
  public Object remove(String namespace, Object key)
  {
    Map<Object, Object> map = 
      (Map<Object, Object>) ArrayMap.get(_namespaces, namespace);
    
    if (map!=null)
    {
      Object o = map.remove(key);
      if (map.isEmpty())
        _namespaces = ArrayMap.remove(_namespaces, namespace);
      return o;
    }

    return null;
  }

  /**
   * clears all keys from a namespace.
   * @param namespace the namespace to clear.
   */
  public void clear(String namespace)
  {
    _namespaces = ArrayMap.remove(_namespaces, namespace);
  }

  /**
   * clears all bindings for all namespaces.
   */
  public void clear()
  {
    _namespaces = null;
  }

  /**
   * Returns an Iterator over all the namespaces added to the map.
   */
  @SuppressWarnings("unchecked")
  public Iterator<Map<Object, Object>> getNamespaceIterator()
  {
    Object[] namespaces = _namespaces;
    if (namespaces == null)
      return null;
    int i = namespaces.length - 2;
    
    ArrayList<Map<Object, Object>> namespacesList = 
      new ArrayList<Map<Object, Object>>();
    
    // -= Simon Lessard =-
    // FIXME: Extremely strong coupling to with ArrayMap's internal structure.
    //        This is some bad design, this map should instead extends ArrayMap
    //        or use one as its internal state rather than Object[].
    while(i>=0)
    {
      namespacesList.add((Map<Object, Object>)namespaces[i]);
      i=i-2;
    }
    
    return namespacesList.iterator();
  }


  /**
   * Returns an Enumeration over all the values added to the map.
   */
  public Iterator<Object> getValueIterator()
  {
    return new Enum();
  }

  /**
   * Returns an Iterator over all of the values for a particular namespace.
   */
  @SuppressWarnings("unchecked")
  public Iterator<Object> getValueIterator(String namespace)
  {
    Map<Object, Object> map = 
      (Map<Object, Object>)ArrayMap.get(_namespaces, namespace);
    
    if (map == null)
      return null;

    return map.values().iterator();
  }

  /**
   * Returns an Iterator over all of the keys for a particular namespace.
   */
  @SuppressWarnings("unchecked")
  public Iterator<Object> getKeysIterator(String namespace)
  {
    Map<Object, Object> map = 
      (Map<Object, Object>)ArrayMap.get(_namespaces, namespace);
    
    if (map == null)
      return null;

    return map.keySet().iterator();
  }


  /**
   * Returns a clone of the NamespaceMap
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object clone()
  {
    NamespaceMap namespaceMap;

    try
    {
      namespaceMap = (NamespaceMap)super.clone();
    }
    catch (CloneNotSupportedException e)
    {
      // this should never happen
      throw new IllegalStateException();
    }

    //
    // clone the key/value pairs
    //
    if (_namespaces != null)
    {
      int length = _namespaces.length;
      Object[] namespaces = new Object[length];
      System.arraycopy(_namespaces, 0, namespaces, 0, length);
      // -= Simon Lessard =-
      // FIXME: Strong ArrayMap's internal functionality coupling here as well
      for (int i = 1; i < length; i += 2)
      {
        HashMap<Object, Object> hm = (HashMap<Object, Object>) namespaces[i];
        if (hm != null)
          namespaces[i] = hm.clone();
      }

      namespaceMap._namespaces = namespaces;
    }

    return namespaceMap;
  }

  @SuppressWarnings("unchecked")
  //
  // =-=AEW Package-private version, needed by BindableNamespaceMap
  // since it returns the old value.  It'd be nice to change the
  // real version, but backwards compatibility says no.
  //
  // associates a value with a namespace and a key
  // @param namespace namespace of the value
  // @param key the key to associate the value with
  // @param value the value to associate with the key.
  // @return the previous value associated with the namespace/value
  Object __put(String namespace, Object key, Object value)
  {
    if (value == null)
    {
      return remove(namespace, key);
    }
    else
    {
      Map<Object, Object> map = 
        (Map<Object, Object>) ArrayMap.get(_namespaces, namespace);
      
      if (map==null)
      {
        map = new HashMap<Object, Object>(_defaultSize);
        _namespaces = ArrayMap.put(_namespaces, namespace, map);
      }

      return map.put(key, value);
    }
  }

  //
  // Package-private accessor for namespace array;  used
  // in BindableNamespaceMap
  //
  Object[] __getNamespaces()
  {
    return _namespaces;
  }

  private Object[] _namespaces;
  private int      _defaultSize;

  //
  // Internal enumeration class
  //
  private class Enum implements Iterator<Object>
  {
    public Enum()
    {
      if (_namespaces == null)
      {
        _index = -1;
      }
      else
      {
        // Go one beyond the end, then "advance" to the
        // next iterator
        _index = _namespaces.length + 1;
        _advanceIterator();
      }
    }

    public boolean hasNext()
    {
      return (_index > 0);
    }

    public Object next()
    {
      if (!hasNext())
        throw new NoSuchElementException();

      Object next = _iterator.next();
      if (!_iterator.hasNext())
        _advanceIterator();

      return next;
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    // Advance to the next iterator (actually, we go backwards)
    @SuppressWarnings("unchecked")
    private void _advanceIterator()
    {
      int index = _index - 2;
      _index = index;
      if (index > 0)
        _iterator = ((Map<Object, Object>)_namespaces[index]).values().iterator();
    }

    // Current iterator
    private Iterator<Object> _iterator;

    // Index into the _namespaces array
    private int      _index;
  }


}
