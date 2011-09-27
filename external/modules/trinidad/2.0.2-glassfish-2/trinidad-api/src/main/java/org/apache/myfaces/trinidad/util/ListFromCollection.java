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
package org.apache.myfaces.trinidad.util;

import java.util.AbstractMap;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.RandomAccess;
import java.util.Set;

/**
 * <p>
 * Bean that can dynamically produce Lists from Collections.
 * The Collections must implement size().  Create an instance
 * of this bean as a managed bean:
 * </p>
 * <h3>Example:</h3>
 * <pre>
 *  &lt;managed-bean&gt;
 *    &lt;managed-bean-name&gt;makeList&lt;/managed-bean-name&gt;
 *    &lt;managed-bean-class&gt;
 *       org.apache.myfaces.trinidad.util.ListFromCollection
 *    &lt;/managed-bean-class&gt;
 *    &lt;managed-bean-scope&gt;
 *       request
 *    &lt;/managed-bean-scope&gt;
 *    &lt;!-- Let's buffer 25 rows at a time (the default is 50) --&gt;
 *    &lt;managed-property&gt;
 *      &lt;property-name&gt;size&lt;/property-name&gt;
 *      &lt;value&gt;25&lt;/value&gt;
 *    &lt;managed-property&gt;
 *  &lt;/managed-bean&gt;
 * </pre>
 * <pre>
 *   &lt;h:dataTable value="#{makeList.list[someSet]}&gt;
 *   &lt;/h:dataTable&gt;
 * </pre>
 * <p>
 * Note, though, that it is extremely expensive to use this bean for
 * the items of an tr:forEach (or c:forEach in JSF 1.2 or Facelets).
 * </p>
 */
public class ListFromCollection
{
  public ListFromCollection()
  {
    _map = new MakeList();
    _size = _DEFAULT_SIZE;
  }

  public Map<Collection<?>, List<?>> getList()
  {
    return _map;
  }

  public int getSize()
  {
    return _size;
  }
  
  public void setSize(int size)
  {
    _size = size;
  }

  private class MakeList extends AbstractMap<Collection<?>, List<?>>
  {
    @Override
    public List<?> get(Object o)
    {
      if (!(o instanceof Collection))
        return null;
      
      // Just send RandomAccess lists out;  wrap any other Collection
      // into a List
      if ((o instanceof List) &&
          (o instanceof RandomAccess))
        return (List) o;

      Collection<?> c = (Collection) o;
      if (c.isEmpty())
        return Collections.emptyList();

      return new ListImpl(c, getSize());
    }

    @Override
    public Set<Map.Entry<Collection<?>, List<?>>> entrySet()
    {
      // Not worth implementing at the moment;  this Map is only
      // accessed from 
      return Collections.emptySet();
    }
  }

  private static class ListImpl extends AbstractList<Object>
  {
    public ListImpl(Collection<?> c, int size)
    {
      _c = c;
      _cSize = c.size();
      if (size == 0)
        _bufferSize = _cSize;
      else
        _bufferSize = Math.min(size, _cSize);

      _buffer = new ArrayList<Object>(_bufferSize);
      _offset = -1;
    }

    @Override
    public int size()
    {
      return _cSize;
    }

    @Override
    public Object get(int index)
    {
      if ((index < 0) || (index >= _cSize))
        throw new IndexOutOfBoundsException();

      int offset = (index / _bufferSize) * _bufferSize;
      if (offset != _offset)
      {
        _loadBuffer(offset);
        _offset = offset;
      }

      return _buffer.get(index - _offset);
    }

    private void _loadBuffer(int offset)
    {
      Iterator<? extends Object> iter = _c.iterator();
      int i = 0;

      while (i < offset)
      {
        assert iter.hasNext();
        iter.next();
        i++;
      }

      _buffer.clear();

      int count = 0;
      while ((count < _bufferSize) && (i < _cSize))
      {
        assert iter.hasNext();
        _buffer.add(iter.next());
        i++;
        count++;
      }
    }

    private final Collection<? extends Object> _c;
    private final int         _bufferSize;
    private final int         _cSize;
    private int               _offset;
    private ArrayList<Object> _buffer;
  }

  private Map<Collection<?>, List<?>> _map;
  private int                   _size;

  static private int _DEFAULT_SIZE = 50;
}
