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

import java.util.Iterator;

/** not thread safe, though it could be as thread-safe as the underlying Iterators at a slight cost to performance and size */
public final class CompositeIterator<T> implements Iterator<T>
{
  public CompositeIterator(Iterator<T> firstIterator, Iterator<T> secondIterator)
  {
    if (firstIterator == null)
      throw new NullPointerException();

    if (secondIterator == null)
      throw new NullPointerException();

    // determine whether we have another element aggressively
    _hasNext = firstIterator.hasNext();

    if (_hasNext)
    {
      _firstIterator = firstIterator;
      _secondIterator = secondIterator;
    }
    else
    {
      // first iterator is empty so shift up the second iterator
      _firstIterator = secondIterator;
      _secondIterator = null;
      _hasNext = secondIterator.hasNext();
    }
  }

  public boolean hasNext()
  {
    // return cached value
    return _hasNext;
  }

  public T next()
  {
    // remove is only valid after next(), so assign it here.
    // This also deals with issue where next() is called on the
    // last element of the firstIterator and then remove is called
    // this way the removeIterator correctly lags shifting from the
    // old _firstIterator to the _secondIterator
    _removeIterator = _firstIterator;

    T nextValue = _firstIterator.next();
    
    _hasNext = _firstIterator.hasNext();

    if (!_hasNext && (_secondIterator != null))
    {
      // firsIterator is done, so shift up the secondIterator
      _firstIterator = _secondIterator;
      _secondIterator = null;
      _hasNext = _firstIterator.hasNext();
    }

    return nextValue;
  }

  public void remove()
  {
    if (_removeIterator == null)
      throw new IllegalStateException();

    _removeIterator.remove();
  }

  private Iterator<T> _firstIterator;
  private Iterator<T> _secondIterator;
  private Iterator<T> _removeIterator;
  private boolean _hasNext;
}
