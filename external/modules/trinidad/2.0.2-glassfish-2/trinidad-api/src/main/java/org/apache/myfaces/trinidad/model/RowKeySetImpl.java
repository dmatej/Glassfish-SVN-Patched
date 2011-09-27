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
package org.apache.myfaces.trinidad.model;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Implements a set of rowKeys. This set is connected with a CollectionModel
 * and is actually a subset of the rowKeys contained by the CollectionModel.
 * This set is used to group together rows that have a common UI property.
 * For example, all the rows that are currently selected by a user might be
 * placed in a single RowKeySet.
 * <p>
 * This class has very efficient implementations for addAll, clear and
 * invertAll.
 */
public final class RowKeySetImpl extends RowKeySet implements Externalizable
{
  /**
   * Creates an initially empty RowKeySet.
   */
  public RowKeySetImpl()
  {
    this(false);
  }

  /**
   * Creates a new RowKeySet.
   * @param addAll whether to add every rowKey to this set.
   */
  public RowKeySetImpl(boolean addAll)
  {
    _default = addAll;
    _set = Collections.emptySet();
    _model = null;
  }

  /**
   * Checks to see the current rowKey is contained by this set.
   * @return true if this set contains the current rowKey
   */
  @Override
  public boolean contains(Object rowKey)
  {
    return _isSelected(rowKey);
  }

  /**
   * Adds the current rowKey to this set.
   * @return true if this set changed
   */
  @Override
  public boolean add(Object rowKey)
  {
    return _setSelected(rowKey, true);
  }

  /**
   * Removes the current rowKey from this set.
   * @return true if this set changed
   */
  @Override
  public boolean remove(Object rowKey)
  {
    return _setSelected(rowKey, false);
  }

  /**
   * Adds the current rowKey to this set if it doesn't already exist, removes
   * it otherwise.
   * @return true if the row is now added. false otherwise.
   */
  @Override
  public boolean invert(Object rowKey)
  {
    Set<Object> set = _getSet(true);
    if (!set.add(rowKey))
    {
      set.remove(rowKey);
      return _default;
    }
    else
    {
      return !_default;
    }
  }

  /**
   * Inverts this set.
   * All of the contents of this set will be removed. Then rowKeys that were
   * not previously in this set will be added.
   * This method executes in constant time.
   */
  @Override
  public void invertAll()
  {
    _default = !_default;
  }

  /**
   * Adds every rowKey to this set.
   * This method executes in constant time.
   */
  @Override
  public void addAll()
  {
    _selectAll(true);
  }

  @Override
  public boolean isContainedByDefault()
  {
    return _default;
  }

  /**
   * Removes every rowKey from this set.
   * This method executes in constant time.
   */
  @Override
  public void clear()
  {
    _selectAll(false);
  }

  @Override
  public boolean removeAll(Collection<?> c)
  {
    if (c instanceof RowKeySetImpl)
    {
      RowKeySetImpl other = (RowKeySetImpl) c;
      if (other._default)
      {
        // the other Set has all keys added by default. It will be too
        // costly to use the default removeAll implementation.
        // use an optimized one here:
        return _processAll(other, false);
      }
    }
    return super.removeAll(c);
  }

  @Override
  public boolean addAll(Collection<? extends Object> c)
  {
    if (c instanceof RowKeySetImpl)
    {
      RowKeySetImpl other = (RowKeySetImpl) c;
      if (other._default)
      {
        // the other Set has all keys added by default. It will be too
        // costly to use the default addAll implementation.
        // use an optimized one here:
        return _processAll(other, true);
      }
    }
    return super.addAll(c);
  }

  private boolean _processAll(RowKeySetImpl other, boolean addAll)
  {
    Set<Object> set = _getSet(false);
    Set<Object> otherSet = other._getSet(false);
    if (_default == addAll)
    {
      // This Set already uses the correct default state. So all we have to do
      // is make sure the Set-of-deltas on this Set is correctly synchronized with
      // the Set-of-deltas on the other Set:

      /* There are two cases that fall into this group:
         1) A.addAll(B) where
              A = {all except X,Y,Z}
              B = {all except W,X,Y}
            result: A = {all except X,Y}

         2) A.removeAll(B) where
              A = {X,Y,Z}
              B = {all except W,X,Y}
            result: A = {X,Y}
      */
      return set.retainAll(otherSet);
    }
    else
    {
      /* There are two cases that fall into this group:
         3) A.addAll(B) where
              A = {X,Y,Z}
              B = {all except W,X,Y}
            result: A = {all except W}

         4) A.removeAll(B) where
              A = {all except X,Y,Z}
              B = {all except W,X,Y}
            result: A = {W}
      */

      // Make sure this Set uses the correct default state:
      _default = addAll;
      // and then the set-of-deltas on the other Set become
      // the set-of-deltas for this Set; so clone it so that we can reuse it:
      otherSet = _clone(otherSet);
      // however, we need to synchronize the two sets-of-deltas:
      otherSet.removeAll(set);
      _set = otherSet;
      return true;
    }
  }

  /**
   * Changes the underlying CollectionModel being used by this set.
   * The current rowKey (that is used by some of the methods in this class)
   * is obtained from this CollectionModel.
   * <P>
   * Users typically do not need to call this method.
   * This method is called by component writers who need to set the models
   * used by their components on this set.
   */
  @Override
  public final void setCollectionModel(CollectionModel model)
  {
    _model = model;
    if (model == null)
    {
      // use a fine warning as null is used to clear the collection model during the
      // component's state being saved
      _LOG.fine("COLLECTIONMODEL_SET_NULL");
    }
  }

  /**
   * Gets the number of rowKeys in this set (if known).
   * @return -1 if the number of rowKeys is unknown.
   */
  @Override
  public int getSize()
  {
    return _getSize(false);
  }

  @Override
  public int size()
  {
    return _getSize(true);
  }

  @Override
  public boolean isEmpty()
  {
    return (getSize() == 0);
  }

  /**
   * Gets an iteration of all the rowKeys contained in this Set.
   * @return each entry is a rowKey.
   * The CollectionModel and this Set should not be mutated while the
   * iterator is being used.
   */
   @Override
  public Iterator<Object> iterator()
   {
     return _default ? _getNotInSetRowKeyIterator() : _getInSetRowKeyIterator();
   }

  /**
   * Gets the number of rowKeys in this set.
   * @param fetchAll if true, this method will exhaustively figure out
   * the number of rowKeys in this set, even when the total number of
   * rowKeys is not known by the underlying CollectionModel.
   * @return the total number of rowKeys in this set. If fetchAll is false,
   * this might return -1 if the number of rowKeys is not known.
   */
  private int _getSize(boolean fetchAll)
  {
    int setSize = _getSet(false).size();
    if (_default)
    {
      CollectionModel model = getCollectionModel();
      // if the default is to addAll then we need to subtract the setSize from
      // the total size:
      int total = model.getRowCount();
      if (total < 0)
      {
        if (fetchAll)
        {
          // should we cache the return value?
          // I don't think so because once the size is known the underlying
          // CollectionModel should cache it. If we try to cache it here, then
          // we won't know when to update it.
          total = ModelUtils.getRowCount(model);
        }
        else
          return -1;
      }
      return total - setSize;
    }
    return setSize;
  }

  /**
   * Sets whether or not the given rowKey is added to this set.
   * @param isSelected true if the item is to be added
   * @param rowKey the rowKey of the item.
   * @return true if this set changed
   */
  private boolean _setSelected(Object rowKey, boolean isSelected)
  {
    if (isSelected == _default)
    {
      if (!_set.isEmpty()) // _set != Collections.emptySet()
      {
        return _set.remove(rowKey);
      }
      return false;
    }
    else
    {
      return _getSet(true).add(rowKey);
    }
  }

  @SuppressWarnings("unchecked")
  private Iterator<Object> _getNotInSetRowKeyIterator()
  {
    CollectionModel table = getCollectionModel();
    final Iterator<Object> rowKeyIterator = ModelUtils.getRowKeyIterator(table);
    final Set<Object> set = _getSet(false);
    Iterator<Object> iter = new Iterator<Object>()
    {
      public Object next()
      {
        if (!hasNext())
          throw new NoSuchElementException();
        _current = _next;
        _next = _next();
        _first = false;
        return _current;
      }

      public void remove()
      {
        if (_current == null)
          throw new IllegalStateException(_LOG.getMessage(
            "NO_ELEMENT_TO_REMOVE"));
        Set<Object> mutable = _getSet(true);
        // since this is the not-in-set iterator, we "remove" the element
        // by adding it to the Set:
        mutable.add(_current);
        _current = null;
      }

      public boolean hasNext()
      {
        return (_next != null || _first);
      }

      private Object _next()
      {
        while(rowKeyIterator.hasNext())
        {
          Object rowKey = rowKeyIterator.next();
          if (!set.contains(rowKey))
            return rowKey;
        }
        return null;
      }

      private boolean _first = true;
      private Object _next = null;
      private Object _current = null;
    };

    iter.next(); // initialize;
    return iter;
  }

  private Iterator<Object> _getInSetRowKeyIterator()
  {
    return _getSet(false).iterator();
  }

  private void _selectAll(boolean isSelected)
  {
    _default = isSelected;
    _set = Collections.emptySet();
  }

  private Set<Object> _getSet(boolean create)
  {
    if (create && (_set == Collections.emptySet()))
    {
      _set = _createSet(10);
    }
    return _set;
  }

  private Set<Object> _createSet(int sz)
  {
    // must be cloneable:
    return new HashSet<Object>(sz);
  }

  private boolean _isSelected(Object rowKey)
  {
    Set<Object> set = _getSet(false);
    boolean isInSet = set.contains(rowKey);
    return isInSet ^ _default;
  }

  // see java.io.Externalizable
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeBoolean(_default);
    Set<Object> set = _getSet(false);
    int sz = set.size();
    out.writeInt(sz);
    Iterator<Object> iter = set.iterator();
    for(int i=0; i<sz; i++)
    {
      out.writeObject(iter.next());
    }
  }

  // see java.io.Externalizable
  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    _default = in.readBoolean();
    int sz = in.readInt();
    if (sz>0)
    {
      _set = _createSet(sz);
      for(int i=0; i<sz; i++)
      {
        _set.add(in.readObject());
      }
    }
    else
      _set = Collections.emptySet();
  }

  /**
   * Creates a shallow clone of this RowKeySet.
   * Keys may be added or removed from the clone without affecting
   * this instance.
   */
  @Override
  public RowKeySetImpl clone()
  {
    RowKeySetImpl clone = (RowKeySetImpl) super.clone();
    Set<Object> set = _getSet(false);
    clone._set = _clone(set);
    return clone;
  }

  /**
   * Returns a clone of the given Set.
   * The clone is mutable only if the given Set is not empty.
   * If the other Set is empty, then the clone is immutable
   * (although the remove, removeAll and retainAll) methods will still work.
   */
  @SuppressWarnings("unchecked")
  private <T> Set<T> _clone(Set<T> other)
  {
    if (other.isEmpty())
      return Collections.emptySet();
    else
      return (Set<T>) ((HashSet<T>) other).clone();
  }

  /**
   * Gets the CollectionModel associated with this set.
   * The current rowKey (that is used by some of the methods in this class)
   * is obtained from this CollectionModel.
   */
  @Override
  protected CollectionModel getCollectionModel()
  {
    // This code used to contain an assertion that the collection model
    // was non-null - but a null collection model is a perfectly
    // legitimate state.  Users of a row key set might want to assert
    // the collection model is non-null.
    return _model;
  }


  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RowKeySetImpl.class);

  private boolean _default;
  private Set<Object> _set;
  private transient CollectionModel _model;
  private static final long serialVersionUID = 1L;
}
