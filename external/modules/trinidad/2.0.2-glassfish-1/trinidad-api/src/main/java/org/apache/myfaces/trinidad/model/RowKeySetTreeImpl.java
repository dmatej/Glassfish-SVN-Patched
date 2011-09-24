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

import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.Stack;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Implements a collection of rowKeys from a TreeModel.
 * The methods on this class are optimized such that it is possible
 * to add/remove all the rowkeys in a subtree in constant time.
 * <P>
 * The generic type E is the type of a rowKey.
 */
public class RowKeySetTreeImpl extends RowKeySet implements Serializable
{
  /**
   * Creates a new Set that is initially empty.
   */
  public RowKeySetTreeImpl()
  {
    this(false);
  }

  /**
   * Creates a new Set, that may contain every rowKey by default.
   * @param addAll if this is true, every rowKey is initially added to this set.
   */
  public RowKeySetTreeImpl(boolean addAll)
  {
    _root = new Node<Object>(addAll);
  }

  /**
   * Tests to see if the given rowKey is included in this Set.
   * @return true If the rowKey is included in this Set.
   */
  @Override
  public boolean contains(Object rowKey)
  {
    return _isContained(rowKey);
  }

  /**
   * @deprecated do not use. this will be removed post Tier 1.
   */
  @Override
  @Deprecated
  public boolean isContainedByDefault()
  {
    TreeModel model = getCollectionModel();
    
    if (model != null)
    {
      Object rowkey = model.getRowKey();
      return new Search().find(rowkey).isDefaultContained;
    }
    return false;
  }

  @Override
  public Iterator<Object> iterator()
  {
    if(_root.isDefaultContained)
      return new PathIterator();
    else
      return new NodeIterator();
  }

  /**
   * Adds the given rowKey to this Set.
   * @return false if the given rowKey was already in this Set.
   * @see #remove(Object)
   * @see #addAll()
   */
  @Override
  public boolean add(Object rowKey)
  {
    return _setContained(rowKey, true);
  }

  /**
   * Removes the given rowKey from this Set.
   * @return false if the given rowKey was already not in this Set.
   * @see #add
   * @see #removeAll()
   */
  @Override
  public boolean remove(Object rowKey)
  {
    return _setContained(rowKey, false);
  }

  /**
   * Adds the current rowKey and all rowKeys beneath the current rowKey to this Set.
   * This method executes in constant time.
   * @see #add
   * @see #removeAll()
   */
  @Override
  public void addAll()
  {
    _selectAll(true);
  }

  /**
   * Removes the current rowKey and all rowKeys beneath the current rowKey to this Set.
   * This method executes in constant time.
   * @see #remove(Object)
   * @see #clear()
   * @see #addAll()
   */
  @Override
  public void removeAll()
  {
    _selectAll(false);
  }

  /**
   * {@inheritDoc}
   * <P>
   * If the parameter is another RowKeySetTreeImpl, this method is
   * optimized to give superior performance and avoid iteration.
   */
  @Override
  public boolean addAll(Collection<? extends Object> other)
  {
    if (other instanceof RowKeySetTreeImpl)
    {
      RowKeySetTreeImpl otherset = (RowKeySetTreeImpl) other;
      return _processOperation(this._root, otherset._root, true);
    }
    return super.addAll(other);
  }

  /**
   * {@inheritDoc}
   * <P>
   * If the parameter is another RowKeySetTreeImpl, this method is
   * optimized to give superior performance and avoid iteration.
   */
  @Override
  public boolean removeAll(Collection<?> other)
  {
    if (other instanceof RowKeySetTreeImpl)
    {
      RowKeySetTreeImpl otherset = (RowKeySetTreeImpl) other;
      return _processOperation(this._root, otherset._root, false);
    }
    return super.removeAll(other);
  }

  private boolean _processOperation(Node<Object> set1, Node<Object> set2, boolean add)
  {
    /*
     * setXdef = setX.isDefaultContained
     * setXdif = setX.isDifferent
     * asterisks (*) indicate changes.
     *
     * TABLE ---------------------------------------------------
     |-----------Inputs---------|--------Outputs---------------|
     | set1def | set2def | add  | removeAll | addAll | set1def |
     |    0    |    0    |  1   |     0     |   1    |    0    |
     |    0    |    1    |  1   |     1     |   1    |    1*   |
     |    1    |    0    |  1   |     0     |   0    |    1    |
     |    1    |    1    |  1   |     1     |   0    |    1    |
     |    0    |    0    |  0   |     0     |   0    |    0    |
     |    0    |    1    |  0   |     1     |   0    |    0    |
     |    1    |    0    |  0   |     0     |   1    |    1    |
     |    1    |    1    |  0   |     1     |   1    |    0*   |
     |---------------------------------------------------------|
     */

    boolean hasChanges = false;

    // See TABLE (above) 'removeAll' column:
    // if set2 contains everything, then there is no point hanging on to
    // any set1-deltas that are not in set2, so remove them:
    if (set2.isDefaultContained && set1.keySet().retainAll(set2.keySet()))
        hasChanges = true;

    // See TABLE (above) 'addAll' column:
    // this "addAll" flag controls whether to process any set2-deltas that are not
    // already in set1. If set1 has everything by default and we're adding set2,
    // then there is no point processing any set2-deltas not already in set1.
    // Similarly, if set1 has nothing by default and we're removing set2,
    // then there is no point processing any set2-deltas not already in set1.
    // So only process the set2-deltas if we're doing an add (and set1
    // does not contain everything) or we're doing a remove (and set1
    // has everything):
    boolean addAll = add ^ set1.isDefaultContained;

    for(Entry<Object, Node<Object>> en:set2.entrySet())
    {
      Object segment = en.getKey();
      Node<Object> subset2 = en.getValue();
      Node<Object> subset1 = set1.get(segment);

      if (subset1 == null)
      {
        if (addAll)
        {
          subset1 = new Node<Object>(set1, segment);
          hasChanges = true;
        }
        else
          continue;
      }
      if (_processOperation(subset1, subset2, add))
        hasChanges = true;
    }

    // See TABLE (above) 'Outputs/set1Def' column:
    // if set2 contains everything by default, then that will affect
    // the default flag of set1:
    if (set2.isDefaultContained && (set1.isDefaultContained != add))
    {
      set1.isDefaultContained = add;
      // since we toggled the default state, toggle the diff state
      // as well so that we maintain the status for this node:
      set1.isDifferent = !set1.isDifferent;
      hasChanges = true;
    }

    // if this node is contained by set2, then depending on the
    // add flag, this node should (not) be contained by set1:
    if ((set2.isDefaultContained ^ set2.isDifferent) &&
        ((set1.isDefaultContained ^ set1.isDifferent) != add))
    {
      set1.isDifferent = !set1.isDifferent;
      hasChanges = true;
    }

    return hasChanges;
  }

  /**
   * Removes all rowKeys from this Set.
   * This method executes in the same time as
   * {@link HashMap#clear()}
   */
  @Override
  public void clear()
  {
    _root.clear();
    _root.isDefaultContained = _root.isDifferent = false;
  }

  /**
   * Gets the number of elements contained by this set.
   * Does not force the underlying model to compute its size.
   * @return -1 if the number of elements is unknown.
   */
  @Override
  public int getSize()
  {
    return _getSize(null, _root, getCollectionModel(), false);
  }

  /**
   * Gets the number of elements in this Set.
   * This might force the underlying model to compute its size.
   * @return a non-negative number.
   */
  @Override
  public int size()
  {
    return _getSize(null, _root, getCollectionModel(), true);
  }

  @Override
  public boolean isEmpty()
  {
    return (getSize() == 0);
  }

  /**
   * Sets the TreeModel associated with this Set.
   * @param model This must be of type {@link TreeModel}
   */
  @Override
  public final void setCollectionModel(CollectionModel model)
  {
    if (model != null && !(model instanceof TreeModel))
      throw new IllegalArgumentException();

    _model = (TreeModel) model;
  }

  /**
   * Creates a clone of this Set. RowKeys may be added/removed from the
   * clone without affecting this instance.
   */
  @Override
  public RowKeySetTreeImpl clone()
  {
    RowKeySetTreeImpl clone = (RowKeySetTreeImpl) super.clone();
    clone._root = _root.clone();
    return clone;
  }

  /**
   * @deprecated not implemented.
   */
  @Deprecated
  @Override
  public void invertAll()
  {
    // TODO
    throw new UnsupportedOperationException();
  }

  /**
   * Gets the TreeModel associated with this set.
   * This TreeModel will be used to get the current rowKey, and also to
   * get parent rowKeys, from child rowKeys.
   * @see TreeModel#getRowKey
   */
  @Override
  protected TreeModel getCollectionModel()
  {
    return _model;
  }

  /**
   * Gets the total number of nodes in the subtree of the given TreeModel.
   *
   * WARNING: this method changes the TreeModel's currency.
   * The caller is responsible for restoring the model currency.
   *
   * @param exclusions any rowKeys present in this Set are excluded from the count.
   */
  @SuppressWarnings("unchecked")
  private int _getTreeSize(TreeModel model, Set<Object> exclusions)
  {
    int sz = 0;
    for(int i=0;true;i++)
    {
      model.setRowIndex(i);
      if (model.isRowAvailable())
      {
        Object rowkey = model.getRowKey();
        if (exclusions.contains(rowkey))
          continue;
        sz++;
        if (model.isContainer())
        {
          model.enterContainer();
          Set<Object> empty = Collections.emptySet();
          sz += _getTreeSize(model, empty);
          model.exitContainer();
        }
      }
      else
        return sz;
    }
  }

  private int _getSize(Object rowkey, Node<Object> set, TreeModel model,  boolean fetchall)
  {
    // special-case the root collection:
    int sz = ((rowkey != null) && (set.isDefaultContained ^ set.isDifferent)) ? 1 : 0;
    if (set.isDefaultContained)
    {
      if (!fetchall || model == null)
        return -1;

      Object old = model.getRowKey();
      try
      {
        model.setRowKey(rowkey);
        // special-case the root collection:
        if (rowkey == null)
        {
          sz += _getTreeSize(model, set.keySet());
        }
        else if (model.isContainer())
        {
          model.enterContainer();
          sz += _getTreeSize(model, set.keySet());
        }
      }
      finally
      {
        model.setRowKey(old);
      }
    }

    for(Entry<Object, Node<Object>> en:set.entrySet())
    {
      Object newrowkey = en.getKey();
      Node<Object> subset = en.getValue();
      int size = _getSize(newrowkey, subset, model, fetchall);
      if (size < 0)
        return -1;
      sz+= size;
    }
    return sz;
  }

  /**
   * adds or removes all the paths rooted at the current path
   * @param isSelectAll if true does an add-all. else does remove-all.
   */
  private void _selectAll(final boolean isSelectAll)
  {
    Search search = new Search()
    {
      @Override
      protected boolean create(Node<Object> parent, Object rowkey)
      {
        // if the parent does not have the correct default, then
        // we need to add entries for the children, since we need
        // to store a delta:
        return (parent.isDefaultContained != isSelectAll);
      }

      @Override
      protected Node<Object> found(Node<Object> child)
      {
        child.isDefaultContained = isSelectAll;
        child.isDifferent = false;
        child.clear();
        return null;
      }
    };

    TreeModel model = getCollectionModel();
    Object rowkey = model.getRowKey();
    search.find(rowkey);
  }

  private boolean _isContained(Object rowkey)
  {
    Search search = new Search()
    {
      @Override
      protected Node<Object> notFound(Node<Object> parent, Object rowkey)
      {
        return parent.isDefaultContained ? parent : null;
      }

      @Override
      protected Node<Object> found(Node<Object> child)
      {
        return (child.isDefaultContained ^ child.isDifferent) ? child : null;
      }
    };

    return (search.find(rowkey) != null);
  }

  /**
   * Adds or removes the given path from this set.
   * @param isContained If true, the current path is added. Otherwise,
   * it is removed.
   * @return true if this Set changed due to this operation.
   */
  private boolean _setContained(Object rowkey, final boolean isContained)
  {
    Search search = new Search()
    {
      @Override
      protected boolean create(Node<Object> parent, Object rowkey)
      {
        // only need to create child deltas, if the parent's
        // default is wrong:
        return parent.isDefaultContained != isContained;
      }

      @Override
      protected Node<Object> notFound(Node<Object> parent, Object rowkey)
      {
        return null;
      }
    };

    Node<Object> current = search.find(rowkey);
    if ((current != null) &&
        ((current.isDefaultContained ^ current.isDifferent) != isContained))
    {
      current.isDifferent = !current.isDifferent;
      return true;
    }
    return false;
  }

  /**
   * Advances the currency of the given TreeModel to the next node in a
   * depth-first walk of the tree.
   * @param minDepth the minimum depth of the rowkey. use this to
   * walk within a subtree. Use 0 to walk entire tree.
   * @param recurseChildren if true, will walk children.
   * @return true if the currency of the model was successfully advanced to
   * the next rowData.
   */
  private static boolean _advanceToNextItem(
    TreeModel model, int minDepth, boolean recurseChildren)
  {
    assert minDepth >= 0;

    if (recurseChildren && model.isRowAvailable() && model.isContainer())
    {
      model.enterContainer();
      model.setRowIndex(-1);
    }
    while(true)
    {
      int ri = model.getRowIndex();
      model.setRowIndex(ri+1);
      if (model.isRowAvailable())
        return true;

      int depth = model.getDepth();
      if (depth <= minDepth)
        return false;

      model.exitContainer();
    }
  }

  /**
   * Check for "default contained" nodes in the set
   * @return true if there are "default contained" nodes
   */
  private boolean _containsDefaultNodes()
  {
    if(_root.isDefaultContained)
      return true;

    SetLoop loop = new SetLoop()
    {
      protected boolean next(Object rowKey, Node<Object> value)
      {
        return value.isDefaultContained;
      }
    };
    return loop.run(_root);
  }

  /**
   * Utility to dump Node attributes
   */
  private void _dumpFlags()
  {
    System.out.println("root " + _root.isDefaultContained + " " + _root.isDifferent);
    SetLoop loop = new SetLoop()
    {
      protected boolean next(Object rowKey, Node<Object> value)
      {
        System.out.println(rowKey + " " + value.isDefaultContained + " " + value.isDifferent);
        return false;
      }
    };
    loop.run(_root);
  }

  // Needs to be Serializable and Cloneable - but HashMap already is
  private static final class Node<K> extends HashMap<K, Node<K>>
     /* implements Serializable, Cloneable */
  {
    public boolean isDifferent = false;
    public boolean isDefaultContained = false;

    public Node(boolean isDefaultContained)
    {
      this.isDefaultContained = isDefaultContained;
    }

    public Node(Node<K> parent, K segment)
    {
      this(parent.isDefaultContained);
      parent.put(segment, this);
    }

    // clone all the values as well:
    private void _deepClone(Node<K> root)
    {
      for(Entry<K, Node<K>> en:root.entrySet())
      {
        Node<K> original = en.getValue();
        Node<K> clone = original.clone();
        en.setValue(clone);
      }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Node<K> clone()
    {
      Node<K> clone = (Node<K>) super.clone();
      _deepClone(clone);
      return clone;
    }

    private static final long serialVersionUID = 1L;
  }

  private class Search
  {
    public Search()
    {
    }

    protected boolean create(Node<Object> parent, Object rowkey)
    {
      return false;
    }

    protected Node<Object> notFound(Node<Object> parent, Object rowkey)
    {
      return parent;
    }

    protected Node<Object> found(Node<Object> result)
    {
      return result;
    }

    public Node<Object> find(Object rowkey)
    {
      Node<Object> current = _root;
      if (rowkey != null)
      {
        TreeModel model = getCollectionModel();
        if (model == null)
        {
          return notFound(current, rowkey);
        }
        List<Object> parentkeys = model.getAllAncestorContainerRowKeys(rowkey);
        List<Object> allkeys = new ArrayList<Object>(parentkeys.size() + 1);
        allkeys.addAll(parentkeys);
        allkeys.add(rowkey);
        for(Object key:allkeys)
        {
          Node<Object> next = current.get(key);
          if (next == null)
          {
            if (create(current, key))
              next = new Node<Object>(current, key);
            else
              return notFound(current, key);
          }
          current = next;
        }
      }
      return found(current);
    }
  }

  /**
   * Loop (depth first) over the set or a subset and call a callback function
   * for each node
   */
  private static abstract class SetLoop
  {
    public SetLoop()
    {
    }

    public boolean run (Node<Object> set)
    {
      for(Entry<Object, Node<Object>> en : set.entrySet())
      {
        Object keyEnt = en.getKey();
        Node<Object> subset = en.getValue();
        if(next(keyEnt, subset))
          return true;
        if(run(subset))
          return true;
      }
      return false;
    }

    protected abstract boolean next(Object rowKey, Node<Object> value );
  }

  private class PathIterator implements Iterator<Object>
  {
    PathIterator()
    {
      _value = (getCollectionModel() == null || isEmpty()) ? null : nextItem(); // initialize;
    }

    PathIterator(Object noop)
    {
    }

    public Object next()
    {
      if (!hasNext())
        throw new NoSuchElementException();
      Object value = _value;
      _value = nextItem();
      return value;
    }

    public boolean hasNext()
    {
      return (_value != null);
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    protected Object nextItem()
    {
      return nextModelKey(0);
    }

    protected Object nextModelKey(int minDepth)
    {
      TreeModel model = getCollectionModel();
      if (model == null)
        return null;

      Object oldPath = model.getRowKey();
      try
      {
        model.setRowKey(_currPath);
        while(true)
        {
          boolean searchChildren = _containsSubtree(_currPath);
          boolean hasMore = _advanceToNextItem(model, minDepth, searchChildren);
          if (!hasMore)
            return null;

          _currPath = model.getRowKey();
          if (contains(_currPath))
            return _currPath;
        }
      }
      finally
      {
        model.setRowKey(oldPath);
      }
    }

    private boolean _containsSubtree(Object rowkey)
    {
      Search search = new Search()
      {
        @Override
        protected Node<Object> notFound(Node<Object> parent, Object rowkey)
        {
          return parent.isDefaultContained ? parent : null;
        }
      };
      Node<Object> current = search.find(rowkey);
      return (current != null) &&
        ((!current.isEmpty()) || current.isDefaultContained);
    }

    protected Object _value;
    protected Object _currPath = null;
  }

  /**
   * An iterator which avoids looping over the model by default (like the
   * PathIterator does). Instead NodeIterator loops over the model only for nodes that are "default contained".
   * Otherwise, it just does a depth first walk of the set and returns "isDifferent" nodes.
   */
  private class NodeIterator extends PathIterator
  {
    public NodeIterator()
    {
      super(null);
      _currIterator = _root.entrySet().iterator();
      _value = (getCollectionModel() == null || isEmpty()) ? null : nextItem(); // initialize;
    }

    protected Object nextItem()
    {
      Object nextKey = null;

      while(((nextKey = _nextEntry()) == null) && _iteratorStack.size() > 0)
        if(_currPath == null)
          _currIterator = _iteratorStack.pop();
      return nextKey;
    }

    private Object _nextEntry()
    {
      Object nextKey = null;
      if(_currPath != null)
      {
        nextKey = nextModelKey(_minDepth);
        if(nextKey == null)
        {
          _currPath = null;
          _nextEntry();
        }
      }
      else
      {
        Map.Entry<Object, Node<Object>> nextNode;
        while(nextKey == null && _currIterator.hasNext())
        {
          nextNode = _currIterator.next();
          if(_isContained(nextNode.getKey()))
            nextKey = nextNode.getKey();
          _iteratorStack.push(_currIterator);
          _currIterator = nextNode.getValue().entrySet().iterator();
          if(nextNode.getValue().isDefaultContained)
          {
            _currPath = nextNode.getKey();
            TreeModel model = getCollectionModel();
            Object oldPath = model.getRowKey();
            model.setRowKey(_currPath);
            _minDepth = model.getDepth()+1;
            model.setRowKey(oldPath);
            return nextKey;
          }
        }
      }
      return nextKey;
    }

    private Stack<Iterator <Map.Entry<Object, Node<Object>>>> _iteratorStack =
      new Stack<Iterator <Map.Entry<Object, Node<Object>>>>();
    private Iterator <Map.Entry<Object, Node<Object>>> _currIterator;
    private int _minDepth;
  }

  private Node<Object> _root;
  private transient TreeModel _model = null;
  private static final long serialVersionUID = 1L;
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RowKeySetTreeImpl.class);
}
