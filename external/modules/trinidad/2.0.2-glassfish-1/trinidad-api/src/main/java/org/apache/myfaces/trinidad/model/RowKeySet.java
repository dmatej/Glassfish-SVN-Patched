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

import java.util.AbstractSet;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This Set is a mutable collection
 * of rowKeys.
 * This class is meant to be used with models that have a current rowKey concept.
 * Therefore, the {@link #add()}, {@link #remove()} and {@link #isContained} methods
 * do not need to take the rowKey as an argument, since the rowKey is implied.
 * The implied key is obtained by calling {@link #getCollectionModel}.getRowKey()
 * <P>
 * Because this Set has a reference to the underlying model, operations like
 * {@link #addAll()}, {@link #removeAll()} and {@link #invertAll()} may
 * execute in constant time.
 * <P>
 * Note that the {@link #size()} method on the this Set might be expensive to
 * compute. Use the {@link #getSize()} method on this class for an inexpensive size.
 */
public abstract class RowKeySet extends AbstractSet<Object> implements Cloneable
{
  public RowKeySet()
  {
  }

  /**
   * @deprecated  remove asap
   */
  @Deprecated
  public abstract boolean isContainedByDefault();

  /**
   * Changes the underlying CollectionModel being used by this set.
   * The current rowKey (that is used by some of the methods in this class)
   * is obtained from this CollectionModel.
   * <P>
   * Users typically do not need to call this method.
   * This method is called by component writers who need to set the models
   * used by their components on this set.
   */
  public abstract void setCollectionModel(CollectionModel model);

  /**
   * Gets the underlying model used by this set.
   * @see #setCollectionModel
   */
  protected abstract CollectionModel getCollectionModel();

  /**
   * Adds the given rowKey to this set if it doesn't already exist, removes
   * it otherwise.
   * @return true if the row is now added. false otherwise.
   */
  public boolean invert(Object rowKey)
  {
    // doing "add" followed by an optional "remove" is faster than switching on
    // "contains"; the latter does two hashtable lookups all the time,
    // while the former does two hashtable lookups half the time.

    if (add(rowKey))
      return true; // the key was not present earlier, so now we're done.

    // rowKey was already present, so remove it:
    remove(rowKey);
    return false;
  }

  /**
   * Adds the current rowKey to this set if it doesn't already exist; removes
   * it otherwise.
   * @return true if the row is now added. false otherwise.
   */
  @SuppressWarnings("unchecked")
  public final boolean invert()
  {
    return invert(getCollectionModel().getRowKey());
  }

  /**
   * Checks to see if the current key is contained by this set.
   * @return true if this set contains the current key
   */
  public final boolean isContained()
  {
    Object rowkey = getCollectionModel().getRowKey();
    return contains(rowkey);
  }
  
  /**
   * Adds or removes the current key.
   * @param isContained if true, the current key is added to this set.
   * if false, the current key is removed from this set.
   */
  public final void setContained(boolean isContained)
  {
    if (isContained)
      add();
    else
      remove();
  }
  
  /**
   * Adds the current key to this set.
   * @return true if this set changed. ie: true is returned if this set
   * did not previously contain the current key.
   */
  @SuppressWarnings("unchecked")
  public final boolean add()
  {
    return add(getCollectionModel().getRowKey());
  }
  
  /**
   * Removes the current key from this set.
   * @return true if this set changed. ie: true is returned if this set
   * previously contained the current key.
   */
  public final boolean remove()
  {
    Object rowkey = getCollectionModel().getRowKey();
    return remove(rowkey);
  }
  
  /**
   * Gets the number of elements contained by this set. The difference between
   * this method and {@link #size()} is that this method may return -1 if the
   * size is expensive to compute.
   * This implementation simply calls {@link #size()}.
   * @return -1 if the number of elements is expensive to compute.
   */
  public int getSize()
  {
    return size();
  }

  /**
   * Adds all the rowKeys in the current collection into this Set.
   * If the underlying model is a List, then all the rowKeys in the List
   * are added to this Set. If the underlying model is a tree, then all the
   * rowKeys in the current subtree are added to this Set.
   */
  public abstract void addAll();
  
  /**
   * Removes all the rowKeys in the current collection from this Set.
   * If the underlying model is a List, then all the rowKeys in the List
   * are removed from this Set. If the underlying model is a tree, then all the
   * rowKeys in the current subtree are removed from this Set.
   * <P>
   * For List models, this method and {@link #clear} behave the same.
   * For tree models, this method only operates on the current subtree, while
   * the {@link #clear} method removes everything from this Set.
   * <P>
   * This implementation simply calls {@link #clear}
   */
  public void removeAll()
  {
    clear();
  }
  
  /**
   * Inverts this Set. Every element that is in this Set is removed, and
   * every element that is not in this Set is added to this Set.
   * <P>
   * For List models, this method operates on the entire List.
   * For tree models, this method only operates on the current subtree.
   */
  public abstract void invertAll();

  /**
   * Creates a shallow clone of this set.
   * Keys may be added or removed from the clone without affecting 
   * this instance. The keys themselves may not be cloned.
   * This implementation simply calls
   * {@link Object#clone}
   */
  @SuppressWarnings("unchecked")
  @Override
  public RowKeySet clone()
  {
    try
    {
      return (RowKeySet) super.clone();
    }
    catch (CloneNotSupportedException e)
    {
      // should not happen:
      throw new UnsupportedOperationException(_LOG.getMessage(
        "CANNOT_CLONE", e), e);
    }
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RowKeySet.class);
}
