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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;


/**
 * The data model used by Trinidad Tree components.  A TreeModel is
 * responsible for understanding how to iterate through an
 * object graph, enter and leave containers, and identify
 * rows of objects within each container.  Within any one
 * container, a TreeModel looks just like a CollectionModel,
 * which extends the JSF DataModel class.  (So, to understand
 * this class, start by learning how DataModel works).
 * <p>
 * <h3>Entering and exiting containers</h3>
 * <p>
 * TreeModel extends CollectionModel to add support for container rows,
 * which are entered and exited with enterContainer() and exitContainer()
 * methods.  Within a container, row indices (get/setRowIndex())
 * are relative to the container.  However, row keys - get/setRowKey(),
 * from the CollectionModel API - are always global for the entire
 * tree model, so it is sufficient to call setRowKey() to enter and
 * exit all the needed parents.
 * <p>
 * <h3>Lazy loading of contents</h3>
 * <p>
 * When a tree or treeTable iterates through the model,
 * it will generally seek to see if a given node is a
 * container - with the <code>isContainer()</code> method -
 * and also see if the node is empty (and therefore
 * not expandable) with the <code>isContainerEmpty()</code>
 * method.  The default implementation of that latter
 * method involves entering the child and seeing how
 * many children it has.  As a result, by default,
 * you will see one more level of content being
 * requested than is actually visible on screen.  To
 * avoid this, provide a custom override of <code>
 * isContainerEmpty()</code> to return a value
 * without actually entering the container.  It
 * is acceptable for this method to return a "false negative" -
 * to return false when there might actually not be any
 * contents - if that is the most efficient approach possible.
 * <p>
 * The <code>ChildPropertyTreeModel</code> class is a useful
 * basic subclass, but largely requires that you have the
 * entire object model fully loaded.  If you require
 * lazy loading, you'll likely need a custom implementation.
 * <p>
 * <h3>Further documentation</h3>
 * <p>
 * Rows in the TreeModel may (recursively) contain other rows.
 * To figure out if the current row is a container, call the
 * {@link #isContainer} method.
 * If a row is a container, use the {@link #enterContainer} method
 * to access its child rows. Once the {@link #enterContainer} method is called
 * all the CollectionModel API's methods (like {@link #getRowCount}) 
 * operate on the child collection.
 * To return back to the parent row, use the {@link #exitContainer} method.
 * <P>
 * Given the following tree structure:
 * <pre>
 * |-Root1 (rowKey="r1", rowIndex=0)
 * |  |-Folder1 (rowKey="r1f1", rowIndex=0)
 * |  |  |-Node1 (rowKey="r1f1n1", rowIndex=0)
 * |  |  |-Node2 (rowKey="r1f1n2", rowIndex=1)
 * |  |  |-Node3 (rowKey="r1f1n3", rowIndex=2)
 * |  |
 * |  |-Folder2 (rowKey="r1f2", rowIndex=1)
 * |     |-Node4 (rowKey="r1f2n1", rowIndex=0)
 * |
 * |-Root2 (rowKey="r2", rowIndex=1)
 * |-Root3 (rowKey="r3", rowIndex=2)
 * |-Root4 (rowKey="r4", rowIndex=3)
 * </pre>
 * To point the tree to the root collection call:
 * <code>setRowKey(null)</code>.<br>
 * Now, <code>getRowCount()</code> returns 4.<br>
 * <code>setRowIndex(1);getRowData()</code> returns <code>Root2</code>.<br>
 * <code>setRowKey("r4");getRowData()</code> returns <code>Root4</code>.
 * <P>
 * To access <code>Node4</code> use:
 * <pre>
 * setRowIndex(0); // isContainer()==true
 * enterContainer(); // enter Root1, getRowCount()==2
 * setRowIndex(1); // isContainer()==true
 * enterContainer(); // enter Folder2, getRowCount()==1
 * setRowIndex(0); // isContainer()==false
 * getRowData();
 * </pre>
 * Or, more simply:
 * <pre>
 * setRowKey("r1f2n1");
 * getRowData();
 * </pre>
 * At this point, to get at <code>Node3</code> use:
 * <pre>
 * exitContainer(); // exit Folder2, Root1 is now the current row.
 * setRowIndex(0);
 * enterContainer(); // enter Folder1, getRowCount()==3
 * setRowIndex(2);
 * getRowData();
 * </pre>
 * Or, more simply:
 * <pre>
 * setRowKey("r1f1n3");
 * getRowData();
 * </pre>
 */
public abstract class TreeModel extends CollectionModel implements TreeLocalRowKeyIndex
{

  /**
   * Tests to see if the row identified by getRowData() is a container element.
   * Use {@link #isContainerEmpty} to see if the current container element actually
   * has children, or is an empty container.
   * @return true if the current element may contain children.
   */
  public abstract boolean isContainer();

  /**
   * Tests to see if the current container element actually has children.
   * This could be more efficient than calling
   * {@link #enterContainer} followed by {@link #getRowCount}.
   * This method is permitted to return false even if the container is actually
   * empty.
   * This method should only be called if {@link #isContainer} returns true.
   * @return true if the current container element has no children. If there
   * is any doubt as to whether or not the container has children, this method
   * should return false.
   */
  public boolean isContainerEmpty()
  {
    if (!isContainer())
      return true;

    enterContainer();
    try
    {
      int kids = getRowCount();
      if (kids < 0)
      {
        setRowIndex(0);
        return !isRowAvailable();
      }
      return (kids == 0);
    }
    finally
    {
      exitContainer();
    }
  }
  
  /**
   * This Collection changes to reflect the children of the current rowData,
   * and the current rowData changes to be null.
   * The current rowIndex becomes -1. This method should only be called
   * if {@link #isContainer()} returns true.
   * {@link #getRowCount} can be used to get the number of children. 
   */
  public abstract void enterContainer();
  
  /**
   * Pops back up to the parent collection.
   * The current rowData becomes the rowData of the parent.
   * This Collection will change to include the children of the new rowData.
   */
  public abstract void exitContainer();
  
  /**
   * Gets the rowKey of the current row's container row.
   * This implementation calls {@link #getContainerRowKey(Object)} with
   * the current rowKey.
   */
  public final Object getContainerRowKey()
  {
    Object key = getRowKey();
    Object parentKey = getContainerRowKey(key);
    return parentKey;
  }

  /**
   * Gets the rowkey of each container, starting from the top most
   * container, down to the container of the given child rowKey.
   * The root container (which always has the null rowKey) is not included in
   * this list. The given childRowKey is not included in this list.
   * <p>
   * Given the following tree structure:
   * <pre>
   * |-Root1 (rowKey="r1")
   * |  |-Folder1 (rowKey="r1f1")
   * |  |  |-Node1 (rowKey="r1f1n1")
   * </pre>
   * Calling <code>getAllAncestorContainerRowKeys("r1f1n1")</code>
   * returns a List of two items:"r1" and "r1f1", in that order.
   * 
   * @param childRowKey identifies the child row. 
   * @return An empty list is returned if the child row is a root row and
   * has no parent containers. Each item in this list is a rowKey
   * and is of type {@link Object}.
   * The first rowKey (in this list) is the top most container. The last
   * rowKey is the immediate container of the given childRowKey.
   */
  public List<Object> getAllAncestorContainerRowKeys(Object childRowKey)
  {
    if (childRowKey == null)
      return Collections.emptyList();

    int size = getDepth(childRowKey);
    if (size <= 0)
      return Collections.emptyList();
      
    Object[] keys = new Object[size];
    for(int i=size-1; i>=0; i--)
    {
      childRowKey = getContainerRowKey(childRowKey);
      assert childRowKey != null;
      keys[i] = childRowKey;
    }
    return Collections.unmodifiableList(Arrays.asList(keys));
  }
  
  /**
   * Gets the rowKey of a given child row's container row. 
   * <pre>
   * |-Root1 (rowKey="r1", containerRowKey=null)
   * |  |-Folder1 (rowKey="r1f1", containerRowKey="r1")
   * |  |  |-Node1 (rowKey="r1f1n1", containerRowKey="r1f1")
   * |  |  |-Node2 (rowKey="r1f1n2", containerRowKey="r1f1")
   * </pre>
   * @param childRowKey the rowKey of the child row.
   * @return the rowKey of the container, or null if the child is a root row.
   */
  public abstract Object getContainerRowKey(Object childRowKey);
  
  /**
   * Gets the depth of the current row within this tree hierarchy.
   * <br>
   * This implementation simply calls {@link #getDepth(Object)} with
   * the current rowKey.
   */
  public final int getDepth()
  {
    Object key = getRowKey();
    return getDepth(key);
  }
  
  /**
   * Gets the depth of the given row within the tree hierarchy.
   * The depth is a measure of how far the given row is from its top-level
   * container row.
   * Root-level rows have a depth of zero. All the immediate children of each
   * root row have a depth of one.
   * <pre>
   * |-Root1 (depth=0)
   * |  |-Folder1 (depth=1)
   * |  |  |-Node1 (depth=2)
   * |  |  |-Node2 (depth=2)
   * |  |  |-Node3 (depth=2)
   * |  |-Folder2 (depth=1)
   * |-Root2 (depth=0)
   * </pre>
   */
  public int getDepth(Object rowKey)
  {
    Object key = rowKey;
    int depth = 0;
    while(true)
    {
      key = getContainerRowKey(key);
      if (key == null)
        break;
      depth++;
    }
    return depth;
  }
  

  //
  // Below is the default implemenation for the TreeLocalRowKeyIndex interface.  
  //
  
  /**
   * Indicates whether data for a child model (children of the current node) is 
   * locally available. Locally available means no data fetch is required 
   * as a result of a call to  {@link #enterContainer}. 
   * @return The default implementation returns <code>false</code>
   * <p> 
   * Override this method if the TreeModel implementation supports caching of nodes.  
   * If caching is supported, the implementation should also return a caching strategy from 
   * <code>CollectionModel.getCachingStrategy()</code>
   */
  public boolean isChildCollectionLocallyAvailable()
  {
    return false;
  }

  /**
   * Indicates whether child data for the node with the given index is
   * locally available.   This method first checks to see if the parent node
   * at the given index is locally available by calling {@link #isRowLocallyAvailable(int}.
   * If the parent node is locally available, this method moves the model to the
   * parent node and calls {@link #isChildCollectionLocallyAvailable()}
   * The current row does not change after this call
   * @param index
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(int index)
  {
    if (isRowLocallyAvailable(index))
    {
      int oldIndex = getRowIndex();
      try
      {
        setRowIndex(index);
        return isChildCollectionLocallyAvailable();
      }
      finally
      {
        setRowIndex(oldIndex);
      }
    }
    else
    {
      return false;
    }
  }

  /**
   * Indicates whether child data for the node with the given row key is
   * locally available.   
   * <p>
   * This method first checks to see if the parent node
   * with the given row key is locally available by calling {@link #isRowLocallyAvailable(Object)}.
   * If the parent node is locally available, this method moves the model to the
   * parent node and calls {@link #isChildCollectionLocallyAvailable()}
   * The current row does not change after this call
   * @param rowKey
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(Object rowKey)
  {
    if (isRowLocallyAvailable(rowKey))
    {
      Object oldKey = getRowKey();
      try
      {
        setRowKey(rowKey);
        return isChildCollectionLocallyAvailable();
      }
      finally
      {
        setRowKey(oldKey);
      }
    }
    else
    {
      return false;
    }
  }

  /**
   * Check if a range of rows is locally available starting from a row index. The range
   * can include child nodes in any expanded nodes within the range.
   * <p> 
   * This implementation checks the row at startIndex for availability and,  if
   * available, moves the model to startIndex and calls 
   * <code>areRowsLocallyAvailable(rowCount, disclosedRowKeys)</code>.
   * The current row does not change after this call
   * @param startIndex staring index for the range  
   * @param rowCount number of rows in the range
   * @return true if range of rows is locally available false otherwise
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowCount, RowKeySet disclosedRowKeys)
  {
    
    boolean available = false;
    if (isRowLocallyAvailable(startIndex))
    {
      Object oldKey = getRowKey();
      try
      {
        setRowIndex(startIndex);
        available = areRowsLocallyAvailable(rowCount, disclosedRowKeys);
      }
      finally
      {
        setRowKey(oldKey);
      }
    }
    return available;
  }

  /**
   * Check if a range of rows is locally available starting from a row key. The range
   * can include child nodes in any expanded nodes within the range.
   * <p> 
   * This implementation checks the row at startRowKey for availability and,  if
   * available, moves the model to startRowKey and calls 
   * <code>areRowsLocallyAvailable(rowCount, disclosedRowKeys)</code>.
   * The current row does not change after this call
   * @param startRowKey staring row key for the range  
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * @return true if range of rows is locally available false otherwise
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowCount, RowKeySet disclosedRowKeys)
  {
    boolean available = false;
    if (isRowLocallyAvailable(startRowKey))
    {
      Object oldKey = getRowKey();
      try
      {
        setRowKey(startRowKey);
        available = areRowsLocallyAvailable(rowCount, disclosedRowKeys);
      }
      finally
      {
        setRowKey(oldKey);
      }
    }
    return available;
  }
  
  /**
   * Check if a range of rows is locally available starting from current position. The range
   * can include child nodes in any expanded nodes within the range.
   * This implementation walks locally available nodes in the current collection and drills into any expanded child
   * collections.  The node traversal can continue to the siblings of the current node. 
   * Node traversal ends when a node or a child collection is not locally available or
   * when rowCount nodes are visited or the last root node is reached.  
   * The current row does not change after this call.
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * @return true if range of rows is locally available false otherwise
   */
  public boolean areRowsLocallyAvailable(int rowCount, RowKeySet disclosedRowKeys)
  {
    
    boolean available = false;    
    Object startingRowKey = getRowKey();
    
    if (startingRowKey != null)
    {
      available = _areRowsLocallyAvailable(startingRowKey, rowCount, disclosedRowKeys);  
    }
    return available;
  }


  /**
   * Check if a total of "count" rows are locally available starting from a "rowKey". Take into account
   * child rows in expanded nodes within the range
   * @param rowKey starting row key to check
   * @param count row count to check
   * @param disclosedRows set of expanded nodes
   * @return true if count rows are locally available false otherwise
   */
  private boolean _areRowsLocallyAvailable(Object rowKey, int count, RowKeySet disclosedRows)
  {
    if (!isRowLocallyAvailable(rowKey))
      return false;
    
    Object oldKey = getRowKey();
    try
    {
      setRowKey(rowKey);
      int startIndex = getRowIndex();
      // start from the current node and walk up the parent hierarchy if necessary
      while ((count = _walkAvailableNodes(startIndex, count, disclosedRows)) > 0 && getDepth() > 0)
      {
        exitContainer();        
        startIndex = getRowIndex();
        startIndex += 1;
      }
      return count >= 0;
    }
    finally
    {
      setRowKey(oldKey);
    }
  }

  /**
   * Walk available child nodes starting from a row index in the current collection 
   * and consume "count" nodes.  Drill into expanded nodes.
   * @param startIndex starting child index within the current collection
   * @param count number of nodes to check
   * @param disclosedRows set of expanded nodes
   * @return -1 if rows are not available;  0 if "count" rows are available; > 0
   * if the requested count is greater than child row count for the current 
   * collection and all rows up to child row count are locally available
   */
  private int _walkAvailableNodes(int startIndex, int count, RowKeySet disclosedRows)
  {
    int index = startIndex;    
    int rowCount = getRowCount();
    
    if (rowCount < 0)
      return -1;
    
    while (index < rowCount && count > 0)
    {
      if (!isRowLocallyAvailable(index))
        return -1;
      
      --count;
      setRowIndex(index);
      Object key = getRowKey();
      if (disclosedRows.contains(key))
      {
        if (isChildCollectionLocallyAvailable())
        {
          enterContainer();
          setRowIndex(0);
          count = _walkAvailableNodes(0, count, disclosedRows);
          if (count < 0)
            return -1;
          exitContainer();
        }
        else
        {
          // children of expanded node are not available
          return -1;
        }
      }
      ++index;
    }
    
    return count;
  }
}
