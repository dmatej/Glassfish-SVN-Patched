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

import java.io.IOException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;

import javax.el.MethodExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.FocusEvent;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.RangeChangeListener;
import org.apache.myfaces.trinidad.model.RowKeySet;

/**
 * Base class for TreeTable component. The behaviours implemented by the
 * TreeTable include expanding/collapsing subtrees and focusing into subtrees.
 * @version $Name:  $ ($Revision: 957080 $) $Date: 2010-06-22 16:39:35 -0700 (Tue, 22 Jun 2010) $
 */
abstract public class UIXTreeTableTemplate extends UIXTree
{
/**/  public abstract int[] getRowsByDepth();
/**/  abstract public MethodExpression getRangeChangeListener();

  /**
   * Override to update the container client id cache before decode
   */
  @Override
  public void decode(FacesContext context)
  {
    _resetContainerClientIdCache();
    super.decode(context);    
  }
  
  /**
   * Override to update the container client id cache before validations
   */
  @Override
  public void processValidators(FacesContext context)
  {
    _resetContainerClientIdCache();
    super.processValidators(context);
  }
  
  /**
   * Override to update the container client id cache before updates
   */  
  @Override
  public void processUpdates(FacesContext context)
  {
    _resetContainerClientIdCache();
    super.processUpdates(context);
  }  

  /**
   * Override to update the container client id cache before encode
   */
  @Override
  protected void __encodeBegin(FacesContext context) throws IOException
  {
    _resetContainerClientIdCache();
    super.__encodeBegin(context);
  }


  /**
   * Override to return clientd ids with no currency for items in header/footer facets
   */
  @Override
  public String getContainerClientId(FacesContext context, UIComponent child)
  {
    String id;
    if (_containerClientIdCache == null || _isStampedChild(child))
    {   
      // call the UIXCollection getContainerClientId, which attaches currency string to the client id
      id = getContainerClientId(context);
    }
    else
    {
      // The target is not a stamped child, so return a client id with no currency string
      id = getClientId(context);
    }

    return id;
  }

  @Deprecated
  public void setRangeChangeListener(MethodBinding binding)
  {
    setRangeChangeListener(adaptMethodBinding(binding));
  }

  /**
   * Gets the maximum number of rows to show.
   * This changes depending on the depth of the current row in the tree
   * hierarchy.
   * The rows per depth is obtained from
   * {@link #getRowsByDepth}.
   * @return 0 if all rows must be shown at this level.
   */
  @Override
  public final int getRows()
  {
    int depth = getTreeModel().getDepth();
    assert depth >= 0;

    // the root element is selected when depth is zero:
    if (depth==0)
      return 1; // the treeTable only shows the first root node.

    int[] rows = getRowsByDepth();
    if ((rows == null) || (rows.length == 0))
      return 0;

    depth--;

    // in a treeTable, the the first "rows" property affects how many
    // children of the root element to show.
    return (depth >= rows.length) ? rows[rows.length - 1] : rows[depth];
  }

  /**
   * Gets the range start index for the current collection.
   * The current collection is the children of the parent of the
   * current rowData. ie: the current collection is the collection of
   * siblings of the current rowData.
   * @return zero based index of the row that must be displayed first.
   * @see #getRowData()
   */
  @Override
  public final int getFirst()
  {
    // "first" does not change per path. It changes per parent path.
    // this is because "first", "rows" and "rowCount" applies to the container
    // element and not the current element:
    Object container = _getContainerPath();
    Integer first = _firstMap.get(container);
    return (first != null) ? first.intValue() : 0;
  }

  /**
   * Sets the range start index for the current collection.
   * The current collection is the children of the parent of the
   * current rowData. ie: the current collection is the collection of
   * siblings of the current rowData.
   * @param index zero based index of the row that must be displayed first.
   * @see #getRowData()
   */
  public void setFirst(int index)
  {
    // "first" does not change per path. It changes per parent path.
    // this is because "first", "rows" and "rowCount" applies to the container
    // element and not the current element:
    Object container = _getContainerPath();
    Map<Object, Integer> comparant = Collections.emptyMap();
    if (_firstMap == comparant)
      _firstMap = new HashMap<Object, Integer>(3);

    if (index <= 0)
      _firstMap.remove(container);
    else
      _firstMap.put(container, Integer.valueOf(index));
  }

  /**
   * Adds a RangeChangeListener.
   */
  public void addRangeChangeListener(RangeChangeListener listener)
  {
    addFacesListener(listener);
  }

  /**
   * Removes a RangeChangeListener.
   */
  public void removeRangeChangeListener(RangeChangeListener listener)
  {
    removeFacesListener(listener);
  }


  /**
   * Retrieves all RangeChangeListeners
   */
  public RangeChangeListener[] getRangeChangeListeners()
  {
    return (RangeChangeListener[]) getFacesListeners(RangeChangeListener.class);
  }

  @Override
  public Object saveState(FacesContext context)
  {
    Object[] array = new Object[2];
    array[0] = super.saveState(context);
    array[1] = (_firstMap.isEmpty()) ? null : _firstMap;

    if (array[0] == null && array[1] == null)
      return null;

    return array;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void restoreState(FacesContext context, Object state)
  {
    Object[] array = (Object[]) state;
    super.restoreState(context, array[0]);
    _firstMap = (Map<Object, Integer>) array[1];
    if (_firstMap == null)
      _firstMap = Collections.emptyMap();
  }

  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Notify the specified disclosure listener method (if any)
    if (event instanceof FocusEvent)
    {
      setFocusRowKey(getRowKey());
      //pu: Implicitly record a Change for 'focusPath' attribute
      addAttributeChange("focusPath",
                         getFocusRowKey());
      // it is nice to expand the focused item:
      getDisclosedRowKeys().add();

      broadcastToMethodExpression(event, getFocusListener());
    }
    else if (event instanceof RangeChangeEvent)
    {
      RangeChangeEvent rce = (RangeChangeEvent) event;
      setFirst(rce.getNewStart());
      broadcastToMethodExpression(event, getRangeChangeListener());
    }

    // Perform standard superclass processing
    super.broadcast(event);
  }


  /**
   * Gets the stamps. This returns the children of this component plus
   * the nodeStamp stamp (if any).
   */
  // TODO cache the result.
  @Override
  protected final List<UIComponent> getStamps()
  {
    
    List<UIComponent> children = getChildren();
    List<UIComponent> stamps;
    
    if (children.isEmpty())
    {
      // no children, so use Node stamps as the stamp
      stamps = super.getStamps();
    }
    else
    {
      UIComponent nodeStamp = getNodeStamp();
      
      if (nodeStamp == null)
      {
        // no node stamp, so stamp, is only the children
        stamps = children;
      }
      else
      {
        // stamps are the children plus the node stamp
        stamps = new ArrayList<UIComponent>(children.size() + 1);
        stamps.addAll(children);
        stamps.add(nodeStamp);
      }
    }
    
    return stamps;
  }

  /**
   * Restores the state for the given stamp.
   * This method avoids changing the state of facets on columns.
   */
  @Override
  protected final void restoreStampState(FacesContext context, UIComponent stamp,
                                         Object stampState)
  {
    if (stamp instanceof UIXColumn)
    {
      // if it is a column, we don't want the facets processed.
      // Only the children:
      StampState.restoreChildStampState(context, stamp, this, stampState);
    }
    else
      super.restoreStampState(context, stamp, stampState);
  }

  /**
   * Saves the state for the given stamp.
   * This method avoids changing the state of facets on columns.
   */
  @Override
  protected final Object saveStampState(FacesContext context, UIComponent stamp)
  {
    if (stamp instanceof UIXColumn)
    {
      // if it is a column, we don't want the facets processed.
      // Only the children:
      return StampState.saveChildStampState(context, stamp, this);
    }
    else
      return super.saveStampState(context, stamp);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    // process all the facets of this hgrid just once
    // (except for the "nodeStamp" facet which must be processed once
    // per row):
    TableUtils.processFacets(context, this, this, phaseId,
      UIXTreeTable.NODE_STAMP_FACET);

    UIComponent nodeStamp = getNodeStamp();
    // process any facets of the nodeStamp column:
    TableUtils.processFacets(context, this, nodeStamp, phaseId, null);

    // process all the facets of this table's column children:
    TableUtils.processColumnFacets(context, this, this, phaseId);

    // recursively process any grandchild columns of the nodeStamp column:
    TableUtils.processColumnFacets(context, this, nodeStamp, phaseId);

    Object oldPath = getRowKey();
    RowKeySet state = getDisclosedRowKeys();
    try
    {
      Object path = getFocusRowKey();
      setRowKey(path);
      if (path == null)
      {
        HierarchyUtils.__iterateOverTree(context,
                                         phaseId,
                                         this,
                                         state,
                                         true);        

      }
      else
      {
        TableUtils.processStampedChildren(context, this, phaseId);
        processComponent(context, nodeStamp, phaseId); // bug 4688568
  
        if (state.isContained())
        {
          enterContainer();
          HierarchyUtils.__iterateOverTree(context,
                                           phaseId,
                                           this,
                                           state,
                                           true);
        }
      }
    }
    finally
    {
      setRowKey(oldPath);
    }
  }

  @Override
  protected boolean visitChildren(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    // need to override to do the default since our superclass
    // UIXTree does stuff here we don't want
    return defaultVisitChildren(visitContext, callback);
  }

  @Override
  protected boolean visitUnstampedFacets(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    // Visit the facets except for the node stamp
    int facetCount = getFacetCount();
    
    if (facetCount > 0)
    {
      UIComponent nodeStamp = getNodeStamp();
      
      // if our only facet is the node stamp, we don't need to do this
      if ((facetCount > 1) || (nodeStamp == null))
      {
        for (UIComponent facet : getFacets().values())
        {
          // ignore the nodeStamp facet, since it is stamped
          if (facet != nodeStamp)
          {
            if (UIXComponent.visitTree(visitContext, facet, callback))
            {
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  @Override
  protected boolean visitData(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    Object focusedPath = getFocusRowKey();
    Object oldRowKey = null;
    
    // start from the focused area
    if (focusedPath != null)
    {
      oldRowKey = getRowKey();
      setRowKey(focusedPath);
    }
    
    boolean done;
    
    try
    {
      done = super.visitData(new NoColumnFacetsVisitContext(visitContext), callback);
    }
    finally
    {
      if (focusedPath != null)
      {
        setRowKey(oldRowKey);
      }
    }
    
    return done;
  }

  /**
   * Gets the path of the parent
   */
  private Object _getContainerPath()
  {
    Object parentKey = getTreeModel().getContainerRowKey();
    return parentKey;
  }

  /**
   * Is target a stamped child UIComponent in the treeTable body
   */
  private boolean _isStampedChild(UIComponent target)
  {
    assert _containerClientIdCache != null;
    return !_containerClientIdCache.containsKey(target);
  }

  /**
   * Reset the cache of child components used in getContainerClientId
   */
  private void _resetContainerClientIdCache()
  {
    if(_containerClientIdCache == null)
      _containerClientIdCache = new IdentityHashMap<UIComponent, Boolean>();
    else
      _containerClientIdCache.clear();

    // cache treeTable header/footer items
    TableUtils.cacheHeaderFooterFacets(this, _containerClientIdCache);
    // cache child column header/footer items, including nested columns
    TableUtils.cacheColumnHeaderFooterFacets(this, _containerClientIdCache);

    UIComponent nodeStamp = getNodeStamp();
    if(nodeStamp != null)
    {
      // cache nodeStamp header/footer items
      TableUtils.cacheHeaderFooterFacets(nodeStamp, _containerClientIdCache);
      // cache any nested columns in nodeStamp facet
      TableUtils.cacheColumnHeaderFooterFacets(nodeStamp, _containerClientIdCache);      
    }
  }

  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[2];
    state[0] = super.__getMyStampState();
    state[1] = (_firstMap.isEmpty()) ? null : _firstMap;

    return state;
  }
  
  /**
   * Sets the internal state of this component.
   * @param stampState the internal state is obtained from this object.
   */
  @Override
  @SuppressWarnings("unchecked")
  void __setMyStampState(Object stampState)
  {
    Object[] state = (Object[]) stampState;
    super.__setMyStampState(state[0]);
    _firstMap = (Map<Object, Integer>) state[1];
    if (_firstMap == null)
      _firstMap = Collections.emptyMap();
  }

  private Map<Object, Integer> _firstMap = Collections.emptyMap();
  // cache of child components inside this treeTable header/footer facets and column header/footer
  // facets
  transient private IdentityHashMap<UIComponent, Boolean> _containerClientIdCache = null;
}
