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

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.event.FocusEvent;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.event.SortEvent;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.TreeModel;

/**
 * utility methods for dealing with tables.
 */
public final class TableUtils 
{

  /**
   * Gets the index of the last visible row that should be
   * displayed by the given table. This is usually 
   * {@link CollectionComponent#getFirst} added to
   * {@link CollectionComponent#getRows} minus 1, but it changes if 
   * {@link CollectionComponent#getRowCount} returns
   * insufficient rows.
   * @return if this table is empty, this returns 
   * {@link CollectionComponent#getFirst()} - 1
   */
  public static int getLast(CollectionComponent table)
  {
    return getLast(table, table.getFirst());
  }

  /**
   * Gets the index of the last visible row that should be
   * displayed by the given table. This is usually 
   * rangeStart added to
   * {@link CollectionComponent#getRows} minus 1, but it changes if 
   * {@link CollectionComponent#getRowCount} returns
   * insufficient rows.
   * @return if this table is empty, this returns 
   * rangeStart - 1
   */
  public static int getLast(CollectionComponent table, int rangeStart)
  {
    final int rangeEnd;
    int blockSize = table.getRows();
    // if the blockSize is zero, that means show everthing.
    if (blockSize <= 0)
    {
      rangeEnd = Integer.MAX_VALUE;
    }
    else
    {
      rangeEnd = rangeStart + blockSize;
    }
    return ModelUtils.findLastIndex(table, rangeStart, rangeEnd) - 1;
  }

  /**
   * Sets up an EL variable on the requestScope.
   * @param name The name of the EL variable
   * @param value The value of the EL variable
   * @return any old value that was bound to the EL variable, or null
   * if there was no old value.
   */
  @SuppressWarnings("unchecked")
  public static Object setupELVariable(FacesContext context, String name, Object value)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    if (value == null)
      return requestMap.remove(name);
    else
      return requestMap.put(name, value);
  }

  /**
   * Perform a safe expand all.
   * Checks to make sure that a complete expand-all operation won't result in
   * too many nodes being displayed to the user. If too many nodes will end
   * up being displayed, this method only expands the immediate children before
   * terminating. Otherwise, a complete expand-all operation is performed.
   * @param maxSize the maximum number of nodes to display. A complete expand-all
   * operation will not take place if more than this number of nodes 
   * will end up being displayed.
   * @param model this tree model must have its path pointing to a particular 
   * node which must be a container.
   */
  static void __doSafeExpandAll(
      TreeModel model, 
      RowKeySet state, 
      int       maxSize)
  {
    int size = _getSizeOfTree(model, maxSize);
    if ((size < 0) || (size > maxSize))
    {
      // not safe to do expand all.
      // first expand the current node:
      state.add();
      
      // now only expand immediate children:
      model.enterContainer();
      int i=0;
      while(true)
      {
        model.setRowIndex(i++);
        if (!model.isRowAvailable())
          break;
        state.add();
      }
      model.exitContainer();
    }
    else // safe to do expand all:
      state.addAll();
  }

  /**
   * Computes the number of nodes in a subtree.
   * @param model the path must point to a node.
   * @param maxSize the maximum number of nodes that will be searched.
   * this method returns once this limit is reached.
   * @return -1 if the number of nodes is unknown.
   * If the limit is hit, then returns the number of nodes that are known
   * to exist at the time the limit was hit. This number may be larger than
   * maxSize.
   */
  static int _getSizeOfTree(TreeModel model, int maxSize)
  {
    if (model.isRowAvailable() && model.isContainer())
    {
      model.enterContainer();
      try
      {
        int size = model.getRowCount();
        for(int i=0, sz=size; i<sz; i++)
        {
          if (size > maxSize)
            return size;
          model.setRowIndex(i);
          int kidSize = _getSizeOfTree(model, maxSize - size);
          if (kidSize < 0)
            return -1;
          size += kidSize;
        }
        return size;        
      }
      finally
      {
        model.exitContainer();
      }
    }
    return 0;
  }

  /**
   * This method sets the phaseID of the event
   * according to the "immediate" property of this
   * component.
   * If "immediate" is set to true, this calls
   * {@link FacesContext#renderResponse}
   */
  static void __handleQueueEvent(UIComponent comp, FacesEvent event)
  {
    if (_isImmediateEvent(comp, event))
    {
      String immediateAttr = UIXTree.IMMEDIATE_KEY.getName();
      Object isImmediate = comp.getAttributes().get(immediateAttr);
      if (Boolean.TRUE.equals(isImmediate))
      {
        event.setPhaseId(PhaseId.ANY_PHASE);
        FacesContext context = FacesContext.getCurrentInstance();
        context.renderResponse();
      }
      else
      {
        // the event should not execute before model updates are done. 
        // otherwise, the updates will be done to the wrong rows.
  
        // we can't do this at the end of the UPDATE_MODEL phase because
        // if there are errors during that phase, then we want to immediately render
        // the response, and not deliver this ui event:
        event.setPhaseId(PhaseId.INVOKE_APPLICATION);
      }
    }
  }

  /**
   * Process all the facets of a component; these are
   * generally not processed once per row.
   * @param skipFacet the name of any facet that should not be processed 
   * at this time.
   */
  @SuppressWarnings("unchecked")
  public static void processFacets(
    FacesContext context,
    final UIXCollection table,
    UIComponent  component,
    final PhaseId phaseId,
    String skipFacet)
  {
    Map<String, UIComponent> facets = component.getFacets();
    final UIComponent skip = (skipFacet != null)
      ? (UIComponent) facets.get(skipFacet)
      : null;
                                           
    new ChildLoop()
    {
      @Override
      protected void process(FacesContext context, UIComponent facet)
      {
        if (facet != skip)
          table.processComponent(context, facet, phaseId);
      }
    }.runAlways(context, facets.values());
  }

  /**
   * Process all the facets of any children that are columns; these are
   * generally not processed once per row.
   */
  public static void processColumnFacets(
    FacesContext context,
    final UIXCollection table,
    UIComponent  column,
    final PhaseId phaseId)
  {
    new ChildLoop()
    {
      @Override
      protected void process(FacesContext context, UIComponent child)
      {
        if (child instanceof UIXColumn && child.isRendered())
        {
          // process any facets of the child column:
          processFacets(context, table, child, phaseId, null);
          // recursively process the facets of any grandchild columns:
          processColumnFacets(context, table, child, phaseId);
        }
      }
    }.runAlways(context, column);
  }

  /**
   * Process all the children of the given table
   */
  public static void processStampedChildren(
    FacesContext context,
    final UIXCollection table,
    final PhaseId phaseId)
  {
    new ChildLoop()
    {
      @Override
      protected void process(FacesContext context, UIComponent child)
      {
        // make sure that any cached clientIds are cleared so that
        // the clientIds are recalculated with the new row index
        UIXComponent.clearCachedClientIds(child);
        table.processComponent(context, child, phaseId);
      }
    }.runAlways(context, table);
  }
  
  /**
   * Process all the children of the given table
   */
  @SuppressWarnings("unchecked")
  static void __processChildren(
    FacesContext context,
    final UIXCollection comp,
    final PhaseId phaseId)
  {

    // process the children
    int childCount = comp.getChildCount();
    if (childCount != 0)
    {
      List<UIComponent> children = comp.getChildren();

      for (int i = 0; i < childCount; i++)
      {
        UIComponent child = children.get(i);
        comp.processComponent(context, child, phaseId);
      }
    }          
  }  

  static void cacheHeaderFooterFacets(UIComponent parent, Map<UIComponent, Boolean> cache)
  {
    // grab the header facet and it's children
    UIComponent headerFacet = parent.getFacets().get("header");
    if (headerFacet != null)
    {
      _cacheDescendants(headerFacet, cache, true);
    }

    // grab the footer facet and it's children
    UIComponent footerFacet = parent.getFacets().get("footer");
    if (footerFacet != null)
    {
      _cacheDescendants(footerFacet, cache, true);
    }
  }

  static void cacheColumnHeaderFooterFacets(UIComponent parent, Map<UIComponent, Boolean> cache)
  {
    List<UIComponent> children = parent.getChildren();
    for (UIComponent child : children)
    {
      if (child instanceof UIXColumn)
      {
        cacheHeaderFooterFacets(child, cache);
        cacheColumnHeaderFooterFacets(child, cache);
      }
    }
  }
  

  private static void _cacheDescendants(UIComponent parent, Map<UIComponent, Boolean> cache, boolean inclusive)
  {
    if(inclusive)
      cache.put(parent, Boolean.TRUE);
    
    List<UIComponent> children = parent.getChildren();
    for (UIComponent child : children)
    {
      _cacheDescendants(child, cache, true);
    }
  }

  /**
   * Checks to see if the given event could possible be affected by the 
   * "immediate" property of the given component.
   */
  private static boolean _isImmediateEvent(UIComponent comp, FacesEvent event)
  {
    if (event.getComponent() == comp)
    {
      return 
          (event instanceof RangeChangeEvent) ||
          (event instanceof DisclosureEvent) ||
          (event instanceof RowDisclosureEvent) ||
          (event instanceof SelectionEvent) ||
          (event instanceof SortEvent) ||
          (event instanceof FocusEvent);
    }
    return false;
  }

  private TableUtils()
  {
  }
}
