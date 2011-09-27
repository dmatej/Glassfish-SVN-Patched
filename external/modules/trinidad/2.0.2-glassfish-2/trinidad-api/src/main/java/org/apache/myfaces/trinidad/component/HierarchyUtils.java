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

import java.util.ArrayList;
import java.util.List;

import javax.el.MethodExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.model.RowKeySet;


/**
 * utility methods for dealing with tree structures that do disclosure.
 */
public final class HierarchyUtils
{
  static void __handleBroadcast(
    UIXHierarchy  comp,
    FacesEvent    event,
    RowKeySet     state,
    MethodExpression method) throws AbortProcessingException
  {
    // Notify the specified disclosure listener method (if any)
    if (event instanceof RowDisclosureEvent)
    {
      RowDisclosureEvent dEvent = (RowDisclosureEvent) event;
      state.removeAll(dEvent.getRemovedSet());
      state.addAll(dEvent.getAddedSet());
      //pu: Implicitly record a Change for 'disclosedRowKeys' attribute
      comp.addAttributeChange("disclosedRowKeys", state);
      comp.broadcastToMethodExpression(event, method);
    }
  }


  static void __handleEncodeBegin(
    UIXHierarchy comp,
    RowKeySet    set
    )
  {
    if (comp.__isFirstRender())
    {
      // @todo - the focusPath in tree is an attr, the focusPath in
      // navigationTree comes off the model, okay???
      Object focusRowKey = comp.getFocusRowKey();
      if ( focusRowKey != null)
      {
        List<Object> focusPath =
          new ArrayList<Object>(comp.getAllAncestorContainerRowKeys(focusRowKey));
        focusPath.add(focusRowKey);
        int size = focusPath.size();
        for ( int i = 0 ; i < size; i++)
        {
          Object subkey = focusPath.get(i);
          set.add(subkey);
        }
      }
    }
  }


  static void __processLevel(
    FacesContext context,
    PhaseId      phaseId,
    UIXHierarchy comp
    )
  {
    if (comp.getRowCount() != 0)
    {
      UIComponent nodeStamp = comp.getFacet("nodeStamp");

      if ( nodeStamp != null)
      {
        Object oldRowKey = comp.getRowKey();
        try
        {
          int startIndex = comp.getFirst();
          int endIndex = TableUtils.getLast(comp, startIndex);

          for (int i = startIndex; i <= endIndex; i++)
          {
            comp.setRowIndex(i);
            comp.processComponent(context, nodeStamp, phaseId);
          }
        }
        finally
        {
          comp.setRowKey(oldRowKey);
        }
      }
    }
  }

  static void __iterateOverTree(
    FacesContext context,
    PhaseId      phaseId,
    UIXHierarchy comp,
    RowKeySet    state,
    boolean      processChildrenAsStamps
    )
  {
    UIComponent nodeStamp = comp.getFacet("nodeStamp");
    int oldRow = comp.getRowIndex();

    try
    {
      int first = comp.getFirst();
      int last = TableUtils.getLast(comp, first);
      for(int i=first; i<=last; i++)
      {
        comp.setRowIndex(i);
        if (processChildrenAsStamps)
          TableUtils.processStampedChildren(context, comp, phaseId);
        comp.processComponent(context, nodeStamp, phaseId);
        if (comp.isContainer() && (state.isContained()))
        {
          comp.enterContainer();

          try
          {
            __iterateOverTree( context,
                               phaseId,
                               comp,
                               state,
                               processChildrenAsStamps);
          }
          finally
          {
            comp.exitContainer();
          }
        }
      }
    }
    finally
    {
      comp.setRowIndex(oldRow);
    }
  }

  // sets the currency to the row or container that we want to start rendering
  // from:
  static boolean __setStartDepthPath(
    UIXHierarchy component,
    int          startDepth)
  {
    boolean isNewPath = false;
    Object focusKey = component.getFocusRowKey();

    if (focusKey != null )
    {
      List<Object> focusPath = component.getAllAncestorContainerRowKeys(focusKey);
      focusPath = new ArrayList<Object>(focusPath);
      focusPath.add(focusKey);
      int focusSize =  focusPath.size();
      if ( focusSize > startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusPath.get(startDepth));
      }
      else if ( focusSize == startDepth )
      {
        isNewPath = true;
        component.setRowKey(focusKey);
        component.enterContainer();
      }
    }
    else
    {
      if (startDepth  == 0)
      {
        isNewPath = true;
        component.setRowKey(null);
      }
    }

    return isNewPath;
  }

  private HierarchyUtils()
  {
  }
}
