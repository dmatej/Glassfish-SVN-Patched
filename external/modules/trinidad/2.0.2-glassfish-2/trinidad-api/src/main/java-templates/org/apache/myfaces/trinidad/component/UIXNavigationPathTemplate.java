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

import javax.faces.component.UIComponent;

import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;

import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.util.ComponentUtils;


/**
 * Base class for the NavigationPath component.
 *
 * @version $Name:  $ ($Revision: 1090836 $) $Date: 2011-04-10 10:12:59 -0700 (Sun, 10 Apr 2011) $
 */
abstract public class UIXNavigationPathTemplate extends UIXNavigationHierarchy
{
	
  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    Object oldPath = getRowKey();
    
    Object focusPath = getFocusRowKey();
    
    if (focusPath != null )
    {
      List<Object> paths = 
        new ArrayList<Object>(getAllAncestorContainerRowKeys(focusPath));
      
      paths.add(focusPath);
      int focusPathSize = paths.size();
      UIComponent nodeStamp = getFacet("nodeStamp");
      
      if (nodeStamp != null)
      {
        for (int i = 0; i < focusPathSize; i++)
        {
          setRowKey(paths.get(i));
          processComponent(context, nodeStamp, phaseId);
        }
      }
    }
    
    setRowKey(oldPath);
    
    // process the children
    TableUtils.__processChildren(context, this, phaseId);
  }  

  @Override
  protected boolean visitChildren(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    if (ComponentUtils.isSkipIterationVisit(visitContext))
    {
      return visitChildrenWithoutIterating(visitContext, callback);
    }
    else
    {
      return _visitChildrenIterating(visitContext, callback);
    }
  }

  private boolean _visitChildrenIterating(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    boolean done = visitData(visitContext, callback);
    
    if (!done)
    {
      // process the children
      int childCount = getChildCount();
      if (childCount > 0)
      {
        for (UIComponent child : getChildren())
        {
          done = UIXComponent.visitTree(visitContext, child, callback);
          
          if (done)
            break;
        }
      }          
    }
    
    return done;
  }
  
  @Override
  protected boolean visitData(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    Object focusPath = getFocusRowKey();
    Object oldRowKey = null;
    
    boolean done = false;
    
    // start from the focused area
    if (focusPath != null)
    {
      List<UIComponent> stamps = getStamps();
      
      if (!stamps.isEmpty())
      {
        List<Object> paths =  new ArrayList<Object>(getAllAncestorContainerRowKeys(focusPath));
        paths.add(focusPath);
        
        int focusPathSize = paths.size();
        
        try
        {
          for (int i = 0; i < focusPathSize && !done; i++)
          {
            setRowKey(paths.get(i));
            
            for (UIComponent stamp : stamps)
            {
              done = UIXComponent.visitTree(visitContext, stamp, callback);
              
              if (done)
                break;
            }
          }
        }
        finally
        {
          setRowKey(oldRowKey);
        }
      }
    }
    
    return done;
  } 
}
