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

import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIComponent;

import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;

import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.util.ComponentUtils;


/**
 * Base class for the Page component.
 *
 * @version $Name:  $ ($Revision: 1090836 $) $Date: 2011-04-10 10:12:59 -0700 (Sun, 10 Apr 2011) $
 */
abstract public class UIXPageTemplate extends UIXMenuHierarchy
{
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet state);
/**/  public abstract MethodBinding getRowDisclosureListener();
/**/  static public final PropertyKey DISCLOSED_ROW_KEYS_KEY = null;

  /**
   * Sets the phaseID of UI events depending on the "immediate" property.
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    TableUtils.__handleQueueEvent(this, event);
    super.queueEvent(event);
  }

  /**
   * Delivers an event.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    HierarchyUtils.__handleBroadcast(this,
                                      event,
                                      getDisclosedRowKeys(),
                                      getRowDisclosureListener());
    super.broadcast(event);
  }

  @Override
 public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    TreeModel model = (TreeModel)super.createCollectionModel(current, value);
    RowKeySet treeState = getDisclosedRowKeys();
    treeState.setCollectionModel(model);
    return model;
  }

  @Override
  @SuppressWarnings("unchecked")
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    Object oldPath = getRowKey();
    setRowKey(null);

    HierarchyUtils.__iterateOverTree(context,
                                      phaseId,
                                      this,
                                      getDisclosedRowKeys(),
                                      false);

    setRowKey(oldPath);

    // process the children
    TableUtils.__processChildren(context, this, phaseId);

    Map<String, UIComponent> facets = getFacets();
    Iterator<String> facetKeys = facets.keySet().iterator();

    while(facetKeys.hasNext())
    {
      String facetKey = facetKeys.next();
      if (!"nodeStamp".equals(facetKey))
      {
        processComponent(context, facets.get(facetKey), phaseId);
      }
    }
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
      
      // process the non-stamp facet children
      if (!done)
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
                  done = true;
                  break;
                }
              }
            }
          }
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
    Object oldPath = getRowKey();

    // if we are only visiting rendered stamps, then pass in the disclosed row keys, otherwise
    // pass in null, indicating that all row keys should be visited
    RowKeySet disclosedRowKeys = (visitContext.getHints().contains(VisitHint.SKIP_UNRENDERED))
                                   ? getDisclosedRowKeys()
                                   : null;

    setRowKey(null);

    boolean done;
    
    try
    {
      done = visitHierarchy(visitContext, callback, getStamps(), disclosedRowKeys);
    }
    finally
    {
      setRowKey(oldPath);
    }
    
    return done;
  }

  @Override
  void __encodeBegin(FacesContext context) throws IOException
  {
    HierarchyUtils.__handleEncodeBegin(this, getDisclosedRowKeys());
    super.__encodeBegin(context);
  }

  @Override
  void __init()
  {
    super.__init();
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetTreeImpl());
  }

  @Override
  protected FacesBean createFacesBean(String rendererType)
  {
    return new RowKeyFacesBeanWrapper(super.createFacesBean(rendererType));
  }

  private class RowKeyFacesBeanWrapper
    extends FacesBeanWrapper
  {
    private boolean _retrievingDisclosedRows = false;

    RowKeyFacesBeanWrapper(FacesBean bean)
    {
      super(bean);
    }

    @Override
    public Object getProperty(PropertyKey key)
    {
      Object value = super.getProperty(key);

      if (key == DISCLOSED_ROW_KEYS_KEY && !_retrievingDisclosedRows && value instanceof RowKeySet)
      {
        // Ensure that when we are retrieving and setting the collection model, this property
        // is not asked for which would create an infinite loop
        _retrievingDisclosedRows = true;

        try
        {
          RowKeySet rowKeys = (RowKeySet) value;
          // row key sets need the most recent collection model, but there is no one common entry
          // point to set this on the set besides when code asks for the value from the bean
          __flushCachedModel();  //insist that we populate with the very lastest instance of the collection model
          rowKeys.setCollectionModel(getCollectionModel());
        }
        finally
        {
          _retrievingDisclosedRows = false;
        }
      }

      return value;
    }

    @Override
    public Object saveState(FacesContext context)
    {
      RowKeySet rowKeys = (RowKeySet)super.getProperty(DISCLOSED_ROW_KEYS_KEY);
      if (rowKeys != null)
      {
        // make sure the set does not pin the model in memory
        rowKeys.setCollectionModel(null);
      }
      return super.saveState(context);
    }
  }
}
