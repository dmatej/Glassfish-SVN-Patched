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

import javax.el.MethodExpression;

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
 * Base class for the NavigationTree component.
 *
 * @version $Name:  $ ($Revision: 1090836 $) $Date: 2011-04-10 10:12:59 -0700 (Sun, 10 Apr 2011) $
 */
abstract public class UIXNavigationTreeTemplate extends UIXNavigationHierarchy
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public int getStartLevel();
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet state);
/**/  public abstract MethodExpression getRowDisclosureListener();
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
  @SuppressWarnings("unchecked")
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
    // this component has no facets that need to be processed once.
    // instead process the "nodeStamp" facet as many times as necessary:
    Object oldPath = getRowKey();
    try
    {
      HierarchyUtils.__setStartDepthPath(this, getStartLevel());
      HierarchyUtils.__iterateOverTree(context,
                                        phaseId,
                                        this,
                                        getDisclosedRowKeys(),
                                        true);
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
    if (ComponentUtils.isSkipIterationVisit(visitContext))
    {
      return visitChildrenWithoutIterating(visitContext, callback);
    }
    else
    {
      return visitData(visitContext, callback);
    }
  }

  @Override
  protected boolean visitData(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    Object oldRowKey = getRowKey();

    // if we are only visiting rendered stamps, then pass in the disclosed row keys, otherwise
    // pass in null, indicating that all row keys should be visited
    RowKeySet disclosedRowKeys = (visitContext.getHints().contains(VisitHint.SKIP_UNRENDERED))
                                   ? getDisclosedRowKeys()
                                   : null;

    boolean done;

    HierarchyUtils.__setStartDepthPath(this, getStartLevel());

    try
    {
      done = visitHierarchy(visitContext, callback, getStamps(), disclosedRowKeys);
    }
    finally
    {
      setRowKey(oldRowKey);
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
