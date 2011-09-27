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

import javax.faces.component.UIComponent;

import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;

import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.util.ComponentUtils;

/**
 * Base class for Tree component.
 * @version $Name:  $ ($Revision: 1096825 $) $Date: 2011-04-26 10:57:06 -0700 (Tue, 26 Apr 2011) $
 */
abstract public class UIXTreeTemplate extends UIXHierarchy
{
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet keys);
/**/  public abstract RowKeySet getSelectedRowKeys();
/**/  public abstract void setSelectedRowKeys(RowKeySet keys);
/**/  public abstract MethodExpression getRowDisclosureListener();
/**/  public abstract UIComponent getNodeStamp();
/**/  public abstract boolean isInitiallyExpanded();
/**/  static public final PropertyKey DISCLOSED_ROW_KEYS_KEY = null;
/**/  static public final PropertyKey SELECTED_ROW_KEYS_KEY = null;

/**/  static public final FacesBean.Type TYPE = new FacesBean.Type(org.apache.myfaces.trinidad.component.UIXHierarchy.TYPE);

  // These are "fake" properties that allow the table to get the disclosed row keys and the
  // selected row key without triggering the call to getCollectionModel from the
  // RowKeyFacesBeanWrapper class. See the stamp state saving code for its usage.
  static private final PropertyKey _DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY =
    TYPE.registerKey("disclosedRowKeysWithoutModel", RowKeySet.class);
  static private final PropertyKey _SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY =
    TYPE.registerKey("selectedRowKeysWithoutModel", RowKeySet.class);

  @Deprecated
  public void setRowDisclosureListener(MethodBinding binding)
  {
    setRowDisclosureListener(adaptMethodBinding(binding));
  }

  @Deprecated
  public void setSelectionListener(MethodBinding binding)
  {
    setSelectionListener(adaptMethodBinding(binding));
  }

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
    if (event instanceof SelectionEvent)
    {
      //pu: Implicitly record a Change for 'selectionState' attribute
      //=-=pu: This ain't getting restored. Check with Arj or file a bug.
      addAttributeChange("selectedRowKeys",
                         getSelectedRowKeys());
      broadcastToMethodExpression(event, getSelectionListener());
    }

    HierarchyUtils.__handleBroadcast(this,
                                      event,
                                      getDisclosedRowKeys(),
                                      getRowDisclosureListener());
    super.broadcast(event);
  }

  @Override
  protected void postCreateCollectionModel(CollectionModel model)
  {
    RowKeySet selectedRowKeys = getSelectedRowKeys();

    if (selectedRowKeys == null)
    {
      selectedRowKeys = new RowKeySetTreeImpl();
      setSelectedRowKeys(selectedRowKeys);
    }

    RowKeySet disclosedRowKeys = getDisclosedRowKeys();

    if (disclosedRowKeys == null)
    {
      disclosedRowKeys = new RowKeySetTreeImpl();
      setDisclosedRowKeys(disclosedRowKeys);
    }

    selectedRowKeys.setCollectionModel(model);
    disclosedRowKeys.setCollectionModel(model);
  }

  @Override
  public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    TreeModel model = ModelUtils.toTreeModel(value);
    model.setRowKey(null);
    return model;
  }

  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    // this component has no facets that need to be processed once.
    // instead process the "nodeStamp" facet as many times as necessary:
    HierarchyUtils.__iterateOverTree(context,
                                     phaseId,
                                     this,
                                     getDisclosedRowKeys(),
                                     true);
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
    // if we are only visiting rendered stamps, then pass in the disclosed row keys, otherwise
    // pass in null, indicating that all row keys should be visited
    RowKeySet disclosedRowKeys = (visitContext.getHints().contains(VisitHint.SKIP_UNRENDERED))
                                   ? getDisclosedRowKeys()
                                   : null;

    return visitHierarchy(visitContext, callback, getStamps(), disclosedRowKeys);
  }

  @Override
  void __init()
  {
    super.__init();
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetTreeImpl());
    if (getSelectedRowKeys() == null)
      setSelectedRowKeys(new RowKeySetTreeImpl());
  }


  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[4];
    state[0] = super.__getMyStampState();
    state[1] = getFocusRowKey();

    // Use "hidden" property keys to allow the row key sets to be retrieved without the
    // RowKeyFacesBeanWrapper trying to resolve the collection model to be set into the row key
    // set. This is needed to stop the unnecessary lookup of the collection model when it is not
    // needed during stamp state saving of the table.
    RowKeySet selectedRowKeys = (RowKeySet)getProperty(_SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY);
    RowKeySet disclosedRowKeys = (RowKeySet)getProperty(_DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY);

    state[2] = selectedRowKeys;
    state[3] = disclosedRowKeys;

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
    setFocusRowKey(state[1]);
    setSelectedRowKeys((RowKeySet) state[2]);
    setDisclosedRowKeys((RowKeySet) state[3]);
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
    private boolean _retrievingSelectedRows = false;

    RowKeyFacesBeanWrapper(FacesBean bean)
    {
      super(bean);
    }

    @Override
    public Object getProperty(PropertyKey key)
    {
      if (key == _DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY)
      {
        // This case is only true if the table is trying to serialize the disclosed row keys to
        // the stamp state of a parent UIXCollection. This work-around prevents EL evaluation to
        // get the collection model during stamp state saving. This should be permissible as the
        // state saving code does not need the collection model to be set in the row key set in
        // order to save its state.
        return super.getProperty(DISCLOSED_ROW_KEYS_KEY);
      }
      else if (key == _SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY)
      {
        // This case is only true if the table is trying to serialize the selected row keys to
        // the stamp state of a parent UIXCollection. This work-around prevents EL evaluation to
        // get the collection model during stamp state saving. This should be permissible as the
        // state saving code does not need the collection model to be set in the row key set in
        // order to save its state.
        return super.getProperty(SELECTED_ROW_KEYS_KEY);
      }

      Object value = super.getProperty(key);
      if (key == DISCLOSED_ROW_KEYS_KEY)
      {
        if (!_retrievingDisclosedRows && value instanceof RowKeySet)
        {
          // Ensure that when we are retrieving and setting the collection model, this property
          // is not asked for which would create an infinite loop
          _retrievingDisclosedRows = true;

          try
          {
            RowKeySet rowKeys = (RowKeySet) value;
            // row key sets need the most recent collection model, but there is no one common entry
            // point to set this on the set besides when code asks for the value from the bean
            rowKeys.setCollectionModel(getCollectionModel());
          }
          finally
          {
            _retrievingDisclosedRows = false;
          }
        }
      }
      else if (key == SELECTED_ROW_KEYS_KEY)
      {
        if (!_retrievingSelectedRows && value instanceof RowKeySet)
        {
          // Ensure that when we are retrieving and setting the collection model, this property
          // is not asked for which would create an infinite loop
          _retrievingSelectedRows = true;

          try
          {
            RowKeySet rowKeys = (RowKeySet) value;
            // row key sets need the most recent collection model, but there is no one common entry
            // point to set this on the set besides when code asks for the value from the bean
            rowKeys.setCollectionModel(getCollectionModel());
          }
          finally
          {
            _retrievingSelectedRows = false;
          }
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
      rowKeys = (RowKeySet)super.getProperty(SELECTED_ROW_KEYS_KEY);
      if (rowKeys != null)
      {
        // make sure the set does not pin the model in memory
        rowKeys.setCollectionModel(null);
      }
      return super.saveState(context);
    }
  }

  /**
   * @see org.apache.myfaces.trinidad.component.UIXCollection#__encodeBegin(javax.faces.context.FacesContext)
   */
  @Override @SuppressWarnings("unchecked")
  protected void __encodeBegin(FacesContext context) throws IOException
  {
    if (isInitiallyExpanded() && !Boolean.TRUE.equals(getAttributes().get(EXPAND_ONCE_KEY)))
    {
      Object oldRowKey = getRowKey();
      try
      {
        Object rowKey = getFocusRowKey();
        if (rowKey == null)
        {
          setRowIndex(0);
          rowKey = getRowKey();
        }

        setRowKey(rowKey);
        RowKeySet old = getDisclosedRowKeys();
        RowKeySet newset = old.clone();
        newset.addAll();

        // use an event to ensure the row disclosure listener is called
        broadcast(new RowDisclosureEvent(old, newset, this));
      }
      finally
      {
        setRowKey(oldRowKey);
      }

      // use attributes to store that we have processed the initial expansion
      // as there is no support for private properties in the plug-in at the
      // moment
      getAttributes().put(EXPAND_ONCE_KEY, Boolean.TRUE);
    }
    super.__encodeBegin(context);
  }

  private final static String EXPAND_ONCE_KEY = "initialExpandCompleted";
}
