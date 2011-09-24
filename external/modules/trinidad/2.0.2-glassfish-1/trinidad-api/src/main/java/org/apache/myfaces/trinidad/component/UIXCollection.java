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
import java.io.ObjectInputStream;
import java.io.Serializable;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitContextWrapper;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ComponentSystemEvent;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;
import javax.faces.render.Renderer;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFComponent;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.ComponentContextChange;
import org.apache.myfaces.trinidad.context.ComponentContextManager;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.LocalRowKeyIndex;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.render.ClientRowKeyManager;
import org.apache.myfaces.trinidad.render.ClientRowKeyManagerFactory;
import org.apache.myfaces.trinidad.util.ComponentUtils;


/**
 * Base class for components that do stamping.
 * This class set the EL 'var' variable correctly when the rowData changes.
 * And it wraps events that are queued, so that the correct rowData can be
 * restored on this component when the event is broadcast.
 */
@JSFComponent
public abstract class UIXCollection extends UIXComponentBase
  implements NamingContainer
{
  static public final FacesBean.Type TYPE = new FacesBean.Type(
    UIXComponentBase.TYPE);
  static public final PropertyKey VAR_KEY =
    TYPE.registerKey("var", String.class, PropertyKey.CAP_NOT_BOUND);

  protected UIXCollection(String rendererType)
  {
    super(rendererType);
  }

  protected UIXCollection()
  {
    this(null);
  }

  /**
   * Gets the name of the EL variable used to reference each element of
   * this collection.  Once this component has completed rendering, this
   * variable is removed (or reverted back to its previous value).
   */
  final public String getVar()
  {
    return ComponentUtils.resolveString(getProperty(VAR_KEY));
  }

  /**
   * Sets the name of the EL variable used to reference each element of
   * this collection.  Once this component has completed rendering, this
   * variable is removed (or reverted back to its previous value).
   */
  final public void setVar(String var)
  {
    setProperty(VAR_KEY, (var));
    InternalState iState = _getInternalState(false);
    if (iState != null)
    {
      iState._var = var;
    }
  }

  /**
   * Queues an event. If there is a currency set on this table, then
   * the event will be wrapped so that when it is finally delivered, the correct
   * currency will be restored.
   * @param event a FacesEvent
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    if (event.getSource() == this)
    {
      // Remember non-SelectionEvents on ourselves.  This
      // is a hack to support validation in tableSelectXyz.
      if (!(event instanceof SelectionEvent))
      {
        InternalState iState = _getInternalState(true);
        iState._hasEvent = true;
      }
    }

    // we want to wrap up
    // the event so we can execute it in the correct context (with the correct
    // rowKey/rowData):
    Object currencyKey = getRowKey();
    event = new TableRowEvent(this, event, currencyKey);

    // Queue a CollectionContextEvent in order to allow this class to setup the component change
    // before sub-classes attempt to process the table row event instance.
    super.queueEvent(new CollectionContextEvent(this, event));
  }

  /**
   * Delivers a wrapped event to the appropriate component.
   * If the event is a special wrapped event, it is unwrapped.
   * @param event a FacesEvent
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    // Unwrap CollectionContextEvent events so that the original event is broadcast
    // within a component change event context.
    if (event instanceof CollectionContextEvent)
    {
      _setupContextChange();
      try
      {
        this.broadcast(((CollectionContextEvent)event).getEvent());
      }
      finally
      {
        _tearDownContextChange();
      }
    }
    else
    {
      // For "TableRowEvents", set up the data before firing the
      // event to the actual component.
      if (event instanceof TableRowEvent)
      {
        TableRowEvent rowEvent = (TableRowEvent) event;
        Object old = getRowKey();
        setRowKey(rowEvent.getCurrencyKey());
        FacesEvent wrapped = rowEvent.getEvent();
        wrapped.getComponent().broadcast(wrapped);
        setRowKey(old);
      }
      else
      {
        super.broadcast(event);
      }
    }
  }

  /**
   * Decodes this component before decoding the facets.
   * Decodes the children as many times as they are stamped.
   * @param context the FacesContext
   */
  @Override
  public final void processDecodes(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    _setupContextChange();
    try
    {
      _init();

      InternalState iState = _getInternalState(true);
      iState._isFirstRender = false;

      if (!isRendered())
        return;

      __flushCachedModel();

      // Make sure _hasEvent is false.
      iState._hasEvent = false;

      // =-=AEW Because I'm getting the state in decode(), I need
      // to do it before iterating over the children - otherwise,
      // they'll be working off the wrong startIndex.  When state
      // management is integrated, I can likely put this back in the
      // usual order

      // Process this component itself
      decode(context);

      // Process all facets and children of this component
      decodeChildren(context);
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  @Override
  protected void decodeChildrenImpl(FacesContext context)
  {
    processFacetsAndChildren(context, PhaseId.APPLY_REQUEST_VALUES);
  }

  @Override
  protected void validateChildrenImpl(FacesContext context)
  {
    processFacetsAndChildren(context, PhaseId.PROCESS_VALIDATIONS);
  }

  @Override
  protected void updateChildrenImpl(FacesContext context)
  {
    processFacetsAndChildren(context, PhaseId.UPDATE_MODEL_VALUES);
  }

  /**
   * Resets this component's stamps to their
   * uninitialized state. This is useful when the user wants to
   * undo any edits made to an editable table.
   */
  public void resetStampState()
  {
    InternalState iState = _getInternalState(true);
    // TODO: this is over kill. for eg, It clears out any toggled showDetails.
    Object initKey = _getCurrencyKeyForInitialStampState();
    // do not clear the initial stamp state: a subtle bug could
    // result where the initial state of each component is gone, so we
    // fail to roll back to the initial default values
    if (iState._stampState != null)
      iState._stampState.clear(initKey);
  }

  @Override
  public Object processSaveState(FacesContext context)
  {
    _setupContextChange();
    try
    {
      // If we saved state in the middle of processing a row,
      // then make sure that we revert to a "null" rowKey while
      // saving state;  this is necessary to ensure that the
      // current row's state is properly preserved, and that
      // the children are reset to their default state.
      Object currencyKey = _getCurrencyKey();

      // since this is the end of the request, we expect the row currency to be reset back to null
      // setting it and leaving it there might introduce multiple issues, so log a warning here
      if (currencyKey != null)
      {
        if (_LOG.isWarning())
        {
          String scopedId = ComponentUtils.getScopedIdForComponent(this, context.getViewRoot());
          String viewId = context.getViewRoot()==null?null:context.getViewRoot().getViewId();
          _LOG.warning("ROWKEY_NOT_RESET", new Object[]{scopedId, viewId});
        }
      }

      Object initKey = _getCurrencyKeyForInitialStampState();
      if (currencyKey != initKey) // beware of null currencyKeys if equals() is used
      {
        setRowKey(initKey);
      }

      Object savedState = super.processSaveState(context);

      if (currencyKey != initKey) // beware of null currencyKeys if equals() is used
      {
        setRowKey(currencyKey);
      }

      // Finally clean up any internal model state that we might be holding on to. We do not want to hold onto any
      // application data in between requests
      InternalState iState = _getInternalState(false);
      if (iState != null)
      {
        iState._value = null;
        iState._model= null;
      }

      return savedState;
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  @Override
  public Object saveState(FacesContext context)
  {
    // _stampState is stored as an instance variable, so it isn't
    // automatically saved
    Object superState = super.saveState(context);
    final Object stampState, clientKeyMgr;

    // becareful not to create the internal state too early:
    // otherwise, the internal state will be shared between
    // nested table stamps:
    InternalState iState = _getInternalState(false);
    if (iState != null)
    {
      stampState = iState._stampState;
      clientKeyMgr = iState._clientKeyMgr;
    }
    else
    {
      stampState = null;
      clientKeyMgr = null;
    }

    if ((superState != null) || (stampState != null) || (clientKeyMgr != null))
      return new Object[]{superState, stampState, clientKeyMgr};
    return null;
  }


  @SuppressWarnings("unchecked")
  @Override
  public void restoreState(FacesContext context, Object state)
  {
    final Object superState, stampState, clientKeyMgr;
    Object[] array = (Object[]) state;
    if (array != null)
    {
      superState = array[0];
      stampState = array[1];
      clientKeyMgr = array[2];
    }
    else
    {
      superState = null;
      stampState = null;
      clientKeyMgr = null;
    }
    super.restoreState(context, superState);

    if ((stampState != null) || (clientKeyMgr != null))
    {
      InternalState iState = _getInternalState(true);
      iState._stampState = (StampState) stampState;
      iState._clientKeyMgr = (ClientRowKeyManager) clientKeyMgr;
    }
    else
    {
      // becareful not to force creation of the internal state
      // too early:
      InternalState iState = _getInternalState(false);
      if (iState != null)
      {
        iState._stampState = null;
        iState._clientKeyMgr = null;
      }
    }
  }

  /**
   * Checks to see if the current row is available. This is useful when the
   * total number of rows is not known.
   * @see CollectionModel#isRowAvailable()
   * @return true iff the current row is available.
   */
  public final boolean isRowAvailable()
  {
    return getCollectionModel().isRowAvailable();
  }

  /**
   * Check for an available row by row key.
   * @param rowKey the row key for the row to check.
   * @return true if a value exists; false otherwise.
   */
  public final boolean isRowAvailable(Object rowKey)
  {
    return getCollectionModel().isRowAvailable(rowKey);
  }

  /**
   * Get row data by row key.
   * @param rowKey the row key for the row to get data.
   * @return row data
   */
  public final Object getRowData(Object rowKey)
  {
    return getCollectionModel().getRowData(rowKey);
  }

  /**
   * Check if a range of rows is available starting from the current position
   * @param rowCount number of rows to check
   * @return true if all rows in range are available
   */
  public final boolean areRowsAvailable(int rowCount)
  {
    return getCollectionModel().areRowsAvailable(rowCount);
  }

  /**
   * Check if a range of rows is available from a starting index without
   * requiring the client to iterate over the rows
   * @param startIndex the starting index for the range
   * @param rowCount number of rows to check
   * @return true if all rows in range are available
   */
  public final boolean areRowsAvailable(int startIndex, int rowCount)
  {
    return getCollectionModel().areRowsAvailable(startIndex, rowCount);
  }

  /**
   * Check if a range of rows is available from a starting row key without
   * requiring the client to iterate over the rows
   * @param startRowKey the starting row key for the range
   * @param rowCount number of rows to check
   * @return true if all rows in range are available
   */
  public final boolean areRowsAvailable(Object startRowKey, int rowCount)
  {
    return getCollectionModel().areRowsAvailable(startRowKey, rowCount);
  }



  /**
   * Gets the total number of rows in this table.
   * @see CollectionModel#getRowCount
   * @return -1 if the total number is not known.
   */
  public final int getRowCount()
  {
    return getCollectionModel().getRowCount();
  }

  /**
   * Gets the index of the current row.
   * @see CollectionModel#getRowIndex
   * @return -1 if the current row is unavailable.
   */
  public final int getRowIndex()
  {
    return getCollectionModel().getRowIndex();
  }

  /**
   * Gets the rowKey of the current row.
   * @see CollectionModel#getRowKey
   * @return null if the current row is unavailable.
   */
  public final Object getRowKey()
  {
    InternalState iState = _getInternalState(true);
    if (iState._currentRowKey == _NULL)
    {
      // See bug 4534104.
      // Sometimes the rowKey for a particular row changes during update model
      // (this happens in ADFM if you edit the primary key of a row).
      // It is bad if the rowKey changes after _restoreStampState() and
      // before _saveStampState(). Therefore, we cache it:
      iState._currentRowKey = getCollectionModel().getRowKey();
    }

    return iState._currentRowKey;
  }

  /**
   * Gets the data for the current row.
   * @see CollectionModel#getRowData(int)
   * @return null if the current row is unavailable
   */
  public final Object getRowData()
  {
    CollectionModel model = getCollectionModel();
    // we need to call isRowAvailable() here because the 1.0 sun RI was
    // throwing exceptions when getRowData() was called with rowIndex=-1
    return model.isRowAvailable() ? model.getRowData() : null;
  }

  /**
   * Checks to see if the row at the given index is available.
   * @see CollectionModel#isRowAvailable(int)
   * @param rowIndex the index of the row to check.
   * @return true if data for the row exists.
   */
  public boolean isRowAvailable(int rowIndex)
  {
    return getCollectionModel().isRowAvailable(rowIndex);
  }

  /**
   * Gets the rowData at the given index.
   * @see CollectionModel#getRowData(int)
   * @param rowIndex the index of the row to get data from.
   * @return the data for the given row.
   */
  public Object getRowData(int rowIndex)
  {
    return getCollectionModel().getRowData(rowIndex);
  }

  /**
   * Gets the EL variable name to use to expose the varStatusMap.
   * @see #createVarStatusMap()
   */
  public abstract String getVarStatus();

  /**
   * Makes a row current.
   * This method calls {@link #preRowDataChange} and
   * {@link #postRowDataChange} as appropriate.
   * @see CollectionModel#setRowKey
   * @param rowKey The rowKey of the row that should be made current. Use null
   * to clear the current row.
   */
  public void setRowKey(Object rowKey)
  {
    _verifyComponentInContext();

    preRowDataChange();
    getCollectionModel().setRowKey(rowKey);
    postRowDataChange();
    if (_LOG.isFine() && (rowKey != null) && (!isRowAvailable()))
      _LOG.fine("no row available for rowKey:"+rowKey);
  }

  /**
   * Makes a row current.
   * This method calls {@link #preRowDataChange} and
   * {@link #postRowDataChange} as appropriate.
   * @see CollectionModel#setRowIndex
   * @param rowIndex The rowIndex of the row that should be made current. Use -1
   * to clear the current row.
   */
  public void setRowIndex(int rowIndex)
  {
    _verifyComponentInContext();

    preRowDataChange();
    getCollectionModel().setRowIndex(rowIndex);
    postRowDataChange();
    if (_LOG.isFine() && (rowIndex != -1) && (!isRowAvailable()))
      _LOG.fine("no row available for rowIndex:"+rowIndex);
  }

  /**
   * @param property a property name in the model
   * @return  true if the model is sortable by the given property.
   * @see CollectionModel#isSortable
   */
  public final boolean isSortable(String property)
  {
    return getCollectionModel().isSortable(property);
  }

  /**
   * Sorts this collection by the given criteria.
   * @param criteria Each element in this List must be of type SortCriterion.
   * @see org.apache.myfaces.trinidad.model.SortCriterion
   * @see CollectionModel#setSortCriteria
   */
  public void setSortCriteria(List<SortCriterion> criteria)
  {
    getCollectionModel().setSortCriteria(criteria);
  }

  /**
   * Gets the criteria that this collection is sorted by.
   * @return each element in this List is of type SortCriterion.
   * An empty list is returned if this collection is not sorted.
   * @see org.apache.myfaces.trinidad.model.SortCriterion
   * @see CollectionModel#getSortCriteria
   */
  public final List<SortCriterion> getSortCriteria()
  {
    return getCollectionModel().getSortCriteria();
  }

  /**
   * Clear the rowKey-to-currencyString cache.
   * The cache is not cleared immediately; instead it will be cleared
   * when {@link #encodeBegin(FacesContext)} is called.
   * @deprecated Have your Renderer implement {@link ClientRowKeyManagerFactory}
   * and create your own {@link ClientRowKeyManager} instances. Then you can
   * manage the lifecycle of each key inside your ClientRowKeyManager.
   */
  @Deprecated
  protected void clearCurrencyStringCache()
  {
    _getInternalState(true)._clearTokenCache = true;
  }

  /**
   * Clears all the currency strings.
   */
  @Override
  public final void encodeBegin(FacesContext context) throws IOException
  {
    _setupContextChange();
    boolean teardown = true;
    try
    {
      _init();

      InternalState istate = _getInternalState(true);
      // we must not clear the currency cache everytime. only clear
      // it in response to specific events: bug 4773659

      // TODO all this code should be removed and moved into the renderer:
      if (istate._clearTokenCache)
      {
        istate._clearTokenCache = false;
        ClientRowKeyManager keyMgr = getClientRowKeyManager();
        if (keyMgr instanceof DefaultClientKeyManager)
          ((DefaultClientKeyManager) keyMgr).clear();
      }
      __flushCachedModel();

      Object assertKey = null;
      assert ((assertKey = getRowKey()) != null) || true;
      __encodeBegin(context);
      // make sure that the rendering code preserves the currency:
      assert _assertKeyPreserved(assertKey) : "CurrencyKey not preserved";

      teardown = false;
    }
    finally
    {
      if (teardown)
      {
        // Tear down on errors & exceptions
        _tearDownContextChange();
      }
    }
  }

  @Override
  public void encodeEnd(FacesContext context) throws IOException
  {
    try
    {
      Object assertKey = null;
      assert ((assertKey = getRowKey()) != null) || true;
      super.encodeEnd(context);
      // make sure that the rendering code preserves the currency:
      assert _assertKeyPreserved(assertKey) : "CurrencyKey not preserved";
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  @Override
  protected void setupVisitingContext(FacesContext context)
  {
    super.setupVisitingContext(context);
    _setupContextChange();
  }

  @Override
  protected void tearDownVisitingContext(FacesContext context)
  {
    _tearDownContextChange();
    super.tearDownVisitingContext(context);
  }

  private boolean _assertKeyPreserved(Object oldKey)
  {
    Object newKey = getRowKey();
    return (oldKey != null) ? oldKey.equals(newKey) : (newKey == null);
  }

  void __encodeBegin(FacesContext context) throws IOException
  {
    super.encodeBegin(context);
  }

  /**
   * Checks to see if processDecodes was called. If this returns true
   * then this is the initial request, and processDecodes has not been called.
   */
  boolean __isFirstRender()
  {
    InternalState iState = _getInternalState(true);
    return iState._isFirstRender;
  }

  /**
   * @deprecated use getClientRowKey
   * @see #getClientRowKey
   */
  @Deprecated
  public String getCurrencyString()
  {
    return getClientRowKey();
  }

  /**
   * @deprecated use setClientRowKey
   * @see #setClientRowKey
   */
  @Deprecated
  public void setCurrencyString(String currency)
  {
    setClientRowKey(currency);
  }


  /**
   * Gets a String version of the current rowkey.
   * The contents of the String are controlled by the current
   * {@link ClientRowKeyManager}.
   * This String can be passed into the {@link #setClientRowKey} method
   * to restore the current rowData.
   * The lifetime of this String is short and it may not be valid during
   * future requests; however, it is guaranteed to be valid
   * for the next subsequent request.
   * @see UIXCollection#setClientRowKey(java.lang.String)
   * @see UIXCollection#getClientRowKeyManager()
   * @see ClientRowKeyManager#getClientRowKey
   */
  public String getClientRowKey()
  {
    // only call getCurrencyKey if we already have a dataModel.
    // otherwise behave as though no currency was set.
    // we need to do this because we don't want dataModel created for components
    // that are not rendered. The faces RI calls getClientId even on components
    // that are not rendered and this in turn was calling this method:
    Object currencyObject = _getCurrencyKey();
    if (currencyObject == null)
      return null;

    Object initKey = _getCurrencyKeyForInitialStampState();
    if (_equals(currencyObject, initKey))
      return null;

    FacesContext fc = FacesContext.getCurrentInstance();
    String key = getClientRowKeyManager().getClientRowKey(fc, this, currencyObject);
    return key;
  }

  /**
   * This is a safe way of getting currency keys and not accidentally forcing
   * the model to execute. When rendered="false" we should never execute the model.
   * However, the JSF engine calls certain methods when rendered="false" such as
   * processSaveState and getClientId.
   * Those methods, in turn, get the CurrencyKey.
   */
  private Object _getCurrencyKey()
  {
    // use false so that we don't create an internal state.
    // if the internal state is created too soon, then the same internal
    // state will get shared across all nested table instances.
    // this was causing bug 4616844:
    InternalState iState = _getInternalState(false);
    if (iState == null)
      return null;

    return (iState._model != null)
      ? getRowKey()
      : _getCurrencyKeyForInitialStampState();
  }

  /**
     * Restores this component's rowData to be what it was when the given
     * client rowKey string was created.
     * @see UIXCollection#getClientRowKey()
     */
  public void setClientRowKey(String clientRowKey)
  {
    if (clientRowKey == null)
    {
      setRowKey(_getCurrencyKeyForInitialStampState());
      return;
    }

    FacesContext fc = FacesContext.getCurrentInstance();
    Object rowkey = getClientRowKeyManager().getRowKey(fc, this, clientRowKey);

    if (rowkey == null)
    {
      _LOG.severe("CANNOT_FIND_ROWKEY",clientRowKey);
    }
    else
      setRowKey(rowkey);
  }

  public void processRestoreState(
    FacesContext context,
    Object       state)
  {
    _setupContextChange();
    try
    {
      super.processRestoreState(context, state);
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  public void processUpdates(FacesContext context)
  {
    _setupContextChange();
    try
    {
      super.processUpdates(context);
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  public void processValidators(FacesContext context)
  {
    _setupContextChange();
    try
    {
      super.processValidators(context);
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  public void processEvent(ComponentSystemEvent event)
    throws AbortProcessingException
  {
    _setupContextChange();
    try
    {
      super.processEvent(event);
    }
    finally
    {
      _tearDownContextChange();
    }
  }

  /**
     * Gets the client-id of this component, without any NamingContainers.
     * This id changes depending on the currency Object.
     * Because this implementation uses currency strings, the local client ID is
     * not stable for very long. Its lifetime is the same as that of a
     * currency string.
     * @see UIXCollection#getClientRowKey()
     * @return the local clientId
     */
  @Override
  public final String getContainerClientId(FacesContext context)
  {
    String id = getClientId(context);
    String key = getClientRowKey();
    if (key != null)
    {
      StringBuilder bld = __getSharedStringBuilder();
      bld.append(id).append(NamingContainer.SEPARATOR_CHAR).append(key);
      id = bld.toString();
    }

    return id;
  }

  /**
   * Prepares this component for a change in the rowData.
   * This method should be called right before the rowData changes.
   * It saves the internal states of all the stamps of this component
   * so that they can be restored when the rowData is reverted.
   */
  protected final void preRowDataChange()
  {
    _saveStampState();
    InternalState iState = _getInternalState(true);
    // mark the cached rowKey as invalid:
    iState._currentRowKey = _NULL;
  }

  /**
   * Sets up this component to use the new rowData.
   * This method should be called right after the rowData changes.
   * It sets up the var EL variable to be the current rowData.
   * It also sets up the internal states of all the stamps of this component
   * to match this new rowData.
   */
  protected final void postRowDataChange()
  {
    Object rowData = getRowData();
    if (_LOG.isFinest() && (rowData == null))
    {
      _LOG.finest("rowData is null at rowIndex:"+getRowIndex()+
                  " and currencyKey:"+getRowKey());
    }

    InternalState iState = _getInternalState(true);
    if (rowData == null)
    {
      // if the rowData is null, then we will restore the EL 'var' variable
      // to be whatever the value was, before this component started rendering:
      if (iState._prevVarValue != _NULL)
      {
        _setELVar(iState._var, iState._prevVarValue);
        iState._prevVarValue = _NULL;
      }
      if (iState._prevVarStatus != _NULL)
      {
        _setELVar(iState._varStatus, iState._prevVarStatus);
        iState._prevVarStatus = _NULL;
      }
    }
    else
    {
      if (iState._var != null)
      {
        Object oldData = _setELVar(iState._var, rowData);
        if (iState._prevVarValue == _NULL)
          iState._prevVarValue = oldData;
      }

      // varStatus is not set per row. It is only set once.
      // if _PrevVarStatus has not been assigned, then we have not set the
      // varStatus yet:
      if ((iState._varStatus != null) && (iState._prevVarStatus == _NULL))
      {
        Map<String, Object> varStatusMap = createVarStatusMap();
        iState._prevVarStatus = _setELVar(iState._varStatus, varStatusMap);
      }
    }

    _restoreStampState();

    // ensure the client IDs are reset on the component, otherwise they will not get the
    // proper stamped IDs. This mirrors the behavior in UIData and follows the JSF specification
    // on when client IDs are allowed to be cached and when they must be reset
    List<UIComponent> stamps = getStamps();

    for (UIComponent stamp : stamps)
      UIXComponent.clearCachedClientIds(stamp);
  }

  /**
   * Gets the UIComponents that are considered stamps.
   * This implementation simply returns the children of this component.
   * @return each element must be of type UIComponent.
   */
  @SuppressWarnings("unchecked")
  protected List<UIComponent> getStamps()
  {
    return getChildren();
  }

  /**
   * Gets the currencyObject to setup the rowData to use to build initial
   * stamp state.
   * <p>
   *   This allows the collection model to have an initial row key outside of the UIComponent.
   *   Should the model be at a row that is not the first row, the component will restore the row
   *   back to the initial row key instead of a null row key once stamping is done.
   * </p>
   */
  private Object _getCurrencyKeyForInitialStampState()
  {
    InternalState iState = _getInternalState(false);
    if (iState == null)
      return null;

    Object rowKey = iState._initialStampStateKey;
    return (rowKey == _NULL) ? null : rowKey;
  }

  /**
   * Saves the state of a stamp. This method is called when the currency of this
   * component is changed so that the state of this stamp can be preserved, before
   * the stamp is updated with the state corresponding to the new currency.
   * This method recurses for the children and facets of the stamp.
   * @return this object must be Serializable if client-side state saving is
   * used.
   */
  @SuppressWarnings("unchecked")
  protected Object saveStampState(FacesContext context, UIComponent stamp)
  {
    if (stamp.isTransient())
      return Transient.TRUE;

    boolean needsTearDownContext = false;

    if(stamp instanceof FlattenedComponent && stamp instanceof UIXComponent)
    {
      ((UIXComponent)stamp).setupVisitingContext(context);
      needsTearDownContext = true;
    }

    Object[] state = null;

    try
    {
      // The structure we will use is:
      //   0: state of the stamp
      //   1: state of the children (an array)
      //   2: state of the facets (an array of name-key pairs)
      // If there is no facet state, we have a two-element array
      // If there is no facet state or child state, we have a one-elment array
      // If there is no state at all, we return null

      Object stampState = StampState.saveStampState(context, stamp);

      // StampState can never EVER be an Object array, as if we do,
      // we have no possible way of identifying the difference between
      // just having stamp state, and having stamp state + child/facet state
      assert(!(stampState instanceof Object[]));

      int facetCount = stamp.getFacetCount();

      if (facetCount > 0)
      {
        boolean facetStateIsEmpty = true;
        Object[] facetState = null;

        Map<String, UIComponent> facetMap = stamp.getFacets();

        int i = 0;
        for(Map.Entry<String, UIComponent> entry : facetMap.entrySet())
        {
          Object singleFacetState = saveStampState(context, entry.getValue());
          if ((singleFacetState == null) ||
              (singleFacetState == Transient.TRUE))
            continue;

          // Don't bother allocating anything until we have some non-null
          // and non-transient facet state
          if (facetStateIsEmpty)
          {
            facetStateIsEmpty = false;
            facetState = new Object[facetCount * 2];
          }

          int base = i * 2;
          assert(facetState != null);
          facetState[base] = entry.getKey();
          facetState[base + 1] = singleFacetState;
          i++;
        }

        // OK, we had something:  allocate the state array to three
        // entries, and insert the facet state at position 2
        if (!facetStateIsEmpty)
        {
          // trim the facetState array if necessary
          if(i < facetCount)
          {
            Object[] trimmed = new Object[i*2];
            System.arraycopy(facetState, 0, trimmed, 0, i*2);
            facetState = trimmed;
          }
          state = new Object[3];
          state[2] = facetState;
        }
      }

      // If we have any children, iterate through the array,
      // saving state
      Object childState = StampState.saveChildStampState(context,
                                                         stamp,
                                                         this);
      if (childState != null)
      {
        // If the state hasn't been allocated yet, we only
        // need a two-element array
        if (state == null)
          state = new Object[2];
        state[1] = childState;
      }

      // If we don't have an array, just return the stamp
      // state
      if (state == null)
        return stampState;

      // Otherwise, store the stamp state at index 0, and return
      state[0] = stampState;
    }
    finally
    {
      if(needsTearDownContext)
        ((UIXComponent)stamp).tearDownVisitingContext(context);
    }
    return state;
  }

  /**
   * Restores the state of a stamp. This method is called after the currency of this
   * component is changed so that the state of this stamp can be changed
   * to match the new currency.
   * This method recurses for the children and facets of the stamp.
   */
  @SuppressWarnings("unchecked")
  protected void restoreStampState(FacesContext context, UIComponent stamp,
                                   Object stampState)
  {
    // Just a transient component - return
    if ((stampState == Transient.TRUE) || (stampState == null))
    {
      return;
    }

    // If this isn't an Object array, then it's a component with state
    // of its own, but no child/facet state - so restore and be done
    if (!(stampState instanceof Object[]))
    {
      StampState.restoreStampState(context, stamp, stampState);
      // NOTE early return
      return;
    }

    Object[] state = (Object[]) stampState;
    int stateSize = state.length;
    // We always have at least one element if we get to here
    assert(stateSize >= 1);

    StampState.restoreStampState(context, stamp, state[0]);


    // If there's any facet state, restore it
    if (stateSize >= 3)
    {
      Object[] facetStateArray = (Object[]) state[2];
      // This had better be non-null, otherwise we never
      // should have allocated a three-element array!
      assert(facetStateArray != null);

      for(int i=0; i<facetStateArray.length; i+=2)
      {
        String facetName = (String) facetStateArray[i];
        Object facetState = facetStateArray[i + 1];
        if (facetState != Transient.TRUE)
          restoreStampState(context, stamp.getFacet(facetName), facetState);
      }
    }

    // If there's any child state, restore it
    if (stateSize >= 2)
    {
      StampState.restoreChildStampState(context,
                                        stamp,
                                        this,
                                        state[1]);
    }
  }

  /**
   * Process a component.
   * This method calls {@link #processDecodes(FacesContext)},
   * {@link #processValidators} or
   * {@link #processUpdates}
   * depending on the {#link PhaseId}.
   */
  protected final void processComponent(
    FacesContext context,
    UIComponent  component,
    PhaseId      phaseId)
  {
    if (component != null)
    {
      if (phaseId == PhaseId.APPLY_REQUEST_VALUES)
        component.processDecodes(context);
      else if (phaseId == PhaseId.PROCESS_VALIDATIONS)
        component.processValidators(context);
      else if (phaseId == PhaseId.UPDATE_MODEL_VALUES)
        component.processUpdates(context);
      else
        throw new IllegalArgumentException(_LOG.getMessage(
          "BAD_PHASEID",phaseId));
    }
  }

  /**
   * Process this component's facets and children.
   * This method should call {@link #processComponent}
   * as many times as necessary for each facet and child.
   * {@link #processComponent}
   * may be called repeatedly for the same child if that child is
   * being stamped.
   */
  protected abstract void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId);

  /**
   * Gets the CollectionModel to use with this component.
   */
  protected final CollectionModel getCollectionModel()
  {
    return getCollectionModel(true);
  }

  /**
   * Gets the ClientRowKeyManager that is used to handle the
   * {@link #getClientRowKey} and
   * {@link #setClientRowKey} methods.
   * If the manager does not already exist a new one is created.
   * In order to create your own manager, your Renderer (for this component)
   * must implement
   * {@link ClientRowKeyManagerFactory}
   */
  public final ClientRowKeyManager getClientRowKeyManager()
  {
    // this method must be public, because specific renderers
    // need access to the ClientRowKeyManager so that they might prune it.

    InternalState iState = _getInternalState(true);
    if (iState._clientKeyMgr == null)
    {
      FacesContext fc = FacesContext.getCurrentInstance();
      Renderer r = getRenderer(fc);
      iState._clientKeyMgr = (r instanceof ClientRowKeyManagerFactory)
        ? ((ClientRowKeyManagerFactory) r).createClientRowKeyManager(fc, this)
        : new DefaultClientKeyManager();
    }
    return iState._clientKeyMgr;
  }

  public boolean invokeOnComponent(FacesContext context,
                                   String clientId,
                                   ContextCallback callback)
    throws FacesException
  {
    boolean invokedComponent;
    setupVisitingContext(context);

    try
    {
      String thisClientId = getClientId(context);
      if (clientId.equals(thisClientId))
      {
        if (!_getAndMarkFirstInvokeForRequest(context, clientId))
        {
          // Call _init() since __flushCachedModel() assumes that
          // selectedRowKeys and disclosedRowKeys are initialized to be non-null
          _init();

          __flushCachedModel();
        }

        pushComponentToEL(context, null);

        try
        {
          callback.invokeContextCallback(context, this);
        }
        finally
        {
          popComponentFromEL(context);
        }

        invokedComponent = true;
      }
      else
      {
        // If we're on a row, set the currency, and invoke
        // inside
        int thisClientIdLength = thisClientId.length();
        if (clientId.startsWith(thisClientId) &&
            (clientId.charAt(thisClientIdLength) == NamingContainer.SEPARATOR_CHAR))
        {
          if (!_getAndMarkFirstInvokeForRequest(context, thisClientId))
          {
            // Call _init() since __flushCachedModel() assumes that
            // selectedRowKeys and disclosedRowKeys are initialized to be non-null
            _init();

            __flushCachedModel();
          }

          String postId = clientId.substring(thisClientIdLength + 1);
          int sepIndex = postId.indexOf(NamingContainer.SEPARATOR_CHAR);
          // If there's no separator character afterwards, then this
          // isn't a row key
          if (sepIndex < 0)
            return invokeOnChildrenComponents(context, clientId, callback);
          else
          {
            String currencyString = postId.substring(0, sepIndex);
            Object rowKey = getClientRowKeyManager().getRowKey(context, this, currencyString);

            // A non-null rowKey here means we are on a row and we should set currency,  otherwise
            // the client id is for a non-stamped child component in the table/column header/footer.
            if (rowKey != null)
            {
              Object oldRowKey = getRowKey();
              try
              {
                setRowKey(rowKey);
                invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
              }
              finally
              {
                // And restore the currency
                setRowKey(oldRowKey);
              }
            }
            else
            {
              invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
            }
          }
        }
        else
        {
          // clientId isn't in this subtree
          invokedComponent = false;
        }
      }
    }
    finally
    {
      tearDownVisitingContext(context);
    }

    return invokedComponent;
  }

  /**
   * <p>
   * Override default children visiting code to visit the facets and facets of the columns
   * before delegating to the <code>visitData</code> to visit the individual rows of data.
   * </p><p>
   * Subclasses should override this method if they wish to change the way in which the non-stamped
   * children are visited.  If they wish to change the wash the the stamped children are visited,
   * they should override <code>visitData</code> instead.
   * </p>
   * @param visitContext
   * @param callback
   * @return <code>true</code> if all of the children to visit have been visited
   * @see #visitData
   */
  @Override
  protected boolean visitChildren(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    return defaultVisitChildren(visitContext, callback);
  }

  /**
   * Performs a non-iterating visit of the children.  The default implementation visits all
   * of the children.  If the UIXCollection subclass doesn't visit some of its children in
   * certain cases, it needs to override this method.
   * @param visitContext
   * @param callback
   * @return
   */
  protected boolean visitChildrenWithoutIterating(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    return visitAllChildren(visitContext, callback);
  }

  /**
   * Default implementation of child visiting of UIXCollection subclasses for cases where a
   * UIXCollection subclass wants to restore the default implementation that one of its
   * superclasses have overridden.
   * @param visitContext
   * @param callback
   * @return
   */
  protected final boolean defaultVisitChildren(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    if (ComponentUtils.isSkipIterationVisit(visitContext))
    {
      return visitChildrenWithoutIterating(visitContext, callback);
    }
    else
    {
      boolean doneVisiting;

      // Clear out the row index if one is set so that
      // we start from a clean slate.
      int oldRowIndex = getRowIndex();
      setRowIndex(-1);

      try
      {
        // visit the unstamped children
        doneVisiting = visitUnstampedFacets(visitContext, callback);

        if (!doneVisiting)
        {
          doneVisiting = _visitStampedColumnFacets(visitContext, callback);

          // visit the stamped children
          if (!doneVisiting)
          {
            doneVisiting = visitData(visitContext, callback);
          }
        }
      }
      finally
      {
        // restore the original rowIndex
        setRowIndex(oldRowIndex);
      }

      return doneVisiting;
    }
  }

  /**
   * Hook method for subclasses to override to change the behavior
   * of how unstamped facets of the UIXCollection are visited.  The
   * Default implementation visits all of the facets of the
   * UIXCollection.
   */
  protected boolean visitUnstampedFacets(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    // Visit the facets with no row
    if (getFacetCount() > 0)
    {
      for (UIComponent facet : getFacets().values())
      {
        if (UIXComponent.visitTree(visitContext, facet, callback))
        {
          return true;
        }
      }
    }

    return false;
  }


  /**
   * VistiContext that visits the facets of the UIXColumn children, including
   * nested UIXColumn childrem
   */
  private static class ColumnFacetsOnlyVisitContext extends VisitContextWrapper
  {
    public ColumnFacetsOnlyVisitContext(VisitContext wrappedContext)
    {
      _wrapped = wrappedContext;
    }

    @Override
    public VisitContext getWrapped()
    {
      return _wrapped;
    }

    @Override
    public VisitResult invokeVisitCallback(UIComponent component, VisitCallback callback)
    {
      if (component instanceof UIXColumn)
      {
        if (component.getFacetCount() > 0)
        {
          // visit the facet children without filtering for just UIXColumn children
          for (UIComponent facetChild : component.getFacets().values())
          {
            if (UIXComponent.visitTree(getWrapped(), facetChild, callback))
              return VisitResult.COMPLETE;
          }

          // visit the indexed children, recursively looking for more columns
          for (UIComponent child : component.getChildren())
          {
            if (UIXComponent.visitTree(this, child, callback))
              return VisitResult.COMPLETE;
          }
        }
      }

      // at this point, we either have already manually processed the UIXColumn's children, or
      // the component wasn't a UIXColumn and shouldn't be processed
      return VisitResult.REJECT;
    }

    private final VisitContext _wrapped;
  }

  /**
   * VisitContext implementation that doesn't visit any of the Facets of
   * UIXColumn children.  This is used when stamping children
   */
  protected static final class NoColumnFacetsVisitContext extends VisitContextWrapper
  {
    NoColumnFacetsVisitContext(VisitContext wrapped)
    {
      _wrapped = wrapped;
    }

    @Override
    public VisitContext getWrapped()
    {
      return _wrapped;
    }

    @Override
    public VisitResult invokeVisitCallback(UIComponent component, VisitCallback callback)
    {
      if (component instanceof UIXColumn)
      {
        if (component.getChildCount() > 0)
        {
          // visit only the indexed children of the columns
          for (UIComponent child : component.getChildren())
          {
            if (UIXComponent.visitTree(this, child, callback))
              return VisitResult.COMPLETE;
          }
        }

        return VisitResult.REJECT;
      }
      else
      {
        if (UIXComponent.visitTree(getWrapped(), component, callback))
          return VisitResult.COMPLETE;
        else
          return VisitResult.REJECT;
      }
    }

    private final VisitContext _wrapped;
  }

  /**
   * Implementation used to visit each stamped row
   */
  private boolean _visitStampedColumnFacets(
    VisitContext      visitContext,
    VisitCallback     callback)
  {
    // visit the facets of the stamped columns
    List<UIComponent> stamps = getStamps();

    if (!stamps.isEmpty())
    {
      VisitContext columnVisitingContext = new ColumnFacetsOnlyVisitContext(visitContext);

      for (UIComponent stamp : stamps)
      {
        if (UIXComponent.visitTree(columnVisitingContext, stamp, callback))
        {
          return true;
        }
      }
    }

    return false;
  }


  /**
   * Visit the rows and children of the columns of the collection per row-index. This should
   * not visit row index -1 (it will be perfomed in the visitTree method). The columns
   * themselves should not be visited, only their children in this function.
   *
   * @param visitContext The visiting context
   * @param callback The visit callback
   * @return true if the visiting should stop
   * @see #visitChildren(VisitContext, VisitCallback)
   */
  protected abstract boolean visitData(
    VisitContext  visitContext,
    VisitCallback callback);

  /**
   * Gets the CollectionModel to use with this component.
   *
   * @param createIfNull  creates the collection model if necessary
   */
  protected final CollectionModel getCollectionModel(
    boolean createIfNull)
  {
    InternalState iState = _getInternalState(true);
    if (iState._model == null && createIfNull)
    {
      //  _init() is usually called from either processDecodes or encodeBegin.
      //  Sometimes both processDecodes and encodeBegin may not be called,
      //  but processSaveState is called (this happens when
      //  component's rendered attr is set to false). We need to make sure that
      //  _init() is called in that case as well. Otherwise we get nasty NPEs.
      //  safest place is to call it here:
      _init();

      iState._value = getValue();
      iState._model = createCollectionModel(null, iState._value);
      postCreateCollectionModel(iState._model);
      assert iState._model != null;
    }
    // model might not have been created if createIfNull is false:
    if ((iState._initialStampStateKey == _NULL) &&
        (iState._model != null))
    {
      // if we have not already initialized the initialStampStateKey
      // that means that we don't have any stamp-state to use as the default
      // state for rows that we have not seen yet. So...
      // we will use any stamp-state for the initial rowKey on the model
      // as the default stamp-state for all rows:
      iState._initialStampStateKey = iState._model.getRowKey();
    }
    return iState._model;
  }

  /**
   * Creates the CollectionModel to use with this component.
   * The state of the UIComponent with the new model instance is not fully initialized until
   * after this method returns. As a result,  other component attributes that need
   * a fully initialized model should not be initialized in this method.  Instead,
   * model-dependent initialization should be done in <code>postCreateCollectionModel</code>
   * @see #postCreateCollectionModel
   * @param current the current CollectionModel, or null if there is none.
   * @param value this is the value returned from {@link #getValue()}
   */
  protected abstract CollectionModel createCollectionModel(
    CollectionModel current,
    Object value);

  /**
    * Hook called with the result of <code>createCollectionModel</code>.
    * Subclasses can use this method to perform initialization after the CollectionModel
    * is fully initialized.
    * Subclassers should call super before accessing any component state to ensure
    * that superclass initialization has been performed.
    * @see #createCollectionModel
    * @param model The model instance returned by<code><createCollectionModel</code>
    */
  protected void postCreateCollectionModel(CollectionModel model)
  {
    // do nothing
  }


  /**
   * Gets the value that must be converted into a CollectionModel
   */
  protected abstract Object getValue();

  /**
   * Gets the Map to use as the "varStatus".
   * This implementation supports the following keys:<ul>
   * <li>model - returns the CollectionModel
   * <li>index - returns the current rowIndex
   * <li>rowKey - returns the current rowKey
   * <li>current - returns the current rowData
   * </ul>
   */
  protected Map<String, Object> createVarStatusMap()
  {
    return new AbstractMap<String, Object>()
    {
      @Override
      public Object get(Object key)
      {
        // some of these keys are from <c:forEach>, ie:
        // javax.servlet.jsp.jstl.core.LoopTagStatus
        if ("model".equals(key))
          return getCollectionModel();
        if ("rowKey".equals(key))
          return getRowKey();
        if ("index".equals(key)) // from jstl
          return Integer.valueOf(getRowIndex());
        if ("current".equals(key)) // from jstl
          return getRowData();
        return null;
      }

      @Override
      public Set<Map.Entry<String, Object>> entrySet()
      {
        return Collections.emptySet();
      }
    };
  }


  //
  // LocalRowKeyIndex implementation
  //

  /**
   * Given a row index, check if a row is locally available
   * @param rowIndex index of row to check
   * @return true if row is locally available
   */
  public boolean isRowLocallyAvailable(int rowIndex)
  {
    return getCollectionModel().isRowLocallyAvailable(rowIndex);
  }

  /**
   * Given a row key, check if a row is locally available
   * @param rowKey row key for the row to check
   * @return true if row is locally available
   */
  public boolean isRowLocallyAvailable(Object rowKey)
  {
    return getCollectionModel().isRowLocallyAvailable(rowKey);
  }

  /**
   * Check if a range of rows is locally available starting from current position
   * @param rowCount number of rows in the range
   * @return true if range of rows is locally available
   */
  public boolean areRowsLocallyAvailable(int rowCount)
  {
    return getCollectionModel().areRowsLocallyAvailable(rowCount);
  }

  /**
   * Check if a range of rows is locally available starting from a row index
   * @param startIndex staring index for the range
   * @param rowCount number of rows in the range
   * @return true if range of rows is locally available
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowCount)
  {
    return getCollectionModel().areRowsLocallyAvailable(startIndex, rowCount);
  }

  /**
   * Check if a range of rows is locally available starting from a row key
   * @param startRowKey staring row key for the range
   * @param rowCount number of rows in the range
   * @return true if range of rows is locally available
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowCount)
  {
    return getCollectionModel().areRowsLocallyAvailable(startRowKey, rowCount);
  }

  /**
   * Convenient API to return a row count estimate.  This method can be optimized
   * to avoid a data fetch which may be required to return an exact row count
   * @return estimated row count
   */
  public int getEstimatedRowCount()
  {
    return getCollectionModel().getEstimatedRowCount();
  }


  /**
   * Helper API to determine if the row count returned from {@link #getEstimatedRowCount}
   * is EXACT, or an ESTIMATE
   */
  public LocalRowKeyIndex.Confidence getEstimatedRowCountConfidence()
  {
    return getCollectionModel().getEstimatedRowCountConfidence();
  }

  /**
   * clear all rows from the local cache
   */
  public void clearLocalCache()
  {
    getCollectionModel().clearLocalCache();
  }

  /**
   * Clear the requested range of rows from the local cache
   * @param startingIndex starting row index for the range to clear
   * @param rowsToClear number of rows to clear from the cache
   */
  public void clearCachedRows(int startingIndex,  int rowsToClear)
  {
    getCollectionModel().clearCachedRows(startingIndex, rowsToClear);
  }

  /**
   * Clear the requested range of rows from the local cache
   * @param startingRowKey starting row key for the range to clear
   * @param rowsToClear number of rows to clear from the cache
   */
  public void clearCachedRows(Object startingRowKey, int rowsToClear)
  {
    getCollectionModel().clearCachedRows(startingRowKey, rowsToClear);
  }

  /**
   * Clear a row from the local cache by row index
   * @param index row index for the row to clear from the cache
   */
  public void clearCachedRow(int index)
  {
    getCollectionModel().clearCachedRow(index);
  }

  /**
   * Clear a row from the local cache by row key
   * @param rowKey row key for the row to clear from the cache
   */
  public void clearCachedRow(Object rowKey)
  {
    getCollectionModel().clearCachedRow(rowKey);
  }

  /**
   * Indicates the caching strategy supported by the model
   * @see LocalRowKeyIndex.LocalCachingStrategy
   * @return caching strategy supported by the model
   */
  public LocalRowKeyIndex.LocalCachingStrategy getCachingStrategy()
  {
    return getCollectionModel().getCachingStrategy();
  }


  /**
   * override this method to place initialization code that must run
   * once this component is created and the jsp engine has finished setting
   * attributes on it.
   */
  void __init()
  {
    InternalState iState = _getInternalState(true);
    iState._var = getVar();
    if (_LOG.isFine() && (iState._var == null))
    {
      _LOG.fine("'var' attribute is null.");
    }
    iState._varStatus = getVarStatus();
    if (_LOG.isFinest() && (iState._varStatus == null))
    {
      _LOG.finest("'varStatus' attribute is null.");
    }
 }

  /**
   * Hook for subclasses like UIXIterator to initialize and flush the cache when visting flattened
   * children when parented by a renderer that needs to use
   * UIXComponent.processFlattenedChildren().
   * This is to mimic what happens in the non flattening case where similar logic is invoked
   * during encodeBegin().
   */
  void __processFlattenedChildrenBegin()
  {
    // Call _init() since __flushCachedModel() assumes that
    // selectedRowKeys and disclosedRowKeys are initialized to be non-null.
    _init();
    __flushCachedModel();
  }

  private void _init()
  {
    InternalState iState = _getInternalState(true);
    if (!iState._isInitialized)
    {
      assert iState._model == null;
      iState._isInitialized = true;
      __init();
    }
  }

  void __flushCachedModel()
  {
    InternalState iState = _getInternalState(true);
    Object value = getValue();
    if (iState._value != value)
    {
      iState._value = value;
      iState._model = createCollectionModel(iState._model, value);
      postCreateCollectionModel(iState._model);
    }
  }

  //
  // Returns true if this is the first request to invokeOnComponent()
  //
  static private boolean _getAndMarkFirstInvokeForRequest(
    FacesContext context, String clientId)
  {
    // See if the request contains a marker that we've hit this
    // method already for this clientId
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    String key = _INVOKE_KEY + clientId;
    // Yep, we have, so return true
    if (requestMap.containsKey(key))
      return true;

    // Stash TRUE for next time, and return false
    requestMap.put(key, Boolean.TRUE);
    return false;
  }

  /**
   * Gets the internal state of this component.
   * This is to support table within table.
   */
  Object __getMyStampState()
  {
    return _state;
  }

  /**
   * Sets the internal state of this component.
   * This is to support table within table.
   * @param stampState the internal state is obtained from this object.
   */
  void __setMyStampState(Object stampState)
  {
    InternalState iState = (InternalState) stampState;
    _state = iState;
  }

  /**
   * Returns true if an event (other than a selection event)
   * has been queued for this component.  This is a hack
   * to support validation in the tableSelectXyz components.
   */
  boolean __hasEvent()
  {
    InternalState iState = _getInternalState(true);
    return iState._hasEvent;
  }

  /**
   * Saves the state of all the stamps of this component.
   * This method should be called before the rowData of this component
   * changes. This method gets all the stamps using {@link #getStamps} and
   * saves their states by calling {@link #saveStampState}.
   */
  private void _saveStampState()
  {
    // Never read and created by _getStampState
    //InternalState iState = _getInternalState(true);

    StampState stampState = _getStampState();
    FacesContext context = getFacesContext();
    Object currencyObj = getRowKey();

    // Note: even though the currencyObj may be null, we still need to save the state. The reason
    // is that the code does not clear out the state when it is saved, instead, the un-stamped
    // state is saved. Once the row key is set back to null, this un-stamped state is restored
    // onto the children components. This restoration allows editable value holders, show detail
    // items and nested UIXCollections to clear their state.
    // For nested UIXCollections, this un-stamped state is required to set the nested collection's
    // _state (internal state containing the stamp state) to null when not on a row key. Without
    // that call, the nested UIXCollection components would end up sharing the same stamp state
    // across parent rows.

    int position = 0;
    for (UIComponent stamp : getStamps())
    {
      Object state = saveStampState(context, stamp);
//      String stampId = stamp.getId();
      // TODO
      // temporarily use position. later we need to use ID's to access
      // stamp state everywhere, and special case NamingContainers:
      String stampId = String.valueOf(position++);
      stampState.put(currencyObj, stampId, state);
      if (_LOG.isFinest())
        _LOG.finest("saving stamp state for currencyObject:"+currencyObj+
          " and stampId:"+stampId);
    }
  }


  /**
   * Restores the state of all the stamps of this component.
   * This method should be called after the currency of this component
   * changes. This method gets all the stamps using {@link #getStamps} and
   * restores their states by calling
   * {@link #restoreStampState}.
   */
  private void _restoreStampState()
  {
    StampState stampState = _getStampState();
    FacesContext context = getFacesContext();
    Object currencyObj = getRowKey();
    int position = 0;
    for(UIComponent stamp : getStamps())
    {
//      String stampId = stamp.getId();
      // TODO
      // temporarily use position. later we need to use ID's to access
      // stamp state everywhere, and special case NamingContainers:
      String stampId = String.valueOf(position++);
      Object state = stampState.get(currencyObj, stampId);
      if (state == null)
      {
        Object iniStateObj = _getCurrencyKeyForInitialStampState();
        state = stampState.get(iniStateObj, stampId);
        /*
        if (state==null)
        {
          _LOG.severe("NO_INITIAL_STAMP_STATE", new Object[]{currencyObj,iniStateObj,stampId});
          continue;
        }*/
      }
      restoreStampState(context, stamp, state);
    }
  }

  private InternalState _getInternalState(boolean create)
  {
    if ((_state == null) && create)
    {
      _state = new InternalState();
    }
    return _state;
  }

  private StampState _getStampState()
  {
    InternalState iState = _getInternalState(true);
    if (iState._stampState == null)
      iState._stampState = new StampState();

    return iState._stampState;
  }

  /**
   * sets an EL variable.
   * @param varName the name of the variable
   * @param newData the value of the variable
   * @return the old value of the variable, or null if there was no old value.
   */
  private Object _setELVar(String varName, Object newData)
  {
    if (varName == null)
      return null;

    // we need to place each row at an EL reachable place so that it
    // can be accessed via the 'var' variable. Let's place it on the
    // requestMap:
    return TableUtils.setupELVariable(getFacesContext(), varName, newData);
  }

  private static boolean _equals(Object a, Object b)
  {
    if (b == null)
      return (a == null);

    return b.equals(a);
  }

  private void _setupContextChange()
  {
    if (_inSuspendOrResume)
    {
      // This situation will occur when the CollectionComponentChange is currently setting the
      // row key.
      return;
    }

    ComponentContextManager compCtxMgr =
      RequestContext.getCurrentInstance().getComponentContextManager();

    compCtxMgr.pushChange(new CollectionComponentChange(this));
  }

  private void _tearDownContextChange()
  {
    if (_inSuspendOrResume)
    {
      // This situation will occur when the CollectionComponentChange is currently setting the
      // row key.
      return;
    }

    try
    {
      ComponentContextManager compCtxMgr =
        RequestContext.getCurrentInstance().getComponentContextManager();
      ComponentContextChange change = compCtxMgr.peekChange();

      if (change instanceof CollectionComponentChange &&
          ((CollectionComponentChange)change)._component == this)
      {
        // Remove the component context change if one was added
        compCtxMgr.popChange();
      }
      else
      {
        _LOG.severe("COLLECTION_CHANGE_TEARDOWN", new Object[] { getId(), change });
      }
    }
    catch (RuntimeException re)
    {
      _LOG.severe(re);
    }
  }

  private void _verifyComponentInContext()
  {
    if (_inSuspendOrResume)
    {
      return;
    }

    ComponentContextManager compCtxMgr =
      RequestContext.getCurrentInstance().getComponentContextManager();
    ComponentContextChange change = compCtxMgr.peekChange();

    if (!(change instanceof CollectionComponentChange) ||
        ((CollectionComponentChange)change)._component != this)
    {
      _LOG.warning("COLLECTION_NOT_IN_CONTEXT", getId());
      if (_LOG.isFine())
      {
        Thread.currentThread().dumpStack();
      }
    }
  }

  private static final class DefaultClientKeyManager extends ClientRowKeyManager
  {
    public void clear()
    {
      _currencyCache.clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getRowKey(FacesContext context, UIComponent component, String clientRowKey)
    {
      ValueMap<Object,String> currencyCache = _currencyCache;
      Object rowkey = currencyCache.getKey(clientRowKey);
      return rowkey;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getClientRowKey(FacesContext context, UIComponent component, Object rowKey)
    {
      assert rowKey != null;

      ValueMap<Object,String> currencyCache = _currencyCache;
      String key = currencyCache.get(rowKey);
      // check to see if we already have a string key:
      if (key == null)
      {
        // we don't have a string-key, so create a new one.
        key = _createToken(currencyCache);

        if (_LOG.isFiner())
          _LOG.finer("Storing token:"+key+
                     " for rowKey:"+rowKey);

        currencyCache.put(rowKey, key);
      }
      return key;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean replaceRowKey(FacesContext context, UIComponent component, Object oldRowKey, Object newRowKey)
    {
      assert oldRowKey != null && newRowKey != null;

      ValueMap<Object,String> currencyCache = _currencyCache;
      String key = currencyCache.remove(oldRowKey);
      // check to see if we already have a string key:
      if (key != null)
      {
        currencyCache.put(newRowKey, key);
      }
      return key != null;
    }



    private static String _createToken(ValueMap<Object,String> currencyCache)
    {
      String key = String.valueOf(currencyCache.size());
      return key;
    }

    private ValueMap<Object,String> _currencyCache = new ValueMap<Object,String>();
    private static final long serialVersionUID = 1L;
  }

  // this component's internal state is stored in an inner class
  // rather than in individual fields, because we want to make it
  // easy to quickly suck out or restore its internal state,
  // when this component is itself used as a stamp inside some other
  // stamping container, eg: nested tables.
  private static final class InternalState implements Serializable
  {
    private transient boolean _hasEvent = false;
    private transient Object _prevVarValue = _NULL;
    private transient Object _prevVarStatus = _NULL;
    private transient String _var = null;
    private transient String _varStatus = null;
    private transient Object _value = null;
    private transient CollectionModel _model = null;
    private transient Object _currentRowKey = _NULL;
    private transient boolean _clearTokenCache = false;
    // this is true if this is the first request for this viewID and processDecodes
    // was not called:
    private transient boolean _isFirstRender = true;
    private transient boolean _isInitialized = false;
    // this is the rowKey used to retrieve the default stamp-state for all rows:
    private transient Object _initialStampStateKey = _NULL;

    private ClientRowKeyManager _clientKeyMgr = null;
    private StampState _stampState = null;

    private void readObject(ObjectInputStream in)
       throws IOException, ClassNotFoundException
    {
      in.defaultReadObject();
      // Set values of all transients to their defaults
      _prevVarValue = _NULL;
      _prevVarStatus = _NULL;
      _currentRowKey = _NULL;
      _initialStampStateKey = _NULL;
      // However, leave _isFirstRender set to false - since that's
      // necessarily the state we'd be in if we're reconstituting this
      _isFirstRender = false;
    }

    private static final long serialVersionUID = 1L;
  }

  /**
   * Class to be able to suspend the context of the collection.
   * <p>Current implementation removes the var and varStatus from the request while the
   * collection is suspended.</p>
   */
  private static class CollectionComponentChange
    extends ComponentContextChange
  {
    private CollectionComponentChange(
      UIXCollection component)
    {
      _component = component;
    }

    public void suspend(
      FacesContext facesContext)
    {
      _component._inSuspendOrResume = true;

      try
      {
        InternalState iState = _component._getInternalState(false);
        if (iState == null || iState._model == null || iState._currentRowKey == _NULL)
        {
          // If we were to try to call getRowKey() here, this would call getCollectionModel().
          // The get collection model may result in EL being evaluated, which is undesirable
          // and will cause bugs when called while we are suspending. This is because evaluating
          // EL may need to suspend or resume other component context changes, and we do not want
          // re-entrant calls to the component context stack while we are already suspending.

          // Note that this code will fail if someone has set the _model to null while on a rowKey
          // (Should not happen, would be considered a bug if that were to be done).
          _rowKey = null;
        }
        else
        {
          _rowKey = _component.getRowKey();

          // Set the row key back to null to force the collection into the un-stamped state. This
          // will ensure that the collection is not in a row key while the component context is
          // not setup. Only do this if the row key is not already on the null row key.
          if (_rowKey != null)
          {
            _component.setRowKey(null);
          }
        }
      }
      finally
      {
        _component._inSuspendOrResume = false;
      }
    }

    public void resume(
      FacesContext facesContext)
    {
      _component._inSuspendOrResume = true;
      try
      {
        // Only set the row key if one was stored during the suspend.
        if (_rowKey != null)
        {
          _component.setRowKey(_rowKey);
        }
      }
      finally
      {
        _component._inSuspendOrResume = false;
      }
    }

    @Override
    public String toString()
    {
      String className = _component.getClass().getName();
      String componentId = _component.getId();
      return new StringBuilder(58 + className.length() + componentId.length())
        .append("UIXCollection.CollectionComponentChange[Component class: ")
        .append(className)
        .append(", component ID: ")
        .append(componentId)
        .append("]")
        .toString();
    }

    private final UIXCollection _component;
    private CollectionModel _collectionModel;
    private Object _rowKey;
  }

  private static class CollectionContextEvent
    extends WrapperEvent
  {
    public CollectionContextEvent(
      UIComponent source,
      FacesEvent  event)
    {
      super(source, event);
    }

    @SuppressWarnings("compatibility:-7639429485707197863")
    private static final long serialVersionUID = 1L;
  }

  // do not assign a non-null value. values should be assigned lazily. this is
  // because this value is preserved by stampStateSaving, when table-within-table
  // is used. And if a non-null value is used, then all nested tables will
  // end up sharing this stampState. see bug 4279735:
  private InternalState _state = null;
  private boolean _inSuspendOrResume = false;

  // use this key to indicate uninitialized state.
  // all the variables that use this are transient so this object need not
  // be Serializable:
  private static final Object _NULL = new Object();
  private static final String _INVOKE_KEY =
    UIXCollection.class.getName() + ".INVOKE";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXCollection.class);

  // An enum to throw into state-saving so that we get a nice
  // instance-equality to test against for noticing transient components
  // (and better serialization results)
  // We need this instead of just using null - because transient components
  // are specially handled, since they may or may not actually still
  // be there when you go to restore state later (e.g., on the next request!)
  enum Transient { TRUE };
}
