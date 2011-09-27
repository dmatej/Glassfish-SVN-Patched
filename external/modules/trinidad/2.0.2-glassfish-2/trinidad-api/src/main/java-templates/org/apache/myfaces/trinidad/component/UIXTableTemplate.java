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
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;

import javax.el.MethodExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.event.SortEvent;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;
import org.apache.myfaces.trinidad.model.SortCriterion;

/**
 * Base class for the Table component. The behaviour supported by this base class
 * include record navigation, sorting, selection and detail-disclosure.
 * <p>
 * @version $Name:  $ ($Revision: 1096825 $) $Date: 2011-04-26 10:57:06 -0700 (Tue, 26 Apr 2011) $
 */
abstract public class UIXTableTemplate extends UIXIteratorTemplate
  implements CollectionComponent
{
/**/  static public final FacesBean.Type TYPE = new FacesBean.Type(UIXIterator.TYPE);

  // These are "fake" properties that allow the table to get the disclosed row keys and the
  // selected row key without triggering the call to getCollectionModel from the
  // RowKeyFacesBeanWrapper class. See the stamp state saving code for its usage.
  static private final PropertyKey _DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY =
    TYPE.registerKey("disclosedRowKeysWithoutModel", RowKeySet.class);
  static private final PropertyKey _SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY =
    TYPE.registerKey("selectedRowKeysWithoutModel", RowKeySet.class);


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
  void __encodeBegin(FacesContext context) throws IOException
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

  @Override
  public void setSortCriteria(List<SortCriterion> criteria)
  {
    _sortCriteria = criteria;
    super.setSortCriteria(criteria);
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
   * Delivers an event to the appropriate listeners.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    // the order of processing is
    // 1. do any default action handling
    // 2. invoke any actionListener method binding
    // 3. call all the registered ActionListener instances.

    // Deliver to the default RangeChangeListener
    if (event instanceof RangeChangeEvent)
    {
      RangeChangeEvent rEvent = (RangeChangeEvent) event;
      int first = rEvent.getNewStart();
      setFirst(first);
      //pu: Implicitly record a Change for 'first' attribute
      addAttributeChange("first", Integer.valueOf(first));

      if ((first == 0) && (rEvent.getNewEnd() == getRowCount()))
      {
        setShowAll(true);
        //pu: Implicitly record a Change for 'showAll' attribute
        addAttributeChange("showAll", Boolean.TRUE);
      }
      else if (isShowAll())
      {
        setShowAll(false);
        //pu: Implicitly record a Change for 'showAll' attribute
        addAttributeChange("showAll", Boolean.FALSE);
      }
      // since the range is now different we can clear the currency cache:
      clearCurrencyStringCache();

      broadcastToMethodExpression(event, getRangeChangeListener());
    }
    else if (event instanceof RowDisclosureEvent)
    {
      RowDisclosureEvent eEvent = (RowDisclosureEvent) event;
      RowKeySet set = getDisclosedRowKeys();
      set.addAll(eEvent.getAddedSet());
      set.removeAll(eEvent.getRemovedSet());
      addAttributeChange("disclosedRowKeys", set);
      broadcastToMethodExpression(event, getRowDisclosureListener());
    }
    else if (event instanceof SortEvent)
    {
      SortEvent sEvent = (SortEvent) event;
      setSortCriteria(sEvent.getSortCriteria());
      broadcastToMethodExpression(event, getSortListener());
    }
    else if (event instanceof SelectionEvent)
    {
      //pu: Implicitly record a Change for 'selectionState' attribute
      addAttributeChange("selectedRowKeys",
                         getSelectedRowKeys());
      broadcastToMethodExpression(event, getSelectionListener());
    }

    super.broadcast(event);
  }


/**/  abstract public void setDisclosedRowKeys(RowKeySet state);
/**/  abstract public RowKeySet getDisclosedRowKeys();
/**/  public abstract RowKeySet getSelectedRowKeys();
/**/  public abstract void setSelectedRowKeys(RowKeySet model);
/**/  abstract public void setFirst(int first);
/**/  abstract public void setShowAll(boolean showAll);
/**/  abstract public boolean isShowAll();
/**/  abstract public UIComponent getDetailStamp();
/**/  public abstract MethodExpression getRangeChangeListener();
/**/  public abstract MethodExpression getSortListener();
/**/  public abstract MethodExpression getRowDisclosureListener();
/**/  public abstract MethodExpression getSelectionListener();
/**/  public abstract boolean isImmediate();
/**/  static public final PropertyKey DISCLOSED_ROW_KEYS_KEY = null;
/**/  static public final PropertyKey SELECTED_ROW_KEYS_KEY = null;

  @Deprecated
  public void setRangeChangeListener(MethodBinding binding)
  {
    setRangeChangeListener(adaptMethodBinding(binding));
  }

  @Deprecated
  public void setSortListener(MethodBinding binding)
  {
    setSortListener(adaptMethodBinding(binding));
  }

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

  @Override
  @SuppressWarnings("unchecked")
  public Object saveState(FacesContext context)
  {
    Object o = super.saveState(context);
    if ((o == null) &&
        ((_sortCriteria == null) || _sortCriteria.isEmpty()))
      return null;

    return new Object[]{o, _sortCriteria};
  }

  @Override
  @SuppressWarnings("unchecked")
  public void restoreState(FacesContext context, Object state)
  {
    Object[] array = (Object[]) state;
    super.restoreState(context, array[0]);


    // Get the sort criteria - but *don't* call setSortCriteria()
    // here;  doing so would require getting the collection model,
    // and that may invoke client code that isn't quite in a state
    // to be invoked, in part because component "binding"s have not been
    // evaluated yet.
    List<SortCriterion> criteria = (List<SortCriterion>) array[1];
    _sortCriteria = criteria;
  }


  /**
   * Gets the data for the first selected row.
   * This is useful when using EL to get at column data for the selected
   * row when using a table with single selection.
   * @return null if there is nothing selected in the table.
   */
  public Object getSelectedRowData()
  {
    RowKeySet state = getSelectedRowKeys();
    Iterator<Object> keys = state.iterator();
    if (keys.hasNext())
    {
      Object key = keys.next();
      CollectionModel model = getCollectionModel();
      Object old = model.getRowKey();
      try
      {
        model.setRowKey(key);
        if (isRowAvailable())
          return model.getRowData();
      }
      finally
      {
        model.setRowKey(old);
      }
    }
    return null;
  }

  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    // process all the facets of this table just once
    // (except for the "detailStamp" facet which must be processed once
    // per row):
    TableUtils.processFacets(context, this, this, phaseId,
      UIXTable.DETAIL_STAMP_FACET);

    // process all the facets of this table's column children:
    TableUtils.processColumnFacets(context, this, this, phaseId);

    // process all the children and the detailStamp as many times as necessary
    processStamps(context, phaseId);
  }

  /**
   * Gets the stamps. This returns the children of this component plus
   * the detail stamp (if any).
   */
  // TODO cache the result
  @Override
  protected final List<UIComponent> getStamps()
  {
    List<UIComponent> children = super.getStamps();
    UIComponent detail = getDetailStamp();
    if (detail != null)
    {
      List<UIComponent> stamps = new ArrayList<UIComponent>(children.size() + 1);
      stamps.addAll(children);
      stamps.add(detail);
      return stamps;
    }
    return children;
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

  @Override
  protected final CollectionModel createCollectionModel(
    CollectionModel current,
    Object value)
  {
    return super.createCollectionModel(current, value);
  }

  @Override
  protected void postCreateCollectionModel(CollectionModel model)
  {
    RowKeySet selectedRowKeys = getSelectedRowKeys();

    if (selectedRowKeys == null)
    {
      selectedRowKeys = new RowKeySetImpl();
      setSelectedRowKeys(selectedRowKeys);
    }

    RowKeySet disclosedRowKeys = getDisclosedRowKeys();

    if (disclosedRowKeys == null)
    {
      disclosedRowKeys = new RowKeySetImpl();
      setDisclosedRowKeys(disclosedRowKeys);
    }

    selectedRowKeys.setCollectionModel(model);
    disclosedRowKeys.setCollectionModel(model);

    // If we were perviously sorted, restore the sort order:
    if (_sortCriteria != null)
    {
      model.setSortCriteria(_sortCriteria);
    }
  }

  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[6];
    state[0] = _sortCriteria;
    state[1] = super.__getMyStampState();
    state[2] = Integer.valueOf(getFirst());
    state[3] = Boolean.valueOf(isShowAll());

    // Use "hidden" property keys to allow the row key sets to be retrieved without the
    // RowKeyFacesBeanWrapper trying to resolve the collection model to be set into the row key
    // set. This is needed to stop the unnecessary lookup of the collection model when it is not
    // needed during stamp state saving of the table.
    RowKeySet selectedRowKeys = (RowKeySet)getProperty(_SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY);
    RowKeySet disclosedRowKeys = (RowKeySet)getProperty(_DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY);

    state[4] = selectedRowKeys;
    state[5] = disclosedRowKeys;

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
    _sortCriteria = (List<SortCriterion>) state[0];
    super.__setMyStampState(state[1]);
    setFirst(((Integer) state[2]).intValue());
    setShowAll(Boolean.TRUE == state[3]);
    setSelectedRowKeys((RowKeySet) state[4]);
    setDisclosedRowKeys((RowKeySet) state[5]);
  }

  protected void processStamps(
    FacesContext context,
    PhaseId phaseId)
  {
    // Process all the children
    CollectionModel tableData = getCollectionModel();
    if (tableData.getRowCount() != 0)
    {
      int startIndex = getFirst();
      int endIndex = isShowAll() ? getRowCount()-1 : TableUtils.getLast(this);

      UIComponent detail = getDetailStamp();
      RowKeySet disclosureState =
        (detail == null) ? null : getDisclosedRowKeys();

      for (int i = startIndex; i <= endIndex; i++)
      {
        setRowIndex(i);
        if (isRowAvailable())
        {
          TableUtils.processStampedChildren(context, this, phaseId);

          if ((disclosureState != null) && disclosureState.isContained())
          {
            assert getRowIndex() == i;
            processComponent(context, detail, phaseId);
          }
        }
      }

      setRowIndex(-1);
    }
  }

  /**
   * Is target a stamped child UIComponent in the table body
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

    TableUtils.cacheHeaderFooterFacets(this, _containerClientIdCache);
    TableUtils.cacheColumnHeaderFooterFacets(this, _containerClientIdCache);
  }


  @Override
  void __init()
  {
    super.__init();
    if (getSelectedRowKeys() == null)
      setSelectedRowKeys(new RowKeySetImpl());
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetImpl());
    // if "first" is valueBound, we can't deal with it changing
    // during the lifecycle. So stash it as a local value.
    // see bug 4537121:
    setFirst(getFirst());
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
            __flushCachedModel();  //insist that we populate with the very lastest instance of the collection model
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
            __flushCachedModel();  //insist that we populate with the very lastest instance of the collection model
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

  transient private List<SortCriterion> _sortCriteria = null;
  // cache of child components inside this table header/footer facets and column header/footer
  // facets
  transient private IdentityHashMap<UIComponent, Boolean> _containerClientIdCache = null;
}
