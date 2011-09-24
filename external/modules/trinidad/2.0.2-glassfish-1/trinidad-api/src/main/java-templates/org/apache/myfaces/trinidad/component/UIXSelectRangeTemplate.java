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
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.RangeChangeListener;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;


/**
 * Base class for SelectRange
 *
 * @version $Name:  $ ($Revision: 594297 $) $Date: 2007-11-12 13:03:17 -0800 (Mon, 12 Nov 2007) $
 */
public abstract class UIXSelectRangeTemplate extends UIXComponentBase
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public MethodExpression getRangeChangeListener();
/**/  abstract public void setFirst(int first);
/**/  abstract public boolean isImmediate();
/**/  abstract public Object getValue();

  @Deprecated
  public void setRangeChangeListener(MethodBinding binding)
  {
    setRangeChangeListener(adaptMethodBinding(binding));
  }

  @Override
  public void encodeBegin(FacesContext context) throws IOException
  {
    _flushCachedDataModel();
    super.encodeBegin(context);
  }

  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Notify the specified RangeChanged listener method (if any)
    if (event instanceof RangeChangeEvent)
    {
      RangeChangeEvent gtEvent = (RangeChangeEvent)event;
      // update first when the event is delivered
      setFirst(gtEvent.getNewStart());

      broadcastToMethodExpression(event, getRangeChangeListener());
    }

    // Perform standard superclass processing
    super.broadcast(event);
  }
  
  /**
   * * We don't want to update model if we have validation errors
   * on the page, so if not immediate, queue the event in
   * INVOKE_APPLICATION phase.
   */
  @Override
  public void queueEvent(FacesEvent e)
  {
    if ((e instanceof RangeChangeEvent) && (e.getSource() == this))
    {
      if (isImmediate())
      {
        e.setPhaseId(PhaseId.ANY_PHASE);
      }
      else
      {
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
      }
    }

    super.queueEvent(e);
  }

  /**
   * Makes a row current.
   * @see CollectionModel#setRowIndex
   * @param rowIndex the zero-based row-index of the row that should be made
   * current. Use -1 to clear the current row.
   */
  public final void setRowIndex(int rowIndex)
  {
    _getDataModel().setRowIndex(rowIndex);
  }

  /**
   * @see CollectionModel#getRowIndex
   * @return the zero-based row-index of the current row, or -1
   *  if now row is current.
   */
  public final int getRowIndex()
  {
    return _getDataModel().getRowIndex();
  }

  /**
   * Gets the total number of rows in this table.
   * @see CollectionModel#getRowCount
   * @return -1 if the total number is not known.
   */
  public final int getRowCount()
  {
    return _getDataModel().getRowCount();
  }

  /**
  * Checks to see if the current row is available. This is useful when the
  * total number of rows is not known.
  * @see CollectionModel#isRowAvailable
  * @return true iff the current row is available.
  */
   public final boolean isRowAvailable()
   {
     return _getDataModel().isRowAvailable();
   }

  /**
  * Checks to see if the given row is available. This is useful when the
  * total number of rows is not known.
  * @see CollectionModel#isRowAvailable(int)
  * @param rowIndex identifies the row to check
  * @return true iff the current row is available.
  */
   public final boolean isRowAvailable(int rowIndex)
   {
     return _getDataModel().isRowAvailable(rowIndex);
   }

  /**
   * Gets the data for the current row.
   * @see CollectionModel#getRowData
   * @return null if the current row is unavailable
   */
  public final Object getRowData()
  {
    CollectionModel model = _getDataModel();
    // we need to call isRowAvailable() here because the 1.0 sun RI was
    // throwing exceptions when getRowData() was called with rowIndex=-1
    return model.isRowAvailable() ? model.getRowData() : null;
  }

  /**
   * Gets the data for the current row.
   * @param rowIndex identifies the row to get data from
   * @see CollectionModel#getRowData(int)
   * @return null if the current row is unavailable
   */
  public final Object getRowData(int rowIndex)
  {
    CollectionModel model = _getDataModel();
    // we need to call isRowAvailable() here because the 1.0 sun RI was
    // throwing exceptions when getRowData() was called with rowIndex=-1
    return model.isRowAvailable(rowIndex) ? model.getRowData(rowIndex) : null;
  }

  private CollectionModel _getDataModel()
  {
    if (_dataModel == null)
    {
      Object value = getValue();

      _dataModel = ModelUtils.toCollectionModel(value);

    }

    return _dataModel;
  }

  //
  // Flush the cached data model, if needed
  //
  private void _flushCachedDataModel()
  {

     _dataModel = null;

  }

  private transient CollectionModel   _dataModel = null;
}
