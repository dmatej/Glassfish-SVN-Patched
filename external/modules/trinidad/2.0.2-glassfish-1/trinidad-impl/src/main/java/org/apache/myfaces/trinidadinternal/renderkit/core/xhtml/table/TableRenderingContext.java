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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.CollectionComponent;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 */
public class TableRenderingContext
{
  static public TableRenderingContext getCurrentInstance()
  {
    return _CURRENT_CONTEXT.get();
  }

  public TableRenderingContext(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component)
  {
    CollectionComponent collectionComponent =
      (CollectionComponent) component;
    // =-=AEW Don't like this here:  move it out to the Renderer
    collectionComponent.setRowIndex(-1);

    String tableId = component.getClientId(context);

    _table = component;
    _tableId = tableId;
    _columnCount = component.getChildCount();
    _collectionComponent = collectionComponent;
    // Bug 3931544:  don't use colons in Javascript variable names.
    // We'll just replace colons with underscores;  not perfect, but adequate
    _jsVarName = "_uixt_" + XhtmlUtils.getJSIdentifier(tableId);

    _rowData    = new RowData(this);
    _columnData = new ColumnData();

    int length = getColumnCount();
    _hiddenColumns         = new int[length];

    _gatherChildInformation(component);

    _columnData.setColumnCount(length - _hiddenColumnCount);

    // create the RenderStage
    _renderStage = new RenderStage();

    _detail =       CoreRenderer.getFacet(component,
                                              CoreTable.DETAIL_STAMP_FACET);

    _tableWidth = _getAttr(component, CoreTable.WIDTH_KEY);
    // special secret attribute to get scrolling in tables for ECM:
    _tableHeight = component.getAttributes().get("height");

    int rows = collectionComponent.getRowCount();
    boolean hasNav =
      (rows > getRowData().getVisibleRowCount()) || (rows < 0);

    _hasNavigation = hasNav;
    String rowSelection = (String) _getAttr(component, CoreTable.ROW_SELECTION_KEY);
    if ("single".equals(rowSelection))
      _rowSelection = Boolean.FALSE;
    else if ("multiple".equals(rowSelection))
      _rowSelection = Boolean.TRUE;
    else
      _rowSelection = null;
      
    _verticalGrid = 
      ((Boolean) _getAttr(component, CoreTable.VERTICAL_GRID_VISIBLE_KEY)).booleanValue();
    _horizontalGrid = 
      ((Boolean) _getAttr(component, CoreTable.HORIZONTAL_GRID_VISIBLE_KEY)).booleanValue();
  }

  /**
   * Gets an attribute value from the component.
   */
  private Object _getAttr(UIComponent component, PropertyKey key)
  {
    // we use the attribute map, instead of the PropertyKey, because
    // this class needs to work with both table and treeTable:
    return component.getAttributes().get(key.getName());
  }
  
  public void install()
  {
    // Set up the ThreadLocal
    _previous = getCurrentInstance();
    _CURRENT_CONTEXT.set(this);
  }

  public void release()
  {
    _CURRENT_CONTEXT.set(_previous);
  }

  public final String getJSVarName()
  {
    return _jsVarName;
  }

  public final RenderStage getRenderStage()
  {
    return _renderStage;
  }


  public final Object getTableWidth()
  {
    return _tableWidth;
  }

  public final Object getTableHeight()
  {
    return _tableHeight;
  }

  /**
   * Constant indicating that this column is visible
   */
  public static final int NORMAL_COLUMN        = 0;

  /**
   * Constant indicating that this column is not rendered
   */
  public static final int HIDDEN_COLUMN        = 1;

  /**
   * Constant indicating that this column is a user-invisible column
   */
  public static final int USER_INVISIBLE_COLUMN    = 2;

  //
  // Methods for managing TableBean name transformations and current data.
  //

  /**
   * Enables/Disables explicit header ID mode.
   * The default is disabled.
   */
  public final void setExplicitHeaderIDMode(boolean isEnabled)
  {
    if (!XhtmlRenderer.isInaccessibleMode(
           RenderingContext.getCurrentInstance()))
      _explicitHeaderIDMode = isEnabled;
  }

  /**
   * In explicit header ID mode, all table data cells have a headers attribute
   * that lists the IDs of all the headers that apply to that data cell.
   * @return true if explicit header ID mode is turned on.
   */
  public final boolean isExplicitHeaderIDMode()
  {
    return _explicitHeaderIDMode;
  }


  //
  // Methods for accessing table data
  //

  /**
   * Get the component.
   */
  public final UIComponent getTable()
  {
    return _table;
  }

  /**
   * Get the table id
   */
  public final String getTableId()
  {
    return _tableId;
  }

  /**
   * Get the CollectionComponent
   */
  public final CollectionComponent getCollectionComponent()
  {
    return _collectionComponent;
  }

  /**
   * Gets the selection state for this table.
   * This is overwritten in HGridRenderingContext
   */
  public RowKeySet getSelectedRowKeys()
  {
    return ((UIXTable) getCollectionComponent()).getSelectedRowKeys();
  }

  /**
   * Is this table validated on navigation?
   */
  public final boolean isImmediate()
  {
    if (_immediate == null)
    {
      Object value = getTable().getAttributes().get("immediate");
      _immediate = _toBoolean(value, false);
    }

    return _immediate.booleanValue();
  }

  /**
   * @return true iff this table needs a navigation bar
   */
  public final boolean hasNavigation()
  {
    return _hasNavigation;
  }


  /**
   * @return the width of the specified column
   */
  public final Object getColumnWidth(int physicalIndex)
  {
    return _columnData.getWidth(physicalIndex);
  }


  /**
   * @return true if any of these columns require column header stamps
   * @see #columnHeadersPresent()
   */
  public final boolean hasColumnHeaders()
  {
    _hasColumnHeadersUsed = true;

    return _hasColumnHeaders;
  }

  /**
   * indicates that this table has column headers, so the tableRenderer
   * must render the table's columnHeader region.
   * @see #hasColumnHeaders()
   */
  public final void columnHeadersPresent()
  {
    if (_hasColumnHeadersUsed)
    {
      throw new IllegalStateException();
    }

    _hasColumnHeaders = true;
  }

  /**
   * sets the physical index of the details column
   */
  public final void setDetailColumnIndex(int physicalIndex)
  {
    _detailColumnIndex = physicalIndex;
  }

  /**
   * gets the physical index of the details column
   */
  public final int getDetailColumnIndex()
  {
    return _detailColumnIndex;
  }

  /**
   * Get the detail node (hide/show)
   */
  public UIComponent getDetail()
  {
    return _detail;
  }

  public final void setRowHidden(int index)
  {
    int rowCount = 0;

    if (_rowsHidden == null)
    {
      rowCount = index * 2;
      _rowsHidden = new boolean[rowCount];
    }
    else if (index >= _rowsHidden.length)
    {
      // have to add capacity to the array
      rowCount = Math.max(_rowsHidden.length, index) * 2;

      boolean[] newArray = new boolean[rowCount];

      System.arraycopy(_rowsHidden, 0, newArray,
                       0, _rowsHidden.length);

      _rowsHidden = newArray;
    }
    _rowsHidden[index] = true;
  }

  public final boolean isRowHidden(int index)
  {
    boolean rv = false;
    if ((_rowsHidden != null) && (_rowsHidden.length > index))
    {
      rv = _rowsHidden[index];
    }
    return rv;
  }

  /**
   * Does this table have "select all/none"?
   */
  public final boolean hasSelectAll()
  {
    return (Boolean.TRUE.equals(_rowSelection));
  }

  public final boolean hasSelection()
  {
    return (_rowSelection != null);
  }

  /**
   * Get a boolean array of columns, with true representing each hidden column.
   */
  public final int[] getHiddenColumns()
  {
    return _hiddenColumns;
  }


  /**
   * Get the count of indexed children.
   */
  public int getColumnCount()
  {
    return _columnCount;
  }

  /**
   * Get the actual column count.  This is the actual number of physical
   * columns in the table. It is the sum of the visible columns and any
   * special columns.  This calls getSpecialColumnCount and caches the
   * result.
   */
  public final int getActualColumnCount()
  {
    if (_actualColumnCount < 0)
    {
      int columns = _columnData.getColumnCount();
      _actualColumnCount = columns + getSpecialColumnCount();
    }

    return _actualColumnCount;
  }

  /**
   * Computes the number of special columns, including selection, detail
   * and row header.
   */
  public int getSpecialColumnCount()
  {
    int columns = 0;

    // add one for the selection, if necessary
    if (hasSelection())
      columns++;

    // add one for the detail, if necessary
    if (getDetail() != null)
      columns++;

    return columns;
  }

  public final BandingData getBanding()
  {
    if (_banding == null)
    {
      _banding = BandingData.create(this);
    }
    return _banding;
  }

  public final ColumnData getColumnData()
  {
    return _columnData;
  }

  public final RowData getRowData()
  {
    return _rowData;
  }

  /**
   * check to see if this column or row should render a grid before the
   * row or column at the specified index.
   * @param before the index of the row or the physicalIndex of the column
   * @param vertical true for columns (vertical grids),
   * false for rows (horizontal grids).
   * @return true for grid. false if no grid.
   * @todo Support definition of row grid banding??
   */
  public boolean hasGrid(int before, boolean vertical)
  {
    if (before >= 0)
    {
      return (vertical) ? _verticalGrid : _horizontalGrid;
    }
    return true;
  }

  /**
   * Returns the opaque nodeList object used by column groups
   * to store the header structure.
   */
  public Object getHeaderNodesList()
  {
    return _nodeList;
  }

  /**
   * Returns the opaque nodeList object used by column groups
   * to store the header structure.
   */
  public void setHeaderNodeList(Object nodeList)
  {
    _nodeList = nodeList;
  }

  //
  // Private methods
  //

  /**
   * Converts a value to a Boolean object efficiently.
   * @param value the value that needs to be converted.
   * @param defaultValue the Boolean to return if <code>value</code> is null
   *  or not of type Boolean.
   * @return Boolean.TRUE or FALSE depending on the instance of
   *  <code>value</code>. If <code>value</code> is not an instance of Boolean,
   *  then <code>defaultValue</code> is returned.
   */
  static private Boolean _toBoolean(Object value, boolean defaultValue)
  {
    if (defaultValue)
    {
      return (Boolean.FALSE.equals(value)) ? Boolean.FALSE : Boolean.TRUE;
    }
    else
    {
      return (Boolean.TRUE.equals(value)) ? Boolean.TRUE : Boolean.FALSE;
    }
  }

  @SuppressWarnings("unchecked")
  private void _gatherChildInformation(
    UIComponent      parent)
  {
    List<UIComponent> children = parent.getChildren();
    int count = children.size();
    for (int index = 0; index < count; index++)
    {
      UIComponent child = children.get(index);

      if (!child.isRendered())
      {
        if (_hiddenColumns[index] != HIDDEN_COLUMN)
        {
          _hiddenColumns[index] = HIDDEN_COLUMN;
          _hiddenColumnCount++;
        }
      }

    }
  }

  public static boolean isInsideContentOfTable()
  {
    TableRenderingContext tContext = _CURRENT_CONTEXT.get();

    if (tContext == null)
      return false;

    return (tContext.getRenderStage().getStage() == RenderStage.DATA_STAGE);
  }

  /**
   * gets a property that is local to this table. Compare this to
   * RenderingContext.getProperty(). That method returns a global property.
   * this method is used to support nested tables.
   */
  public final Object getTableProperty(Object key)
  {
    Map<Object, Object> props = _tableProps;
    return (props == null) ? null : props.get(key);
  }

  /**
   * sets a property that is local to this table. Compare this to
   * RenderingContext.setProperty(). That method sets a global property.
   * this method is used to support nested tables.
   * @return the previous value (if any).
   */
  public final Object setTableProperty(Object key, Object value)
  {
    Map<Object, Object> props = _tableProps;
    if (props == null)
    {
      props = new HashMap<Object, Object>(5);
      _tableProps = props;
    }
    return props.put(key, value);
  }

  private final UIComponent   _table;

  private final String        _tableId;
  private String              _jsVarName;
  private Object              _tableWidth;
  private Object              _tableHeight;
  private CollectionComponent _collectionComponent;

  private int                 _detailColumnIndex = -1;
  private int                 _columnCount   = -1;
  private int                 _actualColumnCount    = -1;
  private Boolean             _immediate;
  private UIComponent         _detail;
  private boolean             _hasNavigation;
  private int[]               _hiddenColumns;
  private int                 _hiddenColumnCount;
  private boolean[]           _rowsHidden;
  private boolean             _hasColumnHeaders = false;

  private BandingData         _banding = null;
  private ColumnData          _columnData;
  private RowData             _rowData;
  private boolean             _verticalGrid;
  private boolean             _horizontalGrid;

  private final Boolean       _rowSelection;

  private RenderStage      _renderStage;

  private boolean          _explicitHeaderIDMode = false;

  // these constants are used in debug mode only:
  private boolean _hasColumnHeadersUsed = false;

  // this is a map of properties that are local to this table. This is to
  // support nested tables:
  private Map<Object, Object> _tableProps = null;


  // general fields
  private       TableRenderingContext _previous;
  private       Object                _nodeList;

  /**
   * Indicates that a row or column count is not known.
   */
  public static final int DONT_KNOW = -1;


  static private final ThreadLocal<TableRenderingContext> _CURRENT_CONTEXT = 
    ThreadLocalUtils.newRequestThreadLocal();
}
