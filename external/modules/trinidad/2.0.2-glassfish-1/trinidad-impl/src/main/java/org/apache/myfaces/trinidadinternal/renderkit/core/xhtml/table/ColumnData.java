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

import java.awt.Dimension;

import java.util.ArrayList;
import java.util.List;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/table/ColumnData.java#0 $) $Date: 10-nov-2005.19:02:34 $
 */
public final class ColumnData
{
  /**
   * corresponds to {@link CoreColumn#ALIGN_LEFT}
   */
  public static final int TEXT_FORMAT   = 0;

  /**
   * corresponds to {@link CoreColumn#ALIGN_RIGHT}
   */
  public static final int NUMBER_FORMAT = 1;

  /**
   * corresponds to {@link CoreColumn#ALIGN_CENTER}
   */
  public static final int ICON_FORMAT   = 2;

  /**
   * the logical index of a special column such as select,details,etc...
   */
  public static final int SPECIAL_COLUMN_INDEX = -1;


  public ColumnData()
  {
  }

  /**
   * gets the rowIndex for a columnHeader. this is used for rowSpanning in
   * column headers.
   * @return
   */
  public int getRowIndex()
  {
    return _rowIndex;
  }

  /**
   * sets the rowIndex for a columnHeader. this is used for rowSpanning in
   * column headers.
   * @param index
   */
  public void setRowIndex(int index)
  {
    _rowIndex = index;
  }

  /**
   * @return the number of visible table columns.
   * @see #setColumnCount(int)
   */
  public int getColumnCount()
  {
    return _colCount;
  }

  /**
   * sets the number of visible columns in the data part of this table.  The
   * column count must NOT include special columns like select and details.
   */
  public void setColumnCount(int columns)
  {
    _colCount = columns;
  }

  /**
   * gets the physical index of the designated object name column.
   * @return
   */
  public int getObjectNameColumnIndex()
  {
    return _objectNameColumn;
  }

  /**
   * gets the physical index of the first column with a footer
   */
  public int getPhysicalIndexOfFirstFooter()
  {
    return _firstFooterPhysicalColumn;
  }

  /**
   * indicates that the current column has a footer
   */
  public void setCurrentColumnHasFooter()
  {
    if ((_firstFooterPhysicalColumn == -1) ||
        (_physicalColumn < _firstFooterPhysicalColumn))
    {
      _firstFooterPhysicalColumn = _physicalColumn;
    }
  }


  /**
   * @see #setColumnIndex(int, int)
   */
  public int getPhysicalColumnIndex()
  {
    return _physicalColumn;
  }

  /**
   * @see #setColumnIndex(int, int)
   */
  public int getLogicalColumnIndex()
  {
    return _logicalColumn;
  }

  /**
   * sets the (zero based) indices of the column that is currently being
   * rendered
   * @param physical this is the index of the column as it appears visually on
   * the screen. rowHeaders will always have a physical index of zero.
   * @param logical this is the index of the indexed child (of the table)
   * being used to render this column
   * @see #getPhysicalColumnIndex()
   * @see #getLogicalColumnIndex()
   * @see #incrementColumnIndex() */
  public void setColumnIndex(int physical, int logical)
  {
    _physicalColumn = physical;
    _logicalColumn = logical;
  }

  /**
   * increments both the physical and the logical column indices by one.
   * @see #setColumnIndex(int,int)
   */
  public void incrementColumnIndex()
  {
    _physicalColumn++;
    _logicalColumn++;
  }

  /**
   * indicate to a header node what row/colSpan it should use.
   */
  public void setCurrentSpan(int rowSpan, int colSpan)
  {
    _currentSpan.width = colSpan;
    _currentSpan.height = rowSpan;
  }

  /**
   * Gets the row/colSpan that the current header node should use.
   */
  public Dimension getCurrentSpan()
  {
    return _currentSpan;
  }


  /**
   * @return the rowSpan of this table's entire columnHeader.
   * the default value is one.
   * @see #setHeaderRowSpan(int)
   */
  public int getHeaderRowSpan()
  {
    assert _assertInitModeOff();
    return _headerRowSpan;
  }

  /**
   * @param span the rowSpan of the entire column header. The span will only
   * be set if it is greater than the current span.
   */
  public void setHeaderRowSpan(int span)
  {
    assert(_assertInitMode);

    if (span > _headerRowSpan)
      _headerRowSpan = span;
  }


  /**
   * gets the list of header IDs that apply to the given column.
   */
  public String getHeaderIDs(int physicalIndex)
  {
    return _get(physicalIndex).headerIDs;
  }

  /**
   * gets the id for this column
   * @param physicalIndex
   * @return
   */
  public String getHeaderID(int physicalIndex)
  {
    return _get(physicalIndex).headerID;
  }

  /**
   * sets the id for this column
   * @param physicalIndex
   * @param headerID
   */
  public void setHeaderID(int physicalIndex, String headerID)
  {
    _get(physicalIndex).headerID = headerID;
  }

  /**
   * sets the list of IDs that apply to this column
   * @param physicalIndex
   * @param headerIDs
   */
  public void setHeaderIDs(int physicalIndex, String headerIDs)
  {
    _get(physicalIndex).headerIDs = headerIDs;
  }

  public void setCurrentHeaderID(String id)
  {
    _currHeaderID = id;
  }

  public String getCurrentHeaderID()
  {
    return _currHeaderID;
  }

  /**
   * sets whether the current header should be wrapped.
   * this is to pass information from a columnGroup to a sortableHeader.
   */
  public void setCurrentHeaderNoWrap(boolean isNoWrap)
  {
    _currHeaderNoWrap = isNoWrap;
  }

  /**
   * gets whether the current header should be wrapped
   */
  public boolean getCurrentHeaderNoWrap()
  {
    return _currHeaderNoWrap;
  }

  /**
   * returs true if the specified column's useSeparateRows attr is true.
   */
  public boolean useSeparateRows(int physicalIndex)
  {
    return _get(physicalIndex).useSeparateRows;
  }

  /**
   * returns true if the specified column's rowHeader attribute is true.
   */
  public boolean isRowHeader(int physicalIndex)
  {
    return _get(physicalIndex).isRowHeader;
  }

  public void setColumnData(
    Object width,
    Object align,
    boolean noWrap,
    boolean headerNoWrap,
    boolean separateRows,
    boolean rowHeader)
  {
    // make sure that no-one called any of the get methods on this object,
    // prior to this set method being called:
    assert(_assertInitMode);

    int physicalIndex = getPhysicalColumnIndex();
    Data d = _create(physicalIndex);

    _setWidth(d, width);
    _setDataFormat(d, align);
    d.noWrap = noWrap;

    d.headerNoWrap = headerNoWrap;
    d.useSeparateRows = separateRows;
    d.isRowHeader = rowHeader;
  }

  public void setSpecialColumnData(
    TableRenderingContext tContext,
    RenderingContext   arc,
    boolean               noWrap,
    boolean               allowHeaderWrap,
    String                formatType)
  {
    // make sure that no-one called any of the get methods on this object,
    // prior to this set method being called:
    assert(_assertInitMode);

    int physicalIndex = getPhysicalColumnIndex();
    Data d = _create(physicalIndex);
    d.headerNoWrap = !allowHeaderWrap;
    d.noWrap       = noWrap;
    _setDataFormat(d, formatType);
    d.width        = _getSpecialWidth(tContext, arc);
  }

  public Object getWidth(int physicalIndex)
  {
    assert _assertInitModeOff();

    return _get(physicalIndex).width;
  }

  /**
   * @return one of {@link #TEXT_FORMAT}, {@link #NUMBER_FORMAT} or
   * {@link #ICON_FORMAT}
   */
  private int _getDataFormat(int physicalIndex)
  {
    assert _assertInitModeOff();

    return _get(physicalIndex).dataFormat;
  }

  /**
   * @param physicalIndex the physical index of the column.
   * @return true, if this column should have text wrapping turned off
   */
  public boolean getNoWrap(int physicalIndex)
  {
    assert _assertInitModeOff();

    return _get(physicalIndex).noWrap;
  }

  /**
   * @param physicalIndex the physical index of the column.
   * @return true, if this column should have text wrapping turned off for the
   * header
   */
  public boolean getHeaderNoWrap(int physicalIndex)
  {
    assert _assertInitModeOff();

    return _get(physicalIndex).headerNoWrap;
  }

  /**
   * @return true if we are currently rendering the column group header
   */
  public boolean isColumnGroupHeader()
  {
    return _isColumnGroupHeader;
  }

  /**
   * @param isColumnGroupHeader true if we are currently rendering the column
   * group header
   */
  public void setColumnGroupHeader(boolean isColumnGroupHeader)
  {
    _isColumnGroupHeader = isColumnGroupHeader;
  }

  private void _setWidth(Data data, Object width)
  {
    if (width != null)
    {
      data.width = width;
    }
  }

  public void setDataFormat(int physicalIndex, Object format)
  {
    _setDataFormat(_get(physicalIndex), format);
  }

  /**
   * @todo Support "object name" format type - which is hacked out
   *    at the moment.
   */
  private void _setDataFormat(Data data, Object format)
  {
    if ((format==null) ||
        CoreColumn.ALIGN_START.equals(format) ||
        CoreColumn.ALIGN_LEFT.equals(format))
      ; // this is the most common case, and is also the default
    else if (CoreColumn.ALIGN_END.equals(format) ||
             CoreColumn.ALIGN_RIGHT.equals(format))
      data.dataFormat = NUMBER_FORMAT;
    else if (CoreColumn.ALIGN_CENTER.equals(format))
      data.dataFormat = ICON_FORMAT;
    else
    {
      _LOG.warning("UNKNOWN_VALUE_FOR_ALIGN", format);
    }
  }

  private Data _get(int physicalIndex)
  {
    Data d = _data.get(physicalIndex);
    assert (d != null)
        : "no column data for physicalIndex:" + physicalIndex;
    return d;
  }

  private Data _create(int physicalIndex)
  {
    //ystem.out.println("creating index:"+index);

    int sz = _data.size();
    final Data d;
    if (physicalIndex < sz)
    {
      d = _get(physicalIndex);
    }
    else
    {
      assert physicalIndex == sz
         : "physicalIndex:"+physicalIndex+",size:"+sz;

      d = new Data();
      _data.add(d);
    }

    return d;
  }

  // indicate that we can no longer initialize this columnData.
  // this function is only used with assert
  private boolean _assertInitModeOff()
  {
    _assertInitMode = false;
    return true;
  }

  /**
   * @param tContext the column being processed is the one at
   * the physicalColumnIndex in this table context.
   * @param textClass the styleClass to return if the column align is "left"
   * @param numberClass the styleClass to return if the column align is "right"
   * @param iconClass the string to return if the column align is "center".
   * @return a column style class to use depending on the type of data in that
   * column.
   */
  public static String selectFormat(TableRenderingContext tContext,
                                    String textClass,
                                    String numberClass,
                                    String iconClass)
  {
    ColumnData colData = tContext.getColumnData();
    int dataFormat = colData._getDataFormat(colData.getPhysicalColumnIndex());

    switch (dataFormat)
    {
    case NUMBER_FORMAT:
      return numberClass;
    case ICON_FORMAT:
      return iconClass;
    default:
      return textClass;
    }
  }

  static private Object _getSpecialWidth(
    TableRenderingContext tContext,
    RenderingContext   arc)
  {
    // =-= ACW:
    // if we don't set a width, then we get bugs:
    // 1874079: TABLE SELECTION COLUMN BY DEFAULT GROWS LARGE
    // 2290663: APPS: HGRID FOCUS COLUMN IS VERY WIDE WHEN INITIALLY RENDERED
    // IN NETSCAPE

    // if we set the width to "1" then we get bugs:
    // 1999842: SELECT COLUMN HEADER WRAPS IN MULTIBYTE LANGUAGES
    // 2464092: APPS:COLUMN HEADER FOR A SINGLESELECTION IN A TABLE IS LEFT
    // ALIGNED IN IE and NN4.7

    // if we set the width to "1%" then we get bugs:
    // 2342291: HGRID COLUMN HEADER WRAPS UNNECESSARILY
    // 2116815: THE TABLE COLUMN WIDTH IS VERY LARGE IN NETSCAPE.


    // check to see if we are inside an TreeTable
    if (tContext.getTable() instanceof UIXTreeTable)
    {
      // if we are inside HGrid, then we need to handle IE carefully because
      // of bug 2342291. We can use "1" in this case, because the HGrid does
      // not have columnFooters, and so bug 2464092 cannot occur:
      return XhtmlRenderer.isIE(arc) ? "1" : "1%";
    }
    else
    {
      // if we are not in HGrid then we must return "1%"
      return "1%";
    }
  }

  private static final class Data
  {
    public int dataFormat = TEXT_FORMAT;
    public Object width = null;
    public boolean useSeparateRows = false;
    public boolean isRowHeader = false;
    public boolean noWrap = false, headerNoWrap = false;
    public String headerIDs = null;
    public String headerID = null;
  }

  private boolean _isColumnGroupHeader = false;
  private String _currHeaderID = null;
  private boolean _currHeaderNoWrap = false;
  private int _headerRowSpan = 1;
  private int _physicalColumn = -1, _logicalColumn = -1, _colCount = 0;
  private int _firstFooterPhysicalColumn = -1;
  // Physical Index of the object name column:
  private int _objectNameColumn = 0;
  private int _rowIndex = -1;

  private final Dimension _currentSpan = new Dimension(1,1);
  private final List<Data> _data = new ArrayList<Data>(10);

  // this constants is used for assert only:
  private boolean _assertInitMode = true;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColumnData.class);
}
