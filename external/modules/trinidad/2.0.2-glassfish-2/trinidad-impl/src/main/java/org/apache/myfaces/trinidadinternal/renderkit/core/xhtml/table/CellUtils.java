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

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/table/CellUtils.java#0 $) $Date: 10-nov-2005.19:02:33 $
 */
public class CellUtils
{
  /**
   * renders abbr, scope, width and nowrap
   */
  public static void renderHeaderAttrs(FacesContext context,
                                       TableRenderingContext tContext,
                                       // Need SHORT_TEXT from node???
                                       Object abbreviation,
                                       Object width,
                                       boolean isNoWrap,
                                       boolean isColumnHeader)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // This is for accessibility support. Gives this header an abbreviation
    // for use with screen readers.
    writer.writeAttribute("abbr", abbreviation, null);

    if (!tContext.isExplicitHeaderIDMode())
    {
      // This is for accessibility support. Identifies whether this header
      // is a header for columns or for rows:
      writer.writeAttribute("scope", (isColumnHeader) ? "col" : "row", null);
    }

    // put a column width on
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);

    // only no-wrap header cells if specified
    if (isNoWrap)
      writer.writeAttribute(XhtmlConstants.NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
  }

  public static void renderSpan(FacesContext rContext,
                                boolean isRowSpan,
                                int value) throws IOException
  {
    if (value > 1)
    {
      ResponseWriter writer = rContext.getResponseWriter();
      writer.writeAttribute(isRowSpan
                              ? XhtmlConstants.ROWSPAN_ATTRIBUTE
                              : XhtmlConstants.COLSPAN_ATTRIBUTE,
                            value,
                            null);
    }
  }

  /**
   * figure out any grid line borders for IE / Moz.
   * This is for header cells.
   */
  public static String getHeaderBorderStyle(
    TableRenderingContext tContext,
    RenderingContext   afContext,
    boolean isColumnHeader,
    boolean isSortable)
  {

    final ColumnData colData = tContext.getColumnData();
    final RowData rowData = tContext.getRowData();
    
    // not bothering with StringBuffers here since we will at most be
    // concatenating 2 strings one time. Buffer would be slower.
    String style = null;
    if (isColumnHeader)
    {
      int physicalIndex = colData.getPhysicalColumnIndex();
      
      // sortable column headers don't get the border style classes. The borders are
      // included in the sortable-header-text selectors.
      if (!isSortable)
      {
        final boolean right, left, bottom;
        if (colData.isColumnGroupHeader())
        {
          if (physicalIndex == 0)
          {
            bottom = true;
            right = left = false;            
          }
          else
          {
            left = bottom = true;
            right = false;
          }
        }
        else
        {
          bottom = false;
          if (afContext.isRightToLeft())
          {
            if(physicalIndex == tContext.getActualColumnCount()-1) 
            {
              right = false;
            }
            else
              right = tContext.hasGrid(physicalIndex, true);
           
            left = false;
          }
          else
          {
            // non-sortable column headers get a left border iff
            // it's not the first column and the format calls for a grid
            left = ((physicalIndex>0) &&
                    tContext.hasGrid(physicalIndex, true));
            right = false;
          }
        }
        
        style = getBorderClass(false /*top*/, left,
                               bottom, right);
      }
    }
    else // isColumnHeader==false
    {
      int logicalIndex = rowData.getRangeIndex();
      // row headers get a top border iff
      // 1) it's the first row and there is a column header OR
      // 2) it's not the first row and the format calls for a grid
      if (((logicalIndex == 0) && tContext.hasColumnHeaders()) ||
          ((logicalIndex > 0)  &&
           tContext.hasGrid(logicalIndex,false)))
      {
        style =
        CellUtils.getBorderClass(true, false, false, false);
      }
    }
    return style;
  }



  /**
   * gets a style attribute for a table data cell at the given
   * physical/logical column indices and row index.
   */
  public static String getDataBorderStyle(
    RenderingContext   arc,
    TableRenderingContext tContext
    )
  {
    final ColumnData colData = tContext.getColumnData();
    final RowData rowData = tContext.getRowData();
    int physicalColumn = colData.getPhysicalColumnIndex();
    int row = rowData.getRangeIndex();
    
    if ((tContext.getTableHeight() != null) &&
        XhtmlRenderer.isGecko(arc))
    {
      return getBorderClass(false, false, false, false);
    }
    
    
    // =-= bts temporary flag for whether the agent actually really
    //         supports collapsing borders
    boolean supportsCollapse = true;
    
    final int nextRow = row+1;
    final int lastColumnIndex = tContext.getActualColumnCount() - 1;
    
    boolean top    = (row == 0);
    boolean left   = ((physicalColumn == 0) ||
                      (tContext.hasGrid(physicalColumn, true)));
    boolean bottom = tContext.hasGrid(nextRow, false);
    
    boolean right;
    
    //
    // If we can collapse borders, less output will be generated if
    // we request a style with borders on all 4 sides, rather than
    // borders on 1 - 3 sides.  As a result, if we need borders on two
    // sides we will ask for borders on the other 2 sides unless
    // the client has specifically requested that we not put them there.
    //
    // If we can't get borders on all 4 sides, we will try to get borders
    // on as few sides as possible
    //
    if (left && bottom && supportsCollapse)
    {
      if (!top)
      {
        top |= tContext.hasGrid(row, false);
      }
      
      if (top)
      {
        right = tContext.hasGrid(physicalColumn + 1, true);
        
        if (!right)
        {
          top &= !tContext.hasColumnHeaders();
        }
      }
      else
      {
        right = (physicalColumn == lastColumnIndex);
      }
    }
    else
    {
      top &= !tContext.hasColumnHeaders();
      
      right = (physicalColumn == lastColumnIndex);
    }

    return getBorderClass(top,
                          left,
                          bottom,
                          right);
    
  }



  /**
   * Get the style string for a given border combination.
   */
  public static String getBorderClass(
    boolean          top,
    boolean          left,
    boolean          bottom,
    boolean          right
    )
  {
    int offset = _getOffset( top, left, bottom, right);
    return BORDER_CLASS[offset];
  }

  private static int _getOffset(
     boolean top,
     boolean left,
     boolean bottom,
     boolean right
  )
  {
     int offset = 0;

     if (top)
       offset += 8;

     if (right)
       offset += 4;

     if (bottom)
       offset += 2;

     if (left)
       offset += 1;

    return offset;
  }

  protected CellUtils()
  {
  }

  public static final String [] BORDER_CLASS = new String[]
  {
    null,
    SkinSelectors.TABLE_BORDER_0001_STYLE,
    SkinSelectors.TABLE_BORDER_0010_STYLE,
    SkinSelectors.TABLE_BORDER_0011_STYLE,
    SkinSelectors.TABLE_BORDER_0100_STYLE,
    SkinSelectors.TABLE_BORDER_0101_STYLE,
    SkinSelectors.TABLE_BORDER_0110_STYLE,
    SkinSelectors.TABLE_BORDER_0111_STYLE,
    SkinSelectors.TABLE_BORDER_1000_STYLE,
    SkinSelectors.TABLE_BORDER_1001_STYLE,
    SkinSelectors.TABLE_BORDER_1010_STYLE,
    SkinSelectors.TABLE_BORDER_1011_STYLE,
    SkinSelectors.TABLE_BORDER_1100_STYLE,
    SkinSelectors.TABLE_BORDER_1101_STYLE,
    SkinSelectors.TABLE_BORDER_1110_STYLE,
    SkinSelectors.TABLE_BORDER_1111_STYLE,
  };

}
