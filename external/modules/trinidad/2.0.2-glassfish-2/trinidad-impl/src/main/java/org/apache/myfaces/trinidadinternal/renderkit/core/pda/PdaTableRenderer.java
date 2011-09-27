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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.CollectionComponent;
import org.apache.myfaces.trinidad.component.UIXColumn;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TableRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.BandingData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RowData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableUtils;


public class PdaTableRenderer extends TableRenderer
{
  /**
   */
  public PdaTableRenderer()
  {
    super(CoreTable.TYPE);
  }

  /**
   * renders attributes on the outermost table element.
   * this includes width, cellpadding, cellspacing, border.
   */
  @Override
  protected void renderTableAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    Object           cellPadding,
    Object           border
    ) throws IOException
  {
    Object width = getWidth(component, bean);

    // On mobile devices, table width is set to full browser width unless
    // the width is specified.
    if (width == null || width == "")
    {
      width = "100%";
    }


    OutputUtils.renderLayoutTableAttributes(context,
                                            rc,
                                            cellPadding,
                                            "0",    // cell spacing
                                            border,
                                            width); // table width
  }

  @Override
  protected final void renderControlBar(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    UIComponent action = getFacet(component, CoreTable.ACTIONS_FACET);
    boolean tableNotEmpty = !tContext.getRowData().isEmptyTable();
    boolean hasNav = tContext.hasNavigation() && tableNotEmpty;

    if (hasNav || (action != null))
    {
      ResponseWriter writer = context.getResponseWriter();

      // start control bar row
      writer.startElement("tr", null);
      // start control bar
      writer.startElement("td", null);
      renderStyleClass(context, rc,
                       SkinSelectors.AF_TABLE_CONTROL_BAR_TOP_STYLE);

      if (action != null)
      {
        encodeChild(context, action);
        writer.endElement("td");
        writer.endElement("tr");
        writer.startElement("tr", null);
        writer.startElement("td", null);
        renderStyleClass(context, rc,
            SkinSelectors.AF_TABLE_CONTROL_BAR_TOP_STYLE);
      }

      if ( hasNav)
      {
        writer.startElement("div", null);
        /*
          if (arc.isRightToLeft())
            writer.writeAttribute("align", "left", null);
          else
            writer.writeAttribute("align", "right", null);
        */
        // =-=AEW Is "valign" even a real attr for divs???
        writer.writeAttribute("valign", "middle", null);
        delegateRenderer(context, rc, component,
                         getFacesBean(component), getSharedNavBarRenderer());

        writer.endElement("div");
      }

      // end control bar row
      writer.endElement("td");
      writer.endElement("tr");

    }
  }

  @Override
  protected void renderSubControlBar(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean               isUpper
    ) throws IOException
  {
    // No-op.
  }

  private void _renderEmptyCell(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    boolean               isSelect,
    Object                emptyText
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("td", null);
    String cellClass = _getCellFormat(tContext, isSelect);
    renderStyleClass(context, rc, cellClass);

    if (emptyText == null)
      emptyText = XhtmlConstants.NBSP_STRING;
    writer.writeText(emptyText, null);

    // end the cell
    writer.endElement("td");
  }

  // render the actual table content, with headers
  @Override
  protected void renderTableContent(
    FacesContext                context,
    final RenderingContext      rc,
    final TableRenderingContext tContext,
    final UIComponent           component
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    FacesBean bean = getFacesBean(component);
    //
    // no nested tables, so end the previous table and start a
    // new one
    //
    writer.endElement("table");


    //
    // start the content table with the same attributes as the title table
    //
    writer.startElement("table", component);
    renderTableAttributes(context, rc, component, bean,
                          "2", "1");

    //
    // write out the grid color as the border color for the grid lines
    // and border of the table
    //
    renderStyleClass(context, rc, SkinSelectors.AF_TABLE_CONTENT_STYLE);

    //
    // 1. Gather all the data we need to render
    //

    final RowData rowData = tContext.getRowData();
    boolean isEmptyTable      = rowData.isEmptyTable();
    final UIComponent detail = tContext.getDetail();

    //
    // 2. Render the top / column header
    //
    _renderTableHeader(context, rc, tContext, component);

    // render the column header
    if (tContext.hasColumnHeaders())
      _renderColumnHeader(context, rc, tContext, component);

    //
    // 3. Render each row
    //

    ColumnData colData = tContext.getColumnData();
    final RenderStage renderStage = tContext.getRenderStage();

    renderStage.setStage(RenderStage.DATA_STAGE);


    // use the special response writer in our data section that
    // defaults data cells with no data to <br>
    //tContext.setDataResponseWriterUsed(true);
    int physicalCol = 0;

    if (isEmptyTable)
    {
      writer.startElement("tr", null);
      if (tContext.hasSelection())
      {
        colData.setColumnIndex(physicalCol++, ColumnData.SPECIAL_COLUMN_INDEX);
        _renderEmptyCell(context, rc, tContext, true, null);
      }

      // render detail control (hide/show for the row)
      if (detail != null)
      {
        colData.setColumnIndex(physicalCol++, ColumnData.SPECIAL_COLUMN_INDEX);
        _renderEmptyCell(context, rc, tContext, true, null);
      }

      int objectNameColumnIndex = colData.getObjectNameColumnIndex();
      if (objectNameColumnIndex < physicalCol)
        objectNameColumnIndex = physicalCol;
      for (int columns = colData.getColumnCount(); physicalCol < columns;)
      {
        colData.setColumnIndex(physicalCol, ColumnData.SPECIAL_COLUMN_INDEX);

        final Object emptyText;
        if (objectNameColumnIndex == physicalCol)
        {
          emptyText = getEmptyText(component, bean);
        }
        else
        {
          emptyText =  null;
        }

        _renderEmptyCell(context, rc, tContext, false, //isSelect
                         emptyText);
        physicalCol++;
      }
      writer.endElement("tr");
    }
    else //not an empty table
    {
      TableUtils.RowLoop loop = new TableUtils.RowLoop()
        {
          @Override
          protected void processRowImpl(FacesContext fc, CollectionComponent tableBase)
            throws IOException
          {
            ResponseWriter rw = fc.getResponseWriter();
            // compute all the rowSpans for the current row:
            rowData.setCurrentRowSpan(-1); //reset
            renderStage.setStage(RenderStage.START_ROW_STAGE);
            renderSingleRow(fc, rc, tContext, component);
            renderStage.setStage(RenderStage.DATA_STAGE);
            // render each of the individual rows in the rowSpan:
            for(int i=0, sz=rowData.getCurrentRowSpan(); i<sz; i++)
            {
              // start the row
              rw.startElement("tr", null);
              renderSingleRow(fc, rc, tContext, component);
              rowData.incCurrentSubRow();
              // end the row
              rw.endElement("tr");
            }

            // if necessary, render a detail row
            if ((detail != null) &&
                ((UIXTable) tableBase).getDisclosedRowKeys().isContained())
            {
              renderStage.setStage(RenderStage.DETAIL_ROW_STAGE);

              rw.startElement("tr", null);
              rw.startElement("td", null);
              rw.writeAttribute("colspan",
                                IntegerUtils.getString(tContext.getActualColumnCount()),
                                null);

              renderStyleClass(fc, rc, SkinSelectors.AF_TABLE_DETAIL_STYLE);

              encodeChild(fc, detail);

              rw.endElement("td");
              rw.endElement("tr");

              // restore the data stage
              renderStage.setStage(RenderStage.DATA_STAGE);
            }

          }
        };
      loop.run(context, tContext.getCollectionComponent());
    }
    // render the column footer
    _renderColumnFooter(context,rc,tContext,component);

    // we're done with the defaulting data response writer
    //context.setDataResponseWriterUsed(false);
  }

  @SuppressWarnings("unchecked")
  private void _renderColumnFooter(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    tContext.getRenderStage().setStage(RenderStage.COLUMN_FOOTER_STAGE);
    final ColumnData colData = tContext.getColumnData();
    UIComponent footer = getFacet(component, CoreTable.FOOTER_FACET);
    if (footer != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      /*  boolean useScroll = (getHeight(getFacesBean(component)) != null) && isIE(arc);
     if (useScroll)
     {
       writer.writeAttribute("style", "position:relative;"+
                                      "bottom:expression("+
                                       "this.offsetParent.scrollHeight-this.offsetParent.scrollTop-"+
                                       "this.offsetParent.clientHeight+1);" +
                                      "left:-1px", null);
     }
  */
      writer.startElement(XhtmlConstants.TABLE_HEADER_ELEMENT, null);
      // total rows may need an ID. see bug 3211593:
      /* Need new scheme for generateUniqueId()?
     String rowID = XhtmlLafUtils.generateUniqueID(tContext);
     writer.writeAttribute(XhtmlLafConstants.ID_ATTRIBUTE, rowID, null);
     tContext.getRowData().setCurrentRowHeaderID(rowID);
     */
      final int firstFooterPhysicalIndex =
        colData.getPhysicalIndexOfFirstFooter();
      final int colSpan =
        (firstFooterPhysicalIndex > 0)? firstFooterPhysicalIndex:
        tContext.getActualColumnCount();
      writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE,
          IntegerUtils.getString(colSpan), null);
      renderStyleClass(context, rc,
          SkinSelectors.AF_TABLE_COLUMN_FOOTER_STYLE);
      encodeChild(context, footer);
      writer.endElement(XhtmlConstants.TABLE_HEADER_ELEMENT);
      if (firstFooterPhysicalIndex > 0)
      {
        colData.setColumnIndex(tContext.getSpecialColumnCount(),
            0) /*logicalColumnIndex*/;

        for (UIComponent child:
          (List<UIComponent>) component.getChildren())
        {
          if (child.isRendered())
          {
            encodeChild(context, child);
          }
        }
      }
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
    }
  }

  @Override
  protected final void renderSingleRow(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    int stage = tContext.getRenderStage().getStage();

    switch(stage)
    {
      case RenderStage.COLUMN_HEADER_STAGE:
        return;
      case RenderStage.INITIAL_STAGE:
      case RenderStage.DATA_STAGE:
      case RenderStage.END_STAGE:
      case RenderStage.START_ROW_STAGE:
        break;
      default:
        throw new AssertionError("Bad renderStage:"+stage);
    }

    ColumnData colData = tContext.getColumnData();
    int[] hidden = tContext.getHiddenColumns();
    int columns = tContext.getColumnCount();

    // render the special columns, such as selection and details:
    int physicalColumn = renderSpecialColumns(context,
                                              rc,
                                              tContext,
                                              component,
                                              0);

    for (int currCol = 0; currCol < columns; currCol++)
    {
      if (hidden[currCol] == TableRenderingContext.NORMAL_COLUMN)
      {
        UIComponent child =
          (UIComponent) component.getChildren().get(currCol);
        if (!(child instanceof UIXColumn))
          continue;

        UIXColumn column = (UIXColumn) child;
        boolean isRowHeader = Boolean.TRUE.equals(
            column.getAttributes().get(CoreColumn.ROW_HEADER_KEY.getName()));
        if (!isRowHeader)
        {
          colData.setColumnIndex(physicalColumn,currCol);
          encodeChild(context, column);
          // ColumnBeans automatically increment the physical and logical
          // column indices (these may be increase by more than one, if
          // there are columnGroups). So we must not increment the column
          // indices here
          physicalColumn = colData.getPhysicalColumnIndex();
        }
      }
    }

  }

  // render the complete column header
  private void _renderColumnHeader(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    tContext.getRenderStage().setStage(RenderStage.COLUMN_HEADER_STAGE);

    ResponseWriter writer = context.getResponseWriter();
    ColumnData colData = tContext.getColumnData();
    colData.setRowIndex(0);
    writer.startElement("tr", null);

    int physicalCol = renderSpecialColumns(context, rc, tContext, component, 0);
    int[] hidden = tContext.getHiddenColumns();
    int colCount = component.getChildCount();

    for (int j = 0; j < colCount; j++)
    {
      if (hidden[j] != TableRenderingContext.NORMAL_COLUMN)
        continue;
      UIComponent child =
        (UIComponent) component.getChildren().get(j);
      if (!(child instanceof UIXColumn))
        continue;

      UIXColumn column = (UIXColumn) child;
      boolean isRowHeader = Boolean.TRUE.equals(
        column.getAttributes().get(CoreColumn.ROW_HEADER_KEY.getName()));
      if (!isRowHeader)
      {
        colData.setColumnIndex(physicalCol, j);
        encodeChild(context, column);
        // ColumnBeans automatically increment the physical and logical
        // column indices (these may be increase by more than one, if
        // there are columnGroups). So we must not increment the column
        // indices here
        physicalCol = colData.getPhysicalColumnIndex();
      }
    }

    colData.setRowIndex(-1);

    writer.endElement("tr");
  }

  // get the style class for the current cell
  private String _getCellFormat(
    TableRenderingContext tContext,
    boolean               isSelect
    ) throws IOException
  {
    ColumnData colData = tContext.getColumnData();
    RowData rowData = tContext.getRowData();
    int row = rowData.getRangeIndex();
    int physicalColumn = colData.getPhysicalColumnIndex();
    int logicalColumn = colData.getLogicalColumnIndex();

    BandingData bandingData = tContext.getBanding();
    boolean band = bandingData.getBand(tContext, row,
                                       physicalColumn,
                                       logicalColumn);
    //
    // determine the cell class
    //
    String cellClass;
    if (band)
    {
      if (isSelect)
        cellClass = SkinSelectors.TABLE_BAND_SELECT_CELL_STYLE;
      else
      {
        cellClass = ColumnData.selectFormat(tContext,
                             SkinSelectors.AF_COLUMN_CELL_TEXT_BAND_STYLE,
                             SkinSelectors.AF_COLUMN_CELL_NUMBER_BAND_STYLE,
                             SkinSelectors.AF_COLUMN_CELL_ICON_BAND_STYLE);
      }
    }
    else
    {
      if (isSelect)
        cellClass = SkinSelectors.TABLE_SELECT_CELL_STYLE;
      else
      {
        cellClass = ColumnData.selectFormat(tContext,
                             SkinSelectors.AF_COLUMN_CELL_TEXT_STYLE,
                             SkinSelectors.AF_COLUMN_CELL_NUMBER_STYLE,
                             SkinSelectors.AF_COLUMN_CELL_ICON_FORMAT_STYLE);
      }
    }

    return cellClass;
  }

  /**
   * @todo Reconsider our choice of style for this element!
   */
  private void _renderTableHeader(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    // implement header facet on table: see bug 3788610
    ResponseWriter writer = context.getResponseWriter();
    UIComponent header = getFacet(component, CoreTable.HEADER_FACET);
    if (header != null)
    {
      writer.startElement("thead", null);
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE,
        tContext.getActualColumnCount(), null);
      renderStyleClass(context, rc, SkinSelectors.AF_COLUMN_SORTABLE_HEADER_ICON_STYLE_CLASS);

      encodeChild(context, header);

      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
      writer.endElement("thead");
    }
  }
}
