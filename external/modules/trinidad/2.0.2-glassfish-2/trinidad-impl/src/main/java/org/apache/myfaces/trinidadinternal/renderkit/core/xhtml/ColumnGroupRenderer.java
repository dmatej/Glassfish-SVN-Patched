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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.CellUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;


/**
 * @todo Kill the now-strange "compute mode", since we can
 *       just iterate over the things.
 * @todo Support abbreviation!!!  Missing from Trinidad so far.
 * @todo Support required?  Support messageType?
 */
public class ColumnGroupRenderer extends XhtmlRenderer
{
  protected static final int SORT_NO         = 0;
  protected static final int SORT_SORTABLE   = 1;
  protected static final int SORT_ASCENDING  = 2;
  protected static final int SORT_DESCENDING = 3;

  public ColumnGroupRenderer()
  {
    super(CoreColumn.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _headerTextKey = type.findKey("headerText");
    _headerNoWrapKey = type.findKey("headerNoWrap");
    _noWrapKey = type.findKey("noWrap");
    _separateRowsKey = type.findKey("separateRows");
    _rowHeaderKey = type.findKey("rowHeader");
    _alignKey = type.findKey("align");
    _widthKey = type.findKey("width");
    _sortableKey = type.findKey("sortable");
    _sortPropertyKey = type.findKey("sortProperty");
    _defaultSortOrderKey = type.findKey("defaultSortOrder");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected String getHeaderText(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_headerTextKey));
  }

  protected boolean getHeaderNoWrap(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_headerNoWrapKey);
    if (o == null)
      o = _headerNoWrapKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getNoWrap(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_noWrapKey);
    if (o == null)
      o = _noWrapKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getRowHeader(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_rowHeaderKey);
    if (o == null)
      o = _rowHeaderKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getSeparateRows(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_separateRowsKey);
    if (o == null)
      o = _separateRowsKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected String getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_widthKey));
  }

  protected String getFormatType(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_alignKey));
  }

  protected boolean getSortable(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_sortableKey);
    if (o == null)
      o = _sortableKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }

  protected String getSortProperty(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_sortPropertyKey));
  }

  protected String getDefaultSortOrder(
    UIComponent component,
    FacesBean   bean)
  {
    if (_defaultSortOrderKey == null)
      return null;

    return toString(bean.getProperty(_defaultSortOrderKey));
  }

  static public String getDefaultHeaderStyleClass(
    TableRenderingContext tContext)
  {
    return ColumnData.selectFormat(tContext,
                                   SkinSelectors.AF_COLUMN_HEADER_TEXT_STYLE,
                                   SkinSelectors.AF_COLUMN_HEADER_NUMBER_STYLE,
                                   SkinSelectors.AF_COLUMN_HEADER_ICON_STYLE);
  }

  /**
   * @param tContext the column is identified by the logicalColumnIndex on
   * this context.
   * @return the CSS style class to use for a column header. This will be
   * left-aligned for text, right-aligned for numbers and center-aligned
   * for icons.
   */
  protected String getHeaderStyleClass(
    TableRenderingContext tContext)
  {
    return getDefaultHeaderStyleClass(tContext);
  }

  /**
   * @todo Will need to support TREE_NODE_STAGE
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();

    RenderStage rs = tContext.getRenderStage();
    int stage = rs.getStage();
    switch (stage)
    {
    case RenderStage.INITIAL_STAGE:
      _computeMode(context, tContext, component);
      break;
    case RenderStage.COLUMN_HEADER_STAGE:
      _renderHeaderMode(context, rc, tContext, component);
      break;
    // For these stages, simply render the children; we
    // need no special processing at the column group level
    case RenderStage.DATA_STAGE:
    case RenderStage.START_ROW_STAGE:
    case RenderStage.COLUMN_FOOTER_STAGE:
      _renderChildren(context, component, null /*parentNode*/);
      break;
    case RenderStage.END_STAGE:
      // Do nothing.  =-=AEW This is only legit because we happen
      // to know that ColumnRenderer does nothing here too.
      break;
    default:
      throw new AssertionError("Bad renderStage:"+stage);
    }
  }

  private void _renderHeaderMode(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column
    ) throws IOException
  {
    final NodeData parentNode = getParentNode(tContext);
    final boolean areWeRoot;
    final NodeData currentNode;

    if (parentNode == null)
    {
      areWeRoot = true;
      currentNode = _getNodeList(tContext, false).getNext();
    }
    else
    {
      areWeRoot = false;
      currentNode = parentNode.get(parentNode.currentChild);
    }

    final ColumnData colData = tContext.getColumnData();
    int row = colData.getRowIndex();
    int waitUntilRow = currentNode.waitUntilRow;
    // check to see if this columnGroup's header has been rendered:
    if (waitUntilRow==0)
    {
      // this columnGroup's header has not been rendered. So we will render it
      // and skip rendering the headers of our children.
      int totalRows = colData.getHeaderRowSpan();
      int rowSpan = (totalRows - row) - currentNode.rows + 1;

      // This columnGroup may have a rowSpan > 1. So we need to indicate on
      // which row our children will start rendering:
      currentNode.waitUntilRow = rowSpan + row;

      String headerID = _renderColumnHeader(context, rc, tContext, column,
                                            rowSpan, currentNode.cols);
      if (headerID != null)
      {
        if (areWeRoot)
        {
          currentNode.headerIDs = headerID;
        }
        else
        {
          currentNode.headerIDs = parentNode.headerIDs+" "+headerID;
        }
      }
    }
    else if (row >= waitUntilRow)
    {
      // this columnGroup's header has already been rendered. And we have
      // skipped as many rows as necessary (for this columnGroup's
      // rowSpan). So now we can render the headers of our children.
      _setParentNode(tContext, currentNode);
      _renderChildren(context, column, currentNode);
      _setParentNode(tContext, parentNode);
      // and we are done. Do not increment the physicalIndex as our children
      // would have done so:
      return;
    }

    // at this point we need to increment the physical column index, since our
    // children did not render:
    // this columnGroup occupies as many physicalIndices as its colSpan:
    colData.setColumnIndex(colData.getPhysicalColumnIndex() +
                           currentNode.cols,
                           colData.getLogicalColumnIndex());
  }

  /**
   * @return the column header ID
   */
  private String _renderColumnHeader(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column,
    int                   rowSpan,
    int                   colSpan
    ) throws IOException
  {
    ColumnData colData = tContext.getColumnData();

    // only no-wrap header cells if specified
    boolean isNoWrap = getHeaderNoWrap(column, getFacesBean(column));

    // indicate to the headerNode that it is a column group header
    colData.setColumnGroupHeader(true);
    // indicate to the headerNode whether or not to permit wrapping:
    colData.setCurrentHeaderNoWrap(isNoWrap);

    final String colID =
      renderHeaderAndSpan(context, rc, tContext, column,
                          rowSpan, colSpan);
    colData.setColumnGroupHeader(false);
    colData.setCurrentHeaderNoWrap(false);

    return colID;
  }

  /**
   * @todo Add renderkit test assertion that no one test writes
   * out the same ID twice!
   */
  /*
  protected final Object getID(RenderingContext rContext, UINode column)
  {
    // we already render our ID on the <TH>. If we don't return null here,
    // then our subclasses might render our ID a second time on the <TD>
    return null;
  }
  */

  /**
   * @return the headerID
   * @todo Generate unique ID
   * @todo If we support required and message type, fix nowrap
   * @todo Refactor this function
   */
  protected final String renderHeaderAndSpan(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column,
    int                   rowSpan,
    int                   colSpan
    ) throws IOException
  {
    ColumnData colData = tContext.getColumnData();
    String colID;
    if (shouldRenderId(context, column) ||
        tContext.isExplicitHeaderIDMode())
      // =-=AEW THIS WILL GENERATE DIFFS FROM "unique ID" land
      colID = getClientId(context, column);
    else
      colID = null;


    int physicalIndex = colData.getPhysicalColumnIndex();
    int sortability = getSortability(tContext, column);
    boolean sortable = (sortability != SORT_NO) &&
                       supportsNavigation(rc);

    if(sortable)
    {
      // the sortable script has a "state" parameter, so add this
      // to the form data if the agent does not support dynamic
      // generation of elements (on those that do, form data elements
      // can be created on the fly as necessary); see the JS
      // referenced in this.getSortingOnclick
      Object domLevel =
        rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_DOM);
      if(
        domLevel == null ||
        domLevel == TrinidadAgent.DOM_CAP_NONE ||
        domLevel == TrinidadAgent.DOM_CAP_FORM)
      {
        FormData formData = rc.getFormData();
        if(formData != null)
        {
          formData.addNeededValue(XhtmlConstants.STATE_PARAM);
        }
      }
    }

    // we do not want to wrap if wrapping has explicitly been disabled. if we
    // are inside a columnGroup then we need to check the header format on the
    // columnGroup: bug 3201579:
    boolean isNoWrap = colData.isColumnGroupHeader()
      ? colData.getCurrentHeaderNoWrap()
    // =-=AEW It's weird that we're going back to colData instead
    // of just looking on ourselves!  When we're in a columnGroup, sure.
      : colData.getHeaderNoWrap(physicalIndex);


    String sortIconName = _getIconName(sortability);
    Icon sortIcon = rc.getIcon(sortIconName);
    boolean hasSortingIcon = (sortIcon != null) && !sortIcon.isNull();

    // we do not want to wrap if there is an icon on the header:
    // On PDA, where screen width is limited, we cannot afford not to
    // wrap.  isPDA check is used in several places in this class.
    // PDA specific logic will be moved to PDA render kit in the future.
    if (!isPDA(rc))
    {
      isNoWrap = isNoWrap || hasSortingIcon;
    }
    //       || getRequired(bean);
    //       || getMessageType(bean);

    Object width = tContext.getColumnWidth(physicalIndex);

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("th", column);
    rw.writeAttribute("id", colID, "id");

    CellUtils.renderHeaderAttrs(context, tContext,
                                null, //abbreviation (MISSING!)
                                width,
                                isNoWrap,
                                true); //isColHeader

    String styleClass = getSortableHeaderStyleClass(tContext, sortability);
    String borderStyleClass =
      CellUtils.getHeaderBorderStyle(tContext,
                                     rc,
                                     true, //isColHeader
                                     sortable);

    renderStyleClasses(context, rc, new String[]{ styleClass,
                                                   borderStyleClass});

    String style = getHeaderInlineStyle(rc);
    renderInlineStyleAttribute(context, rc, column, style);

    if (colSpan > 1)
      rw.writeAttribute("colspan", IntegerUtils.getString(colSpan), null);

    if (rowSpan == 0)
      rowSpan = colData.getHeaderRowSpan();
    if (rowSpan > 1)
      rw.writeAttribute("rowspan", IntegerUtils.getString(rowSpan), null);

    String sortOnclick = "";
    if (supportsScripting(rc))
    {
      sortOnclick = getSortingOnclick(rc, tContext, column, sortability);
    }

    //=-=AEW Review: Does this need to support any other handlers?

    //=-=AEW Apparently in PDA, we don't bother rendering
    //  the onclick on the cell, just on the icon.  I think
    //  that the only reason desktop renders it in both places
    //  was for Netscape.  If I'm right, then really this decision should
    //  be driven off an "event bubbling" agent property.
    // - HKuhn if printable mode (supportScripting is disabled),
    // then no need for rendering onclick
    if (!isPDA(rc) && supportsScripting(rc))
      rw.writeAttribute("onclick", sortOnclick, null);

    // TODO: we should pass in null for "event bubbling" systems
    renderHeaderContents(context,
                         rc,
                         tContext,
                         column,
                         sortability,
                         sortIcon,
                         sortOnclick);


    rw.endElement("th");

    return colID;
  }

  /**
   * @return an inline style String to be rendered on headers (used on
   *  special subclasses)
   */
  protected String getHeaderInlineStyle(
    RenderingContext rc)
  {
    return null;
  }


  /**
   */
  protected String getSortingOnclick(
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column,
    int                   sortability)
  {
    FacesBean bean = getFacesBean(column);
    String onclick  = getOnclick(column, bean);
    if (sortability == SORT_NO)
      return onclick;

    if (rc.getFormData() == null)
    {
      _LOG.warning("SORTING_DISABLED_TABLE_NOT_IN_FORM");
      return onclick;
    }

    String formName = rc.getFormData().getName();
    String source   = tContext.getTableId();
    String value    = getSortProperty(column, bean);
    // Note that "state" refers to the current state, not
    // the state will be set after clicking
    String state = findSortState(sortability, column, bean);

    StringBuffer buffer = new StringBuffer(33+
                                           formName.length() +
                                           source.length() +
                                           value.length() +
                                           state.length()
                                           );
    buffer.append("return _tableSort('");
    buffer.append(formName);
    buffer.append(tContext.isImmediate() ? "',0,'" : "',1,'");
    buffer.append(source);
    buffer.append("','");
    buffer.append(value);
    if (state != "")
    {
      buffer.append("','");
      buffer.append(state);
    }

    buffer.append("');");
    String sortJS = buffer.toString();
    if (onclick != null)
    {
      sortJS = XhtmlUtils.getChainedJS(onclick, sortJS, true);
    }

    return sortJS;
  }

  protected void renderHeaderContents(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column,
    int                   sortability,
    Icon                  sortIcon,
    String                sortOnclick
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    UIComponent header = getFacet(column, CoreColumn.HEADER_FACET);
    if (header != null)
    {
      encodeChild(context, header);
    }
    else
    {
      String headerText = getHeaderText(column, getFacesBean(column));
      if (headerText != null)
        rw.writeText(headerText, "headerText");
    }

     renderSortOrderSymbol(context, rc, column, tContext,
                                    sortability, sortIcon, sortOnclick);

  }

  /**
   * @todo IMPLEMENT
   */
  protected void renderSortOrderSymbol(
    FacesContext          context,
    RenderingContext      rc,
    UIComponent           column,
    TableRenderingContext tContext,
    int                   sortability,
    Icon                  icon,
    String                sortOnclick
    ) throws IOException
  {
    if ((icon == null) || icon.isNull())
      return;

    ResponseWriter writer = context.getResponseWriter();
    boolean supportNav = supportsNavigation(rc);
    boolean NonJavaScriptBrowser = false;
    boolean renderedInput = false;
    if (supportNav)
    {
      if (isPDA(rc))
        writer.writeText(XhtmlConstants.NBSP_STRING, null);

      NonJavaScriptBrowser = !supportsScripting(rc);
      if (NonJavaScriptBrowser)
      {
        renderedInput = true;
        writer.startElement("input", null);
        writer.writeAttribute("type", "submit", null);
        String source = tContext.getTableId();
        FacesBean bean = getFacesBean(column);
        String value = getSortProperty(column, bean);
        String state = findSortState(sortability, column, bean);
        String nameAttri;
        if (state != "")
        {
          nameAttri =  XhtmlUtils.getEncodedParameter
                                   (XhtmlConstants.SOURCE_PARAM)
                       + XhtmlUtils.getEncodedParameter(source)
                       + XhtmlUtils.getEncodedParameter
                                   (XhtmlConstants.VALUE_PARAM)
                       + XhtmlUtils.getEncodedParameter(value)
                       + XhtmlUtils.getEncodedParameter
                                   (XhtmlConstants.EVENT_PARAM)
                       + XhtmlUtils.getEncodedParameter
                                   (XhtmlConstants.SORT_EVENT)
                       + XhtmlUtils.getEncodedParameter
                                   (XhtmlConstants.STATE_PARAM)
                       + state;
        }
        else
        {
          nameAttri =  XhtmlUtils.getEncodedParameter
                                    (XhtmlConstants.SOURCE_PARAM)
                       + XhtmlUtils.getEncodedParameter(source)
                       + XhtmlUtils.getEncodedParameter
                                    (XhtmlConstants.EVENT_PARAM)
                       + XhtmlUtils.getEncodedParameter
                                    (XhtmlConstants.SORT_EVENT)
                       + XhtmlUtils.getEncodedParameter
                                    (XhtmlConstants.VALUE_PARAM)
                       + value;
        }

        writer.writeAttribute("name", nameAttri, null);
        if (state.equals(XhtmlConstants.SORTABLE_ASCENDING))
        {
          writer.writeAttribute("value",
                                    XhtmlConstants.NON_JS_DESC_ICON, null);
        }
        else
        {
          writer.writeAttribute("value",
                                    XhtmlConstants.NON_JS_ASC_ICON, null);
        }

        writer.writeAttribute("class",
             SkinSelectors.SORTABLE_HEADER_SORT_ICON_STYLE_CLASS, null);
      }
      else
      {
        writer.startElement("a", null);
        writer.writeURIAttribute("href", "#", null);
        writer.writeAttribute("onclick", sortOnclick, null);
      }
    }

    String altTextKey = null;
    if (sortability == SORT_ASCENDING)
    {
      altTextKey = "af_column.SORTED_ASCEND_TIP";
    }
    else if (sortability == SORT_DESCENDING)
    {
      altTextKey = "af_column.SORTED_DESCEND_TIP";
    }
    else if (sortability == SORT_SORTABLE)
    {
      altTextKey = "af_column.SORTED_SORTABLE_TIP";
    }


    String altText = rc.getTranslatedString(altTextKey);

    Object align = OutputUtils.getMiddleIconAlignment(rc);

    //Don't render any child element for input element
    if (!renderedInput)
    {
      // Render the icon, specifying embedded=true.  This
      // allows text-based Icons to render their style class
      // and altText directly on the anchor itself
      OutputUtils.renderIcon(context,
                             rc,
                             icon,
                             altText,
                             align,
                             true);
    }

    if (supportNav)
    {
      if(NonJavaScriptBrowser)
      {
        writer.writeAttribute("title", altText, null);
        writer.endElement("input");
      }
      else
      {
        writer.endElement("a");
      }
    }

  }

  /**
   * @return 0 if not sortable. 1 if sortable, but not sorted.
   * 2 if sorted in ascending order. 3 if sorted in descending order.
   */
  protected final int getSortability(
    TableRenderingContext tContext,
    UIComponent           column)
  {
    // no columns sortable when empty table
    if (tContext.getRowData().isEmptyTable())
      return SORT_NO;

    FacesBean bean = getFacesBean(column);
    // Check to make sure this is a sortable UIComponent column.
    // some columns are uix 2.2 fake columns for "select", "details" and other
    // special columns.
    // =-=AEW FIX
    if (column == null)
      return 0;

    // If there's no sort property, it's not sortable
    String property = getSortProperty(column, bean);
    if (property == null)
      return SORT_NO;

    // And if the renderer-specific "sortable" property is set to false,
    // it's not sortable
    if (!getSortable(column, bean))
      return SORT_NO;

    // Otherwise, look at the first sort criteria
    // =-=AEW This seems slow...
    UIXCollection table = (UIXCollection) tContext.getTable();
    List<SortCriterion> criteria = table.getSortCriteria();
    // We currently only show anything for the primary sort criterion
    if (criteria.size() > 0)
    {
      SortCriterion criterion = criteria.get(0);
      if (property.equals(criterion.getProperty()))
      {
        return criterion.isAscending() ? SORT_ASCENDING : SORT_DESCENDING;
      }
    }

    return table.isSortable(property) ? SORT_SORTABLE : SORT_NO;
  }

  protected boolean hasSortingIcon(
    RenderingContext rc,
    int              sortability)
  {
    return sortability != SORT_NO;
  }

  /**
   * gets the icon name to use
   */
  private String _getIconName(
    int sortable)
  {
    switch (sortable)
    {
      case SORT_SORTABLE:
        return SkinSelectors.AF_COLUMN_UNSORTED_ICON_NAME;
      case SORT_ASCENDING:
        return SkinSelectors.AF_COLUMN_SORTED_ASCEND_ICON_NAME;
      case SORT_DESCENDING:
        return SkinSelectors.AF_COLUMN_SORTED_DESCEND_ICON_NAME;
      default:
        return null;
    }
  }

  private void _computeMode(
    FacesContext          context,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    // since we use colSpan we need headers attributes on all the table's data
    // cells:
    tContext.setExplicitHeaderIDMode(true);

    NodeData parentNode = getParentNode(tContext);
    boolean areWeRoot = (parentNode == null);

    int kids = component.getChildCount();

    NodeData currentNode = new NodeData(kids);
    if (areWeRoot)
    {
      _getNodeList(tContext, true).add(currentNode);
    }

    _setParentNode(tContext, currentNode);

    // "Render" the children to execute their "compute mode"
    _renderChildren(context, component, currentNode);

    ColumnData colData = tContext.getColumnData();
    if (areWeRoot)
    {
      colData.setHeaderRowSpan(currentNode.rows);
      int cols = currentNode.cols + colData.getColumnCount() - 1;
      colData.setColumnCount(cols);
    }
    else
    {
      int rows = currentNode.rows+1;
      if (parentNode.rows < rows)
        parentNode.rows = rows;

      parentNode.cols += currentNode.cols;

      parentNode.set(parentNode.currentChild, currentNode);
    }

    _setParentNode(tContext, parentNode);
  }

  @SuppressWarnings("unchecked")
  private void _renderChildren(
    FacesContext context,
    UIComponent  component,
    NodeData     parentNode
    ) throws IOException
  {
    int i = 0;
    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        // Tell the parent node - if there is one - which child we're rendering
        if (parentNode != null)
        {
          parentNode.currentChild = i;
        }

        encodeChild(context, child);
      }

      i++;
    }
  }

  protected final NodeData getParentNode(
    TableRenderingContext tContext)
  {
    NodeList nl = _getNodeList(tContext, false);
    return (nl == null) ? null : nl.currentNode;
  }

  private void _setParentNode(
    TableRenderingContext tContext,
    NodeData              parentNode)
  {
    _getNodeList(tContext, true).currentNode = parentNode;
  }

  /**
   * Returns the skinning selector for the header
   * taking into account the if the column is sortable or is sorted
   * @param tContext the TableRenderingContext
   * @param sortability the value returned by getSortability()
   * @return the skinning selector for the header
   */
  protected String getSortableHeaderStyleClass(
    TableRenderingContext tContext,
    int                   sortability)
  {
    ColumnData colData = tContext.getColumnData();
    // if we are a columnGroup header, then we must be centered:
    if (colData.isColumnGroupHeader())
    {
      return SkinSelectors.AF_COLUMN_HEADER_ICON_STYLE;
    }

    switch (sortability)
    {
      //not sortable column
      case SORT_NO:
        return getHeaderStyleClass(tContext);
        //sortable column (but not sorted)
      case SORT_SORTABLE:
        return ColumnData.selectFormat(
            tContext,
            SkinSelectors.AF_COLUMN_SORTABLE_HEADER_STYLE_CLASS,
            SkinSelectors.AF_COLUMN_SORTABLE_HEADER_NUMBER_STYLE_CLASS,
            SkinSelectors.AF_COLUMN_SORTABLE_HEADER_ICON_STYLE_CLASS);
        //sorted column
      default:
        return ColumnData.selectFormat(
            tContext,
            SkinSelectors.AF_COLUMN_SORTED_HEADER_STYLE_CLASS,
            SkinSelectors.AF_COLUMN_SORTED_HEADER_NUMBER_STYLE_CLASS,
            SkinSelectors.AF_COLUMN_SORTED_HEADER_ICON_STYLE_CLASS);
    }
  }

  private NodeList _getNodeList(
    TableRenderingContext tContext,
    boolean               create)
  {
    NodeList root =
      (NodeList) tContext.getHeaderNodesList();
    if ((root == null) && create)
    {
      root = new NodeList();
      tContext.setHeaderNodeList(root);
    }

    return root;
  }

  private static final class NodeList
  {
    private final ArrayList<NodeData> _list = new ArrayList<NodeData>(10);
    private int _index = 0;

    public NodeData currentNode = null;

    public void add(NodeData node)
    {
      _list.add(node);
    }

    public NodeData getNext()
    {
      if (_index >= _list.size())
        _index = 0;

      return _list.get(_index++);
    }
  }

  protected static final class NodeData
  {
    private final NodeData[] _kids;
    public int rows, cols, waitUntilRow = 0;
    public int currentChild;
    public String headerIDs = null;

    // for leaf nodes
    public NodeData()
    {
      rows = cols = 1;
      _kids = null;
    }

    // for parent nodes
    private NodeData(int kids)
    {
      rows = cols = 0;
      _kids = new NodeData[kids];
    }

    public void set(int index, NodeData kid)
    {
      _kids[index] = kid;
    }

    public int size()
    {
      return (_kids == null) ? 0 : _kids.length;
    }

    public NodeData get(int index)
    {
      return _kids[index];
    }
  }

  /**
   * @return the state of the sorting after the page submition
   */
  private String findSortState(
    int         sortability,
    UIComponent component,
    FacesBean   bean)
  {
    String state;
    if (sortability == SORT_ASCENDING)
    {
      state = XhtmlConstants.SORTABLE_ASCENDING;
    }
    else if (sortability == SORT_DESCENDING)
    {
      state = XhtmlConstants.SORTABLE_DESCENDING;
    }
    else if ("descending".equals(getDefaultSortOrder(component, bean)))
    {
      state = XhtmlConstants.SORTABLE_ASCENDING;
    }
    else
    {
      state = "";
    }

    return state;
  }

  private PropertyKey _headerTextKey;
  private PropertyKey _headerNoWrapKey;
  private PropertyKey _rowHeaderKey;
  private PropertyKey _separateRowsKey;
  private PropertyKey _noWrapKey;
  private PropertyKey _alignKey;
  private PropertyKey _widthKey;
  private PropertyKey _sortPropertyKey;
  private PropertyKey _sortableKey;
  private PropertyKey _defaultSortOrderKey;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColumnGroupRenderer.class);
}
