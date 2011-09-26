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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.CollectionComponent;
import org.apache.myfaces.trinidad.component.TableUtils;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.UIXColumn;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.SortEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.render.ClientRowKeyManager;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.CellUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.DetailColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.SelectionColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.SpecialColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableSelectManyRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableSelectOneRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;


abstract public class TableRenderer extends XhtmlRenderer
{
  public TableRenderer(
    FacesBean.Type type)
  {
    super(type);
    _resourceKeyMap = createResourceKeyMap();
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _widthKey  = type.findKey("width");
    _emptyTextKey  = type.findKey("emptyText");
    _navBarRenderer = new NavBar(type);
    _selectRenderer = new SelectionColumnRenderer(type);
    _selectOne = new TableSelectOneRenderer(type);
    _selectMany = new TableSelectManyRenderer(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    decodeSelection(facesContext, component);

    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();

    Object source = parameters.get("javax.faces.source");

    // Support the legacy as well as JSF2 parameter name
    if (source == null)
    {
      source = parameters.get("source");
    }

    String id = clientId == null ? component.getClientId(facesContext) : clientId;
    if (!id.equals(source))
      return;

    UIXTable table = (UIXTable) component;
    Object eventParam = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.GOTO_EVENT.equals(eventParam))
    {
      _decodeGoto(table, parameters);
    }
    else if (XhtmlConstants.HIDE_EVENT.equals(eventParam) ||
             XhtmlConstants.SHOW_EVENT.equals(eventParam))
    {
      _decodeHideShow(table, parameters, eventParam);
    }
    else if (XhtmlConstants.SORT_EVENT.equals(eventParam))
    {
      _decodeSort(table, parameters);
    }

    RequestContext.getCurrentInstance().addPartialTarget(table);
  }

  protected final void decodeSelection(
    FacesContext context,
    UIComponent  treeTable)
  {
    String selection = (String)
      treeTable.getAttributes().get(CoreTable.ROW_SELECTION_KEY.getName());
    if ("single".equals(selection))
      _selectOne.decode(context, treeTable);
    else if ("multiple".equals(selection))
      _selectMany.decode(context, treeTable);
  }

  public static RangeChangeEvent createRangeChangeEvent(
    CollectionComponent table,
    int                 newStart)
  {
    int newEnd = TableUtils.getLast(table, newStart);

    return _createRangeChangeEvent(table, newStart, newEnd);
  }


  /**
   * Returns the set of row keys identified by PPR.  Returns
   * the empty set if no row keys are present, and null
   * if row keys could not be properly identified.
   */
  static public Set<Object> getPartialRowKeys(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    String           clientId)
  {
    ClientRowKeyManager rowKeyManager = null;
    if (component instanceof UIXCollection)
      rowKeyManager = ((UIXCollection) component).getClientRowKeyManager();

    Set<Object> rowKeys = null;
    String tablePrefix = clientId + NamingContainer.SEPARATOR_CHAR;
    // Search for any PPR targets that start with "<tableClientId>:"
    PartialPageContext ppc = rc.getPartialPageContext();
    Iterator<String> targets = ppc.getPartialTargets();
    while (targets.hasNext())
    {
      String target = targets.next();
      if (target == null)
        continue;
      if (target.startsWith(tablePrefix))
      {
        // If we don't have a rowkeymanager, we know that the table
        // has partial targets, but we can't process the rows individually
        if (rowKeyManager == null)
          return null;

        // Extract the client rowkey from the clientId
        String clientRowKey = target.substring(tablePrefix.length());
        int ncIndex = clientRowKey.indexOf(NamingContainer.SEPARATOR_CHAR);
        // If we have a target that is in the table, but is not in a
        // particular row, just repaint the whole table
        if (ncIndex < 0)
          return null;

        clientRowKey = clientRowKey.substring(0, ncIndex);

        // Try to turn it into a server rowkey
        Object rowKey = rowKeyManager.getRowKey(context, component, clientRowKey);
        // if this fails, we have to process the whole table
        if (rowKey == null)
          return null;

        // We know this row exists, and needs to be processed
        if (rowKeys == null)
          rowKeys = new HashSet<Object>();
        rowKeys.add(rowKey);
      }
    }

    // If we never found a rowkey, return the empty set, indicating
    // that there are no rows to process
    if (rowKeys == null)
      rowKeys = Collections.emptySet();
    return rowKeys;
  }

  private static RangeChangeEvent _createRangeChangeEvent(
    CollectionComponent table,
    int                 newStart,
    int                 newEnd)
  {
    int oldStart = table.getFirst();
    int oldEnd = TableUtils.getLast(table) + 1;
    return
      new RangeChangeEvent((UIComponent) table, oldStart, oldEnd, newStart, newEnd);
  }

   private void _decodeSort(
    UIXTable            table,
    Map<String, String> parameters)
  {
    String property = parameters.get(XhtmlConstants.VALUE_PARAM);
    Object state = parameters.get(XhtmlConstants.STATE_PARAM);
    boolean sortOrder = !XhtmlConstants.SORTABLE_ASCENDING.equals(state);
    SortCriterion criterion = new SortCriterion(property, sortOrder);

    SortEvent event =
      new SortEvent(table, Collections.singletonList(criterion));
    event.queue();
  }

  private void _decodeGoto(
    UIXTable            table,
    Map<String, String> parameters)
  {
    String value = parameters.get(XhtmlConstants.VALUE_PARAM);
    if (value != null)
    {
      final FacesEvent event;
      if (XhtmlConstants.VALUE_SHOW_ALL.equals(value))
      {
        int newEnd = table.getRowCount();
        if (newEnd >= 0)
          event = _createRangeChangeEvent(table, 0, newEnd);
        else
          return;
      }
      else
      {
        int newStart = Integer.parseInt(value) - 1;
        event = createRangeChangeEvent(table, newStart);
      }

      event.queue();
      /* =-=AEW Don't set current value immediately - since that
           would mean that validate/updateModelValues run on the
           wrong rows!!!  Queue an event.
      System.out.println("DECODE: GOTO " + value);
      component.setAttribute("currentValue",
          new Integer(Integer.parseInt(value)));*/

      // I don't believe we want to skip to "renderResponse()" here,
      // since we want values to be applied!
      // context.renderResponse();
    }
  }


  @SuppressWarnings("unchecked")
  private void _decodeHideShow(
    UIXTable            table,
    Map<String, String> parameters,
    Object              eventParam)
  {
    boolean doExpand = XhtmlConstants.SHOW_EVENT.equals(eventParam);
    Object value = parameters.get(XhtmlConstants.VALUE_PARAM);
    if (value != null)
    {
      RowKeySet old = table.getDisclosedRowKeys();
      RowKeySet newset = old.clone();
      if ("all".equals(value))
      {
        if (doExpand)
          newset.addAll();
        else
          newset.removeAll();
        FacesEvent event = new RowDisclosureEvent(old, newset, table);
        event.queue();
      }
      else
      {
        int rowIndex = Integer.parseInt((String) value);
        int oldIndex = table.getRowIndex();
        table.setRowIndex(rowIndex);
        newset.setContained(doExpand);
        FacesEvent event = new RowDisclosureEvent(old, newset, table);
        event.queue();
        table.setRowIndex(oldIndex);
      }
    }
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    Set<Object> keysToRender = null;
    // See if we can skip rendering altogether
    if (canSkipRendering(context, rc, component))
    {
      // If we're in here, then the table itself as a whole doesn't
      // need to be re-rendered - but the contents might need to be!
      keysToRender = getPartialRowKeys(context,
                                       rc,
                                       component,
                                       getClientId(context, component));
      // getPartialRowKeys() has a weird API.  null means there are contents
      // that need to be rendered, but we couldn't figure out what row key
      // they match up to, so just re-render everything and let PPR figure it
      // out.  An empty set means *there's nothing*, so bail
      //
      if ((keysToRender != null) && keysToRender.isEmpty())
        return;

      // TODO: use keysToRender to only iterate onto rows
      // that have targets
    }

    // save current skin resource map, if any, on the local property
    Map<String, String> oldSkinResourceMap = rc.getSkinResourceKeyMap();

    // store TableRenderer's skin resource map, so that called to
    // context.getTranslatedValue will get the correct key.
    rc.setSkinResourceKeyMap(_resourceKeyMap);

    TableRenderingContext tContext = createRenderingContext(context,
                                        rc,
                                        component);

    try
    {
      tContext.install();

      ResponseWriter rw = context.getResponseWriter();

      rw.startElement("div", component);
      renderId(context, component);
      renderAllAttributes(context, rc, component, bean);

      // If we need to render a "special" empty table, then bail.
      if (renderTableWithoutColumns(context, rc, tContext, component))
          return;

      // start the outer table:
      rw.startElement(XhtmlConstants.TABLE_ELEMENT, null);
      renderTableAttributes(context, rc, component, bean, "0", "0");

      RenderStage renderStage = tContext.getRenderStage();
      assert (renderStage.getStage()==RenderStage.INITIAL_STAGE);

      // give the table's columns a chance to initialize:
      renderSingleRow(context, rc, tContext, component);

      // 1. render the header bars (title, controlbar and subcontrolbar)
      renderNavigationHeaderBars(context, rc, tContext, component, bean);

      // 2. render the table content
      renderTableContent(context, rc, tContext, component);

      // 3. render the footer bars (controlbar) if applicable
      if (_shouldRepeatControlBar(rc))
      {
        renderNavigationFooterBars(context, rc, tContext, component, bean);
      }

      // end the outertable:
      rw.endElement(XhtmlConstants.TABLE_ELEMENT);


      // gives some beans the chance to cleanup:
      renderStage.setStage(RenderStage.END_STAGE);
      renderSingleRow(context, rc, tContext, component);

      String tid = tContext.getTableId();
      FormData formData = rc.getFormData();
      if (formData != null)
      {
        // Add sorting parameters.
        // =-=AdamWiner FIXME: only really needed with sorting.
        formData.addNeededValue(XhtmlConstants.STATE_PARAM);
        formData.addNeededValue(XhtmlConstants.VALUE_PARAM);

        //HKuhn - no need for scripts in printable mode
        if (supportsScripting(rc))
        {
          rw.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
          renderScriptDeferAttribute(context, rc);
          // Bug #3426092:
          // render the type="text/javascript" attribute in accessibility mode
          renderScriptTypeAttribute(context, rc);

          String formName = formData.getName();

          rw.writeText(tContext.getJSVarName()+"="+
                 TreeUtils.createNewJSCollectionComponentState(formName, tid)+";", null);
          rw.endElement(XhtmlConstants.SCRIPT_ELEMENT);

        }
      }

      int first = tContext.getCollectionComponent().getFirst();

      if (supportsScripting(rc))
      {
        XhtmlUtils.addLib(context, rc, "TableProxy()");

        // Bug #2378405: Add a javascript variable giving the row number of
        // the first row in the displayed rowset.

        // Although it seems like we should check for existence here (to
        // prevent duplication), we actually should not. If we have multiple
        // tables on a page, they will all need independent value fields.

        // We'd really like to use the flattened name here, but a colon doesn't
        // work as part of a javascript variable name, so we have to build up a
        // pseudo flattened name of the form _<tableName>_value.
        // (=-=AEW Change code to write the value directly into the window,
        // so a colon *would* work;  however, if a field had an id of "value"
        // in the table, we'd get a conflict;  so don't change?)

        // Also, since 1 is by far the most common value, don't bother
        // writing it out: the Javascript will assume the value is 1 if
        // it isn't.
        int value = first + 1;
        if (value != 1)
        {
          rw.startElement(XhtmlConstants.SCRIPT_NAME, null);
          renderScriptDeferAttribute(context, rc);
          // Bug #3426092:
          // render the type="text/javascript" attribute in accessibility mode
          renderScriptTypeAttribute(context, rc);
          rw.writeText("window[\"_", null);
          rw.writeText(tContext.getTableId(), null);
          rw.writeText(_VALUE_FIELD_NAME, null);
          rw.writeText("\"]=", null);
          rw.writeText(IntegerUtils.getString(value), null);
          rw.endElement(XhtmlConstants.SCRIPT_NAME);
        }
      }

      OutputUtils.renderHiddenField(context,
                                    tContext.getTableId() + ":rangeStart",
                                    IntegerUtils.getString(first));

      rw.endElement("div");
    }
    finally
    {
      // restore current skin resource map. Most likely there won't be one.
      rc.setSkinResourceKeyMap(oldSkinResourceMap);

      if (tContext != null)
        tContext.release();
    }

  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_TABLE_STYLE;
  }

  /**
   * renders attributes on the outermost table element.
   * this includes width, cellpadding, cellspacing, border.
   */
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

    OutputUtils.renderLayoutTableAttributes(context,
                                            rc,
                                            cellPadding,
                                            "0",    // cell spacing
                                            border,
                                            width); // table width
  }

  /**
   * Creates the correct subclass of the TableRenderingContext to
   * use for this Renderer.
   */
  protected TableRenderingContext createRenderingContext(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component
    )
  {
    return new TableRenderingContext(context, rc, component);
  }

  protected abstract void renderSingleRow(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException;

  /**
   * Render an empty table, if necessary.
   * @return true if the table was empty, and an alternative empty
   * version was shown, false otherwise.
   * @TODO COMPRESS JOINED STYLES
   */
  protected boolean renderTableWithoutColumns(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    ColumnData colData = tContext.getColumnData();
    if (colData.getColumnCount() <= 0)
    {
      // see bug 2633464
      if (_LOG.isWarning())
        _LOG.warning("TABLE_HAS_NO_VISIABLE_COLUMN", tContext.getTableId());

      ResponseWriter writer = context.getResponseWriter();

      // render something so that the visual editor previewer will show a
      // simple <table/> tag:
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, component);
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, "30", null);
      renderStyleClasses(context, rc,
                         new String[]{
                           SkinSelectors.AF_COLUMN_CELL_TEXT_STYLE,
                           CellUtils.getBorderClass(
                               true,
                               true,
                               true,
                               true)});

      renderSpacer(context, rc, "30", "30");
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ELEMENT);
      return true;
    }

    return false;
  }

  /**
   * used to render special column headers, like select and details.
   * @return the next physicalColumnIndex
   */
  @SuppressWarnings("unchecked")
  protected int renderSpecialColumns(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           treeTable,
    int                   physicalColumnIndex
    ) throws IOException
  {
    // renders a whole bunch of <TH>...</TH> elements, or <TD>..</TD> elements
    // depending on the RenderStage
    final ColumnData colData = tContext.getColumnData();
    int[] hidden = tContext.getHiddenColumns();
    List<UIComponent> children = treeTable.getChildren();
    int colCount  = children.size();
    for (int i = 0;  i < colCount;  i++)
    {
      if (hidden[i] != TableRenderingContext.NORMAL_COLUMN)
        continue;

      UIComponent child = children.get(i);
      if (!(child instanceof UIXColumn))
        continue;

      UIXColumn column = (UIXColumn) child;
      boolean isRowHeader = Boolean.TRUE.equals(
            column.getAttributes().get(CoreColumn.ROW_HEADER_KEY.getName()));
      if (isRowHeader)
      {
        colData.setColumnIndex(physicalColumnIndex, i);
        encodeChild(context, column);
        // ColumnBeans automatically increment the physical and logical
        // column indices (these may be increase by more than one, if
        // there are columnGroups). So we must not increment the column
        // indices here
        physicalColumnIndex = colData.getPhysicalColumnIndex();
      }
      else
        break;
    }

    // special case... render the selection column
    if (tContext.hasSelection())
    {
      colData.setColumnIndex(physicalColumnIndex, ColumnData.SPECIAL_COLUMN_INDEX);

      _renderSelectionColumn(context, rc, tContext);
      physicalColumnIndex++;
    }
    // special case... render the detail column
    UIComponent detail = tContext.getDetail();
    if (detail != null)
    {
      colData.setColumnIndex(physicalColumnIndex, ColumnData.SPECIAL_COLUMN_INDEX);
      _renderDetailColumn(context, rc);

      physicalColumnIndex++;
    }

    return physicalColumnIndex;
  }

  private void _renderDetailColumn(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    UIComponent column = _detailRenderer.getSpecialColumn();
    delegateRenderer(context, rc, column,
                     getFacesBean(column), _detailRenderer);
  }

  private void _renderSelectionColumn(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext
    ) throws IOException
  {
    Map<String, String> originalResourceKeyMap = rc.getSkinResourceKeyMap();
    setSelectionResourceKeyMap(rc, tContext);
    try
    {
      UIComponent column = _selectRenderer.getSpecialColumn();
      delegateRenderer(context, rc, column,
                       getFacesBean(column), _selectRenderer);
    }
    finally
    {
      rc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

  /**
   * Render the navigation header bars, i.e. all the bars that appear above the
   * actual data table. eg. title, controlbar and subcontrolbar
   */
  protected void renderNavigationHeaderBars(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    FacesBean             bean
    ) throws IOException
  {

    // 2. render the upper control bar - must render tableActions even
    // if table is empty
    _renderControlBar(context, rc, tContext, component, true); //isUpper

    //   render the sub control bar. we need to to this even if the table is empty
    // because we need to render the filter area. bug 3757395
    renderSubControlBar(context, rc, tContext, component, true);
  }

  /**
   * Render the navigation header bars, i.e. all the bars that appear above the
   * actual data table. eg. title, controlbar and subcontrolbar
   */
  protected void renderNavigationFooterBars(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    FacesBean             bean
    ) throws IOException
  {
    // Render the lower control bar - must render tableActions even if table is empty.
    _renderControlBar(context, rc, tContext, component, false); //isUpper
  }

  private boolean _shouldRepeatControlBar(RenderingContext rc)
  {
    Object propValue =
      rc.getSkin().getProperty(SkinProperties.AF_TABLE_REPEAT_CONTROL_BAR);

    if (propValue == null)
    {
      return DEFAULT_REPEAT_CONTROL_BAR;
    }

    return Boolean.TRUE.equals(propValue);
  }

  /**
   * @todo Decide if we really want to support "repeating" regions
   */
  private void _renderControlBar(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean               isUpper
    ) throws IOException
  {
    // indicate that this is a repeating region, so that beans like the
    // choiceBean can stay synchronized
    rc.getProperties().put(XhtmlConstants.REPEAT_PROPERTY,
                         Boolean.TRUE);

    RenderStage rs = tContext.getRenderStage();
    rs.setStage(isUpper
                ? RenderStage.UPPER_CONTROL_BAR_STAGE
                : RenderStage.LOWER_CONTROL_BAR_STAGE);


    // Due to layout problems that occur when performing a partial
    // page replacement of a TableBean (2275703), we need to also perform
    // an explicit partial replacement of the upper navigation bar.  This
    // means that we need to generate a unique ID for the upper
    // navigation bar.  Since we may not actually have a TableRenderingContext
    // when fetching the navigation bar's ID (we may have a
    // PartialRenderingContext - which is another problem altOgether),
    // we just generate the ID here and store it away on the RenderingContext.
    if (isUpper)
    {
      // We only generate the navigation bar ID if the agent is IE
      // and partial rendering is enabled.
      Object id = tContext.getTableId();
      Agent agent = rc.getAgent();

      if ((agent.getAgentName() == Agent.AGENT_IE) &&
          PartialPageUtils.isPPRActive(context))
      {
        String navBarID = id.toString() + "-nb";
        setRenderingProperty(rc, _UPPER_NAV_BAR_ID_PROPERTY, navBarID);
      }
    }


    renderControlBar(context, rc, tContext, component);

    // no longer a repeating region
    rc.getProperties().remove(XhtmlConstants.REPEAT_PROPERTY);

    if (isUpper)
      setRenderingProperty(rc, _UPPER_NAV_BAR_ID_PROPERTY, null);
  }

  /**
   * Renders the control bar
   */
  protected abstract void renderControlBar(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException;

  /**
   * Render sthe area with the filter and links, if necessary
   */
  protected abstract void renderSubControlBar(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean               isUpper
    ) throws IOException;

  /**
   * Renders the actual table content, with headers
   */
  protected abstract void renderTableContent(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException;

  protected String getEmptyText(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_emptyTextKey));
  }

  protected Object getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_widthKey);
  }

  /**
   * Returns the shared UINode used to render detail hide/show
   */
  protected final CoreRenderer getSharedHideShowNode()
  {
    return null;
  }


  /**
   * Returns the shared Renderer used to render navbars
   */
  protected CoreRenderer getSharedNavBarRenderer()
  {
    return _navBarRenderer;
  }

  /**
   */
  public static String getRowHeaderFormatClass()
  {
    return SkinSelectors.AF_COLUMN_ROW_HEADER_TEXT_STYLE;
  }

  /**
   * @param isColumnHeader true if the style for a column header is needed.
   * @todo Eliminate this method altogether;  row headers are static,
   *   and column headers should just call
   *     ColumnGroupRenderer.getHeaderStyleClass()
   */
  public static String getHeaderFormatClass(
    TableRenderingContext tContext,
    boolean               isColumnHeader)
  {
    if (isColumnHeader)
      throw new IllegalStateException(_LOG.getMessage(
        "DONOT_CALL_THIS_FOR_COLUMN_HEADERS"));

    return SkinSelectors.AF_COLUMN_ROW_HEADER_TEXT_STYLE;
  }

  /**
   * Sets the skinResourceKeyMap on the RenderingContext with a map
   * which maps SkinSelectors.AF_COLUMN_CELL* styles to SkinSelectors.AF_TABLE_SELECT_MANY or
   * SkinSelectors.AF_TABLE_SELECT_ONE styles. We look at the selectionNode to figure
   * out if it is tableSelectOne or tableSelectMany
   * @todo Can this be private?
   * @todo reuse these Maps!
   */
  public static void setSelectionResourceKeyMap(
    RenderingContext      rc,
    TableRenderingContext tContext)
  {
    if (tContext.hasSelection())
    {
      Map<String, String> selectionColumnStylesMap =
        new HashMap<String, String>();

      // if selection is multiple-selection:
      if (tContext.hasSelectAll())
      {
        selectionColumnStylesMap.put(SkinSelectors.AF_COLUMN_CELL_ICON_FORMAT_STYLE,
                                SkinSelectors.AF_TABLE_SELECT_MANY_CELL_ICON_FORMAT_STYLE);
        selectionColumnStylesMap.put(SkinSelectors.AF_COLUMN_CELL_ICON_BAND_STYLE,
                                SkinSelectors.AF_TABLE_SELECT_MANY_CELL_ICON_BAND_STYLE);
      }
      else
      {
        selectionColumnStylesMap.put(SkinSelectors.AF_COLUMN_CELL_ICON_FORMAT_STYLE,
                                SkinSelectors.AF_TABLE_SELECT_ONE_CELL_ICON_FORMAT_STYLE);
        selectionColumnStylesMap.put(SkinSelectors.AF_COLUMN_CELL_ICON_BAND_STYLE,
                                SkinSelectors.AF_TABLE_SELECT_ONE_CELL_ICON_BAND_STYLE);
      }
      rc.setSkinResourceKeyMap(selectionColumnStylesMap);
    }

  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  protected Map<String, String> createResourceKeyMap()
  {
    // map the skin resource keys that are used in components used
    // by the table renderer to table keys.
    // This way the table can be customized separately from other
    // components that it uses within it. For example, we can customize
    // af_table.DISCLOSED translation key
    // separately from af_showDetail.DISCLOSED.
    Map<String, String> map = new HashMap<String, String>(6);
    map.put("af_showDetail.DISCLOSED",
            "af_table.DISCLOSED");
    map.put("af_showDetail.UNDISCLOSED",
            "af_table.UNDISCLOSED");
    map.put("af_showDetail.DISCLOSED_TIP",
            "af_table.DISCLOSED_TIP");
    map.put("af_showDetail.UNDISCLOSED_TIP",
            "af_table.UNDISCLOSED_TIP");

    map.put(SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
            SkinSelectors.AF_TABLE_SD_DISCLOSED_ICON_NAME);
    map.put(SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
            SkinSelectors.AF_TABLE_SD_UNDISCLOSED_ICON_NAME);
    map.put(SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME,
            SkinSelectors.AF_TABLE_NB_PREV_ICON_NAME);
    map.put(SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME,
          SkinSelectors.AF_TABLE_NB_NEXT_ICON_NAME);
    map.put(SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME,
          SkinSelectors.AF_TABLE_NB_PREV_DISABLED_ICON_NAME);
    map.put(SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME,
        SkinSelectors.AF_TABLE_NB_NEXT_DISABLED_ICON_NAME);



    return Collections.unmodifiableMap(map);
  }

  static private class NavBar extends SelectRangeChoiceBarRenderer
  {
    public NavBar(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected void renderAllAttributes(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean)
    {
    }

    @Override
    protected boolean getShowAll(
      UIComponent component,
      FacesBean   bean)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      UIComponent c = tContext.getTable();
      if (c instanceof UIXTable)
      {
        UIXTable table = (UIXTable)c;
        return table.isShowAll();
      }

      return false;
    }

    // For now, disable showAll except on UIXTable
    @Override
    protected boolean showAllSupported()
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      UIComponent component = tContext.getTable();
      return (component instanceof UIXTable);
    }

    @Override
    protected String getSource()
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.getTableId();
    }

    /**
     * @todo Deal with repeating!
     */
    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.getTableId() + "-nb";
    }

    @Override
    protected String getVar(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    // No support for range labels
    @Override
    protected UIComponent getRangeLabel(
      UIComponent component)
    {
      return null;
    }

    @Override
    protected int getRowCount(
      UIComponent component)
    {
      return ((CollectionComponent) component).getRowCount();
    }

    @Override
    protected int getRowIndex(
      UIComponent component)
    {
      return ((CollectionComponent) component).getRowIndex();
    }

    @Override
    protected void setRowIndex(
      UIComponent component,
      int         index)
    {
      ((CollectionComponent) component).setRowIndex(index);
    }

    @Override
    protected boolean isRowAvailable(
      UIComponent component)
    {
      return ((CollectionComponent) component).isRowAvailable();
    }

    @Override
    protected boolean isRowAvailable(
      UIComponent component,
      int         rowIndex)
    {
      return ((UIXCollection) component).isRowAvailable(rowIndex);
    }

    @Override
    protected Object getRowData(
      UIComponent component)
    {
      return ((CollectionComponent) component).getRowData();
    }

    @Override
    protected int getRows(
      UIComponent component,
      FacesBean   bean)
    {
      return ((CollectionComponent) component).getRows();
    }

    @Override
    protected int getFirst(
      UIComponent component,
      FacesBean   bean)
    {
      return ((CollectionComponent) component).getFirst();
    }
  }

  private PropertyKey _widthKey;
  private PropertyKey _emptyTextKey;
  private final Map<String, String> _resourceKeyMap;

  // Key for RenderingContext property used to store the generated ID
  // to use for the upper navigation bar.  (Part of fix for 2275703.)
  private static final Object _UPPER_NAV_BAR_ID_PROPERTY = new Object();

  private static final String _VALUE_FIELD_NAME      = "_value";

  /**
   * Whether the table should repeat its control bars above and below the table by default if not
   * specified by the -tr-repeat-control-bar skin property.
   */
  public static final boolean DEFAULT_REPEAT_CONTROL_BAR = false;

  private final SpecialColumnRenderer _detailRenderer = new DetailColumnRenderer();

  private SpecialColumnRenderer _selectRenderer;
  private CoreRenderer _navBarRenderer;
  private CoreRenderer _selectOne;
  private CoreRenderer _selectMany;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TableRenderer.class);
}
