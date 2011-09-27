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
package org.apache.myfaces.trinidadinternal.renderkit.core.desktop;

import java.io.IOException;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.CollectionComponent;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.component.core.data.CoreTreeTable;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ResourceKeyUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.CellUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.FocusColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RowData;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.SpecialColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeNodeColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeTableNavRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeTableRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;


/**
 * Renderer for treeTable
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/desktop/TreeTableRenderer.java#0 $) $Date: 10-nov-2005.19:03:37 $
 */
public class TreeTableRenderer extends DesktopTableRenderer
{
  public TreeTableRenderer()
  {
    super(CoreTreeTable.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey  = type.findKey("immediate");
    _expandAllEnabledKey  = type.findKey("expandAllEnabled");
    _rootNodeRendered = type.findKey("rootNodeRendered");
  }

  /**
   * @todo Set "expanded" vs. "collapsed" correctly on the queued
   *  DisclosureEvent
   * @todo deal with null and exceptions
   */
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

    Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
    clientId = clientId == null ? component.getClientId(facesContext) : clientId;
    if (clientId.equals(source))
    {
      UIXTreeTable treeTable = (UIXTreeTable) component;
      TreeUtils.decodeExpandEvents(parameters, component,
                                   TreeUtils.getFocusRowKey(treeTable));
      TreeUtils.decodeFocusEvent(parameters, component);
      TreeUtils.decodeGotoEvent(parameters, component);
      // bug 4522210:
      RequestContext.getCurrentInstance().addPartialTarget(component);
    }
  }

  /**
   * Creates the correct subclass of the TableRenderingContext to
   * use for this Renderer.
   */
  @Override
  protected TableRenderingContext createRenderingContext(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component
    )
  {
    return new TreeTableRenderingContext(context, rc, component);
  }

  /**
   * Render an empty table, if necessary.
   * @return true if the table was empty, and an alternative empty
   * version was shown, false otherwise.
   */
  @Override
  protected boolean renderTableWithoutColumns(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    return false;
  }

  /**
   * render all pieces of the table
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    FormData fData = rc.getFormData();
    if (fData != null)
    {
      fData.addNeededValue(XhtmlConstants.PARTIAL_TARGETS_PARAM);
      fData.addNeededValue(XhtmlConstants.PARTIAL_PARAM);
    }

    // we cannot render without a nodeStamp:
    if (((UIXTreeTable) component).getNodeStamp() == null)
    {
      _LOG.warning("NODESTAMP_FACET_MISSING");
      return;
    }

    TreeUtils.setDefaultFocusRowKey((UIXTree) component);
    TreeUtils.expandFocusRowKey((UIXTree) component);

    super.encodeAll(context, rc, component, bean);

    // have we rendered the script before?
    // and are we not in printable mode (scripting disabled)?
    if (rc.getProperties().put(_JS_LIBS_KEY, Boolean.TRUE) == null
        && supportsScripting(rc))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
      renderScriptDeferAttribute(context, rc);
      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      renderScriptTypeAttribute(context, rc);
      boolean validate = !isImmediate(component, bean);
      String buff = TreeUtils.setupJSTreeCollectionComponent(validate);
      writer.writeText(buff, null);
      writer.endElement(XhtmlConstants.SCRIPT_ELEMENT);
    }
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  protected boolean isImmediate(
    UIComponent component,
    FacesBean   bean)
  {
    return  Boolean.TRUE.equals(bean.getProperty(_immediateKey));
  }

  /**
   * Render the navigation header bars, i.e. all the bars that appear above the
   * actual data table including the breadcrumbs at the end.
   */
  @Override
  protected void renderNavigationHeaderBars(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    FacesBean             bean
    ) throws IOException
  {
    super.renderNavigationHeaderBars(context, rc, tContext, component, bean);
    _renderBreadCrumbs(context, rc, tContext, component, bean);
  }

  @Override
  protected final void renderRangePagingControl(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           table
    ) throws IOException
  {
    UIXHierarchy comp = (UIXHierarchy) table;
    Object focusKey = comp.getFocusRowKey();
    final Object oldKey = comp.getRowKey();
    try
    {
      // set the collection that is being displayed:
      comp.setRowKey(focusKey);
      super.renderRangePagingControl(context, rc, tContext, table);
    }
    finally
    {
      comp.setRowKey(oldKey);
    }
  }

  @Override
  protected boolean hasControlBarLinks(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component
    ) throws IOException
  {
    return
      super.hasControlBarLinks(context, rc, tContext, component) ||
      isExpandAllEnabled(component);
  }

  @Override
  protected void renderControlBarLinks(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean               useDivider
    ) throws IOException
  {
    boolean hasExpandAll = isExpandAllEnabled(component);
    super.renderControlBarLinks(context, rc, tContext, component,
                                hasExpandAll || useDivider);
    if (hasExpandAll && supportsScripting(rc)) //not in printable mode
    {
      // must render these IDs so that PPR can restore the focus correctly:
      String preId = component.getClientId(context) + NamingContainer.SEPARATOR_CHAR;
      TreeTableRenderingContext hContext = (TreeTableRenderingContext) tContext;
      String onclick =
            TreeUtils.callJSExpandAll(hContext.getUIXTreeTable(),
                                      tContext.getJSVarName(),
                                      true /*isExpand*/);
       renderControlBarLink(context, rc, onclick, _EXPAND_ALL_TEXT_KEY,
                           rc.getIcon(SkinSelectors.AF_TREE_TABLE_EXPAND_ALL_ICON_NAME),
                            preId+"eAll", true);
      onclick =
            TreeUtils.callJSExpandAll(hContext.getUIXTreeTable(),
                                      tContext.getJSVarName(),
                                      false /*isExpand*/);
       renderControlBarLink(context, rc, onclick, _COLLAPSE_ALL_TEXT_KEY,
                           rc.getIcon(SkinSelectors.AF_TREE_TABLE_COLLAPSE_ALL_ICON_NAME),
                            preId+"cAll", useDivider);
    }
  }

  protected void renderControlBarLink(
    FacesContext     context,
    RenderingContext rc,
    String           onclick,
    String           translationKey,
    Icon             icon,
    String           id,
    boolean          hasDivider
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("a", null);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, id, null);
    renderStyleClass(context, rc, SkinSelectors.NAV_BAR_ALINK_STYLE_CLASS);
    writer.writeAttribute("onclick", onclick, null);
    writer.writeURIAttribute("href", "#", null);
    if (icon != null)
    {
      OutputUtils.renderIcon(context, rc, icon, rc.getTranslatedString(translationKey),
                             null);
    } else
    {
      writer.writeText(rc.getTranslatedString(translationKey), null);
    }

    writer.endElement("a");

    if (hasDivider)
      writer.writeText(LINKS_DIVIDER_TEXT, null);
  }

  protected boolean isExpandAllEnabled(
    UIComponent component)
  {
    FacesBean bean = getFacesBean(component);
    Object bool =
      bean.getProperty(_expandAllEnabledKey);
    if (bool == null)
      bool = _expandAllEnabledKey.getDefault();

    return Boolean.TRUE.equals(bool);
  }

  /**
   * used to render special column headers, like select and details.
   * @return the next physicalColumnIndex
   */
  @Override
  protected int renderSpecialColumns(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    int                   physicalColumnIndex
    ) throws IOException
  {
    // This renders a whole bunch of <TH>..</TH> elements or <TD>..</TD>
    // elements depending on the RenderStage

    physicalColumnIndex = super.renderSpecialColumns(context,
                                                     rc,
                                                     tContext,
                                                     component,
                                                     physicalColumnIndex);
    final ColumnData colData = tContext.getColumnData();
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) tContext;
    if (ttrc.isFocusColumnVisible())
    {
      colData.setColumnIndex(physicalColumnIndex++,
                             ColumnData.SPECIAL_COLUMN_INDEX);
      SpecialColumnRenderer focusRenderer = getFocusColumnRenderer();
      UIComponent column = focusRenderer.getSpecialColumn();
      delegateRenderer(context, rc, column,
                       getFacesBean(column), focusRenderer);
    }

    // render the object hierarchy column:
    colData.setColumnIndex(physicalColumnIndex++,
                           ColumnData.SPECIAL_COLUMN_INDEX);
    UIComponent treeNodeColumn = ttrc.getTreeNodeStamp();
    delegateRenderer(context, rc, treeNodeColumn,
                     getFacesBean(treeNodeColumn), _TREE_NODE);

    return physicalColumnIndex;
  }

  /**
   * render all the table rows
   */
  @Override
  protected void renderTableRows(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext trc,
    UIComponent           component,
    FacesBean             bean
    ) throws IOException
  {
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) trc;
    final RowData rowData = trc.getRowData();
    boolean isEmptyTable  = rowData.isEmptyTable();

    // render all the table content rows
    if (isEmptyTable)
      _renderEmptyTableRow(context, rc, ttrc);
    else
      _renderTableRows(context, rc, ttrc, component, bean);

    // render the footer
    renderFooter(context, rc, trc, component);

    // on an HGrid the number of rows depends on whether child nodes are
    // expanded or not.
    //rowData.setRowCount(rows);
  }

  protected String getControlLinkIconName(String translationKey)
  {
    if (translationKey == null)
      return null;

    return translationKey.equals(_SELECT_ALL_TEXT_KEY) ? SkinSelectors.AF_TREE_TABLE_SELECT_ALL_ICON_NAME
                                                       : SkinSelectors.AF_TREE_TABLE_SELECT_NONE_ICON_NAME;
  }


  protected SpecialColumnRenderer getFocusColumnRenderer()
  {
    return _FOCUS;
  }

  protected boolean isRootNodeRendered(
    UIComponent component,
    FacesBean   bean)
  {
    if (_rootNodeRendered == null)
      return true;

    return !Boolean.FALSE.equals(bean.getProperty(_rootNodeRendered));
  }

  //
  // Private methods
  //

  // Renders the hGridLocator Icon
  private void _renderLocatorIcon(
    FacesContext     fc,
    RenderingContext rc
    ) throws IOException
  {
    Icon icon = rc.getIcon(SkinSelectors.AF_TREE_TABLE_LOCATOR_ICON_NAME);

    if (icon != null)
    {
      Object altText = rc.getTranslatedString(_BREADCRUMBS_START_KEY);

      // Use "middle" alignment to center the icon with text
      Object align = OutputUtils.getMiddleIconAlignment(rc);

      // We don't bother specifying the style class.  This can
      // be specified by the Icon itself, since we aren't rendering
      // the Icon within a link (and thus we don't need to render
      // the style class ourselves.)
      OutputUtils.renderIcon(fc, rc, icon, altText, align);
    }
  }

  private void _renderBreadCrumbs(
    FacesContext          fc,
    RenderingContext      rc,
    TableRenderingContext context,
    UIComponent           tree,
    FacesBean             bean
    ) throws IOException
  {
    // this renders <TR><TD><[crumbs]></TD></TR>

    TreeTableRenderingContext hContext = (TreeTableRenderingContext) context;
    // we will render the breadcrumbs only if they exist.
    if (hContext.isFocusColumnVisible())
    {
      ResponseWriter writer = fc.getResponseWriter();
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HEIGHT_ATTRIBUTE, "30", null);

      final String borderStyleClass;

      borderStyleClass = CellUtils.getBorderClass( true,  //top
                                                   true,  //left
                                                   false, //bottom
                                                   true); //right
      renderStyleClasses(fc, rc, new String[] {
                                SkinSelectors.HGRID_LOCATOR_HEADER_STYLE,
                                borderStyleClass}
                        );

      int colspan = context.getActualColumnCount();
      writer.writeAttribute("colspan", IntegerUtils.getString(colspan), null);

      // The locator icon hits the left border if not for this spacer
      renderSpacer(fc, rc, "0", "2");

      _renderLocatorIcon(fc, rc);

      delegateRenderer(fc, rc, tree, bean, _CRUMBS);

      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      // end the <tr> containing the bread crumbs
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
    }
  }

  @SuppressWarnings("unchecked")
  private void _renderTableRows(
    FacesContext                    context,
    final RenderingContext          rc,
    final TreeTableRenderingContext ttrc,
    final UIComponent               component,
    final FacesBean                 bean
    ) throws IOException
  {
    final UIXTreeTable treeTableBase = ttrc.getUIXTreeTable();
    final ResponseWriter writer = context.getResponseWriter();
    final RowKeySet treeState = treeTableBase.getDisclosedRowKeys();
    final int specialColCount = _getSpecialColCount(ttrc);
    final boolean rootNodeRendered = isRootNodeRendered(component, bean);

    TableUtils.RowLoop loop = new TableUtils.RowLoop()
    {
      @Override
      protected void loop(FacesContext context, CollectionComponent treeTable)
        throws IOException
      {
        Object focusPath = TreeUtils.getFocusRowKey(treeTableBase);
        treeTableBase.setRowKey(focusPath);
        processRow(context, treeTable);
      }

      @Override
      protected void processRowImpl(FacesContext context, CollectionComponent treeTable)
        throws IOException
      {
        if (rootNodeRendered || treeTableBase.getDepth() > 0)
        {
          writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
          renderSingleRow(context, rc, ttrc, treeTableBase);
          writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
        }

//
  //        if (hasInvisibleNodes)
  //        {
  //          //context.addHiddenDataObject(treeTableBase.getRowData());
  //        }

        if (treeTableBase.isContainer())
        {
          if (treeState.isContained() || (rootNodeRendered == false && treeTableBase.getDepth() == 0))
          {
            treeTableBase.enterContainer();
            int rows = treeTableBase.getRows();
            int rowCount = treeTableBase.getRowCount();
            boolean renderNavRow = (rows > 0) &&
              ((rowCount < 0) || (rowCount > rows));
            if (renderNavRow)
            {
              renderEmptyTableRow(context, rc, ttrc, specialColCount, _NAV_TOP);
            }
            super.loop(context, treeTable);
            if (renderNavRow)
            {
              renderEmptyTableRow(context, rc, ttrc, specialColCount, _NAV_BOTTOM);
            }
            treeTableBase.exitContainer();
          }
        }
      }
    };

    loop.run(context, treeTableBase);

    //return currRow;
  }


  /**
   * render a table row for empty tables. This blanks everything out and writes
   * some text indicating to the user that the table is empty.
   * @return zero (the number of rows in this table).
   */
  private int _renderEmptyTableRow(
    FacesContext              context,
    RenderingContext          rc,
    TreeTableRenderingContext ttrc
    ) throws IOException
  {
    // we do not include the object hierarchy column in the specialCols
    // count. This is because we want the emptyText to appear in the
    // hierarchy column.
    renderEmptyTableRow(context, rc, ttrc, _getSpecialColCount(ttrc));
    return 0;
  }

  private int _getSpecialColCount(
    TreeTableRenderingContext ttrc)
  {
    int specialCols = 0;
    if (ttrc.hasSelection())
    {
      specialCols++;
    }

    if (ttrc.isFocusColumnVisible())
    {
      specialCols++;
    }
    return specialCols;
  }

  //
  // Private variables
  //

  private static final CoreRenderer _NAV_TOP = new TreeTableNavRenderer(true);
  private static final CoreRenderer _NAV_BOTTOM = new TreeTableNavRenderer(false);
  private static final SpecialColumnRenderer _FOCUS = new FocusColumnRenderer();
  private static final CoreRenderer _TREE_NODE = new TreeNodeColumnRenderer();
  private static final CoreRenderer _CRUMBS = new BreadCrumbsRenderer();

  private static final class BreadCrumbsRenderer
    extends org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.BreadCrumbsRenderer
  {
    @Override
    protected void renderLink(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      child,
      int              renderedCount,
      boolean          isLastChild
      ) throws IOException
    {
      TreeTableRenderingContext ttrc = TreeTableRenderingContext.getInstance();
      UIXTreeTable tree = ttrc.getUIXTreeTable();
      ResponseWriter out = context.getResponseWriter();
      out.startElement("a", tree);
      out.writeURIAttribute("href", "#" , null);
      // put style classes on the links
      if (isLastChild)
      {
        renderStyleClass(
          context, rc,
          SkinSelectors.AF_TREE_TABLE_MP_SELECTED_STYLE_CLASS);
      }
      else
      {
        renderStyleClass(
          context, rc,
          SkinSelectors.AF_TREE_TABLE_MP_STEP_STYLE_CLASS);
      }
      Object oldPath = tree.getRowKey();
      Object focusRowKey = ttrc.getFocusRowKey();
      List<Object> focusPath = tree.getAllAncestorContainerRowKeys(focusRowKey);
      Object crumbPath = (renderedCount < focusPath.size())
        ? focusPath.get(renderedCount)
        : focusRowKey;
      try
      {
        tree.setRowKey(crumbPath);
        String onclick =
          TreeUtils.callJSFocusNode(tree, ttrc.getJSVarName());
        out.writeAttribute("onclick", onclick , null);
        super.renderLink(context, rc, child, renderedCount, isLastChild);
      }
      finally
      {
        tree.setRowKey(oldPath);
      }
      out.endElement("a");
    }

    @Override
    protected boolean hasChildren(
      UIComponent component)
    {
      return false; // do not render the columns. only the pathStamp
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      // the ID is rendered by TreeTableRenderer and not org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.BreadCrumbsRenderer:
      // bug 4587950:
      return false;
    }

    @Override
    protected UIComponent getStamp(
      FacesContext     context,
      RenderingContext rc,
      UIXHierarchy     component,
      FacesBean        bean
      )
    {
      UIComponent stamp = component.getFacet("pathStamp");
      return stamp;
    }
  }

  @Override
  protected Map<String, String> createResourceKeyMap()
  {
    Map<String, String> tablemap = super.createResourceKeyMap();
    Map<String, String> map =
      ResourceKeyUtils.convertResourceKeyMap(tablemap, "table", "treeTable");

    // we need a resource key map since we are using a navigationPath.
    // and we are using table for the styles
    map.put(SkinSelectors.AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME,
            SkinSelectors.AF_TREE_TABLE_MP_SEPARATOR_ICON_NAME);
    map.put(SkinSelectors.AF_NAVIGATION_PATH_STYLE_CLASS,
            SkinSelectors.AF_TREE_TABLE_MP_STYLE_CLASS);
    map.put(SkinSelectors.AF_TABLE_CONTENT_STYLE,
            SkinSelectors.AF_TREE_TABLE_CONTENT_STYLE);
    map.put(SkinSelectors.AF_TABLE_SUB_CONTROL_BAR_STYLE,
            SkinSelectors.AF_TREE_TABLE_SUB_CONTROL_BAR_STYLE);
    map.put(SkinSelectors.AF_TABLE_CONTROL_BAR_TOP_STYLE,
            SkinSelectors.AF_TREE_TABLE_CONTROL_BAR_TOP_STYLE);
    map.put(SkinSelectors.AF_TABLE_CONTROL_BAR_BOTTOM_STYLE,
            SkinSelectors.AF_TREE_TABLE_CONTROL_BAR_BOTTOM_STYLE);

    return Collections.unmodifiableMap(map);
  }

  // translation keys
  private static final String _EXPAND_ALL_TEXT_KEY   =
    "af_treeTable.EXPAND_ALL";
  private static final String _COLLAPSE_ALL_TEXT_KEY =
    "af_treeTable.COLLAPSE_ALL";
  private static final String _BREADCRUMBS_START_KEY =
    "af_treeTable.BREADCRUMB_START_TIP";

  private PropertyKey _immediateKey;
  private PropertyKey _expandAllEnabledKey;
  private PropertyKey _rootNodeRendered;

  private static final Object _JS_LIBS_KEY = new Object();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeTableRenderer.class);
}
