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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


public class TreeNodeColumnRenderer extends ColumnRenderer
{
  @Override
  protected void renderKids(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column
    ) throws IOException
  {
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) tContext;
    boolean isRTL = rc.isRightToLeft();
    UIXTreeTable hGrid = ttrc.getUIXTreeTable();
    final boolean disclosed;
    final String onclick;
    if (hGrid.isContainer())
    {
      RowKeySet treeState = hGrid.getDisclosedRowKeys();
      String jsVarName = ttrc.getJSVarName();
      if (treeState.isContained())
      {
        disclosed = true;
        onclick = TreeUtils.callJSExpandNode(hGrid, jsVarName, false);
      }
      else
      {
        disclosed = false;
        onclick = TreeUtils.callJSExpandNode(hGrid, jsVarName, true);
      }
    }
    else // not a row container
    {
      disclosed = false;
      onclick = null;
    }

    int focusPath = hGrid.getDepth(TreeUtils.getFocusRowKey(hGrid));
    int depth = hGrid.getDepth() + 1 - focusPath;
    if (!ttrc.isRootNodeRendered())
    {
      // decrease the depth by one if the root node is not rendered to avoid
      // unnecessary indentation
      --depth;
    }

    int spacerWidth = _getSpacerWidth(ttrc);

    ResponseWriter writer = context.getResponseWriter();

    // In DOM
    // browsers we use margin-left, and have a floating div to displace the
    // arrow. See bug 2296869 for the reason why this approach is
    // preferred.
    writer.startElement("div", null);

    if (isRTL)
    {
      if (onclick != null)
        depth--;

      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            "margin-right:" +
                            depth * spacerWidth + "px",
              null);
    }
    else
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
              "position:relative;top:0px;left:0px;margin-left:"+
                            depth*spacerWidth+"px",
              null);

    // check to see if this node is in expanded or collapsed state.
    if (onclick != null)
    {
      writer.startElement("a", null);

      _renderIconID(context, tContext);

      // Render the style class on the link, so that we can
      // disable the link's text decoration
      renderStyleClass(context, rc,
                       SkinSelectors.AF_TREE_TABLE_EXPANSION_ICON_STYLE_CLASS);
      //HKuhn - don't render onclick in printable mode
      if (XhtmlRenderer.supportsScripting(rc))
      {
        writer.writeAttribute("onclick", onclick, null);
        writer.writeURIAttribute("href", "#", null);
      }

      // Render the expand/collapse Icon
      _renderExpansionIcon(context, rc, disclosed, onclick);

      writer.endElement("a");
    }

    _renderNodeIcon(context, rc, hGrid, disclosed, onclick != null);

    UIComponent nodeStampColumn = ttrc.getTreeNodeStamp();
    // if in screen reader mode render the node depth from the root as well
    _renderNodeStampBasedOnAccessibilty(context, rc, ttrc, nodeStampColumn);

    writer.endElement("div");
  }

  private int _getSpacerWidth(
    TreeTableRenderingContext ttrc)
  {
    return ttrc.getSpacerWidth();
  }


  // Renders the unique id for the expand/collapse icon
  private void _renderIconID(
    FacesContext          fc,
    TableRenderingContext tContext
    ) throws IOException
  {
    // we need to render a unique ID for the expand/collapse link, so that
    // PPR can restore the focus correctly after a PPR request:
    String tableName = tContext.getTable().getContainerClientId(fc);
    String id = tableName + NamingContainer.SEPARATOR_CHAR + _ICON_ID;
    fc.getResponseWriter().writeAttribute("id", id, null);
  }

  // Renders the expansion Icon
  private void _renderExpansionIcon(
    FacesContext     context,
    RenderingContext rc,
    boolean          disclosed,
    Object           onclick
    ) throws IOException
  {
    final String iconName;
    final String altTextKey;

    if (disclosed)
    {
      iconName = SkinSelectors.AF_TREE_TABLE_EXPANDED_ICON_NAME;

      altTextKey = (onclick == null)
        ? _DISABLED_COLLAPSE_TIP_KEY
        : _COLLAPSE_TIP_KEY;
    }
    else
    {
      iconName = SkinSelectors.AF_TREE_TABLE_COLLAPSED_ICON_NAME;
      altTextKey = _EXPAND_TIP_KEY;
    }

    Icon icon = rc.getIcon(iconName);
    if (icon != null)
    {
      Object altText = rc.getTranslatedString(altTextKey);
      OutputUtils.renderIcon(context, rc, icon, altText, null);
    }
  }

  private void _renderNodeIcon(
    FacesContext     context,
    RenderingContext rc,
    UIXTreeTable     ttr,
    boolean          disclosed,
    boolean          hasChildren
    ) throws IOException
  {
    Icon nodeIcon = getNodeIcon(rc, getNodeType(ttr), disclosed, hasChildren);
    if (nodeIcon != null)
    {
      OutputUtils.renderIcon(context, rc, nodeIcon, null, null);
    }
  }

  protected String getNodeType(
    UIXTreeTable ttr
  )
  {
    String nodeType = null;
    Object rowData = ttr.getRowData();
    Class rowClass = rowData.getClass();
    Method method = null;

    try
    {
      method = rowClass.getMethod("getNodeType");
      if (method != null && method.getReturnType().equals(String.class))
      {
        nodeType = (String) method.invoke(rowData);
      }
    }
    catch (IllegalAccessException e)
    {
    }
    catch (NoSuchMethodException e)
    {
    }
    catch (InvocationTargetException e)
    {
    }
    return nodeType;
  }

  protected String getNodeIconSelector(
    String  nodeType,
    boolean disclosed,
    boolean hasChildren)
  {
    if (hasChildren)
    {
      if (disclosed)
      {
        nodeType += NODE_ICON_EXPANDED_SUFFIX;
      }
      else
      {
        nodeType += NODE_ICON_COLLAPSED_SUFFIX;
      }
    }
    return SkinSelectors.AF_TREE_TABLE_NODE_ICON + ":" + nodeType;
  }

  protected Icon getNodeIcon(
    RenderingContext rc,
    String           nodeType,
    boolean          disclosed,
    boolean          hasChildren)
  {
    if (nodeType == null || nodeType.length() == 0)
    {
      return null;
    }
    Icon icon = rc.getIcon(getNodeIconSelector(nodeType, disclosed, hasChildren));
    if (icon == null)
    {
      if (hasChildren)
      {
        icon = rc.getIcon(getNodeIconSelector(nodeType, disclosed, false));
      }
      else
      {
        icon = rc.getIcon(getNodeIconSelector(nodeType, false, true));
      }
    }
    return icon;
  }

  private void _renderNodeStampBasedOnAccessibilty(
    FacesContext              context,
    RenderingContext          rc,
    TreeTableRenderingContext ttrc,
    UIComponent               column
    ) throws IOException
  {
    if (XhtmlRenderer.isScreenReaderMode(rc))
    {
      int depth = ttrc.getUIXTreeTable().getDepth() + 1;
      if (rc.isRightToLeft())
      {
        super.renderKids(context, rc, ttrc, column);
        TreeUtils.writeNodeLevel(context, rc, depth, _NODE_LEVEL_TEXT_KEY);
      }
      else
      {
        TreeUtils.writeNodeLevel(context, rc, depth, _NODE_LEVEL_TEXT_KEY);
        super.renderKids(context, rc, ttrc, column);
      }
    }
    else
        super.renderKids(context, rc, ttrc, column);
  }

  // This String is included in the generated IDs that are
  // rendered for each expand/collapse icon.
  private static final String _ICON_ID = "hgi";

  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY =
    "af_treeTable.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY =
    "af_treeTable.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY =
    "af_treeTable.EXPAND_TIP";
  private static final String _NODE_LEVEL_TEXT_KEY =
    "af_treeTable.NODE_LEVEL";

  public static final String NODE_ICON_EXPANDED_SUFFIX = "-expanded";
  public static final String NODE_ICON_COLLAPSED_SUFFIX = "-collapsed";

  public static final int NODE_ICON_MAX_WIDTH = 18;
}
