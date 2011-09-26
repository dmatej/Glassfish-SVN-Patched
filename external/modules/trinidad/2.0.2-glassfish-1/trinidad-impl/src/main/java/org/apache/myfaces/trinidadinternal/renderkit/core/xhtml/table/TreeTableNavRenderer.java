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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.component.core.data.CoreTreeTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.share.util.FastMessageFormat;


public class TreeTableNavRenderer extends XhtmlRenderer
{
  public TreeTableNavRenderer(
    boolean isTop)
  {
    super(CoreTreeTable.TYPE);
    _isTop = isTop;
  }

  @Override
  public boolean getRendersChildren()
  {
    // we don't take any children. however, return true so that
    // encodeAll is called:
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    //TODO
    final Object childTypeText = null;

    TreeTableRenderingContext ttrc = TreeTableRenderingContext.getInstance();
    UIXTreeTable hgrid = ttrc.getUIXTreeTable();
    int rows = hgrid.getRows();
    int first = hgrid.getFirst();
    int rowCount = hgrid.getRowCount();
    _renderViewNavBar(context, rc, ttrc, hgrid,
                      first, childTypeText, _isTop, rows, rowCount);
  }

  private void _renderViewNavBar(
     FacesContext              context,
     RenderingContext          rc,
     TreeTableRenderingContext ttrc,
     UIXTreeTable              hgrid,
     int                       rangeStart,
     Object                    text,
     boolean                   isTop,
     int                       viewSize,
     int                       numChildren
    ) throws IOException
  {
    int nextWindowStart = 0;
    int nextWindowEnd = 0;
    boolean incomplete = (numChildren == _INCOMPLETE);
    boolean haveText   = (text != null);
    boolean disabled = true;

    if (isTop)
    {
      if (rangeStart > 0)
      {
        disabled = false;
        nextWindowStart = rangeStart - viewSize;
        if (nextWindowStart < 0)
          nextWindowStart = 0;
        nextWindowEnd = rangeStart;
      }
    }
    else
    {
      if (incomplete)
      {
        disabled = false;
      }
      else
      {
        nextWindowStart = rangeStart + viewSize;
        if (nextWindowStart < numChildren)
        {
          disabled = false;
          nextWindowEnd = nextWindowStart + viewSize;
          if (nextWindowEnd > numChildren)
            nextWindowEnd = numChildren;
        }
      }
    }

    // First get the link direction text
    String direction = rc.getTranslatedString((isTop
                                                ? _PREVIOUS_KEY
                                                : _NEXT_KEY));
    String linkText = direction;
    if (!disabled)
    {
      // now format the actual link text
      Object[] srcs  = new Object[4];
      String formatKey;
      int strNum = 0;
      if (haveText)
      {
        // Always start with title if present
        srcs[strNum++] = text;
      }

      // next is always the direction.
      srcs[strNum++] = direction;

      if (incomplete)
      {
        formatKey = (haveText ? _FORMAT_KEY_TNC : _FORMAT_KEY_NTNC);
        // we can't know the range or max record number, so just give view size
        srcs[strNum] = "" + viewSize;
      }
      else
      {
        formatKey = (haveText ? _FORMAT_KEY_TC : _FORMAT_KEY_NTC);
        // build the range text.
        // note that nextWindowStart must be incremented because the text
        // is based at one:
        srcs[strNum++] = (nextWindowStart+1) + " - " + nextWindowEnd;
        // and end with the maximum record number
        srcs[strNum] = "" + numChildren;
      }

      String format = rc.getTranslatedString(formatKey);
      if (format != null)
      {
        FastMessageFormat fmf = new FastMessageFormat(format);
        linkText = fmf.format(srcs);
      }
    }

    _writeCellContents(context, rc, ttrc, hgrid, linkText, disabled, isTop, nextWindowStart);
  }


  private void _writeCellContents(
    FacesContext              context,
    RenderingContext          rc,
    TreeTableRenderingContext ttrc,
    UIXTreeTable              hgrid,
    String                    text,
    boolean                   disabled,
    boolean                   isTop,
    int                       index
    ) throws IOException
  {
    if (text == null)
      return;

    ResponseWriter writer = context.getResponseWriter();

    String onclick =
      TreeUtils.callJSGotoNode(hgrid, ttrc.getJSVarName(), index);

    boolean isRTL = rc.isRightToLeft();
    int depth = hgrid.getDepth()+1;
    int spacerWidth = _getSpacerWidth();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.startElement("div", null);

    if (isRTL)
    {
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                         "margin-right:" +
                         depth * spacerWidth + "px", null);
    }
    else
    {
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                         "position:relative;top:0px;left:0px;margin-left:"+
                         depth * spacerWidth + "px", null);
    }

    Icon icon = _getIcon(rc, isTop, disabled);

    if (icon != null)
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      String shortDesc =
        rc.getTranslatedString(isTop ? _PREVIOUS_ALT_KEY : _NEXT_ALT_KEY);

      if (!disabled)
      {
        writer.writeURIAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
        writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);

        writer.writeAttribute("title", shortDesc, null);
      }

        String align = OutputUtils.getMiddleIconAlignment(rc);
        OutputUtils.renderIcon(context, rc, icon, shortDesc, align);

        writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }

    writer.startElement(XhtmlConstants.LINK_ELEMENT, null);

    final String styleClass;
    if (disabled)
    {
      styleClass = SkinSelectors.HGRID_NAV_ROW_ILINK_STYLE_CLASS;
    }
    else
    {
      styleClass = SkinSelectors.HGRID_NAV_ROW_ALINK_STYLE_CLASS;
      writer.writeURIAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      String shortDesc =
        rc.getTranslatedString(isTop ? _PREVIOUS_ALT_KEY : _NEXT_ALT_KEY);
      writer.writeAttribute("title", shortDesc, null);
    }

    renderStyleClass(context, rc, styleClass);
    if (text != null)
      writer.writeText(text, null);
    writer.endElement(XhtmlConstants.LINK_ELEMENT);

    writer.endElement("div");
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }

  private Icon _getIcon(
    RenderingContext rc,
    boolean          isTop,
    boolean          isDisabled
   )
  {
    final String iconKey;

    if (isTop)
    {
      iconKey = isDisabled
        ? SkinSelectors.AF_TREE_TABLE_DISABLED_NAV_UP_ICON_NAME
        : SkinSelectors.AF_TREE_TABLE_NAV_UP_ICON_NAME;
    }
    else
    {
      iconKey = isDisabled
        ? SkinSelectors.AF_TREE_TABLE_DISABLED_NAV_DOWN_ICON_NAME
        : SkinSelectors.AF_TREE_TABLE_NAV_DOWN_ICON_NAME;
    }

    return rc.getIcon(iconKey);
  }

  private int _getSpacerWidth()
  {
    return 18;
    //TODO
//    return Integer.parseInt(HideShowUtils.getIconWidth());
  }


//TODO
//  protected String getCellClass(
//    TableRenderingContext context,
//    boolean isSelect)
//  {
//    return HGRID_NAV_CELL_STYLE_CLASS;
//  }

  private final boolean _isTop;

  private static final String _PREVIOUS_ALT_KEY = "af_treeTable.PREVIOUS_TIP";
  private static final String _NEXT_ALT_KEY     = "af_treeTable.NEXT_TIP";
  private static final int _INCOMPLETE = XhtmlConstants.INCOMPLETE_DATA_SET;
  private static final String _PREVIOUS_KEY =
    "af_treeTable.DISABLED_PREVIOUS";
  private static final String _NEXT_KEY =
    "af_treeTable.DISABLED_NEXT";
  // private translation keys -- those that should not be customized
  private static final String _FORMAT_KEY_TC   =
    "af_treeTable.NAV_CELL_FORMAT_T_C_private";
  private static final String _FORMAT_KEY_NTC  =
    "af_treeTable.NAV_CELL_FORMAT_NT_C_private";
  private static final String _FORMAT_KEY_TNC  =
    "af_treeTable.NAV_CELL_FORMAT_T_NC_private";
  private static final String _FORMAT_KEY_NTNC =
    "af_treeTable.NAV_CELL_FORMAT_NT_NC_private";

}
