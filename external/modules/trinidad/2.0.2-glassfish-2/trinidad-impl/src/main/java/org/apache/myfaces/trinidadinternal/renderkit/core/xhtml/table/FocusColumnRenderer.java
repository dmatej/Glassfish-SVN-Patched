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
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;


public class FocusColumnRenderer extends SpecialColumnRenderer
{
  @Override
  protected String getHeaderText(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    return arc.getTranslatedString("af_treeTable.FOCUS_COLUMN_HEADER");
  }

  @Override
  protected String getHeaderStyleClass(
    TableRenderingContext tContext)
  {
    return SkinSelectors.AF_COLUMN_HEADER_ICON_STYLE;
  }

  @Override
  protected String getFormatType(
    UIComponent component,
    FacesBean   bean)
  {
    return CoreColumn.ALIGN_CENTER;
  }

  @Override
  protected void renderKids(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext trc,
    UIComponent           column
    ) throws IOException
  {
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) trc;

    ResponseWriter writer = context.getResponseWriter();
    UIXTreeTable hGrid = ttrc.getUIXTreeTable();

    Object focusRowKey = hGrid.getFocusRowKey();
    assert focusRowKey != null;
    int focusPathSize = hGrid.getDepth(focusRowKey) + 1;

    // do not render a focus icon if the node is not expandable
    // do not render the focus icon if this is the first row
    if (hGrid.isContainer() &&
        (hGrid.getDepth() >= focusPathSize))
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);

      if (supportsNavigation(rc))
      {
        String onclick =
          TreeUtils.callJSFocusNode(hGrid, ttrc.getJSVarName());
        writer.writeURIAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
        writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      }

      _renderFocusIcon(context, rc, rc.getTranslatedString("af_treeTable.FOCUS_TIP"));

      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }
  }

  // Renders the focus icon
  private void _renderFocusIcon(
    FacesContext     facesContext,
    RenderingContext rc,
    String           altText
    ) throws IOException
  {
    Icon icon = rc.getIcon(SkinSelectors.AF_TREE_TABLE_FOCUS_ICON_NAME);

    if (icon != null)
    {
      // Render focus icon with embedded=true, since
      // focus icon is always rendered within its own link.
      OutputUtils.renderIcon(facesContext, rc, icon, altText, null, true);
    }
  }
}
