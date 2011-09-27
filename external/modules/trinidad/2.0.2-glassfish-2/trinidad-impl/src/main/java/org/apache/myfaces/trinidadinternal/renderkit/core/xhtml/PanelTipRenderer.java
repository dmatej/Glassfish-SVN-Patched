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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelTip;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 * Renders the page or section level tip UI element.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/TipRenderer.java#0 $) $Date: 10-nov-2005.18:56:21 $
 */
public class PanelTipRenderer extends XhtmlRenderer
{
  public PanelTipRenderer()
  {
    super(CorePanelTip.TYPE);
  }


  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|panelTip";
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

     // tip label
    rw.startElement("span", null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_TIP_LABEL_STYLE_CLASS);

    String tipText = rc.getTranslatedString("af_panelTip.TIP");
    if (rc.isRightToLeft())
    {
      rw.writeText(XhtmlConstants.NBSP_STRING, null);
      rw.writeText(tipText, null);
    }
    else
    {
      rw.writeText(tipText, null);
      rw.writeText(XhtmlConstants.NBSP_STRING, null);
    }

    rw.endElement("span");

    // content
    rw.startElement("span", null);
    renderStyleClass(context, rc,  SkinSelectors.AF_PANEL_TIP_CONTENT_STYLE_CLASS);

    encodeAllChildren(context, component);

    rw.endElement("span");

    rw.endElement("div");
  }
}
