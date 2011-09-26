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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelHorizontalLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class PanelHorizontalLayoutRenderer extends XhtmlRenderer
{
  public PanelHorizontalLayoutRenderer()
  {
    this(CorePanelHorizontalLayout.TYPE);
  }

  protected PanelHorizontalLayoutRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _valignKey = type.findKey("valign");
    _halignKey = type.findKey("halign");
  }

  @Override
  public boolean getRendersChildren()
  {
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
    ResponseWriter rw = context.getResponseWriter();

    Object valign = getValign(component, bean);
    Object halign = getHalign(component, bean);

    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    if (CorePanelHorizontalLayout.HALIGN_CENTER.equals(halign))
      rw.writeAttribute("align", "center", "halign");

    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    rw.startElement("tr", null);

    _encodeChildren(context, rc, component, valign, halign);

    rw.endElement("tr");
    rw.endElement("table");
  }

  protected Object getValign(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_valignKey);
  }

  protected Object getHalign(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_halignKey);
  }

  /**
   * Render all the children of the PanelGroup
   */
  @SuppressWarnings("unchecked")
  private void _encodeChildren(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    Object           vAlign,
    Object           hAlign
    ) throws IOException
  {

    UIComponent separator = getFacet(component,
                                     CorePanelHorizontalLayout.SEPARATOR_FACET);

    boolean needSeparator = false;
    boolean isFirstChild = true;
    /**
     * To fix the overlapping of the separator with first child,
     * ignore endAligment(<td width = 100%>) in PDA
     */
    boolean isEndAlignment;
    if (isPDA(rc))
      isEndAlignment = false;
    else if (CorePanelHorizontalLayout.HALIGN_END.equals(hAlign))
      isEndAlignment = true;
    else if (CorePanelHorizontalLayout.HALIGN_LEFT.equals(hAlign))
      isEndAlignment = rc.isRightToLeft();
    else if (CorePanelHorizontalLayout.HALIGN_RIGHT.equals(hAlign))
      isEndAlignment = !rc.isRightToLeft();
    else
      isEndAlignment = false;

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (!child.isRendered())
        continue;

      if (isFirstChild)
      {
        isFirstChild = false;
        if (isEndAlignment)
        {
          ResponseWriter rw = context.getResponseWriter();
          rw.startElement("td", null);
          rw.writeAttribute("width", "100%", null);
          rw.endElement("td");
        }
      }

      if (needSeparator)
      {
        encodeSeparator(context, separator, vAlign);
      }

      encodeChild(context, child, vAlign);
      needSeparator = true;
    }
  }

  /**
   * Render a single child (or the separator facet)
   */
  protected void encodeChild(
    FacesContext context,
    UIComponent  child,
    Object       vAlign
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);
    rw.writeAttribute("valign", vAlign, null);

    encodeChild(context, child);

    rw.endElement("td");
  }

  /**
   * Render a separator
   */
  protected void encodeSeparator(
    FacesContext context,
    UIComponent  separator,
    Object       vAlign
    ) throws IOException
  {
    if (separator != null)
      encodeChild(context, separator, vAlign);
  }

  private PropertyKey _valignKey;
  private PropertyKey _halignKey;
}
