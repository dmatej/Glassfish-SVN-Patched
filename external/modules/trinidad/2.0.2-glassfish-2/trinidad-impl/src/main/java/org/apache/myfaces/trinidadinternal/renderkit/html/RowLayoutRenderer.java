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
package org.apache.myfaces.trinidadinternal.renderkit.html;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlCellFormat;
import org.apache.myfaces.trinidad.component.html.HtmlRowLayout;
import org.apache.myfaces.trinidad.component.html.HtmlTableLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;


public class RowLayoutRenderer extends XhtmlRenderer
{
  public RowLayoutRenderer()
  {
    super(HtmlRowLayout.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _widthKey = type.findKey("width");
    _halignKey = type.findKey("halign");
    _valignKey = type.findKey("valign");
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

    boolean needsTable = _shouldRenderTable(component);
    rw.startElement(needsTable ? "table" : "tr", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);
    renderHAlign(context, rc, getHalign(component, bean));
    rw.writeAttribute("valign", getValign(component, bean), "valign");

    if (needsTable)
    {
      OutputUtils.renderLayoutTableAttributes(context, rc, 0, getWidth(component, bean));
      rw.startElement("tr", null);
    }

    encodeAllChildren(context, component);

    rw.endElement("tr");
    if (needsTable)
      rw.endElement("table");

  }

  static private boolean _shouldRenderTable(
    UIComponent component)
  {
    UIComponent parent = XhtmlUtils.getStructuralParent(component);
    return !(parent instanceof HtmlTableLayout);
  }

  @Override
  protected void encodeChild(
    FacesContext context,
    UIComponent  child
    ) throws IOException
  {
    // TODO: handle cellFormat that is inside of (for example) an iterator
    if (child instanceof HtmlCellFormat)
    {
      super.encodeChild(context, child);
    }
    else
    {
      ResponseWriter rw = context.getResponseWriter();
      rw.startElement("td", null);
      super.encodeChild(context, child);
      rw.endElement("td");
    }
  }

  protected Object getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_widthKey);
  }

  protected Object getHalign(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_halignKey);
  }

  protected Object getValign(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_valignKey);
  }

  private PropertyKey _widthKey;
  private PropertyKey _halignKey;
  private PropertyKey _valignKey;
}
