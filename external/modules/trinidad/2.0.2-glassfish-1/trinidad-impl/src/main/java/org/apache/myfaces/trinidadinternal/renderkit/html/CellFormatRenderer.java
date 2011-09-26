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
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


public class CellFormatRenderer extends XhtmlRenderer
{
  public CellFormatRenderer()
  {
    super(HtmlCellFormat.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _widthKey = type.findKey("width");
    _heightKey = type.findKey("height");
    _halignKey = type.findKey("halign");
    _valignKey = type.findKey("valign");
    _shortTextKey = type.findKey("shortText");
    _columnSpanKey = type.findKey("columnSpan");
    _rowSpanKey = type.findKey("rowSpan");
    _wrappingDisabledKey = type.findKey("wrappingDisabled");
    _headersKey = type.findKey("headers");
    _headerKey = type.findKey("header");
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
    String element = isHeader(component, bean) ? "th" : "td";

    rw.startElement(element, component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);
    renderHAlign(context, rc, getHalign(component, bean));
    rw.writeAttribute("valign", getValign(component, bean), "valign");
    rw.writeAttribute("abbr", getShortText(component, bean), "shortText");
    rw.writeAttribute("headers", getHeaders(component, bean), "headers");
    rw.writeAttribute("width", getWidth(component, bean), "width");
    rw.writeAttribute("height", getHeight(component, bean), "height");

    int colspan = getColumnSpan(component, bean);
    if (colspan > 1)
      rw.writeAttribute("colspan", colspan, "columnSpan");
    int rowspan = getRowSpan(component, bean);
    if (rowspan > 1)
      rw.writeAttribute("rowspan", rowspan, "rowSpan");
    if (isWrappingDisabled(component, bean))
    {
      // On PDA browser where the width is limited, nowrap will not be set.
      if (isDesktop(rc))
      {
        rw.writeAttribute("nowrap", Boolean.TRUE, "wrappingDisabled");
      }
    }

    encodeAllChildren(context, component);

    rw.endElement(element);
  }

  protected Object getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_widthKey);
  }

  protected Object getHeight(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_heightKey);
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

  protected boolean isWrappingDisabled(
    UIComponent component,
    FacesBean   bean)
  {
    return Boolean.TRUE.equals(bean.getProperty(_wrappingDisabledKey));
  }

  protected Object getShortText(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_shortTextKey);
  }

  protected int getColumnSpan(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_columnSpanKey);
    if (o == null)
      return 1;

    return toInt(o);
  }

  protected int getRowSpan(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_rowSpanKey);
    if (o == null)
      return 1;

    return toInt(o);
  }

  protected Object getHeaders(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_headersKey);
  }

  protected boolean isHeader(
    UIComponent component,
    FacesBean   bean)
  {
    return Boolean.TRUE.equals(bean.getProperty(_headerKey));
  }

  private PropertyKey _widthKey;
  private PropertyKey _heightKey;
  private PropertyKey _halignKey;
  private PropertyKey _valignKey;
  private PropertyKey _shortTextKey;
  private PropertyKey _columnSpanKey;
  private PropertyKey _rowSpanKey;
  private PropertyKey _wrappingDisabledKey;
  private PropertyKey _headersKey;
  private PropertyKey _headerKey;
}
