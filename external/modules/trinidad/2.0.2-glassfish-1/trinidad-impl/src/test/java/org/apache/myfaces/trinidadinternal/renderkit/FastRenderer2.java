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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.el.ValueBinding;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;


public class FastRenderer2 extends Renderer
{
  public FastRenderer2()
  {
    FacesBean.Type type = CoreOutputText.TYPE;
    _shortDescKey = type.findKey("shortDesc");
    _styleClassKey = type.findKey("styleClass");
    _inlineStyleKey = type.findKey("inlineStyle");
    _onclickKey = type.findKey("onclick");
    _ondblclickKey = type.findKey("ondblclick");
    _onkeydownKey = type.findKey("onkeydown");
    _onkeyupKey = type.findKey("onkeyup");
    _onkeypressKey = type.findKey("onkeypress");
    _onmousedownKey = type.findKey("onmousedown");
    _onmousemoveKey = type.findKey("onmousemove");
    _onmouseoutKey = type.findKey("onmouseout");
    _onmouseoverKey = type.findKey("onmouseover");
    _onmouseupKey = type.findKey("onmouseup");
    _truncateAtKey = type.findKey("truncateAt");
    _escapeKey = type.findKey("escape");
    _descriptionKey = type.findKey("description");
    _converterKey = type.findKey("converter");
    _valueKey = type.findKey("value");
  }

  @Override
  public void encodeBegin(
    FacesContext context,
    UIComponent  comp
    ) throws IOException
  {
    RequestContext.getCurrentInstance();
    FacesBean bean = ((UIXComponent) comp).getFacesBean();
    if (getEscape(comp, bean))
    {
      ResponseWriter rw = context.getResponseWriter();
      rw.startElement("span", comp);

      rw.writeAttribute("id", comp.getId(), "id");
      rw.writeAttribute("title", getShortDesc(comp, bean), "shortDesc");
      rw.writeAttribute("class", getStyleClass(comp, bean), "styleClass");
      rw.writeAttribute("style", getInlineStyle(comp, bean), "inlineStyle");
      rw.writeAttribute("onclick", getOnclick(comp, bean),  null);
      rw.writeAttribute("ondblclick", getOndblclick(comp, bean),  null);
      rw.writeAttribute("onkeydown", getOnkeydown(comp, bean),  null);
      rw.writeAttribute("onkeyup", getOnkeyup(comp, bean),  null);
      rw.writeAttribute("onkeypress", getOnkeypress(comp, bean),  null);
      rw.writeAttribute("onmousedown", getOnmousedown(comp, bean),  null);
      rw.writeAttribute("onmousemove", getOnmousemove(comp, bean),  null);
      rw.writeAttribute("onmouseout", getOnmouseout(comp, bean),  null);
      rw.writeAttribute("onmouseover", getOnmouseover(comp, bean),  null);
      rw.writeAttribute("onmouseup", getOnmouseup(comp, bean),  null);
    }
  }

  @Override
  public void encodeEnd(
    FacesContext context,
    UIComponent  comp
    ) throws IOException
  {
    RequestContext.getCurrentInstance();
    ResponseWriter rw = context.getResponseWriter();
    FacesBean bean = ((UIXComponent) comp).getFacesBean();
    Object value = getConvertedValue(context, comp, bean);

    if (getEscape(comp, bean))
    {
      if (value != null)
      {
        int truncateAt = getTruncateAt(comp, bean);
        if (truncateAt > 0)
        {
          String valueStr = value.toString();
          if (truncateAt < 13)
            truncateAt = 13;
          if (valueStr.length() > truncateAt)
            value = valueStr.substring(0, truncateAt);
        }

        rw.writeText(value, "value");
      }

      Object description = getDescription(comp, bean);
      if (description != null)
      {
        rw.startElement("span", null);
        rw.writeAttribute("class", "magicClass", null);
        rw.writeText(description, "description");
        rw.endElement("span");
      }

      rw.endElement("span");
    }
    else
    {
      if (value != null)
        rw.write(value.toString());
    }
  }

  protected Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    Object value = getValue(component, bean);
    if (value == null)
      return null;

    Converter converter = getConverter(component, bean);
    if ((converter == null) && !(value instanceof String))
    {
      converter = getConverterByType(context, bean);
    }

    if (converter != null)
    {
      return converter.getAsString(context, component, value);
    }

    return value;
  }

  protected Converter getConverterByType(
    FacesContext context,
    FacesBean    bean)
  {
    ValueBinding binding = bean.getValueBinding(_valueKey);
    if (binding == null)
      return null;

    Class<?> type = binding.getType(context);
    return ConverterUtils.createConverter(context, type);
  }

  protected boolean getEscape(
    UIComponent  component,
    FacesBean    bean)
  {
    Object o = bean.getProperty(_escapeKey);
    if (o == null)
      o = _escapeKey.getDefault();
    assert(o != null);
    return !Boolean.FALSE.equals(o);
  }

  protected Object getShortDesc(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_shortDescKey);
  }

  protected Object getStyleClass(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_styleClassKey);
  }

  protected Object getInlineStyle(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_inlineStyleKey);
  }

  protected Object getOnclick(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onclickKey);
  }

  protected Object getOndblclick(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_ondblclickKey);
  }

  protected Object getOnkeydown(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onkeydownKey);
  }

  protected Object getOnkeyup(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onkeyupKey);
  }

  protected Object getOnkeypress(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onkeypressKey);
  }

  protected Object getOnmousedown(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onmousedownKey);
  }

  protected Object getOnmousemove(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onmousemoveKey);
  }

  protected Object getOnmouseout(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onmouseoutKey);
  }

  protected Object getOnmouseover(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onmouseoverKey);
  }

  protected Object getOnmouseup(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_onmouseupKey);
  }

  protected int getTruncateAt(
    UIComponent  component,
    FacesBean    bean)
  {
    Object o = bean.getProperty(_truncateAtKey);
    if (o == null)
      o = _truncateAtKey.getDefault();

    assert(o != null);
    return ((Number) o).intValue();
  }

  protected Object getDescription(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_descriptionKey);
  }

  protected Object getValue(
    UIComponent  component,
    FacesBean    bean)
  {
    return bean.getProperty(_valueKey);
  }

  protected Converter getConverter(
    UIComponent  component,
    FacesBean    bean)
  {
    return (Converter) bean.getProperty(_converterKey);
  }

  private PropertyKey _shortDescKey;
  private PropertyKey _styleClassKey;
  private PropertyKey _inlineStyleKey;
  private PropertyKey _onclickKey;
  private PropertyKey _ondblclickKey;
  private PropertyKey _onkeydownKey;
  private PropertyKey _onkeyupKey;
  private PropertyKey _onkeypressKey;
  private PropertyKey _onmousedownKey;
  private PropertyKey _onmousemoveKey;
  private PropertyKey _onmouseoutKey;
  private PropertyKey _onmouseoverKey;
  private PropertyKey _onmouseupKey;
  private PropertyKey _truncateAtKey;
  private PropertyKey _escapeKey;
  private PropertyKey _descriptionKey;
  private PropertyKey _valueKey;
  private PropertyKey _converterKey;
}
