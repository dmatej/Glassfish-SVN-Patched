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
import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import javax.faces.convert.Converter;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;

public class FastRenderer extends Renderer
{
  @Override
  public void encodeBegin(FacesContext context,
                          UIComponent comp) throws IOException
  {
    RequestContext.getCurrentInstance();
    CoreOutputText cot = (CoreOutputText) comp;
    if (cot.isEscape())
    {
      ResponseWriter rw = context.getResponseWriter();
      rw.startElement("span", comp);
      
      rw.writeAttribute("id", cot.getId(), "id");
      rw.writeAttribute("title", cot.getShortDesc(), "title");
      rw.writeAttribute("class", cot.getStyleClass(), "styleClass");
      rw.writeAttribute("style", cot.getInlineStyle(), "inlineStyle");
      rw.writeAttribute("onclick", cot.getOnclick(),  null);
      rw.writeAttribute("ondblclick", cot.getOndblclick(),  null);
      rw.writeAttribute("onkeydown", cot.getOnkeydown(),  null);
      rw.writeAttribute("onkeyup", cot.getOnkeyup(),  null);
      rw.writeAttribute("onkeypress", cot.getOnkeypress(),  null);
      rw.writeAttribute("onmousedown", cot.getOnmousedown(),  null);
      rw.writeAttribute("onmousemove", cot.getOnmousemove(),  null);
      rw.writeAttribute("onmouseout", cot.getOnmouseout(),  null);
      rw.writeAttribute("onmouseover", cot.getOnmouseover(),  null);
      rw.writeAttribute("onmouseup", cot.getOnmouseup(),  null);
    }
  }

  @Override
  public void encodeEnd(FacesContext context,
                        UIComponent comp) throws IOException
  {
    RequestContext.getCurrentInstance();  
    CoreOutputText cot = (CoreOutputText) comp;
    ResponseWriter rw = context.getResponseWriter();

    Object value = cot.getValue();
    if (!cot.isEscape())
    {
      if (value != null)
      {
        rw.write(value.toString());
      }
    }
    else
    {
      if (value != null)
      {
        String valueStr = value.toString();
        int truncateAt = cot.getTruncateAt();
        if (truncateAt > 0)
        {
          if (truncateAt < 13)
            truncateAt = 13;
          if (valueStr.length() > truncateAt)
            valueStr = valueStr.substring(0, truncateAt);
        }

        rw.writeText(valueStr, "value");
      }

      String description = cot.getDescription();
      if (description != null)
      {
        rw.startElement("span", null);
        rw.writeAttribute("class", "magicClass", null);
        rw.writeText(description, "description");
        rw.endElement("span");
      }
      
      rw.endElement("span");
    }

  }


  protected Object getConvertedValue(
    FacesContext    context,
    CoreOutputText  cot)
  {
    Object value = cot.getValue();
    if (value == null)
      return null;

    Converter converter = cot.getConverter();
    if ((converter == null) && !(value instanceof String))
    {
      converter = getConverterByType(context, cot);
    }

    if (converter != null)
    {
      return converter.getAsString(context, cot, value);
    }

    return value;
  }

  protected Converter getConverterByType(
    FacesContext   context,
    CoreOutputText cot)
  {
    ValueBinding binding = cot.getFacesBean().getValueBinding(_valueKey);
    if (binding == null)
      return null;

    Class<?> type = binding.getType(context);
    return ConverterUtils.createConverter(context, type);
  }

  private PropertyKey _valueKey = CoreOutputText.VALUE_KEY;
}
