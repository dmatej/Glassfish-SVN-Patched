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

package org.apache.myfaces.trinidadinternal.renderkit.htmlBasic;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.ActionEvent;

import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.AutoSubmitUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

public class HtmlCommandButtonRenderer extends Renderer
{
  public HtmlCommandButtonRenderer()
  {
  }

  @SuppressWarnings("unchecked")
  @Override
  public void encodeBegin(FacesContext context, 
                          UIComponent component)
    throws IOException
  {
    if (!component.isRendered())
    {
      return;
    }

    Map<String, Object> attrs = component.getAttributes();
    UICommand command = (UICommand) component;
    // Which button type (SUBMIT, RESET, or BUTTON) should we generate?
    String type = CoreRenderer.toString(attrs.get("type"));
    if (type == null)
    {
      type = "submit";
    }

    ResponseWriter writer = context.getResponseWriter();

    String label = CoreRenderer.toString(command.getValue());

    String imageSrc = CoreRenderer.toResourceUri(context, attrs.get("image"));
    writer.startElement("input", component);
    String id = component.getClientId(context);
    writer.writeAttribute("id", id, "id");
    writer.writeAttribute("name", id, null);
    boolean isImage = (imageSrc != null);
    if (isImage)
    {
      imageSrc = context.getExternalContext().encodeResourceURL(imageSrc);
      writer.writeAttribute("type", "image", "type");
      writer.writeURIAttribute("src", imageSrc, "image");
    }
    else
    {
      writer.writeAttribute("type", type.toLowerCase(), "type");
      writer.writeAttribute("value", label, "value");
    }

    RenderingContext arc = RenderingContext.getCurrentInstance();
    String script;
    // If it's an image, we can't really go through the full-page submit
    // code - we need to preserve the x/y coordinates.  However,
    // we do need the "source" parameter to be set
    if (isImage)
    {
      script =  "this.form.source.value='" + id + "';";
      arc.getFormData().addNeededValue("source");
    }
    else
    {
      script = AutoSubmitUtils.getFullPageSubmitScript(
              arc, id, command.isImmediate(),
              null/*no event*/,
              null,
              false/* return false*/);
    }

    String onclick = CoreRenderer.toString(attrs.get("onclick"));
    script = XhtmlUtils.getChainedJS(onclick, script, true);

    writer.writeAttribute("onclick", script, null);
    _writePassThruAttrs(writer, attrs, _passthruAttributes);
    _writeBooleanPassThruAttr(writer, attrs, "disabled");
    _writeBooleanPassThruAttr(writer, attrs, "ismap");


    Object styleClass = attrs.get("styleClass");
    if (styleClass != null)
    {
      writer.writeAttribute("class", styleClass, "styleClass");
    }

    writer.endElement("input");
  }

  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Object source =
      context.getExternalContext().getRequestParameterMap().get("source");

    String clientId = component.getClientId(context);
    if ((source != null) &&
        source.equals(clientId))
    {
      (new ActionEvent(component)).queue();
    }
  }

  private void _writeBooleanPassThruAttr(
      ResponseWriter out,
      Map<String, Object> attrs,
      String key)
    throws IOException
  {
    Object value = attrs.get(key);
    if (Boolean.TRUE.equals(value))
    {
      out.writeAttribute(key, key, key);
    }
  }

  private void _writePassThruAttrs(
      ResponseWriter out,
      Map<String, Object> attrs,
      String[] keys)
    throws IOException
  {
    for(int i=0; i<keys.length; i++)
    {
      String key = keys[i];
      Object value = attrs.get(key);
      if (value != null)
      {
        out.writeAttribute(key, value, key);
      }
    }
  }

  private static final String[] _passthruAttributes = {
    "accesskey",
    "alt",
    "dir",
    "lang",
    "onblur",
    "onchange",
    "ondblclick",
    "onfocus",
    "onkeydown",
    "onkeypress",
    "onkeyup",
    "onmousedown",
    "onmousemove",
    "onmouseout",
    "onmouseover",
    "onmouseup",
    "onselect",
    "style",
    "tabindex",
    "title",
  };
}
