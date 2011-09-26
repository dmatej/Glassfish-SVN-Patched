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
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreSpacer;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class SpacerRenderer extends XhtmlRenderer
{
  public SpacerRenderer()
  {
    super(CoreSpacer.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _heightKey = type.findKey("height");
    _widthKey = type.findKey("width");
  }

  @Override
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    String id;
    if (shouldRenderId(context, comp))
      id = getClientId(context, comp);
    else
      id = null;

    String width = getWidth(comp, bean);
    String height = getHeight(comp, bean);

    if (width == null)
    {
      if (height == null)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("span", comp);
        writer.writeAttribute("id", id, null);
        writer.endElement("span");
      }
      else
      {
        renderVerticalSpacer(context, height, id, comp);
      }
    }
    else
    {
      renderDecorativeIcon(context,
                           rc,
                           XhtmlRenderer.TRANSPARENT_GIF,
                           width,
                           height,
                           id,
                           null,
                           comp);
    }
  }

  protected String getHeight(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_heightKey));
  }

  protected String getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_widthKey));
  }

  private PropertyKey _heightKey;
  private PropertyKey _widthKey;
}