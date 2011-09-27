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
import org.apache.myfaces.trinidad.component.html.HtmlMeta;

import org.apache.myfaces.trinidad.context.RenderingContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;

public class MetaRenderer extends XhtmlRenderer
{
  public MetaRenderer()
  {
    super(HtmlMeta.TYPE);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _nameKey = type.findKey("name");
    _typeKey = type.findKey("type");
    _contentKey = type.findKey("content");
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
    FacesBean        bean)
    throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    rw.startElement("meta", component);
    // Important, it is invalid HTML to render an ID so we will omit it: renderId(context, component);

    String name = getName(bean);
    if (name != null)
    {
      String type = getType(bean);
      if (type != null && HtmlMeta.TYPE_HTTP_EQUIV.equals(type))
      {
        rw.writeAttribute("http-equiv", name, null);
      }
      else // by default use "name"
      {
        rw.writeAttribute("name", name, null);
      }
    }

    String content = getContent(bean);
    if (content != null)
    {
      rw.writeAttribute("content", content, null);
    }

    encodeAllChildren(context, component);

    rw.endElement("meta");
  }

  protected String getName(FacesBean bean)
  {
    return toString(bean.getProperty(_nameKey));
  }

  protected String getType(FacesBean bean)
  {
    return toString(bean.getProperty(_typeKey));
  }

  protected String getContent(FacesBean bean)
  {
    return toString(bean.getProperty(_contentKey));
  }

  private PropertyKey _nameKey;
  private PropertyKey _typeKey;
  private PropertyKey _contentKey;
}
