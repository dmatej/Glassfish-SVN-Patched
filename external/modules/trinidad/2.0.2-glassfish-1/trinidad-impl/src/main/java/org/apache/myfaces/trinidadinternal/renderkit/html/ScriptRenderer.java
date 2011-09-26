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
import org.apache.myfaces.trinidad.component.html.HtmlScript;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


public class ScriptRenderer extends XhtmlRenderer
{
  public ScriptRenderer()
  {
    super(HtmlScript.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _sourceKey = type.findKey("source");
    _generatesContentKey = type.findKey("generatesContent");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    rw.startElement("script", component);
    renderId(context, component);
    // TODO: should we render language="javascript"?  The
    // old renderer did
    renderScriptTypeAttribute(context, arc);
    if (!getGeneratesContent(component, bean))
    {
      renderScriptDeferAttribute(context, arc);
    }

    String source = getSource(context, component, bean);
    String text = getText(component, bean);
    if (source != null)
    {
      renderEncodedResourceURI(context, "src", source);
    }
    else
    {
      if (text != null)
        rw.writeText(text, "text");
    }

    encodeAllChildren(context, component);

    rw.endElement("script");

    // Sadly, the script renderer has long supported
    // setting both "source" and "text".  Handle this
    // case by rendering the "text" in a bonus script block. This
    // strategy does not, however, really handle PPR
    if (source != null && text != null)
    {
      rw.startElement("script", component);
      renderScriptTypeAttribute(context, arc);
      if (!getGeneratesContent(component, bean))
      {
        renderScriptDeferAttribute(context, arc);
      }

      rw.writeText(text, "text");
      rw.endElement("script");
    }
  }

  protected String getSource(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return toResourceUri(context, bean.getProperty(_sourceKey));
  }

  protected String getText(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_textKey));
  }

  protected boolean getGeneratesContent(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_generatesContentKey);
    if (o == null)
      o = _generatesContentKey.getDefault();

    // TODO: change the default, as very few scripts actually
    // write content
    return Boolean.TRUE.equals(o);
  }

  private PropertyKey _sourceKey;
  private PropertyKey _textKey;
  private PropertyKey _generatesContentKey;
}
