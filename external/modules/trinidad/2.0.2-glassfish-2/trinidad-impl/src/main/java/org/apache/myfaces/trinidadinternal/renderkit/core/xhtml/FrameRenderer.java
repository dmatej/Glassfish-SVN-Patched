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
import org.apache.myfaces.trinidad.component.html.HtmlFrame;
import org.apache.myfaces.trinidad.component.html.HtmlFrameBorderLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Renders a frame.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FrameRenderer.java#0 $) $Date: 10-nov-2005.18:53:54 $
 */
public class FrameRenderer extends XhtmlRenderer
{
  public FrameRenderer()
  {
    this(HtmlFrame.TYPE);
  }

  protected FrameRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _marginWidthKey = type.findKey("marginWidth");
    _marginHeightKey = type.findKey("marginHeight");
    _sourceKey = type.findKey("source");
    _longDescUrlKey = type.findKey("longDescURL");
    _scrollingKey = type.findKey("scrolling");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    renderShortDescAttribute(context, rc, component, bean);
    renderStyleAttributes(context, rc, component, bean);

    writer.writeAttribute("frameborder", "0", null);
    writer.writeAttribute("marginwidth",
                          getMarginWidth(component, bean),
                          "marginWidth");
    writer.writeAttribute("marginheight",
                          getMarginHeight(component, bean),
                          "marginHeight");
    writer.writeAttribute("noresize", Boolean.TRUE, null);

    String source = toResourceUri(context, bean.getProperty(_sourceKey));

    renderEncodedActionURI(context, "src", source);

    String longDesc = toResourceUri(context, bean.getProperty(_longDescUrlKey));
    renderEncodedActionURI(context, "longdesc", longDesc);

    writer.writeAttribute("scrolling",
                          bean.getProperty(_scrollingKey),
                          "scrolling");
  }

  protected Object getMarginWidth(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = bean.getProperty(_marginWidthKey);
    if (value == null)
      value = _marginWidthKey.getDefault();
    return value;
  }

  protected Object getMarginHeight(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = bean.getProperty(_marginHeightKey);
    if (value == null)
      value = _marginHeightKey.getDefault();
    return value;
  }

  /**
   * Renders the client ID as both "id" and "name"
   */
  @Override
  protected void renderId(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      context.getResponseWriter().writeAttribute("id", clientId, "id");
      context.getResponseWriter().writeAttribute("name", clientId, "id");
    }
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    UIComponent parent = component.getParent();

    //
    // complain if our parent isn't a FrameBorderLayout
    //
    if ((parent == null) ||
        !HtmlFrameBorderLayout.COMPONENT_FAMILY.equals(parent.getFamily()))
    {
      _LOG.warning("FRAMES_MUST_INSIDE_FRAMEBORDERLAYOUTS");
    }
    else
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement("frame", component);
      renderId(context, component);
      renderAllAttributes(context, rc, component, bean);
      writer.endElement("frame");
    }
  }

  private PropertyKey _marginWidthKey;
  private PropertyKey _marginHeightKey;
  private PropertyKey _sourceKey;
  private PropertyKey _longDescUrlKey;
  private PropertyKey _scrollingKey;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FrameRenderer.class);
}
