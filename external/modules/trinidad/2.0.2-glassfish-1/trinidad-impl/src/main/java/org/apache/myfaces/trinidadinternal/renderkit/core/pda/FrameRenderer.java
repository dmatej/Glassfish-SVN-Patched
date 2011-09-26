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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

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
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


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

  protected FrameRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _sourceKey = type.findKey("source");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
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

      writer.startElement("a", component);
      renderId(context, component);

      String source = toResourceUri(context, bean.getProperty(_sourceKey));
      String shortDesc = getShortDesc(component, bean);

      // =-=Adam Winer: OraLink is obviously not a good style class
      // here - substitute something like trh|frame in a PDA css
      renderStyleClass(context, rc, "OraLink");
      renderEncodedActionURI(context, "href", source);
      if (shortDesc != null)
        writer.writeText(shortDesc, "shortDesc");
      else if (source != null)
        writer.writeText(source, null);

      writer.endElement("a");
    }
  }

  private PropertyKey _sourceKey;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FrameRenderer.class);
}
