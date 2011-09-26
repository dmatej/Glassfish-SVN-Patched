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
import org.apache.myfaces.trinidad.component.html.HtmlFrameBorderLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FrameBorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:53:53 $
 */
public class PdaFrameBorderLayoutRenderer extends XhtmlRenderer
{
  public PdaFrameBorderLayoutRenderer()
  {
    this(HtmlFrameBorderLayout.TYPE);
  }

  protected PdaFrameBorderLayoutRenderer(
    FacesBean.Type type)
  {
    super(type);
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
    renderShortDescAttribute(context, rc, component, bean);
    renderStyleAttributes(context, rc, component, bean);
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("div", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    String leftName       = HtmlFrameBorderLayout.LEFT_FACET;
    String innerLeftName  = HtmlFrameBorderLayout.INNER_LEFT_FACET;
    String rightName      = HtmlFrameBorderLayout.RIGHT_FACET;
    String innerRightName = HtmlFrameBorderLayout.INNER_RIGHT_FACET;

    UIComponent center = getFacet(component, HtmlFrameBorderLayout.CENTER_FACET);
    UIComponent top    = getFacet(component, HtmlFrameBorderLayout.TOP_FACET);
    UIComponent bottom = getFacet(component, HtmlFrameBorderLayout.BOTTOM_FACET);
    UIComponent left   = getFacet(component, leftName);
    UIComponent right  = getFacet(component, rightName);
    UIComponent innerLeft   = getFacet(component, innerLeftName);
    UIComponent innerRight  = getFacet(component, innerRightName);

    boolean r2l = rc.getLocaleContext().isRightToLeft();

    if (left == null)
    {
      leftName = r2l ? HtmlFrameBorderLayout.END_FACET : HtmlFrameBorderLayout.START_FACET;
      left     = getFacet(component, leftName);
    }

    if (right == null)
    {
      rightName = r2l ? HtmlFrameBorderLayout.START_FACET : HtmlFrameBorderLayout.END_FACET;
      right     = getFacet(component, rightName);
    }

    if (innerLeft == null)
    {
      innerLeftName = r2l ? HtmlFrameBorderLayout.INNER_END_FACET : HtmlFrameBorderLayout.INNER_START_FACET;
      innerLeft     = getFacet(component, innerLeftName);
    }

    if (innerRight == null)
    {
      innerRightName = r2l ? HtmlFrameBorderLayout.INNER_START_FACET : HtmlFrameBorderLayout.INNER_END_FACET;
      innerRight     = getFacet(component, innerRightName);
    }

    _encodeFacet(context, top);
    _encodeFacet(context, left);
    _encodeFacet(context, innerLeft);
    _encodeFacet(context, center);
    _encodeFacet(context, innerRight);
    _encodeFacet(context, right);
    _encodeFacet(context, bottom);

    writer.endElement("div");
  }

  private void _encodeFacet(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    if (component != null)
    {
      encodeChild(context, component);
      context.getResponseWriter().startElement("br", component);
      context.getResponseWriter().endElement("br");
    }
  }
}
