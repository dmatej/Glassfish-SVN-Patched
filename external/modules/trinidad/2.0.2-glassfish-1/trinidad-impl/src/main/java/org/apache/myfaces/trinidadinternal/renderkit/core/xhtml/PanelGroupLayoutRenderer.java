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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroupLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class PanelGroupLayoutRenderer extends XhtmlRenderer
{
  public PanelGroupLayoutRenderer()
  {
    this(CorePanelGroupLayout.TYPE);
  }

  protected PanelGroupLayoutRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _layoutKey = type.findKey("layout");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    // Fix for TRINIDAD-652
    return SkinSelectors.AF_PANEL_GROUP_LAYOUT_STYLE_CLASS;
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

    Object layout        = getLayout(component, bean);
    boolean isVertical   = CorePanelGroupLayout.LAYOUT_VERTICAL.equals(layout);;
    boolean isHorizontal = CorePanelGroupLayout.LAYOUT_HORIZONTAL.equals(layout);

    if (isHorizontal)
    {
      rw.startElement("table", component);
      OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    }
    else if (isVertical)
    {
      rw.startElement("div", component);
    }
    else
    {
      rw.startElement("span", component);
    }

    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    if (isHorizontal)
    {
      rw.startElement("tr", null);
    }

    _encodeChildren(context, component, isVertical, isHorizontal);

    if (isHorizontal)
    {
      rw.endElement("tr");
      rw.endElement("table");
    }
    else if (isVertical)
    {
      rw.endElement("div");
    }
    else
    {
      rw.endElement("span");
    }
  }

  /**
   * Render all the children of the PanelGroup
   */
  @SuppressWarnings("unchecked")
  private void _encodeChildren(
    FacesContext context,
    UIComponent  component,
    boolean      isVertical,
    boolean      isHorizontal
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    UIComponent separator = getFacet(component,
                                     CorePanelGroupLayout.SEPARATOR_FACET);

    boolean needSeparator = false;
    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (!child.isRendered())
        continue;

      if (needSeparator)
      {
        if (isVertical)
          rw.startElement("div", null);
        encodeSeparator(context, separator, isHorizontal);
        if (isVertical)
          rw.endElement("div");
      }

      encodeChild(context, child, isHorizontal);
      needSeparator = true;
    }
  }

  /**
   * Render a single separator
   */
  protected void encodeSeparator(
    FacesContext context,
    UIComponent  separator,
    boolean      isHorizontal
    ) throws IOException
  {
    if (separator != null)
      encodeChild(context, separator, isHorizontal);
  }

  /**
   * Render a single child (or the separator facet)
   */
  protected void encodeChild(
    FacesContext context,
    UIComponent  child,
    boolean      isHorizontal
    ) throws IOException
  {
    if (isHorizontal)
    {
      ResponseWriter rw = context.getResponseWriter();
      rw.startElement("td", null);
      encodeChild(context, child);
      rw.endElement("td");
    }
    else
    {
      encodeChild(context, child);
    }
  }

  protected Object getLayout(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_layoutKey);
  }

  private PropertyKey _layoutKey;
}
