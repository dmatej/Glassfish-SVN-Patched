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
import org.apache.myfaces.trinidad.component.core.data.CoreSelectRangeChoiceBar;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelButtonBar;
import org.apache.myfaces.trinidad.component.core.nav.CoreSingleStepButtonBar;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class PanelButtonBarRenderer extends PanelHorizontalLayoutRenderer
{
  public PanelButtonBarRenderer()
  {
    super(CorePanelButtonBar.TYPE);
  }

  // Not currently supported, but would be easy to add...
  @Override
  protected Object getValign(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  /**
   * This is how we can render both the user defined styleClass and our
   * component style class.
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    renderStyleAttributes(context, rc, component, bean,
      SkinSelectors.AF_PANEL_BUTTON_BAR_STYLE_CLASS);
  }

  /**
   * Render a single child (or the separator facet)
   */
  @Override
  protected void encodeChild(
    FacesContext context,
    UIComponent  child,
    Object       vAlign
    ) throws IOException
  {
    // These two components render themselves
    if ((child instanceof CoreSingleStepButtonBar) ||
        (child instanceof CoreSelectRangeChoiceBar))
    {
      encodeChild(context, child);
    }
    else
    {
      super.encodeChild(context, child, vAlign);
    }
  }

  /**
   * Render a separator
   */
  @Override
  protected void encodeSeparator(
    FacesContext context,
    UIComponent  separator,
    Object       vAlign
    ) throws IOException
  {
    // FIXME Use proper skinning techniques
    RenderingContext rc = RenderingContext.getCurrentInstance();
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("td", null);
    renderSpacer(context, rc, "10", "1");
    writer.endElement("td");
  }
}