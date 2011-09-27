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

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputFormatted;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class OutputFormattedRenderer extends ValueRenderer
{
  public OutputFormattedRenderer()
  {
    super(CoreOutputFormatted.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _styleUsageKey = type.findKey("styleUsage");
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
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    if (canSkipRendering(context, rc, comp))
      return;

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", comp);

    renderId(context, comp);
    renderAllAttributes(context, rc, comp, bean);

    String value = getConvertedString(context, comp, bean);
    renderFormattedText(context, value);
    rw.endElement("span");
  }

  protected String getStyleUsage(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_styleUsageKey));
  }

  @Override
  protected String getStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    String styleClass = super.getStyleClass(component, bean);
    if (styleClass == null)
    {
      String usage = getStyleUsage(component, bean);
      if (usage != null)
      {
        styleClass = _USAGES.get(usage);
      }
    }

    return styleClass;
  }

  static private final Map<String, String> _USAGES;
  static
  {
    _USAGES = new HashMap<String, String>();
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_IN_CONTEXT_BRANDING,
                SkinSelectors.IN_CONTEXT_TEXT_STYLE_CLASS);
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_INSTRUCTION,
                SkinSelectors.INSTRUCTION_TEXT_STYLE_CLASS);
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_PAGE_STAMP,
                SkinSelectors.PAGE_STAMP_TEXT_STYLE_CLASS);
  }

  private PropertyKey _styleUsageKey;
}
