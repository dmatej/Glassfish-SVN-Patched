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

import java.util.Collections;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.output.CoreStatusIndicator;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;


public class StatusIndicatorRenderer extends XhtmlRenderer
{
  public StatusIndicatorRenderer()
  {
    super(CoreStatusIndicator.TYPE);
  }

  protected StatusIndicatorRenderer(
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
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    UIComponent busyFacet = comp.getFacet(CoreStatusIndicator.BUSY_FACET);
    UIComponent readyFacet = comp.getFacet(CoreStatusIndicator.READY_FACET);

    boolean iconMode = false;
    Icon busyIcon = null;
    Icon readyIcon = null;
    if (busyFacet == null && readyFacet == null)
    {
      // Render icons only if no facet was specified.
      busyIcon = rc.getIcon(SkinSelectors.AF_STATUS_INDICATOR_BUSY_ICON);
      readyIcon = rc.getIcon(SkinSelectors.AF_STATUS_INDICATOR_READY_ICON);
      if (busyIcon == null || readyIcon == null)
      {
        // Missing an icon and no faet was specified, log.
        _LOG.warning("STATUS_INDICATOR_MISSING_ICONS");
        return;
      }

      iconMode = true;
    }

    ResponseWriter rw = context.getResponseWriter();

    String clientId = getClientId(context, comp);

    // Renders root DOM
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, comp);
    renderId(context, comp);
    renderAllAttributes(context, rc, comp, bean);

    // Renders ready DOM
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    rw.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, clientId + "::ready", null);

    if (iconMode)
    {
      _renderIcon(context, rc, readyIcon, "af_statusIndicator.READY");
    }
    else
    {
      _renderFacet(context, rc, readyFacet,
                   SkinSelectors.AF_STATUS_INDICATOR_READY_STYLE);
    }

    rw.endElement(XhtmlConstants.SPAN_ELEMENT);

    // Renders busy DOM
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    rw.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, clientId + "::busy", null);
    rw.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "display:none", null);

    if (iconMode)
    {
      _renderIcon(context, rc, busyIcon, "af_statusIndicator.BUSY");
    }
    else
    {
      _renderFacet(context, rc, busyFacet,
                   SkinSelectors.AF_STATUS_INDICATOR_BUSY_STYLE);
    }

    rw.endElement(XhtmlConstants.SPAN_ELEMENT);

    rw.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
    renderScriptTypeAttribute(context, rc);
    rw.writeText("TrStatusIndicator._register(\"" + clientId + "\");", null);
    rw.endElement(XhtmlConstants.SCRIPT_ELEMENT);

    rw.endElement(XhtmlConstants.SPAN_ELEMENT);
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_STATUS_INDICATOR_STYLE;
  }

  private void _renderFacet(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      facet,
    String           styleClass
    ) throws IOException
  {
    if (facet != null && facet.isRendered())
    {
      ResponseWriter rw = context.getResponseWriter();

      rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
      renderStyleClass(context, rc, styleClass);
      encodeChild(context, facet);
      rw.endElement(XhtmlConstants.SPAN_ELEMENT);
    }
  }

  private void _renderIcon(
    FacesContext     context,
    RenderingContext rc,
    Icon             icon,
    String           iconDesc
    ) throws IOException
  {
    if (icon != null && !icon.isNull())
    {
      Map<String, String> attrs =
        Collections.singletonMap(Icon.SHORT_DESC_KEY,
                                 rc.getTranslatedString(iconDesc));

      icon.renderIcon(context, rc, attrs);
    }
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StatusIndicatorRenderer.class);
}
