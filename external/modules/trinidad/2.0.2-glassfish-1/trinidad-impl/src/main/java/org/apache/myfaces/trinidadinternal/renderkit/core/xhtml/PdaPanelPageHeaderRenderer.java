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
import org.apache.myfaces.trinidad.component.core.layout.CorePanelPageHeader;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 *  @version $Header: PdaPanelPageHeaderRenderer.java 10-nov-2005.19:01:41 dosterbe Exp $
 *  made jsf major by gcrawfor
 */

public class PdaPanelPageHeaderRenderer extends XhtmlRenderer
{
  public PdaPanelPageHeaderRenderer()
  {
    super(CorePanelPageHeader.TYPE);
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
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    UIComponent branding    = getFacet(component,
                                CorePanelPageHeader.BRANDING_FACET);
    UIComponent brandingApp = getFacet(component,
                                CorePanelPageHeader.BRANDING_APP_FACET);
    UIComponent navigation1 = getFacet(component,
                                CorePanelPageHeader.NAVIGATION1_FACET);
    UIComponent navigation2 = getFacet(component,
                                CorePanelPageHeader.NAVIGATION2_FACET);
    writer.startElement("span", component);
    renderAllAttributes(context, rc, component, bean);
    renderId(context, component);

    if( branding != null)
      encodeChild(context, branding);

    if (branding != null && brandingApp != null)
      renderSpacer(context, rc, "5", "1");

    if(brandingApp != null)
      encodeChild(context, brandingApp);

    if( navigation1 != null)
    {
      writer.startElement("div", null);
      writer.endElement("div");
      encodeChild(context, navigation1);
    }

    if( navigation2 != null)
    {
      writer.startElement("div", null);
      writer.endElement("div");
      encodeChild(context, navigation2);
    }

    writer.endElement("span");
  }
}
