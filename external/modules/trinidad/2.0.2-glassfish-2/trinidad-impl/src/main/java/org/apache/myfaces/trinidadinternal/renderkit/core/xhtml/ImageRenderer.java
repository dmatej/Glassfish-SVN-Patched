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
import org.apache.myfaces.trinidad.component.core.output.CoreImage;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRendererUtils;


public class ImageRenderer extends XhtmlRenderer
{
  public ImageRenderer()
  {
    super(CoreImage.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _imageMapTypeKey = type.findKey("imageMapType");
    _longDescURLKey = type.findKey("longDescURL");
    _sourceKey = type.findKey("source");
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
    rw.startElement("img", comp);
    renderId(context, rc, comp, rw);
    renderAllAttributes(context, rc, comp, bean);
    rw.endElement("img");
  }

  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    super.renderAllAttributes(context, rc, component, bean);

    renderEncodedResourceURI(context, "src", getSource(component, bean));
    renderEncodedActionURI(context, "longdesc", getLongDescURL(component, bean));

    _renderImageMap(component, bean, rw);
  }

  @Override
  protected void renderShortDescAttribute(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String shortDesc = getShortDesc(component, bean);
    if (shortDesc != null)
    {
      OutputUtils.renderAltAndTooltipForImage(context, rc,
                                              shortDesc);
    }
  }

  protected String getImageMapType(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_imageMapTypeKey));
  }

  protected String getLongDescURL(
    UIComponent component,
    FacesBean   bean)
  {
    return toActionUri(FacesContext.getCurrentInstance(),bean.getProperty(_longDescURLKey));
  }

  protected String getSource(
    UIComponent component,
    FacesBean   bean)
  {
    return toResourceUri(FacesContext.getCurrentInstance(),bean.getProperty(_sourceKey));
  }

  protected void renderId(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    ResponseWriter   writer
    ) throws IOException
  {
    super.renderId(context, comp);

    // only output the name if the agent supports it
    if (shouldRenderId(context, comp)
        && CoreRendererUtils.supportsNameIdentification(rc))
    {
      String clientId = getClientId(context, comp);
      writer.writeAttribute("name", clientId, null);
    }
  }

  private void _renderImageMap(
    UIComponent    component,
    FacesBean      bean,
    ResponseWriter writer
    ) throws IOException
  {
    String mType = getImageMapType(component, bean);
    if (CoreImage.IMAGE_MAP_TYPE_SERVER.equals(mType))
      writer.writeAttribute("ismap", Boolean.TRUE, "imageMapType");
  }

  private PropertyKey _imageMapTypeKey;
  private PropertyKey _longDescURLKey;
  private PropertyKey _sourceKey;
}
