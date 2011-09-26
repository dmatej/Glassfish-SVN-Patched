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
package org.apache.myfaces.trinidadinternal.renderkit.htmlBasic;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;


/**
 * Renderer for h:commandLink.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/htmlBasic/FormRenderer.java#0 $) $Date: 10-nov-2005.19:00:39 $
 */
public class HtmlFormRenderer extends Renderer
{
  @Override
  public void decode(FacesContext context,
                     UIComponent component)
  {
    createRenderer(component).decode(context, component);
  }

  @Override
  public boolean getRendersChildren()
  {
    return false;
  }

  @Override
  public void encodeBegin(FacesContext context,
                             UIComponent component) throws IOException
  {
    createRenderer(component).encodeBegin(context, component);
  }

  @Override
  public void encodeEnd(FacesContext context,
                     UIComponent component) throws IOException
  {
    createRenderer(component).encodeEnd(context, component);
  }

  protected Renderer createRenderer(final UIComponent component)
  {
    final FacesBean bean = new ComponentFacesBean(component);
    return new FormRenderer()
    {
      @Override
      public FacesBean getFacesBean(
        UIComponent comp)
      {
        return bean;
      }

      @Override
      protected String getInlineStyle(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("style"));
      }

      @Override
      protected String getTargetFrame(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("target"));
      }

      @Override
      protected String getShortDesc(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("title"));
      }

      @SuppressWarnings("unchecked")
      @Override
      protected boolean getUsesUpload(
        UIComponent component,
        FacesBean   bean)
      {
        Map<String, Object> attrs = component.getAttributes();
        return "multipart/form-data".equals(attrs.get("enctype"));
      }
    };
  }
}
