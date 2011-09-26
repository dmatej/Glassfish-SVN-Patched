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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.CommandLinkRenderer;


/**
 * Renderer for h:commandLink.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/htmlBasic/CommandLinkRenderer.java#0 $) $Date: 10-nov-2005.19:00:39 $
 */
public class HtmlCommandLinkRenderer extends Renderer
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
    return true;
  }

  @Override
  public void encodeChildren(FacesContext context,
                             UIComponent component) throws IOException
  {
    // Do nothing - we'll do it all in encodeEnd()
  }

  @SuppressWarnings("unchecked")
  @Override
  public void encodeEnd(FacesContext context,
                     UIComponent component) throws IOException
  {
    // The tr:commandLink is not a rendersChildren component,
    // but h:commandLink is.  Hence, the difference in behavior
    Renderer renderer = createRenderer(component);
    renderer.encodeBegin(context, component);

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      RenderUtils.encodeRecursive(context, child);
    }

    renderer.encodeEnd(context, component);
  }

  protected Renderer createRenderer(final UIComponent component)
  {
    final FacesBean bean = new ComponentFacesBean(component);
    return new CommandLinkRenderer()
    {
      @Override
      public FacesBean getFacesBean(
        UIComponent comp)
      {
        return bean;
      }

      @Override
      protected String getText(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("value"));
      }

      @Override
      protected String getShortDesc(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("title"));
      }

      @Override
      protected char getAccessKey(
        UIComponent component,
        FacesBean   bean)
      {
        return toChar(component.getAttributes().get("accesskey"));
      }

      @Override
      protected String getInlineStyle(
        UIComponent component,
        FacesBean   bean)
      {
        return toString(component.getAttributes().get("style"));
      }

      @Override
      protected boolean getDisabled(
        UIComponent component,
        FacesBean   bean)
      {
        return Boolean.TRUE.equals(component.getAttributes().get("disabled"));
      }
    };
  }
}
