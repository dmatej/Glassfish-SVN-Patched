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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;

/**
 * Renderer for footers
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/FooterRenderer.java#0 $) $Date: 10-nov-2005.18:55:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FooterRenderer extends HtmlLafRenderer
{
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    super.renderAttributes(context, node);
    renderStyleClassAttribute(context, FOOTER_STYLE_CLASS);
  }

  @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.writeText(_SEPARATOR, null);
  }  

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    
    ResponseWriter writer = context.getResponseWriter();

    UINode copyright = node.getNamedChild(context, APP_COPYRIGHT_CHILD);
    UINode privacy = node.getNamedChild(context, APP_PRIVACY_CHILD);
    UINode about = node.getNamedChild(context, APP_ABOUT_CHILD);

    writer.startElement(DIV_ELEMENT, null);
    renderStyleClassAttribute(context, FOOTER_BOTTOM_STYLE_CLASS);

    // Disable default link styles - we don't need class="OraLink", 
    // so let's save some space
    LinkUtils.startDefaultStyleClassDisabled(context);

    if ( copyright != null )
    {
      writer.startElement(SPAN_ELEMENT, null);
      renderStyleClassAttribute(context, AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS);
      renderNamedChild(context, node, copyright, APP_COPYRIGHT_CHILD);
      writer.endElement(SPAN_ELEMENT);
    }

    if ( privacy != null )
    {
      writer.startElement(SPAN_ELEMENT, null);
      renderStyleClassAttribute(context, AF_PANEL_PAGE_PRIVACY_STYLE_CLASS);
      renderNamedChild(context, node, privacy, APP_PRIVACY_CHILD);
      writer.endElement(SPAN_ELEMENT);
    }    
    
    if ( about != null )
    {
      writer.startElement(SPAN_ELEMENT, null);
      renderStyleClassAttribute(context, AF_PANEL_PAGE_ABOUT_STYLE_CLASS);
      renderNamedChild(context, node, about, APP_ABOUT_CHILD);
      writer.endElement(SPAN_ELEMENT);
    }        

    // Reset default link styles
    LinkUtils.endDefaultStyleClassDisabled(context);

    writer.endElement(DIV_ELEMENT);
    
    super.postrender(context, node);
  }

  @Override
  protected String getElementName(UIXRenderingContext context, UINode node)
  {
    return DIV_ELEMENT;
  }


  private static final String _SEPARATOR = "\u00a0\u00a0|\u00a0\u00a0";
}
