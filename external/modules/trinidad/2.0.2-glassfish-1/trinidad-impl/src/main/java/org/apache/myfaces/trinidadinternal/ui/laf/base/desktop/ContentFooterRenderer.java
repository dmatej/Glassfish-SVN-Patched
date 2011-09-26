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

/**
 * Renderer for content footers and their children
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ContentFooterRenderer.java#0 $) $Date: 10-nov-2005.18:55:14 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ContentFooterRenderer extends HtmlLafRenderer
{
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {    
    super.renderAttributes(context, node);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);
 
    ResponseWriter writer = context.getResponseWriter();

    //
    // Render the line
    //
    writer.startElement(HORIZONTAL_RULE_ELEMENT, null);
    renderStyleClassAttribute(context, CONTENT_FOOTER_RULE_STYLE_CLASS);
    writer.endElement(HORIZONTAL_RULE_ELEMENT);


    
    writer.startElement(DIV_ELEMENT, null);
    renderStyleClassAttribute( context, CONTENT_FOOTER_CHILDREN_STYLE_CLASS);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();    
    writer.endElement(DIV_ELEMENT);

    writer.startElement(DIV_ELEMENT, null);
    renderStyleClassAttribute( context, CONTENT_FOOTER_START_STYLE_CLASS);
    renderNamedChild(context, node, START_CHILD);
    writer.endElement(DIV_ELEMENT);
    
    writer.startElement(DIV_ELEMENT, null);
    renderStyleClassAttribute( context, CONTENT_FOOTER_BOTTOM_STYLE_CLASS );
    writer.endElement(DIV_ELEMENT);
    
    super.postrender(context, node); 
  }
    
  @Override
  protected String getElementName(UIXRenderingContext context, UINode node)
  {
    return DIV_ELEMENT;
  }

}
