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
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;


/**
 * Renderer for sideBar
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/SideBarRenderer.java#0 $) $Date: 10-nov-2005.18:56:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SideBarRenderer extends HtmlLafRenderer
{
 // issues 
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT;
  }                

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    super.renderAttributes(context, node);
    ResponseWriter writer = context.getResponseWriter();
    renderStyleClassAttribute(context, AF_PANEL_SIDE_BAR_STYLE_CLASS);
    Object width = node.getAttributeValue(context, WIDTH_ATTR);
    
    boolean isMac = context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS;
    if (width != null && !isMac)
    {
      writer.writeAttribute(STYLE_ATTRIBUTE,  
                            _WIDTH_CONSTANT + width.toString(),
							null);
    }
  }

  /**
   *
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {   
    //--pu-- Note: Take care to render start of related link block wrapper as the  
    //first activity in the prerender of SideBarRenderer and its derivative 
    //SideNavRenderer (in case this method is overridden there)
    renderRelatedLinksBlockStart(context, "af_panelSideBar.BLOCK_TITLE");    
    super.prerender(context, node);

    ResponseWriter writer = context.getResponseWriter();

    UINode filter = node.getNamedChild(context, FILTER_CHILD);
    
    if ( filter != null)
    {
      renderNamedChild(context, node, filter, FILTER_CHILD);
      writer.startElement(HORIZONTAL_RULE_ELEMENT, null);
      writer.endElement(HORIZONTAL_RULE_ELEMENT);
    }
    
  }

  @Override
  protected final void postrender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  { 

    ResponseWriter writer = context.getResponseWriter();

    Object width = node.getAttributeValue(context, WIDTH_ATTR);
    // For width tried using min-width in css, but not working 
    // on ie6 or mac, getting overlapping on mozilla 1.3 and netscape 7, 
    // so rendering a div at bottom      
    if ( width == null )
    {         
      writer.startElement(DIV_ELEMENT, null);
      renderStyleClassAttribute(context, 
                                SIDE_BAR_MIN_WIDTH_STYLE_CLASS);
      writer.endElement(DIV_ELEMENT);
    }  
    else
    {
      writer.startElement(DIV_ELEMENT, null);
      writer.writeAttribute(STYLE_ATTRIBUTE,  
                            _WIDTH_CONSTANT + width.toString(),
							null);    
      writer.endElement(DIV_ELEMENT);
    }
    
    
    super.postrender(context, node);
    //--pu-- Note: Take care to render end of related links block wrapper as the 
    //last activity in the prerender of SideBarRenderer and its derivative 
    //SideNavRenderer (in case this method is overridden there)
    renderRelatedLinksBlockEnd(context);    
  }


  static private String _WIDTH_CONSTANT = "width:"; 
}
