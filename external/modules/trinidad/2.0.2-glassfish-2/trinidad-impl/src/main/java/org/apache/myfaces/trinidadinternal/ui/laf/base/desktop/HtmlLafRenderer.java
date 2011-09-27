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

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

import org.apache.myfaces.trinidadinternal.ui.laf.base.LafIconProvider;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

import org.apache.myfaces.trinidad.skin.Icon;


/**
 * Base Rendering class for HTML renderers
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/HtmlLafRenderer.java#0 $) $Date: 10-nov-2005.18:55:21 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class HtmlLafRenderer extends XhtmlLafRenderer
                             implements BaseDesktopConstants
{
  /**
   * Returns true if the current user agent is a Netscape user agent.
   */
  public static boolean isNetscape(
    UIXRenderingContext context
    )
  {
    return (context.getAgent().getAgentApplication() ==
            TrinidadAgent.Application.NETSCAPE);
  }

  /**
   * Returns true if the current user agent is a Safari user agent.
   */
  public static boolean isSafari(
    UIXRenderingContext context
    )
  {
    return (context.getAgent().getAgentApplication() ==
            TrinidadAgent.Application.SAFARI);
  }


  /**
   * Returns true if the current user agent is a Gecko user agent.
   */
  public static boolean isGecko(
    UIXRenderingContext context
    )
  {
    return (context.getAgent().getAgentApplication() ==
            TrinidadAgent.Application.GECKO);
  }

  /**
   * Returns true if the current user agent is an Internet Explorer user agent.
   */
  public static boolean isIE(
    UIXRenderingContext context
    )
  {
    return (context.getAgent().getAgentApplication() ==
            TrinidadAgent.Application.IEXPLORER);
  }

  /**
   * Renders a transparent gif using a script to save space.
   */
  @Override
  protected void renderTransparent(
    UIXRenderingContext context,
    String           width,
    String           height,
    boolean          needsQuoting
    ) throws IOException
  {
    super.renderTransparent(context, width, height, needsQuoting);
  }  
  

  protected void renderRepeatingImage(
    UIXRenderingContext context,
    String           backgroundImageURL
    ) throws IOException
  {
    renderRepeatingImage(context, backgroundImageURL, null);
  }
  

  protected void renderRepeatingImage(
    UIXRenderingContext context,
    String           backgroundImageURL,
    Object           height
    ) throws IOException
  {
    if (backgroundImageURL != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      
      renderLayoutTableHeader(context, ZERO, "100%");
  

      // Get the absolute URL based on the base image uri
      writeAbsoluteImageURI(context, "background", backgroundImageURL);
      if (height != null)
      {
        writer.writeAttribute("height", height, null);
      }
      
      writer.startElement("tr", null);
      writer.startElement("td", null);
	  writer.writeAttribute("height","1px",null);
      
      writer.endElement("td");
      writer.endElement("tr");
      writer.endElement("table");
    }
  }


  protected void renderRepeatingImage(
    UIXRenderingContext context,
    String           backgroundImageURL,
    Object           height,
    String           contentImageURL,
    String           contentHAlign,
    Object           contentWidth,
    Object           contentHeight
   ) throws IOException
  {
    if (backgroundImageURL != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      
      renderLayoutTableHeader(context, ZERO, "100%");
  
      // Get the absolute URL based on the base image uri    
      writeAbsoluteImageURI(context, "background", backgroundImageURL);
      
      if (height != null)
      {
        writer.writeAttribute("height", height, null);
      }
      
      writer.startElement("tr", null);
      writer.startElement("td", null);
      
      if (contentHAlign != null)
      {
        writer.writeAttribute("align", contentHAlign, null);
      }
      
      renderIcon(context, contentImageURL, contentWidth, contentHeight);
      writer.endElement("td");
      writer.endElement("tr");
      writer.endElement("table");
    }
  }

  /**
   * Utility method for rendering an icon in a table data cell that is used
   * for trim, so the alt="".
   */
  protected void renderTableDataIcon(
    UIXRenderingContext context,
    Icon             icon,
    String           styleClass
    ) throws IOException
  { 
    // If we have an icon, render it inside of a table cell
    if (icon != null)
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fContext = context.getFacesContext();
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(TABLE_DATA_ELEMENT, null);

      if (styleClass != null)
        renderStyleClassAttribute(context, styleClass);

      // Render the Icon's width and height
      writer.writeAttribute("width", icon.getImageWidth(arc), null);
      writer.writeAttribute("height", icon.getImageHeight(arc), null);

      // Render the icon     
      // (we need to render alt="" for chrome images.)
      OutputUtils.renderIcon(fContext, arc, icon, "", null);
      

      writer.endElement(TABLE_DATA_ELEMENT);
    }
  }

  protected static void writeCacheImageURI(
    UIXRenderingContext context,
    String           attribute,
    String           uri) throws IOException
  {
    if ((uri == null) || (uri.length() == 0))
      return;

    ResponseWriter writer = context.getResponseWriter();
    String cachedImgURI = LafIconProvider.getCacheImageURI(context) + uri;
    FacesContext facesContext = context.getFacesContext();
    if(facesContext != null)
      cachedImgURI = facesContext.getExternalContext().encodeResourceURL(cachedImgURI);
    writer.writeURIAttribute(attribute,
                          cachedImgURI,
						  null);
  }
}
