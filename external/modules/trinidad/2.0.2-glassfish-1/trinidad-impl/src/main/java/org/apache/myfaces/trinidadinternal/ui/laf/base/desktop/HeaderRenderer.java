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

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/HeaderRenderer.java#0 $) $Date: 10-nov-2005.18:55:17 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class HeaderRenderer 
         extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.HeaderRenderer
         implements BaseDesktopConstants
{

  // This strange hook is needed because we had two very different
  // header implementations... BLAF used an HTML table for the header,
  // whereas MLAF/SLAF use an HTML div.  This is poses a problem for
  // the laf.base.desktop.HeaderRenderer, since it needs to provide
  // a concrete header implementation on behalf of MLAF/SLAF - but
  // this implementation could not be used by BLAF.  So, we explicitly
  // ask whether the header is being rendered as an HTML table.  If
  // so, we don't bother rendering our div-based contents.
  protected boolean rendersTableHeader(
    UIXRenderingContext context
    )
  {
    return false;
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (rendersTableHeader(context))
      super.prerender(context, node);
    else
      _prerender(context, node);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (rendersTableHeader(context))
      super.postrender(context, node);
    else
      _postrender(context, node);
  }

  protected boolean hasQuickLinkAnchor(
     UIXRenderingContext context,
     UINode           node,
     int              headerIndentLevel
     )
  {
    return false;
  }

  protected Object getTopAnchor()
  {
    return _TOP_ANCHOR_HREF;
  }


  protected Object getReturnAltString(
    UIXRenderingContext context
  )
  {
    return getTranslatedValue(context, _QUICK_LINKS_RETURN_TOP_TIP);
  }


  protected Object getReturnString(
    UIXRenderingContext context
  )
  {
    return getTranslatedValue(context, _QUICK_LINKS_RETURN_TOP_TEXT);
  }


  //
  // The following code was taken from the MLAF HeaderReader
  //

  private void _prerender(
    UIXRenderingContext context,
    UINode           node
  ) throws IOException
  {
    int nestLevel = getHeaderNestLevel(context);

    int size = _getSize(context, node);

    String messageType = 
                BaseDesktopUtils.getStringAttributeValue(context, 
                                                            node, 
                                                            MESSAGE_TYPE_ATTR);
   
    Object label = getText( context, node, messageType);

    String headerElement = (size < HEADER_ELEMENT.length)
                                ? HEADER_ELEMENT[size] 
                                : HEADER_ELEMENT[HEADER_ELEMENT.length-1];
    
    ResponseWriter writer = context.getResponseWriter();    
    
    UIComponent component = NodeUtils.getUIComponent(context, node);
    writer.startElement(DIV_ELEMENT, component);  
    super.renderAttributes( context, node);
    if (nestLevel > 0 )
      renderStyleClassAttribute(context, HEADER_NEST_STYLE_CLASS);
    super.prerender(context, node);

    if (hasQuickLinkAnchor(context, node, nestLevel))
    {
      writer.startElement(LINK_ELEMENT, null);
      //name of the anchor is the full label (URI-encoded)

      // See bug 2944473: Mac IE 5 blows it on supporting URL-encoded
      // anchors.  The bug was filed against quicklinks containing spaces;
      // really, this is a more generic problem that would need to be
      // explored in LinkRenderer as well, but to minimize the scope
      // of the changes, I'll only tweak it here.
      if ((context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS) &&
          (context.getAgent().getAgentApplication() == 
          TrinidadAgent.Application.IEXPLORER))

        renderAttribute(context, NAME_ATTRIBUTE, label);
      else
        renderURIAttribute(context, NAME_ATTRIBUTE, label);

      writer.endElement(LINK_ELEMENT);
    }

    writer.startElement(headerElement, null);

    renderStyleClassAttribute(context, AF_PANEL_HEADER_STYLE_CLASS);

    renderIcon(context, node);

    boolean isError = MESSAGE_TYPE_ERROR.equals(messageType);

    if (isError)
    {
      // If this is an error header, render the text within
      // a AF_PANEL_HEADER_ERROR_STYLE_CLASS span so that header will pick up 
      // the error text foreground color
      writer.startElement(SPAN_ELEMENT, null);
      renderStyleClassAttribute(context, AF_PANEL_HEADER_ERROR_STYLE_CLASS);
    }
    
    if (label != null)
      writer.writeText(label, TEXT_ATTR.getAttributeName());

    if (isError)
    {
      // Close up the span
      writer.endElement(SPAN_ELEMENT);
    }

    // Close up the header
    writer.endElement(headerElement);
    
    // increment header nesting
    incrementHeaderNestLevel(context);

    
  }

  // Renders the header's icon.  If the header has a message type,
  // then the icon is retrieved from the Skin.  Otherwise,
  // we use the icon specified via the header's ICON_ATTR.
  protected void renderIcon(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {   
    Object messageType = node.getAttributeValue(context, MESSAGE_TYPE_ATTR);

    if ((messageType != null) &&
        !MESSAGE_TYPE_NONE.equals(messageType))
    {
      // If we've got a message type, get the Icon
      String iconName = _getMessageIconName(messageType);
      if (iconName != null)
      {
        Icon icon = context.getIcon(iconName);

        // If we've got an Icon, render it
        if (icon != null)
        {
          BaseDesktopUtils.renderIcon(context, 
                                      icon,
                                      EMPTY_STRING_ATTRIBUTE_VALUE,
                                      null);

        }
      }

    }
    else
    {
      Object iconURI = getIconURI(context, node, null);
      if( iconURI != null)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement(IMAGE_ELEMENT, null);
        writer.writeAttribute(ALT_ATTRIBUTE, EMPTY_STRING_ATTRIBUTE_VALUE, null);   
        renderStyleClassAttribute(context, AF_PANEL_HEADER_ICON_STYLE_CLASS);
        renderEncodedResourceURI(context, SOURCE_ATTRIBUTE, iconURI);
        writer.endElement(IMAGE_ELEMENT);
      }
    }
  }

  private void _postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    decrementHeaderNestLevel(context);
    resetHeaderSize(context);  
    ResponseWriter writer = context.getResponseWriter();
    writer.endElement(DIV_ELEMENT);  

    // using float to indent in ie on windows, but that means you 
    // need to clear after the header or you get strange behavior
    if ( HtmlLafRenderer.isIE(context) &&
         TrinidadAgent.OS_WINDOWS == context.getAgent().getAgentOS())
    {
      writer.startElement(DIV_ELEMENT, null);
      writer.writeAttribute("style","clear:both", null);
      writer.endElement(DIV_ELEMENT);
    }
    super.postrender(context, node);

  }

  private int _getSize(
    UIXRenderingContext context,
    UINode           node
  )throws IOException
  {
    setPrevHeaderSize( context );

    Number sizeNumber = (Number)getAttributeValue(context, node, 
                                                  SIZE_ATTR, null);
    
    int size; 
    if (sizeNumber != null )
    {
      size = sizeNumber.intValue();
      setContextHeaderSize( context, sizeNumber);
    }    
    else
    {
      sizeNumber = getContextHeaderSize(context, null);

      if ( sizeNumber == null )
        size = 0;
      else
        size = sizeNumber.intValue() + 1;
        
      setContextHeaderSize( context, size);
    }

    return size;
  }

  // Returns the icon name for the specfied messageType
  private String _getMessageIconName(Object messageType)
  {
    String iconName = null;

    if (MESSAGE_TYPE_ERROR.equals(messageType))
      iconName = AF_PANEL_HEADER_ERROR_ICON_NAME;
    else if (MESSAGE_TYPE_WARNING.equals(messageType))
      iconName = AF_PANEL_HEADER_WARNING_ICON_NAME;
    else if (MESSAGE_TYPE_INFO.equals(messageType))
      iconName = AF_PANEL_HEADER_INFO_ICON_NAME;
    else if (MESSAGE_TYPE_CONFIRMATION.equals(messageType))
      iconName = AF_PANEL_HEADER_CONFIRMATION_ICON_NAME;
    else if (MESSAGE_TYPE_PROCESSING.equals(messageType))
      iconName = AF_PANEL_HEADER_PROCESSING_ICON_NAME;

    assert ((iconName != null) ||
                        MESSAGE_TYPE_NONE.equals(messageType));

    return iconName;
  }

  private static final String _TOP_ANCHOR_HREF = "#top";

  private static final String _QUICK_LINKS_RETURN_TOP_TIP =
                                    "af_panelPage.QUICK_LINKS_RETURN_TOP_TIP";


  private static final String _QUICK_LINKS_RETURN_TOP_TEXT =
                                    "af_panelPage.QUICK_LINKS_RETURN_TOP";

  // -= Simon Lessard =-
  // TODO: Never read locally as of 2006-08-09. Remove permanently if
  //       no problem show up.
  // Integer of the indent level to show QuickLinks at
  //private static final String _QUICKLINKS_INDENT_LEVEL = "quickLinksIndent";

  // FALSE if the first QuickLinks header has been rendered
  //private static final String _QUICK_LINKS_FIRST_HEADER_PROPERTY =
  //  "quickLinksFirstHeader";



}
