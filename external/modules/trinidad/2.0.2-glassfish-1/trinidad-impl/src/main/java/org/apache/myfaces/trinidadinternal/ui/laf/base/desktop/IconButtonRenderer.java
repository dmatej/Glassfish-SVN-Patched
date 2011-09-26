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

import org.apache.myfaces.trinidad.skin.Skin;


import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidad.skin.Icon;

/**
 * Base class for Icon-based button Renderers.  Renders an
 * Icon within a link.  Subclasses must override getIconName()
 * to specify the name of the button's Icon.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/IconButtonRenderer.java#0 $) $Date: 10-nov-2005.18:55:23 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class IconButtonRenderer extends HtmlLafRenderer
{
  /**
   * Returns the name of the Icon to render
   */
  abstract String getIconName();

  // Returns the elmenet name - "a"
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Icon buttons are always wrapped in a link
    return LINK_ELEMENT;
  }

  // Renders the link attributes
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Override renderAttributes.  We only need to render
    // three attributes on the outer link - the id, the
    // onClick, and the destination.  Also, we don't actually
    // want to render the short description on the link, so
    // we avoid calling super.renderAttributes()

    // Render the id
    renderID(context, node);

    // Render the onClick/destination if necessary
    Object onClick = getOnClick(context, node);
    if (onClick != null)
    {
      renderAttribute(context, ONCLICK_ATTRIBUTE, onClick);

      Object destination = getDestination(context, node);
      if (destination != null)
        renderEncodedActionURI(context, HREF_ATTRIBUTE, destination);
    }
  }

  // Renders the icon
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object source  = getSource(context, node);
    if (source != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("img", null);
      renderEncodedResourceURI(context, "src", source);
      writer.writeAttribute("border", "0", null);
      renderAltAndTooltipForImage(context, getShortDesc(context, node));
      writer.writeAttribute("align", getVAlign(context, node), null);
      writer.endElement("img");
    }
    else
    {
      String iconName = getIconName();
      assert (iconName != null);
      
      Skin skin = context.getSkin();
      Icon icon = skin.getIcon(iconName);
      
      if (icon != null)
      {
        BaseDesktopUtils.renderIcon(context,
                                    icon,
                                    getShortDesc(context, node),
                                    getVAlign(context, node));
      }
    }
  }

  protected Object getSource(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }

  // Gets the destination for the button
  protected Object getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object destination = node.getAttributeValue(context, DESTINATION_ATTR);
    if (destination == null)
      destination = "#";

    return destination;
  }

  // Returns the vertical alignment for the Icon
  protected Object getVAlign(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Bug #3748837: If we don't set the alignment on the LOV icon, then the
    // alignment of the prompt and input fields are off by about a pixel.
    // Apparently this may cause the world to end, so we explicitly set the
    // icon alignment to TOP for LOV buttons.

    // =-= mll LOV button looks ok with "absmiddle" so let's use that to be 
    // consistent with other icons buttons
//    if (node.getLocalName().equals(UIConstants.LOV_BUTTON_NAME)) 
//      return "top";
      
      if (isScreenReaderMode(context)) 
      {
        return "top";
      }
      else 
      {
        return "absmiddle";  
      }
  }
}
