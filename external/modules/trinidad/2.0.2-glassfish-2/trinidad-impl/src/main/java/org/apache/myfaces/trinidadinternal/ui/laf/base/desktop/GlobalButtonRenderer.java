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

import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * Renderer for global buttons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/GlobalButtonRenderer.java#0 $) $Date: 10-nov-2005.18:55:16 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalButtonRenderer extends HtmlLafRenderer
{

  /**
   *
   */
  @Override
  protected void prerender(UIXRenderingContext context, UINode node)
    throws IOException
  {
   // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the link
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
      action.writeDependencies(context, node);
    super.prerender(context, node);

    Object iconURI = node.getAttributeValue(context, ICON_ATTR);


    // Okay - now we can render.  Get our other render attributes
    Object text        = getText(context, node);
    Object styleClass  = getStyleClass(context, node);
    Object destination = getDestination(context, node);
    Object shortDesc   = getShortDesc(context, node);
    if ( shortDesc == null )
      shortDesc = text;

    boolean isLink = (destination != null);

    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(LINK_ELEMENT, null);
    renderID(context, node);
    super.renderEventHandlers(context, node);
    renderStyleClassAttribute(context, styleClass);

    if (isLink)
    {
      renderEncodedActionURI(context, HREF_ATTRIBUTE, destination);
      renderAttribute(context, node,
                      TARGET_FRAME_ATTRIBUTE, TARGET_FRAME_ATTR);


    }

    if ( iconURI != null )
    {
      writer.startElement(IMAGE_ELEMENT, null);
      renderStyleClassAttribute(context, AF_MENU_BUTTONS_IMAGE_STYLE_CLASS);
      renderEncodedResourceURI(context, SOURCE_ATTRIBUTE, iconURI);
      renderAltAndTooltipForImage(context, shortDesc);
      renderAttribute(context, BORDER_ATTRIBUTE, 0);
      renderAttribute(context, node, WIDTH_ATTRIBUTE, WIDTH_ATTR);
      renderAttribute(context, node, HEIGHT_ATTRIBUTE, HEIGHT_ATTR);
      writer.endElement(IMAGE_ELEMENT);
    }

    if (text != null)
      writer.writeText(text, TEXT_ATTR.getAttributeName());

    writer.endElement(LINK_ELEMENT);
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Don't bother with renderContent - it's all done in the prerender
  }

  /*
   * @todo added basic fireAction support, didn't do the part commented out
   * about _getPartialChangeScript if action is null
   */
  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Disabled is already checked before we render
    if (isSelected(context, node))
      return null;


    // We build up the actual onclick handler by chaining the value the
    // ONCLICK_ATTR with the script of the PRIMARY_CLIENT_ACTION_ATTR.
    // Since we get the onclick handler multiple times (getDestination(),
    // which is called multiple times, and renderEventHandlers()), and
    // since building up the handler can be expensive, we store the
    // script in a local property, so that this work does not need to
    // be repeated.
    Object prop = context.getLocalProperty(0,
                                           _LOCAL_ON_CLICK_KEY,
                                           _NONE);
    if (prop != _NONE)
      return prop;

    Object onClick = super.getOnClick(context, node);
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);

    String actionScript = null;

    if (action != null)
    {
      if (action.renderAsEvent(context, node) &&
          (getDestinationAttr(context, node) == null))
      {
        // We must ignore actionScript if there is a destination or else the
        // destination will never execute because the onclick will run first.
        actionScript = action.getScript(context, node, Boolean.FALSE);
      }
    }
    /*
    else
    {
      // If we don't have a ClientAction, check to see if we've got
      // partial targets provided by an ancestor link container.
      actionScript = _getPartialChangeScript(context, node);
    }
    */

    Object chainedScript = null;

    if ((onClick != null) || (actionScript != null))
    {
      chainedScript = XhtmlLafUtils.getChainedJS(onClick,
                                                 actionScript,
                                                 true);
    }

    // Store away the script for next time
    context.setLocalProperty(_LOCAL_ON_CLICK_KEY, chainedScript);
    return chainedScript;



  }

  protected Object getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (isDisabled(context, node) || isSelected(context, node))
      return null;

    Object destination;
    if (supportsNavigation(context))
      destination = node.getAttributeValue(context, DESTINATION_ATTR);
    else
      destination = null;

    // If we have an onclick handler, always provide a destination
    if ((destination == null) &&
        supportsIntrinsicEvents(context))
    {
      Object onClick = getOnClick(context, node);

      if (onClick != null)
      {
        destination = "#";
      }
    }

    return destination;
  }

  protected final String getDestinationAttr(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalTextAttribute(context,
                                               node,
                                               DESTINATION_ATTR);
  }

  protected static boolean isSelected(
    UIXRenderingContext context,
    UINode node
    )
  {
    boolean selectedAttr = Boolean.TRUE.equals(node.getAttributeValue(context, SELECTED_ATTR));
    boolean linkProp = LinkUtils.isSelected(context);

    return (selectedAttr || linkProp);
  }

  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object styleClass = super.getStyleClass(context, node);

    if (styleClass == null)
    {
      styleClass = (isSelected(context, node))
                     ? AF_MENU_BUTTONS_TEXT_SELECTED_STYLE_CLASS
                     : (isDisabled(context, node))
                       ? AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS
                       : AF_MENU_BUTTONS_TEXT_STYLE_CLASS;
    }

    return styleClass;
  }




  // object indicating that there is no local property
  private static final Object _NONE = new Object();

  // object used to store the local copy of the onClick handler
  private static final Object _LOCAL_ON_CLICK_KEY = new Object();

}
