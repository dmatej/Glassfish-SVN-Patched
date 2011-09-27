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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ImageRenderer.java#0 $) $Date: 10-nov-2005.18:53:58 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ImageRenderer extends XhtmlLafRenderer
{
  @Override
  protected void renderID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderNameAndID(context, node);
  }

  protected Object getSource(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return getFlippableURI(context, node, SOURCE_ATTR);
  }


  protected Object getLongDesc(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, LONG_DESC_URL_ATTR);
  }

  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, TEXT_ATTR);
  }


  /**
   * Returns the destination to use for the ImageRenderer.
   */
  protected Object getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object destination;
    if (supportsNavigation(context))
      destination = getDestinationAttr(context, node);
    else
      destination = null;

    // If we have an onclick handler, always provide a destination
    if ((destination == null) && supportsScripting(context))
    {
      Object onClick = getOnClick(context, node);

      if (onClick != null)
      {
        destination = "#";
      }
    }

    return destination;
  }

  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
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
      actionScript = action.getScript(context, node, Boolean.FALSE);
    }

    Object chainedScript = XhtmlLafUtils.getChainedJS(onClick,
                                                      actionScript,
                                                      true);

    // Store away the script for next time
    context.setLocalProperty(_LOCAL_ON_CLICK_KEY, chainedScript);
    return chainedScript;
  }

  protected Object getOnFocus(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_FOCUS_ATTR);
  }


  protected Object getDestinationAttr(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, DESTINATION_ATTR);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Don't bother with prerendering - it's all done in rendercontent
  }

  @Override
  protected final void postrender(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    // Don't bother with postrendering - it's all done in rendercontent
  }

  /**
   * Called to render the attributes of the <img> tag
   */
  protected void renderImageAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object source = _getLocalSource(context, node);

    renderEncodedResourceURI(context, SOURCE_ATTRIBUTE, source);
    renderAltAndTooltipForImage(context, getShortDesc(context, node));
    renderAttribute(context, node, WIDTH_ATTRIBUTE,  WIDTH_ATTR);
    renderAttribute(context, node, HEIGHT_ATTRIBUTE, HEIGHT_ATTR);
    renderAttribute(context, node, BORDER_ATTRIBUTE, BORDER_WIDTH_ATTR, "0");
    renderAttribute(context, "longdesc", getLongDesc(context, node));
    renderHAlign(context, node);
    _renderImageMap(context, node);
  }

  @Override
  protected void renderHAlign(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object hAlign = node.getAttributeValue(context, H_ALIGN_ATTR);

    if (hAlign==null)
    {
      TableRenderingContext tContext = 
        TableRenderingContext.getCurrentInstance();
      // =-= ACW: if we are inside a table, we must default the align
      // attribute to middle.  if we don't do this we get bug 2188751.
      //  if we always default to left, we get bug 2372048.
      // the 'left' and 'right' values have terrible vertical alignment.
      // 'middle' works best.
      if ((tContext != null) &&
          (tContext.getRenderStage().getStage() == RenderStage.DATA_STAGE))
      {
        hAlign = MIDDLE_ATTRIBUTE_VALUE;
      }
    }

    renderHAlign(context, hAlign);
  }

  /**
   * we render shortDesc as alt text on image. Do not render it as title
   * on the link.
   */
  @Override
  protected void renderShortDesc(UIXRenderingContext context,
                                 UINode node)
  {
  }

  /**
   * Do all of the rendering in renderContent--this does two things--it
   * allows us to make sure that we can't have childern and it ensures that
   * we don't need to call getSource() multiple times per render, as this
   * can be a little expensive for subclasses like ButtonRenderer.
   */
  @Override
  protected final void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the checkBox
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
      action.writeDependencies(context, node);

    // get the image source
    Object source = _getLocalSource(context, node);

    // get the destination
    Object destination = getDestination(context, node);

    boolean hasImage = (source != null);

    boolean hasLink = (destination != null) && !isDisabled(context, node);

    ResponseWriter writer = context.getResponseWriter();

    // determine our outer element
    String element = (hasLink)
      ? LINK_ELEMENT
      : (hasImage)
      ? IMAGE_ELEMENT
      : SPAN_ELEMENT;

    // start it
    writer.startElement(element, node.getUIComponent());

    // write our attributes
    renderAttributes(context, node);

    if (hasLink)
    {
      renderEncodedActionURI(context, HREF_ATTRIBUTE, destination);

      if (supportsAccessKeys(context))
      {
        renderAttribute(context, node, "accesskey", ACCESS_KEY_ATTR);
      }

      if (supportsTarget(context))
      {
        renderAttribute(context, node, TARGET_FRAME_ATTRIBUTE,
                        TARGET_FRAME_ATTR);
      }

      if (supportsScripting(context))
      {
        renderAttribute(context, node, "onblur",  ON_BLUR_ATTR);
        renderAttribute(context, "onfocus", getOnFocus(context, node));
      }

      if (hasImage)
      {
        // start the inner image tag
        writer.startElement(IMAGE_ELEMENT, null);
      }
    }

    // render the attributes of the image
    if (hasImage)
    {
      renderImageAttributes(context, node);
    }
    else
    {
      //
      // If we don't have an image and we have text, write the text as
      // the content.
      //
      Object text = getText(context, node);

      if (text != null)
      {
        writer.writeText(text, null);
      }
    }

    if (hasLink && hasImage)
    {
      // close the inner image tag
      writer.endElement(IMAGE_ELEMENT);
    }

    // finish our element
    writer.endElement(element);
  }


  /**
  * Render the image map attribute if specified
  * if imageMapType is "server", then render the ismap attribute
  * otherwise don't render any image map attribute
  */
  private void _renderImageMap(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object imageMapType = node.getAttributeValue(context, IMAGE_MAP_TYPE_ATTR);
    if (imageMapType != null)
    {
      if (imageMapType.equals(IMAGE_MAP_TYPE_SERVER))
      {
        renderAttribute(context, IS_MAP_ATTRIBUTE, Boolean.TRUE);
      }

    }
  }

  // Get the source attribute from a local property
  private Object _getLocalSource(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Use source AttributeKey as our local property key
    Object source = context.getLocalProperty(0, SOURCE_ATTR, _NONE);

    if (source != _NONE)
      return  source;

    source = getSource(context, node);

    context.setLocalProperty(SOURCE_ATTR, source);

    return source;
  }

  // object indicating that there is no local property
  private static final Object _NONE = new Object();

  // object used to store the local copy of the onClick handler
  private static final Object _LOCAL_ON_CLICK_KEY = new Object();
}
