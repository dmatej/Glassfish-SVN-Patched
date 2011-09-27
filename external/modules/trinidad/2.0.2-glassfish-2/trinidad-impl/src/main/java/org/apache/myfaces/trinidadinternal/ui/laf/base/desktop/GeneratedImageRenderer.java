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

import java.awt.Color;
import java.awt.Font;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Collection;

import javax.faces.component.UIComponent;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.util.IntegerUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.style.ParsedPropertyKey;
import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Base class for Renderers that generate images.
 * This class needs to be deleted. Trinidad does not generate images.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/oracle/desktop/TecateRenderer.java#1 $) $Date: 11-nov-2005.14:59:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract class GeneratedImageRenderer extends HtmlLafRenderer
  implements ImageConstants
{


  protected void renderImage(
    UIXRenderingContext context,
    UINode node,
    ImageProviderResponse response
    )
    throws IOException
  {
    renderImage(context, node, response, null);
  }

  /**
   * we do not want shortDesc rendered by XhtmlLafRenderer as this puts it
   * as the title attribute; instead we write shortDesc as the alt text of the
   * image.
   */
  @Override
  protected void renderShortDesc(UIXRenderingContext context, UINode node)
  {
  }


  protected void renderImage(
    UIXRenderingContext      context,
    UINode                node,
    ImageProviderResponse response,
    String                mapName
    )
    throws IOException
  {
    // We assume that we have an image to render
    assert (response != null);

    boolean disabled    = isDisabled(context, node);
    boolean hasMap    = (response.getMapAreas() != null);
    Object  shortDesc = getShortDesc(context, node);

    boolean hasLink     = !disabled && !hasMap;
    Object destination  = hasLink
                           ? getDestination(context, node)
                           : null;

    renderImage(context,
                node,
                response,
                hasMap,
                mapName,
                shortDesc,
                destination);
  }

  protected void renderImage(
    UIXRenderingContext      context,
    UINode                node,
    ImageProviderResponse response,
    boolean               hasMap,
    String                mapName,
    Object                shortDesc,
    Object                destination
    )
    throws IOException
  {
    assert node != null;

    boolean hasLink     = (destination != null);
    Object  longDesc    = getLongDesc(context, node);
    String  imageStyle  = getImageStyle(context, node);
    String  imageStyleClass  = getImageStyleClass(context, node);

    ResponseWriter writer = context.getResponseWriter();
        UIComponent component = (node == null) ? null : node.getUIComponent();

    if (hasLink)
    {
      writer.startElement("a", component);
      renderEncodedActionURI(context, "href", destination);
      renderAttribute(context, node, "target", TARGET_FRAME_ATTR);

      // Don't render access key on Netscape... Netscape doesn't
      // support access keys - if this ever changes, it would
      // be confusing if we rendered the accessKey attr without
      // also underlining the access key in the corresponding text.
      if (!isNetscape(context))
      {
        renderButtonAccessKey(context, node);
      }

      // If we have a link, we render the standard attributes on
      // the link instead of on the image
      renderAttributes(context, node);
    }

    writer.startElement("img", component);

    // Write out all of the standard attrs
    if (!hasLink)
      renderAttributes(context, node);

    // Write out the image url
    writeCacheImageURI(context, "src", response.getImageURI());

    // Write out the description attrs.
    renderAltAndTooltipForImage(context, shortDesc);
    renderAttribute(context, "longdesc", longDesc);

    // Null out the border
    renderAttribute(context, "border", "0");

    // Render alignment.  Is this necessary?
    renderHAlign(context, node);

    // This is to address bug #2047577
    // Instead of adding an attribute to control placement of the button,
    // we just force it to middle (which is what everybody wants anyway).
    // We have to make sure we don't put the align attribute in twice.
    // We allow the hAlign attribute to take precedence.
    if (node.getAttributeValue(context, H_ALIGN_ATTR) == null)
    {
      Object valign = getVAlign(context, node);
      if (valign != null)
        renderAttribute(context, "align", valign);
    }

    // Render the width/height
    int width = response.getWidth();
    int height = response.getHeight();

    if (width != ImageProviderResponse.UNKNOWN_SIZE)
      renderAttribute(context, "width", IntegerUtils.getString(width));
    if (height != ImageProviderResponse.UNKNOWN_SIZE)
      renderAttribute(context, "height", IntegerUtils.getString(height));

    // The image map
    if (hasMap)
      writer.writeAttribute("usemap", "#" + mapName, null);

    if (imageStyle != null)
      renderAttribute(context, "style", imageStyle);
    if (imageStyleClass != null)
      renderStyleClassAttribute(context, imageStyleClass);

    writer.endElement("img");

    if (hasLink)
      writer.endElement("a");
  }

  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, TEXT_ATTR);
  }

  @Override
  protected Object getShortDesc(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object desc = node.getAttributeValue(context, SHORT_DESC_ATTR);

    if ((desc == null) && !isInaccessibleMode(context))
    {
      return getText(context, node);
    }

    return desc;
  }

  protected Object getImageName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ID_ATTR);
  }

  protected Object getLongDesc(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, LONG_DESC_URL_ATTR);
  }


  /**
   * Returns the destination to use for the GeneratedImageRenderer
   */
  protected Object getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (!supportsNavigation(context))
      return null;

    Object destination = node.getAttributeValue(context, DESTINATION_ATTR);

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


  protected String getImageStyle(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }

  protected String getImageStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }


  protected void renderButtonAccessKey(
    UIXRenderingContext context,
    UINode           node
    )    throws IOException
  {
      renderAttribute(context, node, "accesskey", ACCESS_KEY_ATTR);
  }

  static protected String getURLAttribute(
    UIXRenderingContext context,
    UINode       node,
    AttributeKey attrKey
    )
  {
    Object o = node.getAttributeValue(context, attrKey);
    if (o != null)
      return o.toString();

    return null;
  }

  // Returns the style for the specified name
  static protected CoreStyle getStyle(
    UIXRenderingContext context,
    UINode           node,
    String           name
    )
  {
      return null;
  }

  // Returns the vertical alignment
  protected Object getVAlign(UIXRenderingContext context, UINode node)
  {
    return null;
  }

  static protected int getFontStyle(
    UIXRenderingContext context,
    UINode node,
    CoreStyle classStyle,
    CoreStyle inlineStyle,
    String styleName
    )
  {
    int fontStyle = Font.PLAIN;
    int fontWeight = Font.PLAIN;
    boolean gotStyle = false;
    boolean gotWeight = false;
    Object value = null;

    // First, try getting font-style and font-weight from inline style
    if (inlineStyle != null)
    {
      value = _parseValue(inlineStyle, null, CoreStyle.FONT_STYLE_KEY);

      if (value != null)
      {
        fontStyle = _getAWTFontStyle(value);
        gotStyle = true;
      }

      value = _parseValue(inlineStyle, null, CoreStyle.FONT_WEIGHT_KEY);
      if (value != null)
      {
        fontWeight = _getAWTFontWeight(value);
        gotWeight = true;
      }
    }

    if (classStyle != null)
    {
      if (!gotStyle)
      {
        value = _parseValue(classStyle,
                            styleName, CoreStyle.FONT_STYLE_KEY);
        if (value != null)
          fontStyle = _getAWTFontStyle(value);
      }

      if (!gotWeight)
      {
        value = _parseValue(classStyle,
                            styleName, CoreStyle.FONT_WEIGHT_KEY);

        if (value != null)
          fontWeight = _getAWTFontWeight(value);
      }
    }

    return (fontStyle | fontWeight);
  }

  static protected int getFontSize(
    UIXRenderingContext context,
    UINode node,
    CoreStyle classStyle,
    CoreStyle inlineStyle,
    String styleName
    )
  {
    // First, try size from inline font
    if (inlineStyle != null)
    {
      Object value = _parseValue(inlineStyle,
                                 null, CoreStyle.FONT_SIZE_KEY);

      if (value instanceof Integer)
        return ((Integer)value).intValue();
    }

    if (styleName != null)
    {
      Object value = _parseValue(classStyle,
                                 styleName, CoreStyle.FONT_SIZE_KEY);

      if (value instanceof Integer)
        return ((Integer)value).intValue();
    }

    return _DEFAULT_FONT_SIZE;
  }

  @SuppressWarnings("unchecked")
  static protected Collection<Object> getFontFamilies(
    UIXRenderingContext context,
    UINode node,
    CoreStyle  style,
    String styleName
    )
  {
    if (style != null)
    {
      if(_parseValue(style, null, CoreStyle.FONT_FAMILIES_KEY) instanceof Collection)
      {
        return  (Collection<Object>)_parseValue(style, null, CoreStyle.FONT_FAMILIES_KEY);
      }
      else
      {
        Collection<Object> parsedValueList = new ArrayList<Object>();
        parsedValueList.add(_parseValue(style, null, CoreStyle.FONT_FAMILIES_KEY));
        return  parsedValueList;
      }
    }

    return null;
  }

  static protected Color getBackground(
    UIXRenderingContext context,
    UINode node,
    CoreStyle classStyle,
    CoreStyle inlineStyle,
    String styleName
    )
  {
    if (inlineStyle != null)
    {
      Color background = (Color)_parseValue(inlineStyle,
                                            null, CoreStyle.BACKGROUND_KEY);
      if (background != null)
        return background;

    }

    return (Color)
      _parseValue(classStyle, styleName, CoreStyle.BACKGROUND_KEY);
  }

  static protected Color getForeground(
    UIXRenderingContext context,
    UINode node,
    CoreStyle classStyle,
    CoreStyle inlineStyle,
    String styleName
    )
  {
    if (inlineStyle != null)
    {
      Color foreground = (Color)_parseValue(inlineStyle,
                                            null, CoreStyle.FOREGROUND_KEY);
      if (foreground != null)
        return foreground;

    }

    return (Color)
      _parseValue(classStyle, styleName, CoreStyle.FOREGROUND_KEY);
  }

  static protected Color getSurroundingColor(UIXRenderingContext context)
  {
    if (BaseDesktopUtils.supportsTransparentImages(context))
      return null;

    return BaseDesktopUtils.getBackgroundColor(context);
  }

  static protected boolean isTextAntialiased(
    UIXRenderingContext context,
    UINode node,
    CoreStyle classStyle,
    CoreStyle inlineStyle
    )
  {
    if (inlineStyle != null)
    {
      Object value = inlineStyle.getParsedProperty(CoreStyle.TEXT_ANTIALIAS_KEY);
      return Boolean.TRUE.equals(value);
    }

    if (classStyle != null)
    {
      Object value = classStyle.getParsedProperty(CoreStyle.TEXT_ANTIALIAS_KEY);
      return Boolean.TRUE.equals(value);
    }

    return false;
  }

  static private Object _parseValue(
    CoreStyle style,
    String styleName,
    ParsedPropertyKey key
    )
  {
    if (style == null)
      return null;

    Object value = null;

    try
    {
      value = style.getParsedProperty(key);
    }
    catch (PropertyParseException e)
    {
      if (_LOG.isWarning())
        _LOG.warning(styleName == null ?
                     "Error while parsing inline style" :
                     "Error while parsing style class \"" + styleName + "\"",
                     e);
    }

    return value;
  }

  static private int _getAWTFontStyle(Object style)
  {
    if (style == CoreStyle.ITALIC_FONT_STYLE)
      return Font.ITALIC;

    return Font.PLAIN;
  }

  static private int _getAWTFontWeight(Object weight)
  {
    if (weight == CoreStyle.BOLD_FONT_WEIGHT)
      return Font.BOLD;

    return Font.PLAIN;
  }

  static private final int _DEFAULT_FONT_SIZE = 12;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(GeneratedImageRenderer.class);
}
