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
package org.apache.myfaces.trinidadinternal.image.xml.encode;

import java.awt.Color;
import java.awt.Font;
import java.io.PrintWriter;
import java.util.Map;


import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.apache.myfaces.trinidad.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoderManager;
import org.apache.myfaces.trinidadinternal.image.util.MapArea;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.image.xml.ImageProviderRequestUtils;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

/**
 * Base class for XMLEncoders
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/encode/AbstractXMLEncoder.java#0 $) $Date: 10-nov-2005.19:04:44 $
 */
abstract public class AbstractXMLEncoder
  implements XMLEncoder, ImageConstants, XMLConstants
{
  /**
   * Implementation of XMLEncoder.encodeXML().
   */
  public void encodeXML(
    ImageContext context,
    String       namespaceURI,
    String       localName,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter out
    )
  {
    encodeStartTag(localName, false, false, out);
    encodeAttributes(context, properties, responseProperties, out);

    // If the namespace isn't Tecate, reset the namespace
    if (!ImageConstants.TECATE_NAMESPACE.equals(namespaceURI))
      encodeAttribute("xmlns", namespaceURI, out);

    encodeClose(false, out);
    encodeBody(context, properties, responseProperties, out);
    encodeEndTag(localName, out);
  }

  protected void encodeAttributes(
    ImageContext context,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter out
    )
  {
    Object o;

    if ((o = properties.get(TEXT_ANTIALIAS_KEY)) != null)
      encodeBooleanAttribute(TEXT_ANTIALIAS_ATTR, (Boolean)o, out);
    if ((o = properties.get(DISABLED_KEY)) != null)
      encodeBooleanAttribute(DISABLED_ATTR, (Boolean)o, out);
    if ((o = responseProperties.get(WIDTH_RESPONSE_KEY)) != null)
      encodeIntegerAttribute(WIDTH_ATTR, (Integer)o, out);
    if ((o = responseProperties.get(HEIGHT_RESPONSE_KEY)) != null)
      encodeIntegerAttribute(HEIGHT_ATTR, (Integer)o, out);
    if ((o = properties.get(ENCODING_TYPE_KEY)) != null)
    {
      // Don't bother to encode GIF type - it's the default
      String type = (String)o;
      if (!ImageEncoderManager.GIF_TYPE.equals(type))
        encodeAttribute(ENCODING_TYPE_ATTR, type, out);
    }

    _encodeDirectionAttribute(context, properties, out);
  }

  /**
   * Encodes the body content of the root element being encoded.
   */
  protected void encodeBody(
    ImageContext context,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter out
    )
  {
    Object o;

    if ((o = properties.get(FOREGROUND_KEY)) != null)
      encodeColor(FOREGROUND_NAME, (Color)o, out);
    if ((o = properties.get(BACKGROUND_KEY)) != null)
      encodeColor(BACKGROUND_NAME, (Color)o, out);
    if ((o = properties.get(SURROUNDING_COLOR_KEY)) != null)
    {
      // Don't bother to encode fully-transparent surrounding color
      Color color = (Color)o;
      if (color.getAlpha() != 0)
        encodeColor(SURROUNDING_COLOR_NAME, color, out);
    }
    if ((o = properties.get(FONT_KEY)) != null)
      encodeFont(FONT_NAME, o, out);
    if ((o = responseProperties.get(IMAGE_MAP_AREAS_RESPONSE_KEY)) != null)
    {
      ImageProviderRequestUtils.encodeImageMapAreas(context,
                                       null,  // Let namespace default
                                       XMLConstants.IMAGE_MAP_NAME,
                                       (MapArea[])o,
                                       out);

    }
  }

  /**
   * Encodes a color
   */
  static protected void encodeColor(String name, Color color, PrintWriter out)
  {
    encodeStartTag(name, false, false, out);
    encodeAttribute("red", IntegerUtils.getString(color.getRed()), out);
    encodeAttribute("green", IntegerUtils.getString(color.getGreen()), out);
    encodeAttribute("blue", IntegerUtils.getString(color.getBlue()), out);
    encodeClose(true, out);
  }

  /**
   * Encodes a font.  The specified object must be an
   * instance of AWT Font or org.apache.myfaces.trinidadinternal.style.util.FontProxy.
   */
  static protected void encodeFont(String name, Object font, PrintWriter out)
  {
    assert ((font instanceof FontProxy) || (font instanceof Font));

    String family = null;
    int style = Font.PLAIN;
    int size = 0;

    if (font instanceof Font)
    {
      family = ((Font)font).getName();
      style = ((Font)font).getStyle();
      size = ((Font)font).getSize();
    }
    else
    {
      FontProxy proxy = (FontProxy)font;
      family = proxy.getName();
      style = proxy.getStyle();
      size = proxy.getSize();
    }

    encodeStartTag(name, out);
    encodeValueTag("name", family, out);
    encodeValueTag("size", Integer.toString(size), out);

    if (style == 0)
    {
      encodeValueTag("style", "plain", out);
    }
    else
    {
      if ((style & Font.BOLD) != 0)
        encodeValueTag("style", "bold", out);
      if ((style & Font.ITALIC) != 0)
        encodeValueTag("style", "italic", out);
    }

    encodeEndTag(name, out);
  }

  /**
   * Encodes a start tag
   */
  static protected void encodeStartTag(String name, PrintWriter out)
  {
    encodeStartTag(name, true, false, out);
  }

  /**
   * Encodes a start tag
   */
  static protected void encodeStartTag(
    String name,
    boolean close,
    boolean end,
    PrintWriter out)
  {
    out.print('<');
    out.print(name);

    if (close)
      encodeClose(end, out);
  }

  /**
   * Encodes a closing of a start tag
   */
  static protected void encodeClose(boolean end, PrintWriter out)
  {
    if (end)
      out.println("/>");
    else
      out.println('>');
  }

  /**
   * Encodes an end tag
   */
  static protected void encodeEndTag(
    String name,
    PrintWriter out)
  {
    out.print("</");
    out.print(name);
    out.println('>');
  }

  /**
   * Encodes a boolean attribute
   */
  static protected void encodeBooleanAttribute(
    String name,
    Boolean value,
    PrintWriter out)
  {
    encodeAttribute(name, Boolean.TRUE.equals(value) ? "true" : "false", out);
  }

  /**
   * Encodes a Integer attribute
   */
  static protected void encodeIntegerAttribute(
    String name,
    Integer value,
    PrintWriter out)
  {
    encodeAttribute(name, IntegerUtils.getString(value.intValue()), out);
  }

  /**
   * Encodes an attribute
   */
  static protected void encodeAttribute(
    String name,
    String value,
    PrintWriter out)
  {
    out.print(' ');
    out.print(name);
    out.print("=\"");
    encodeText(value, out);
    out.print('"');
  }

  /**
   * Encodes a tag which contains a single textual value.
   */
  static protected void encodeValueTag(
    String name,
    String value,
    PrintWriter out)
  {
    out.print('<');
    out.print(name);
    out.print('>');
    encodeText(value, out);
    encodeEndTag(name, out);
  }

  /**
   * Encodes a tag which contains a single boolean value.
   */
  static protected void encodeBooleanValueTag(
    String name,
    Boolean value,
    PrintWriter out)
  {
    encodeValueTag(name, value.toString(), out);
  }

  static protected void encodeText(
    String      value,
    PrintWriter out)
  {
    if (value != null)
    {
      int length = value.length();

      int i;
      for (i = 0; i < length; i++)
      {
        char ch = value.charAt(i);
        if ((ch == '&') || (ch == '<') || (ch == '>') || (ch == '"'))
          break;
      }

      if (i == length)
      {
        out.print(value);
      }
      else
      {
        for (i = 0; i < length; i++)
        {
          char ch = value.charAt(i);
          if (ch == '&')
            out.print("&amp;");
          else if (ch == '<')
            out.print("&lt;");
          else if (ch == '>')
            out.print("&gt;");
          else if (ch == '"')
            out.print("&quot;");
          else
            out.print(ch);
        }
      }
    }
  }

  private final void _encodeDirectionAttribute(
    ImageContext context,
    Map<Object, Object> properties,
    PrintWriter out
    )
  {
    int direction = LocaleUtils.getReadingDirection(context.getLocaleContext());

    Integer value = (Integer)properties.get(DIRECTION_KEY);
    if (value != null)
    {
      if (value.intValue() != LocaleUtils.DIRECTION_DEFAULT)
        direction = value.intValue();
    }

    if (direction == LocaleUtils.DIRECTION_LEFTTORIGHT)
      encodeAttribute(DIRECTION_ATTR, LEFT_TO_RIGHT_DIRECTION, out);
    else if (direction == LocaleUtils.DIRECTION_RIGHTTOLEFT)
      encodeAttribute(DIRECTION_ATTR, RIGHT_TO_LEFT_DIRECTION, out);
  }
}
