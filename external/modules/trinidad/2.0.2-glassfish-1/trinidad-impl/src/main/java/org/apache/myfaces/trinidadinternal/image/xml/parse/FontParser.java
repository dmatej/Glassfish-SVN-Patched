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
package org.apache.myfaces.trinidadinternal.image.xml.parse;

import java.awt.Font;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for font elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/FontParser.java#0 $) $Date: 10-nov-2005.19:04:04 $
 */
public class FontParser extends BaseNodeParser
{
  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    // If we've got a name attribute, honor it
    _name = attrs.getValue(XMLConstants.FONT_NAME_NAME);

    // Check for a size attribute
    Integer size = ImageParseUtils.getIntegerAttributeValue(context,
                                               attrs,
                                               XMLConstants.FONT_SIZE_NAME);
    if (size != null)
      _size = size.intValue();

    // Check for a style attribute
    String style = attrs.getValue(XMLConstants.FONT_STYLE_NAME);
    if (style != null)
      _style = _parseStyle(style);
  }

  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    String name = (_name == null) ? _DEFAULT_FONT_NAME : _name;
    int size = (_size == -1) ? _DEFAULT_FONT_SIZE : _size;
    int style = (_style == -1) ? _DEFAULT_FONT_STYLE : _style;

    return new FontProxy(name, style, size);
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    if (localName.equals(XMLConstants.FONT_NAME_NAME) ||
        localName.equals(XMLConstants.FONT_SIZE_NAME) ||
        localName.equals(XMLConstants.FONT_STYLE_NAME))
    {
      return context.getParser(String.class, namespaceURI, localName);
    }

    return null;
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    // Unforunately, since the TextParser preserves all white space,
    // we are forced to strip off leading/trailing white space here.
    String value = _stripWhitespace((String)child);

    if (localName.equals(XMLConstants.FONT_NAME_NAME))
      _name = value;
    else if (localName.equals(XMLConstants.FONT_SIZE_NAME))
      _size = _parseSize(value);
    else if (localName.equals(XMLConstants.FONT_STYLE_NAME))
    {
      int style = _parseStyle(value);
      if (style != -1)
      {
        if (_style == -1)
          _style = style;
        else
          _style = (_style | style);
      }
    }
  }

  // Returns a int representing the size
  private int _parseSize(
    String       value
    )
  {
    if (value == null)
      return -1;

    try
    {
      return Integer.parseInt(value);
    }
    catch (NumberFormatException e)
    {
      ;
    }

    _LOG.warning(_SIZE_ERROR);

    return -1;
  }

  // Returns a int representing the style
  private int _parseStyle(
    String       value
    )
  {
    if (value == null)
      return -1;

    if (value.equalsIgnoreCase(XMLConstants.PLAIN_FONT_STYLE))
      return Font.PLAIN;
    if (value.equalsIgnoreCase(XMLConstants.ITALIC_FONT_STYLE))
      return Font.ITALIC;
    if (value.equalsIgnoreCase(XMLConstants.BOLD_FONT_STYLE))
      return Font.BOLD;

    _LOG.warning(_STYLE_ERROR);

    return -1;
  }

  // Strips whitespace from start/end of the string
  private static String _stripWhitespace(String str)
  {
    if (str == null)
      return null;

    int length = str.length();
    int startIndex = 0;

    while (startIndex < length)
    {
      if (Character.isWhitespace(str.charAt(startIndex)))
        startIndex++;
      else
        break;
    }

    int endIndex = length;
    while (endIndex > 0)
    {
      if (Character.isWhitespace(str.charAt(endIndex - 1)))
        endIndex--;
      else
        break;
    }

    if ((startIndex == 0) && (endIndex == length))
      return str;

    return str.substring(startIndex, endIndex);
  }

  private String  _name;
  private int     _style = -1;
  private int     _size = -1;

  // Default values
  private static final String _DEFAULT_FONT_NAME  = "dialog";
  private static final int    _DEFAULT_FONT_SIZE  = 10;
  private static final int    _DEFAULT_FONT_STYLE = Font.PLAIN;

  // Error messages
  private static final String _SIZE_ERROR =
    "Error while parsing font size value.  Font size must be an integer.";
  private static final String _STYLE_ERROR =
    "Error while parsing font style value.  Valid values are \"plain\", \"bold\" or \"italic\"";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FontParser.class);
}
