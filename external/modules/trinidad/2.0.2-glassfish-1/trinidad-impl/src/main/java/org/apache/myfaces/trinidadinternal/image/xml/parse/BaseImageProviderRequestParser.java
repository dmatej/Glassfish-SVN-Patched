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

import java.awt.Color;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequestImpl;
import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoderManager;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * Base class for NodeParsers which parse ImageProviderRequests.
 * <p>
 * This class parses the following attributes:
 * <ul>
 * <li>name          (NAME_KEY)
 * <li>width         (WIDTH_RESPONSE_KEY)
 * <li>height        (HEIGHT_RESPONSE_KEY)
 * <li>direction     (DIRECTION_KEY)
 * <li>disabled      (DISABLED_KEY)
 * <li>textAntialias (TEXT_ANTIALIAS_KEY)
 * <li>type          (ENCODING_TYPE_KEY)
 * </ul>
 * <p>
 * It also parses the following elements
 * <ul>
 * <li>background    (BACKGROUND_KEY)
 * <li>foreground    (FOREGROUND_KEY)
 * <li>font          (FONT_KEY)
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/BaseImageProviderRequestParser.java#0 $) $Date: 15-nov-2005.18:02:54 $
 */
public class BaseImageProviderRequestParser extends BaseNodeParser
{
  /**
   * Creates a NodeParser with enough storage to hold the number of
   * ImageProviderRequest properties specified by capacity.
   */
  protected BaseImageProviderRequestParser(int capacity)
  {
    // Tack on an extra bucket for the encoding
    _properties = new ArrayMap<Object, Object>(capacity + 1);
  }

  /**
   * Implementation of NodeParser.startElement
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    // Map the old namespaces to the new;  also handle the default
    // (empty) namespace
    if ((namespaceURI == null) ||
        (namespaceURI.length() == 0) ||
        "http://myfaces.apache.org/cabo/tecate".equals(namespaceURI))
      namespaceURI = ImageConstants.TECATE_NAMESPACE;

    // Initialize namespace/name
    _namespaceURI = namespaceURI;
    _localName = localName;

    String name = attrs.getValue(XMLConstants.NAME_ATTR);
    if (name != null)
      setProperty(ImageConstants.NAME_KEY, name);

    // Initialize the boolean properties
    Boolean textAntialias = isTextAntaliased(context, attrs);
    if (textAntialias != null)
      setProperty(ImageConstants.TEXT_ANTIALIAS_KEY, textAntialias);

    Boolean disabled = isDisabled(context, attrs);
    if (disabled != null)
      setProperty(ImageConstants.DISABLED_KEY, disabled);

    // Initialize the width/height
    Integer width = getWidth(context, attrs);
    if (width != null)
      setProperty(ImageConstants.WIDTH_RESPONSE_KEY, width);

    Integer height = getHeight(context, attrs);
    if (height != null)
      setProperty(ImageConstants.HEIGHT_RESPONSE_KEY, height);

    // Initialize the direction
    Integer direction = getDirection(context, attrs);
    if (direction != null)
      setProperty(ImageConstants.DIRECTION_KEY, direction);

    // Intialize the encoding type
    String encoding = getEncodingType(context, attrs);

    // Defaul the encoding to GIF
    if (encoding == null)
      encoding = ImageEncoderManager.GIF_TYPE;

    setProperty(ImageConstants.ENCODING_TYPE_KEY, encoding);
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
    if (_error)
      return null;

    return new ImageProviderRequestImpl(_namespaceURI,
                                        _localName,
                                        _properties);
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
    if (isColorElement(namespaceURI, localName))
      return context.getParser(Color.class, namespaceURI, localName);
    else if (isFontElement(namespaceURI, localName))
      return context.getParser(FontProxy.class, namespaceURI, localName);
    else if ("colorScheme".equals(localName))
    {
      // IMX files generated with Cabo 1.0 contain <colorScheme> elements.
      // Rather than warning for every IMX file, just blow off this error,
      // which doesn't prevent the image from being loaded into the cache.
      return getIgnoreParser();
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
    if (isColorElement(namespaceURI, localName))
      setProperty(getColorKey(namespaceURI, localName), child);
    else if (isFontElement(namespaceURI, localName))
      setProperty(getFontKey(namespaceURI, localName), child);
  }

  /**
   * Sets a property to be included in the ImageProviderRequest
   */
  protected void setProperty(Object key, Object value)
  {
    _properties.put(key, value);
  }

  /**
   * Tests whether the specified element is a color.
   * Subclasses should override this method to indicate that
   * a specified element should be parsed to a Color.  getColorKey()
   * should also be overridden to return the ImageProviderRequest
   * property key to use for any color element.
   */
  protected boolean isColorElement(String namespaceURI, String localName)
  {
    return (localName.equals(XMLConstants.BACKGROUND_NAME) ||
            localName.equals(XMLConstants.FOREGROUND_NAME) ||
            localName.equals(XMLConstants.SURROUNDING_COLOR_NAME));
  }

  /**
   * Tests whether the specified element is a font.
   * Subclasses should override this method to indicate that
   * a specified element should be parsed to a FontProxy.  getFontKey()
   * should also be overridden to return the ImageProviderRequest
   * property key to use for any font element.
   */
  protected boolean isFontElement(String namespaceURI, String localName)
  {
    return (localName.equals(XMLConstants.FONT_NAME));
  }

  /**
   * Returns the key to use for storing the color associated with the
   * specified element in the ImageProviderRequest.
   */
  protected Object getColorKey(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.BACKGROUND_NAME))
      return ImageConstants.BACKGROUND_KEY;
    if (localName.equals(XMLConstants.FOREGROUND_NAME))
      return ImageConstants.FOREGROUND_KEY;
    if (localName.equals(XMLConstants.SURROUNDING_COLOR_NAME))
      return ImageConstants.SURROUNDING_COLOR_KEY;

    return null;
  }

  /**
   * Returns the key to use for storing the font associated with the
   * specified element in the ImageProviderRequest.
   */
  protected Object getFontKey(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.FONT_NAME))
      return ImageConstants.FONT_KEY;

    return null;
  }

  /**
   * Gets the boolean value of the "textAntialias" attribute.
   * <p>
   * Returns null if the textAntialias attribute is not specified.
   * Otherwise, returns a Boolean indicating the value of the
   * textAntialias attribute.
   */
  protected Boolean isTextAntaliased(
    ParseContext context,
    Attributes   attrs
    )
  {
    return ImageParseUtils.getBooleanAttributeValue(context,
                                             attrs,
                                             XMLConstants.TEXT_ANTIALIAS_ATTR);
  }

  /**
   * Gets the Boolean value of the "disabled" attribute.
   * <p>
   * Returns null if the disabled attribute is not specified.
   * Otherwise, returns a Boolean indicating the value of the
   * disabled attribute.
   */
  protected Boolean isDisabled(
    ParseContext context,
    Attributes   attrs
    )
  {
    return ImageParseUtils.getBooleanAttributeValue(context,
                                               attrs,
                                               XMLConstants.DISABLED_ATTR);
  }

  /**
   * Gets the Integer value of the "width" attribute.
   * <p>
   * Returns null if the width attribute is not specified.
   * Otherwise, returns an Integer indicating the value of the
   * width attribute.
   */
  protected Integer getWidth(
    ParseContext context,
    Attributes   attrs
    )
  {
    return ImageParseUtils.getIntegerAttributeValue(context,
                                               attrs,
                                               XMLConstants.WIDTH_ATTR);
  }

  /**
   * Gets the Integer value of the "height" attribute.
   * <p>
   * Returns null if the height attribute is not specified.
   * Otherwise, returns an Integer indicating the value of the
   * height attribute.
   */
  protected Integer getHeight(
    ParseContext context,
    Attributes   attrs
    )
  {
    return ImageParseUtils.getIntegerAttributeValue(context,
                                               attrs,
                                               XMLConstants.HEIGHT_ATTR);
  }

  /**
   * Gets the Integer value of the "direction" attribute.
   * <p>
   * Returns LocaleUtils.DIRECTION_RIGHTTOLEFT for "rtl" directions,
   * LocaleUtils.DIRECTION_LEFTTORIGHT for "ltr" directions, or
   * LocaleUtils.DIRECTION_DEFAULT if the direction is not specified.
   */
  protected Integer getDirection(
    ParseContext context,
    Attributes   attrs
    )
  {
    String value = attrs.getValue(XMLConstants.DIRECTION_ATTR);

    if (value == null)

      return null;

    if (value.equals(XMLConstants.LEFT_TO_RIGHT_DIRECTION))
      return LocaleUtils.DIRECTION_LEFTTORIGHT;
    if (value.equals(XMLConstants.RIGHT_TO_LEFT_DIRECTION))
      return LocaleUtils.DIRECTION_RIGHTTOLEFT;

    // If it isn't "rtl" or "ltr", it's an error
    _LOG.warning(_DIRECTION_ERROR);

    return null;
  }

  /**
   * Gets the image encoding type.
   * <p>
   * Returns one of the ImageEncoderManager.*_TYPE constants.
   */
  protected String getEncodingType(
    ParseContext context,
    Attributes   attrs
    )
  {
    String value = attrs.getValue(XMLConstants.ENCODING_TYPE_ATTR);

    if (value == null)
      return null;

    // Intern the value before we return it so that it
    // it will be indentical to the String constants
    // defined by ImageEncoderManager.
    return value.intern();
  }

  void __errorOccured()
  {
    _error = true;
  }

  static int __getMnemonicIndex(String string)
  {
    if (string == null)
    {
      return _MNEMONIC_INDEX_NONE;
    }

    // Minus one, because a trailing ampersand doesn't count
    int lengthMinusOne = string.length() - 1;
    int i = 0;       // Index in the source sting
    int count = 0;

    while (i < lengthMinusOne)
    {
      int index = string.indexOf(_MNEMONIC_INDICATOR, i);
      // Are we at the end of the string?
      if ((index == -1) || (index >= lengthMinusOne))
        break;

      // if this isn't a double ampersand, return
      if (string.charAt(index + 1) != _MNEMONIC_INDICATOR)
        // This index has to be relative to the _stripped_ string,
        // so subtract off one character for every double ampersand found
        return (index - count);

      // Skip over the double ampersand
      i = index + 2;
      count++;
    }

    return _MNEMONIC_INDEX_NONE;
  }

  static String __stripMnemonic(String string)
  {
    if (string == null)
    {
      return null;
    }

    int length = string.length();

    // Single character (or empty) strings can't have a mnemonic
    if (length <= 1)
      return string;

    StringBuffer buffer = null;
    int i = 0;

    while (i < length)
    {
      int index = string.indexOf(_MNEMONIC_INDICATOR, i);

      // We've reached the append.  Append the rest of the
      // string to the buffer, if one exists, then exit
      if ((index < 0) || (index >= length - 1))
      {
        if (buffer != null)
          buffer.append(string.substring(i));

        break;
      }

      if (buffer == null)
      {
        // If the string starts with an ampersand, but not a double
        // ampersand, then we just want to return
        // stripMnemonic(string.substring(1)).  This is basically
        // what we do here, only I've optimized the tail recursion away.
        if ((index == 0) && (string.charAt(1) != _MNEMONIC_INDICATOR))
        {
          string = string.substring(1);
          length--;
          continue;
        }
        else
        {
          // Allocate the buffer.  We can reserve only space
          // (length - 1), because, by now, we know there's at least
          // 1 ampersand
          buffer = new StringBuffer(length - 1);
        }
      }

      // Append the bits of the string before the ampersand
      buffer.append(string.substring(i, index));

      // And append the character after the ampersand
      buffer.append(string.charAt(index + 1));

      // And skip to after that character
      i = index + 2;
    }

    // If we never allocated a buffer, then there's no mnemonic
    // at all, and we can just return the whole string
    if (buffer == null)
      return string;

    return new String(buffer);
  }


  private BaseImageProviderRequestParser() {}

  private String     _namespaceURI;
  private String     _localName;
  private Map<Object, Object> _properties;

  private boolean   _error;

  // Error messages
  private static final String _DIRECTION_ERROR =
    "Error while parsing direction attribute value.";

  // =-=ags Stolen from StringUtils
  private static final int _MNEMONIC_INDEX_NONE = -1;
  private static final char _MNEMONIC_INDICATOR = '&';

  // This is to allow inline mnemonic stripping to be
  // specifically enabled for ImageGenerator XML files, but not
  // for IMX files.  IMX files should never have inline mnemonics.
  // We avoid stripping mnemonics for IMX files as we
  // can end up stripping ampersands which would should be treated as
  // part of the text.  We could escape ampersands in IMX files, but
  // that gets messy too, as we would need to escape and unescape all
  // text values and attributes.  No thanks.
  static Object __STRIP_MNEMONICS_PROPERTY = "stripMnemonics";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BaseImageProviderRequestParser.class);
}
