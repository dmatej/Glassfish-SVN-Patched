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

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for composite button elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/CompositeButtonParser.java#0 $) $Date: 10-nov-2005.19:04:03 $
 */
public class CompositeButtonParser extends BaseImageProviderRequestParser
{
  public CompositeButtonParser()
  {
    super(_PROPERTY_COUNT);
  }

  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    super.startElement(context, namespaceURI, localName, attrs);

    String lookAndFeelId = attrs.getValue(XMLConstants.LOOK_AND_FEEL_ID_ATTR);
    if (lookAndFeelId != null)
      setProperty(ImageConstants.LOOK_AND_FEEL_ID_KEY, lookAndFeelId);

    Character accessKey = getAccessKey(context, attrs);
    if (accessKey != null)
      setProperty(ImageConstants.ACCESS_KEY_KEY, accessKey);

    // Check for a text attribute.  This wasn't support in 1.0, 
    // but why not honor text attribute if it is set?
    String text = getText(context, attrs);
    if (text != null)
    {
      // Check for mnemonics, but only if we are parsing an ImageGenerator
      // XML file.  We avoid stripping mnemonics for IMX files, as IMX
      // files shouldn't contain inline mnemonics and even worse might 
      // contain unescaped ampersands.
      if (Boolean.TRUE.equals(context.getProperty(
                   ImageConstants.TECATE_NAMESPACE,
                   BaseImageProviderRequestParser.__STRIP_MNEMONICS_PROPERTY)))
      {
        // =-=ags mnemonic methods should move to oracle.bali.share.util.
        int index = __getMnemonicIndex(text);

        // Note - we always have to strip out mnemonics, even if
        // mnemonic index is -1, in case there is an escaped ampersand.
        text = __stripMnemonic(text);

        if (index != -1)
        {
          accessKey = Character.valueOf(text.charAt(index));
          setProperty(ImageConstants.ACCESS_KEY_KEY, accessKey);
        }
      }

      setProperty(ImageConstants.TEXT_KEY, text);
    }
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
    if (XMLConstants.TEXT_NAME.equals(localName)             ||
        XMLConstants.TRANSLATED_TEXT_NAME.equals(localName))
    {
      return context.getParser(String.class, namespaceURI, localName);
    }

    return super.startChildElement(context, namespaceURI, localName, attrs);
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
    if (child instanceof String)
      setProperty(ImageConstants.TEXT_KEY, child);
    else
      super.addCompletedChild(context, namespaceURI, localName, child);
  }

  /**
   * Gets the Character value of the "accessKey" attribute.
   * <p>
   * Returns null if the accessKey attribute is not specified.  
   * Otherwise, returns a Character indicating the value of the 
   * accessKey attribute.
   */
  protected Character getAccessKey(
    ParseContext context,
    Attributes   attrs
    )
  {
    String value = attrs.getValue(XMLConstants.ACCESS_KEY_ATTR);

    if ((value == null) || (value.length() == 0))
      return null;

    return Character.valueOf(value.charAt(0));
  }

  /**
   * Gets the String value of the "text" attribute.
   * <p>
   * Returns null if the text attribute is not specified.  
   * Otherwise, returns a String indicating the value of the 
   * text attribute.
   */
  protected String getText(
    ParseContext context,
    Attributes   attrs
    )
  {
    return attrs.getValue(XMLConstants.TEXT_ATTR);
  }

  /**
   * Gets the String value of the "version" attribute.
   * <p>
   * Returns null if the version attribute is not specified.  
   * Otherwise, returns a String indicating the value of the 
   * version attribute.
   */
  protected String getVersion(
    ParseContext context,
    Attributes   attrs
    )
  {
    return attrs.getValue(XMLConstants.VERSION_ATTR);
  }

  // Amazingly enough, button elements can actually have 15 properties:
  // 1.  TEXT_KEY
  // 2.  BACKGROUND_KEY       (handled by BaseImageProviderRequestParser)
  // 3.  FOREGROUND_KEY       (handled by BaseImageProviderRequestParser)
  // 4.  FONT_KEY             (handled by BaseImageProviderRequestParser)
  // 5.  DIRECTION_KEY        (handled by BaseImageProviderRequestParser)
  // 6.  WIDTH_RESPONSE_KEY   (handled by BaseImageProviderRequestParser)
  // 7.  HEIGHT_RESPONSE_KEY  (handled by BaseImageProviderRequestParser)
  // 8.  ACCESS_KEY_KEY
  // 9.  TEXT_ANTIALIAS_KEY   (handled by BaseImageProviderRequestParser)
  // 10. DISABLED_KEY         (handled by BaseImageProviderRequestParser)
  // 11. START_ROUNDED_KEY
  // 12. END_ROUNDED_KEY
  // 13. NAME_KEY             (handled by BaseImageProviderRequestParser)
  // 14. VERSION_KEY
  private static final int _PROPERTY_COUNT = 14;
}
