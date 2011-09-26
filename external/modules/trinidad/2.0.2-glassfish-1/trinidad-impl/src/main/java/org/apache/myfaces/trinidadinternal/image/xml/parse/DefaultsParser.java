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

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for defaults section of ImageGenerator XML document.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/DefaultsParser.java#0 $) $Date: 10-nov-2005.19:04:03 $
 */
public class DefaultsParser extends BaseImageProviderRequestParser
{
  public DefaultsParser()
  {
    super(_PROPERTY_COUNT);
  }

  /**
   * Override of BaseImageProviderRequestParser.isColorElement()
   * which adds in the borderColor element.
   */
  @Override
  protected boolean isColorElement(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.BORDER_COLOR_NAME)        ||
        localName.equals(XMLConstants.SELECTED_FOREGROUND_NAME) ||
        localName.equals(XMLConstants.SELECTED_BACKGROUND_NAME) ||
        localName.equals(XMLConstants.DISABLED_FOREGROUND_NAME) ||
        localName.equals(XMLConstants.DISABLED_BACKGROUND_NAME))
    {
      return true;
    }

    return super.isColorElement(namespaceURI, localName);
  }

  /**
   * Override of BaseImageProviderRequestParser.isFontElement()
   * which adds in the selected/disabled font elements.
   */
  @Override
  protected boolean isFontElement(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.SELECTED_FONT_NAME)  ||
        localName.equals(XMLConstants.DISABLED_FONT_NAME))
    {
      return true;
    }

    return super.isFontElement(namespaceURI, localName);
  }

  /**
   * Override of BaseImageProviderRequestParser.getColorKey() for
   * borderColor.
   */
  @Override
  protected Object getColorKey(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.BORDER_COLOR_NAME))
      return ImageConstants.BORDER_COLOR_KEY;
    if (localName.equals(XMLConstants.SELECTED_FOREGROUND_NAME))
      return ImageConstants.SELECTED_FOREGROUND_KEY;
    if (localName.equals(XMLConstants.SELECTED_BACKGROUND_NAME))
      return ImageConstants.SELECTED_BACKGROUND_KEY;
    if (localName.equals(XMLConstants.DISABLED_FOREGROUND_NAME))
      return ImageConstants.DISABLED_FOREGROUND_KEY;
    if (localName.equals(XMLConstants.DISABLED_BACKGROUND_NAME))
      return ImageConstants.DISABLED_BACKGROUND_KEY;

    return super.getColorKey(namespaceURI, localName);
  }

  /**
   * Override of BaseImageProviderRequestParser.getFontKey() for
   * selected/disabled fonts.
   */
  @Override
  protected Object getFontKey(String namespaceURI, String localName)
  {
    if (localName.equals(XMLConstants.SELECTED_FONT_NAME))
      return ImageConstants.SELECTED_FONT_KEY;
    if (localName.equals(XMLConstants.DISABLED_FONT_NAME))
      return ImageConstants.DISABLED_FONT_KEY;

    return super.getFontKey(namespaceURI, localName);
  }

  // Defaults support the following properties
  // 1.  BACKGROUND_KEY          (handled by BaseImageProviderRequestParser)
  // 2.  FOREGROUND_KEY          (handled by BaseImageProviderRequestParser)
  // 3.  FONT_KEY                (handled by BaseImageProviderRequestParser)
  // 4.  DIRECTION_KEY           (handled by BaseImageProviderRequestParser)
  // 5.  TEXT_ANTIALIAS_KEY      (handled by BaseImageProviderRequestParser)
  // 6.  DISABLED_KEY            (handled by BaseImageProviderRequestParser)
  // 7.  DISABLED_BACKGROUND_KEY (TabBar property)
  // 8.  DISABLED_FOREGROUND_KEY (TabBar property)
  // 9.  DISABLED_FONT_KEY       (TabBar property)
  // 10. SELECTED_BACKGROUND_KEY (TabBar property)
  // 11. SELECTED_FOREGROUND_KEY (TabBar property)
  // 12. SELECTED_FONT_KEY       (TabBar property)
  // 13. BORDER_COLOR_KEY        (Button property)
  private static final int _PROPERTY_COUNT = 13;
}
