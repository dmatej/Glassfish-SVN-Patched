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

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for colorizedIcon elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ColorizedIconParser.java#0 $) $Date: 10-nov-2005.19:04:01 $
 */
public class ColorizedIconParser extends BaseImageProviderRequestParser
{
  public ColorizedIconParser()
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

    String source = getRequiredAttribute(context,
                                         attrs,
                                         XMLConstants.SOURCE_ATTR);

    if (source != null)
      setProperty(ImageConstants.SOURCE_KEY, source);
    else
      __errorOccured();

    String lafString = attrs.getValue(XMLConstants.LAF_ATTR);

    // If we didn't find the LAF string in the IMX file, we must
    // have an old IMX file.  Assume that we've got a BLAF icon
    if (lafString == null)
      lafString = "org.apache.myfaces.trinidadinternal.ui.laf.oracle.desktop.BrowserLookAndFeel";

    // Map old LAF strings to new strings.
    String newLafString = _mapOldLaf(lafString);
    if (newLafString != null)
      lafString = newLafString;

    Class<?> laf = null;
    try
    {
      laf = ClassLoaderUtils.loadClass(lafString);
    }
    catch ( ClassNotFoundException e )
    {
      if (_LOG.isWarning())
        _LOG.warning("laf \"" + lafString + "\" not found.");
    }

    setProperty(ImageConstants.LAF_KEY, laf);
    if (laf == null)
      __errorOccured();
  }

  static private String _mapOldLaf(String lafString)
  {
    if ("org.apache.myfaces.trinidadinternal.ui.laf.browser.BrowserLookAndFeel".equals(lafString))
      return "org.apache.myfaces.trinidadinternal.ui.laf.oracle.desktop.BrowserLookAndFeel";
    return null;
  }
  
  @Override
  protected boolean isColorElement(String namespaceURI, String localName)
  {
    if ((XMLConstants.DARK_COLOR_NAME.equals(localName)) ||
         XMLConstants.DARK_ACCENT_COLOR_NAME.equals(localName))
      return true;

    return super.isColorElement(namespaceURI, localName);
  }

  /**
   * Override of BaseImageProviderRequestParser.getColorKey() for
   * borderColor.
   */
  @Override
  protected Object getColorKey(String namespaceURI, String localName)
  {
    if (XMLConstants.DARK_COLOR_NAME.equals(localName))
      return ImageConstants.DARK_COLOR_KEY;
    if (XMLConstants.DARK_ACCENT_COLOR_NAME.equals(localName))
      return ImageConstants.DARK_ACCENT_COLOR_KEY;

    // Note - we treat "background" elements as "darkColor" for backward
    // compatibility.  This isn't quite correct, since the background color
    // may actually be the dark accent color, but what the heck.
    if (XMLConstants.BACKGROUND_NAME.equals(localName))
      return ImageConstants.DARK_COLOR_KEY;

    return super.getColorKey(namespaceURI, localName);
  }

  // Colorized icons have the following properties:
  // 1. SOURCE_KEY
  // 2. DIRECTION_KEY        (handled by BaseImageProviderRequestParser)
  // 3. WIDTH_RESPONSE_KEY   (handled by BaseImageProviderRequestParser)
  // 4. HEIGHT_RESPONSE_KEY  (handled by BaseImageProviderRequestParser)
  // 5. NAME_KEY             (handled by BaseImageProviderRequestParser)
  // 6. DARK_COLOR_KEY
  // 7. DARK_ACCENT_COLOR_KEY
  private static final int _PROPERTY_COUNT = 7;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColorizedIconParser.class);
}
