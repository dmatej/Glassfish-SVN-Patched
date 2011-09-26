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

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;

import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for color elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ColorParser.java#0 $) $Date: 10-nov-2005.19:04:02 $
 */
public class ColorParser extends LeafNodeParser
{
  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    Color color = null;

    // First check for an rgb attribute
    String rgb = attrs.getValue(XMLConstants.RGB_ATTR);
    if (rgb != null)
    {
      try
      {
        color = CSSUtils.parseColor(rgb);
      }
      catch (PropertyParseException e)
      {
        _LOG.warning(_RGB_ERROR);
      }
    }

    if (color == null)
    {
      int red = _getColorAttr(context, attrs, XMLConstants.RED_ATTR);
      int green = _getColorAttr(context, attrs, XMLConstants.GREEN_ATTR);
      int blue = _getColorAttr(context, attrs, XMLConstants.BLUE_ATTR);

      color = new Color(red, green, blue);
    }

    return color;
  }

  // Gets the color attribute with the specified name.  If the attribute
  // is unspecified, returns 0.
  private int _getColorAttr(
    ParseContext context,
    Attributes   attrs,
    String       name
    )
  {
    Integer value = ImageParseUtils.getIntegerAttributeValue(context,
                                               attrs,
                                               name);
    if (value == null)
      return 0;

    int intValue = value.intValue();

    if ((intValue < 0) || (intValue > 255))
    {
      _LOG.warning(_COLOR_ERROR);

      intValue = (intValue < 0) ? 0 : 255;

    }

    return intValue;
  }

  // Error message
  private static final String _RGB_ERROR =
    "Error while parsing rgb attribute value.  Values must be specified in #RRGGBB format.";
  private static final String _COLOR_ERROR =
    "Error while parsing color attribute value.  Values must be between 0 and 255.";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColorParser.class);
}
