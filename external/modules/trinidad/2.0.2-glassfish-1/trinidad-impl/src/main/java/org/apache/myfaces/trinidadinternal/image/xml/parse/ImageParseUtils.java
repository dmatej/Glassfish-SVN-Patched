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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParseErrorUtils;

/**
 * Utilities for image parsing.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ImageParseUtils.java#0 $) $Date: 10-nov-2005.19:04:40 $
 */
public class ImageParseUtils
{
  /**
   * Gets a Boolean attribute value.
   * <p>
   * Returns null if the attribute is not specified or can not be parsed.
   * Otherwise, returns Boolean.TRUE or Boolean.FALSE depending on the
   * attribute value.
   */
  public static Boolean getBooleanAttributeValue(
    ParseContext context,
    Attributes   attrs,
    String       localName
    )
  {
    String value = attrs.getValue(localName);
    if (value == null)
      return null;

    if ("true".equalsIgnoreCase(value))
      return Boolean.TRUE;
    if ("false".equalsIgnoreCase(value))
      return Boolean.FALSE;

    String message = ParseErrorUtils.getErrorMessage(context, _BOOLEAN_ERROR);
    if (_LOG.isWarning())
      _LOG.warning(message);

    return null;
  }

  /**
   * Gets an Integer attribute value.
   * <p>
   * Returns null if the attribute is not specified or can not be parsed.
   * Otherwise, returns an Integer object indicating the attribute value.
   */
  public static Integer getIntegerAttributeValue(
    ParseContext context,
    Attributes   attrs,
    String       localName
    )
  {
    String value = attrs.getValue(localName);
    if (value == null)
      return null;

    try
    {
      return Integer.parseInt(value);
    }
    catch (NumberFormatException e)
    {
      // Logged below.
      ;
    }

    String message = ParseErrorUtils.getErrorMessage(context, _INT_ERROR);
    if (_LOG.isWarning())
      _LOG.warning(message);

    return null;
  }

  private ImageParseUtils() {}

  // Error messages
  private static final String _INT_ERROR =
    "Error while parsing integer attribute value.";
  private static final String _BOOLEAN_ERROR =
    "Error while parsing boolean attribute value.";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ImageParseUtils.class);
}
