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
package org.apache.myfaces.trinidad.convert;

import java.awt.Color;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParseException;
import java.text.ParsePosition;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Format for colors.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/convert/ColorFormat.java#0 $) $Date: 10-nov-2005.19:09:10 $
 * @todo This class needs to be part of the public API - need to move it to 
 *        place where our public API exists, once we have figured it out.
 */
abstract class ColorFormat extends Format
{
  /**
   * Returns the value as a Color.
   */
  public final Color parse(
    String        source, 
    ParsePosition status)
  {
    return (Color)parseObject(source, status);
  }
  
  /**
   * Returns the value as a Color.
   */
  public final Color parse(
    String source) throws ParseException
  {
    return (Color)parseObject(source);
  }
  
  /**
   * Parses a string to produce an object.
   *
   * @exception ParseException if the specified string is invalid.
   */
  @Override
  public Object parseObject(
    String source) throws ParseException 
  {
    ParsePosition status = new ParsePosition(0);
    Object result = null;
    boolean illegalArgument = false;
    
    // We expect a IllegalArgumentException in case either of
    // R/G/B color comps were to be out of range.
    try
    {
      result = parseObject(source, status);
    }
    catch(IllegalArgumentException ie)
    {
      _LOG.warning(ie);
      illegalArgument = true;
    }

    int index = status.getIndex();
    if (illegalArgument || index == 0 ||
        (source != null && index < source.length())) 
    {
        throw new ParseException("Format.parseObject(String) failed",
            status.getErrorIndex());
    }
    return result;
  }

  /**
   * Returns the value as a Color.
   */
  @Override
  abstract public Object parseObject(
    String        source, 
    ParsePosition status);
    
  /**
   * Formats a Color into a color string.
   * 
   * @param color  the color value to be formatted into a color string
   * 
   * @return the formatted color string
   */
  public final String format(
    Color color)
  {
    return format(color, new StringBuffer(),new FieldPosition(0)).toString();
  }

  @Override
  public final StringBuffer format(
    Object obj, 
    StringBuffer toAppendTo,
    FieldPosition fieldPosition)
  {
    if (obj instanceof Color)
    {
      return format((Color)obj, toAppendTo, fieldPosition);
    }
    else if (obj instanceof Number)
    {
      return format(new Color(((Number)obj).intValue()),
                    toAppendTo, fieldPosition);
    }
    else 
    {
      throw 
        new IllegalArgumentException(_LOG.getMessage(
          "CANNOT_FORMAT_GIVEN_OBJECT_AS_COLOR"));
    }
  }
  
  abstract public StringBuffer format(
    Color color,
    StringBuffer toAppendTo,
    FieldPosition pos);
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ColorFormat.class);

  private static final long serialVersionUID = 1L;
}
