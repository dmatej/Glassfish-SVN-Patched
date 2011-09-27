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
package org.apache.myfaces.trinidadinternal.taglib.util;

import java.awt.Color;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.faces.application.Application;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.util.DateUtils;


/**
 * Utility class for Tag classes
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/taglib/util/TagUtils.java#1 $) $Date: 11-nov-2005.14:59:38 $
 *
 */
public final class TagUtils
{
  private TagUtils()
  {
  }

  public static void assertNotNull(Object object)
  {
    if (null == object)
      throw new NullPointerException();
  }

  // Helpful with tag auto generation. Though this isn't really required.
  /**
   * Return the same string. It is there for convenience and makes life easy
   * while auto generating tags.
   * @param value
   * @return
   */
  public static String getString(
    Object value)
  {
    if (value == null)
      return null;

    return value.toString();
  }

  /**
   * String --> boolean
   * @param value
   * @return
   */
  public static boolean getBoolean(
    Object  value)
  {
    if (value == null)
      return false;
    
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();

    return Boolean.valueOf(value.toString()).booleanValue();
  }

  /**
   * String --> int
   * @param value
   * @return
   */
  public static int getInteger(
    Object  value)
  {
    if (value == null)
      return 0;

    if (value instanceof Number)
      return ((Number) value).intValue();

    return Integer.valueOf(value.toString()).intValue();

  }

  /**
   * String --> long
   * @param value
   * @return
   */
  public static long getLong(
    Object      value)
  {
    if (value == null)
      return 0;

    return Long.valueOf(value.toString()).longValue();
  }

  /**
   * String --> long
   * @param value
   * @return
   */
  public static double getDouble(
    Object      value)
  {
    if (value == null)
      return 0;

    return Double.valueOf(value.toString()).doubleValue();

  }

  /**
   * String --> long
   * @param value
   * @return
   */
  public static float getFloat(
    Object      value)
  {
    if (value == null)
      return 0;

    return Float.valueOf(value.toString()).floatValue();
  }

  /**
   * These are normally NMTOKEN type in attributes
   * String --> String[]
   * @param value
   * @return
   */
  public static String[] getStringArray(
    Object  value) throws ParseException
  {
    if (value == null)
      return null;

    return _getTokensArray(value.toString());
  }

  /**
   *  ISO Date String --> Date
   * @param value
   * @return
   */
  public static Date getDate(
    Object   value)
  {
    if (value == null)
      return null;

    if (value instanceof Date)
      return ((Date) value);

    return _parseISODate(value.toString());
  }

  /**
  *  ISO Date String --> Date with time components maximized
  * @param value
  * @return
  */
  public static Date getDateWithMaxTime(
    Object value)
  {
    if (value == null)
      return null;

    if (value instanceof Date)
      return ((Date)value);

    Date d = _parseISODate(value.toString());
    Calendar c = Calendar.getInstance();
    TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
    if (tz != null)
      c.setTimeZone(tz);
    c.setTime(d);
    // Original value had 00:00:00 for hours,mins, seconds now maximize those
    // to get the latest time value for the date supplied.
    c.set(Calendar.HOUR_OF_DAY, 23);
    c.set(Calendar.MINUTE, 59);
    c.set(Calendar.SECOND, 59);
    c.set(Calendar.MILLISECOND, 999);
    return (c.getTime());
  }

  /**
   * String --> Locale
   * @param value
   * @return
   */
  public static Locale getLocale(
    Object      value)
  {
    if (value == null)
      return null;

    if (value instanceof Locale)
      return ((Locale) value);

    return _getLocale(value.toString());
  }

  /**
   * String --> TimeZone
   * @param value
   * @return
   */
  public static TimeZone getTimeZone(
    Object value)
  {
    if (value == null)
      return null;

    return DateUtils.getSupportedTimeZone(value.toString());
  }

  /**
   * Takes in a string that is a sequence of hex color codes, converts it as a
   *  java.util.List of java.awt.Color objects and returns it.
   * @throws ParseException In case of any parse errors upon such conversion.
   */
  public static List<Color> getColorList(Object value) throws ParseException
  {
    if (value == null)
      return null;

    String valueStr = value.toString();
    String[] tokenArray = _getTokensArray(valueStr);
    if (tokenArray == null)
      return null;

    String colorCode;
    List<Color> colorList = new ArrayList<Color>();
    for (int index=0; index < tokenArray.length; index++)
    {
      colorCode = tokenArray[index];

      //pu: If we do not have correct starter, stop here
      if (!colorCode.startsWith("#"))
        throw new ParseException(_LOG.getMessage(
          "COLOR_CODE_DOES_NOT_START_WITH_POUNDSIGN", new Object[]{colorCode, value}), valueStr.indexOf(colorCode));

      //pu: Allow NumberFormatException (RTE) to propogate as is, or transform to JspException ?.
      int rgb = Integer.parseInt(colorCode.substring(1), 16);

      //pu: CSSUtils used to cache and re-use color. Revisit if found required.
      colorList.add(new Color(rgb));
    }

    return colorList;
  }

  /**
   * Takes in a string that is a sequence of hex color codes, converts it to a
   *  java.awt.Color object and returns it.
   * @throws ParseException In case of any parse errors upon such conversion.
   */
  public static Color getColor(String value) throws ParseException
  {
    if (value == null)
      return null;

    String colorCode = value.toString();

    // If we do not have correct starter, stop here
    if (!colorCode.startsWith("#"))
    {
      throw new ParseException(_LOG.getMessage(
        "COLOR_CODE_DOES_NOT_START_WITH_POUNDSIGN",
        new Object[]{colorCode, value}), 0);
    }

    // Allow NumberFormatException (RTE) to propogate as is, or transform to JspException ?.
      int rgb = Integer.parseInt(colorCode.substring(1), 16);

      // CSSUtils used to cache and re-use color. Revisit if found required.
      return new Color(rgb);

  }

  /**
   * Takes a string that is a composite of tokens, extracts tokens delimited
   *  by any whitespace character sequence combination and returns a String
   *  array of such tokens.
   * @throws ParseException In case of invalid character in the specified
   *           composite. The only invalid character is a comma (',').
   */
  private static String[] _getTokensArray(String tokenComposite)
    throws ParseException
  {
    if (tokenComposite == null || "".equals(tokenComposite))
      return null;

    return XMLUtils.parseNameTokens(tokenComposite);
  }

  /**
   * Parse a string into a java.util.Date object.  The
   * string must be in ISO 9601 format (yyyy-MM-dd).
   * @todo why not throw the exception in a different format?
   *       why do we kill it here and return null?
   */
  static private final Date _parseISODate(String stringValue)
  {
    try
    {
      return _getDateFormat().parse(stringValue);
    }
    catch (ParseException pe)
    {
      _LOG.info("CANNOT_PARSE_VALUE_INTO_DATE_WITH_YYYY_MM_DD_PATTERN", stringValue);
      return null;
    }
  }

  private static Locale _getLocale(String locale)
  {
    String localeStr = locale.replace('-','_');
    String[] tokens = localeStr.split("[_]", 3);
    Locale locl = null;

    if ( tokens.length == 1)
    {
      locl = new Locale(tokens[0]); //lang
    }
    else if (tokens.length == 2)
    {
      locl = new Locale(tokens[0], tokens[1]); // lang + country
    }
    else if (tokens.length == 3 )
    {
      locl = new Locale(tokens[0], tokens[1], tokens[2]); // lang + country + variant
    }
    else
    {
      assert(tokens.length < 4); // tokens length should not be greater than 3.
    }
    return locl;
  }

  // We rely strictly on ISO 8601 formats
  private static DateFormat _getDateFormat()
  {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
    if (tz != null)
      sdf.setTimeZone(tz);
    return sdf;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TagUtils.class);

}
