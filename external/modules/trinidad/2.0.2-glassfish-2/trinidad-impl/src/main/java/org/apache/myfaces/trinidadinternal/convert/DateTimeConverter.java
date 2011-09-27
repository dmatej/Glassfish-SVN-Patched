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
package org.apache.myfaces.trinidadinternal.convert;

import java.io.IOException;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.el.ValueExpression;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;


/**
 * <p>
 * This class implements client side equivalent of DateTimeConverter. This class
 * pushes all relevant information to the client side so that conversion can be
 * enabled at the client side.
 * </p>
 *
 * @version $Name: $ ($Revision:
 *          adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/convert/DateTimeConverter.java#0 $)
 *          $Date: 10-nov-2005.19:06:22 $
 */
@JSFConverter(
        name="tr:convertDateTime",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.DateTime",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.convert.ConvertDateTimeTag")
public class DateTimeConverter extends
    org.apache.myfaces.trinidad.convert.DateTimeConverter implements
    ClientConverter

{
  public DateTimeConverter()
  {
    super();
  }

  public DateTimeConverter(String pattern)
  {
    super(pattern);
  }

  public DateTimeConverter(String pattern, String secondaryPattern)
  {
    super(pattern, secondaryPattern);
  }

  @Override
  public String getAsString(FacesContext context, UIComponent component,
      Object value)
  {
    if (value == null)
      return null;

    GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
    // we support other types of dates, like oracle.jbo.domain.Date:
    if (!(value instanceof String) && (!(value instanceof Date)) && fac.isConvertible(value, Date.class))
    {
      value = fac.convert(value, Date.class);
    }
    return super.getAsString(context, component, value);
  }

  @Override
  public Object getAsObject(FacesContext context, UIComponent component,
      String value)
  {
    Object date = super.getAsObject(context, component, value);
    if (date == null)
      return null;
    date = __typeConvert(context, this, component, value, date);
    return date;
  }

  /**
   * Super class only returns instances of java.util.Date. However, the expected
   * type of the attribute might be java.sql.Timestamp. Therefore, we must get
   * the expected type and convert if necessary: bug 4549630:
   */
  static Object __typeConvert(FacesContext context, Converter converter,
      UIComponent component, String strValue, Object value) throws ConverterException
  {
    assert value != null;
    ValueExpression expression = component.getValueExpression("value");
    if (expression != null)
    {
      Class<?> expectedType = expression.getExpectedType();

      // If the expectedType is Object ask for the type which may be more specific
      if(expectedType == null || expectedType == Object.class)
      {
        expectedType = expression.getType(context.getELContext());
      }

      // Sometimes the type might be null, if it cannot be determined:
      if ((expectedType != null)
          && (!expectedType.isAssignableFrom(value.getClass())))
      {
        // sometimes we might have to return the date/number as a string.
        // see bug 4602629:
        if (expectedType == String.class)
        {
          ValueHolder holder = (ValueHolder) component;
          // if the submitted string is identical to the existing string
          // then there is no need to convert: bug 4620622:
          if (strValue.equals(holder.getValue()))
            return strValue;
          return converter.getAsString(context, component, value);
        } else
        {
          GenericConverterFactory fac = GenericConverterFactory
              .getCurrentInstance();
          try
          {
            value = fac.convert(value, expectedType);
          }
          catch(TypeConversionException e)
          {
            // Use underlying exception's message if TypeConversionException
            // wrapped exception raised by the converter
            Throwable cause = e.getCause();
            if (cause == null)
              cause = e;

            FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                                                MessageFactory.getString(context, UIXEditableValue.CONVERSION_MESSAGE_ID),
                                                cause.getLocalizedMessage());
            throw new ConverterException(msg, e);
          }
        }
      }
    }
    return value;
  }

  @SuppressWarnings("unchecked")
  public String getClientScript(FacesContext context, UIComponent component)
  {

    if (component == null)
    {
      _LOG
          .severe("The component is null, but it is needed for the client id, so no script written");
      return null;
    }

    // Add a JavaScript Object to store the datefield formats
    // on the client-side. We currently store the format string
    // for each and every field. It'd be more efficient to have
    // an array of formats, then store for each field the
    // index of the format, especially if we could delay outputting
    // these objects to when the <form> closes.
    String clientId = component.getClientId(context);

    if (clientId != null)
    {
      // =-=AEW Only if Javascript...
      Map<String, Object> requestMap =
        context.getExternalContext().getRequestMap();

      // this fetch could be at the place where we append, but has been
      // moved ahead to optimize use of StringBuilder
      String jsPattern = getJSPattern(context);
      String loc = _getLocaleString (context);

      // FIX - figure out size!!!
      // 127 chars for javascript + length of jspattern + locale + 12 chars for
      // tranforming to name in the worst case.
      StringBuilder buff = new StringBuilder(139 + jsPattern.length() + loc.length());

      if (requestMap.get(_PATTERN_WRITTEN_KEY) == null)
      {
        requestMap.put(_PATTERN_WRITTEN_KEY, Boolean.TRUE);
        // only create the _dfs object if it doesn't exist, so we don't
        // wipe out _dfs[xxx] values if we ppr the first date field on a
        // page with multiple date fields.
        buff.append("if(window['_dfs'] == undefined){window['_dfs']=new Object();}if(window['_dl'] == undefined){window['_dl']=new Object();}");
      }

      buff.append("_dfs[\"");
      buff.append(clientId);
      buff.append("\"]=");
      buff.append(jsPattern);
      buff.append(";");

      buff.append("_dl[\"");
      buff.append(clientId);
      buff.append("\"]=");
      buff.append(loc);
      buff.append(";");

      return buff.toString();
    } else
    {
      _LOG.severe("NULL_CLINET_ID_NO_SCRIPT_RENDERED");
    }

    return null;
  }

  public String getClientConversion(FacesContext context, UIComponent component)
  {
    String jsPattern = getJSPattern(context);
    Map<String, String> messages = new HashMap<String, String>();
    if (jsPattern != null)
    {
      String pattern = getPattern();

      if (pattern == null)
        pattern = getSecondaryPattern();

      String key = getViolationMessageKey(pattern);

      Object[] params = new Object[]
      {
          "{0}", "{1}", "{2}"
      };
      Object msgPattern = getMessagePattern(context, key, params, component);
      //if hintFormat is null, no custom hint for date, time or both has been specified
      String hintFormat = _getHint();

      FacesMessage msg = null;
      String detailMessage = null;

      if (msgPattern != null)
      {
        msg = MessageFactory.getMessage(context, key, msgPattern, params,
            component);
        detailMessage = XhtmlLafUtils.escapeJS(msg.getDetail());
      }

      String exampleString = XhtmlLafUtils.escapeJS(getExample(context));
      String escapedType = XhtmlLafUtils.escapeJS(getType().toUpperCase());

      StringBuilder outBuffer = new StringBuilder();
      outBuffer.append("new TrDateTimeConverter(");
      outBuffer.append(jsPattern);
      Locale loc = getLocale ();
      if (loc != null)
      {
        outBuffer.append(",'");
        outBuffer.append (loc.toString());
        outBuffer.append ("','");
      }
      else
      {
        outBuffer.append (",null,'");
      }
      outBuffer.append(exampleString);
      outBuffer.append("','");
      outBuffer.append(escapedType);
      outBuffer.append("'");

      if (msgPattern != null || hintFormat != null)
      {
        messages.put("detail", detailMessage);
        messages.put("hint", hintFormat);
        outBuffer.append(',');

        try
        {
          JsonUtils.writeMap(outBuffer, messages, false);
        }
        catch (IOException e)
        {
          outBuffer.append("null");
        }
      }

      outBuffer.append(')'); // 2

      return outBuffer.toString();
    } else
    {
      // no pattern-matchable date
      return null;
    }
  }

  public Collection<String> getClientImportNames()
  {
    // Load the library for the different locale, so that the locale elements
    // are available for the client converter and validator.
    if (_isDifferentLocale())
    {
      ArrayList<String> names = new ArrayList<String>(2);
      names.add ("TrDateTimeConverter()");
      // Load NamedLocaleInfoScriptlet  "LocaleInfo_<locale>"
      String sloc =  getLocale().toString();
      StringBuffer sb = new StringBuffer (11 + sloc.length());
      sb.append ("LocaleInfo_");
      sb.append (sloc);
      names.add (sb.toString());
      return names;
    }
    else
    {
      return _IMPORT_NAMES;
    }
  }

  public String getClientLibrarySource(FacesContext context)
  {
    return null;
  }

  /**
   * Returns the number of columns of text a field should have to fully display
   * the contents of a valid string.
   */
  public int getColumns()
  {

    // Bug 2084946: This is only a little bit of an improvement over the
    // current state of things. What we really need to do here is find the true
    // maximal length of the date field (expanding things like MMM into
    // "January", zzzz into "Pacific Daylight Time" etc.). We also need to
    // lazily fill a set of Hashtables to cache all the values and/or lengths
    // for all the locales.
    // This full expansion task is filed as bug #2236559
    int len;
    String pattern = getPattern();

    if (pattern != null)
    {
      // for now, just return the length of the date pattern
      len = pattern.length();
    } else
    {
      // If no pattern set, try converting to a pattern,
      // otherwise fall back to the length of "mm/dd/yyyy" -
      // the shortish format.

      String applyPattern = null;

      try
      {
        FacesContext context = FacesContext.getCurrentInstance();
        // this the pattern obtained by applying the styles
        Object format = getDateFormat(context, null, false, null);
        if (format instanceof SimpleDateFormat)
        {
          applyPattern = ((SimpleDateFormat) format).toPattern();
        }
      } catch (ConverterException e)
      {
        // Do nothing here. Any error that was there would have been reported
        ;
      }

      if (applyPattern == null)
      {
        // if we dont have the primary pattern or unable to get it using the
        // styles then apply based on the secondary pattern
        applyPattern = getSecondaryPattern();
      }
      if (applyPattern != null)
        len = applyPattern.length() + 2;
      else
        len = 10;
    }

    return len;
  }

  @Override
  protected Date getDate(FacesContext context, UIComponent component)
  {
    if (false)
    {
      // compile time check to make sure the super class method is overridden:
      super.getDate(context, component);
    }

    if (component instanceof ValueHolder)
    {
      Object value = ((ValueHolder) component).getValue();
      if (value == null || value instanceof String)
        return null;

      if (value instanceof Date)
      {
        return (Date) value;
      }
      GenericConverterFactory fac = GenericConverterFactory
          .getCurrentInstance();
      if (fac.isConvertible(value, Date.class))
      {
        return (Date) fac.convert(value, Date.class);
      }
    }
    return null;
  }

  /**
   * Override to represent the id of the TimeZone used by DateFormat as GMT
   * offset value so that we always format Date based on GMTOffsetTimeZone style
   * (z) instead of using standard short or long TimeZone names of Java, since
   * these names are not available in client side JavaScript.
   */
  @Override
  protected TimeZone getFormattingTimeZone(TimeZone tZone, Date targetDate)
  {
    TimeZone zone = (TimeZone) tZone.clone();

    // set the id as "GMT Sign Hours : Minutes"
    StringBuilder zoneId = new StringBuilder(9);
    int offset; 
    if (targetDate != null)
      offset = zone.getOffset (targetDate.getTime());
    else
      offset = zone.getRawOffset();

    if (offset < 0)
    {
      zoneId.append(_GMT_MINUS);
      // abs value
      offset = -offset;
    } else
    {
      zoneId.append(_GMT_PLUS);
    }

    int hours = offset / _MILLIS_PER_HOUR;
    if (hours < 10)
    {
      zoneId.append('0');
    }
    zoneId.append(hours);

    zoneId.append(':');

    int minutes = (offset % _MILLIS_PER_HOUR) / _MILLIS_PER_MINUTE;
    if (minutes < 10)
    {
      zoneId.append('0');
    }
    zoneId.append(minutes);

    zone.setID(zoneId.toString());
    return zone;
  }

  protected String getJSPattern(FacesContext context)
  {
    String jsPattern = null;
    String datePattern = getPattern();

    // no pattern, so see if we can extract pattern from format
    if (datePattern == null)
    {
      try
      {
        DateFormat format = getDateFormat(context, null, false, null);
        if ((format != null) && (format instanceof SimpleDateFormat))
        {
          datePattern = ((SimpleDateFormat) format).toPattern();
        } else
        {
          // no pattern available
          datePattern = _NO_JS_PATTERN;
        }
      } catch (ConverterException ce)
      {
        // Let us be lenient.. if pattern cannot be created
        datePattern = _NO_JS_PATTERN;
      }
    }
    // If secondary pattern is available push that to the Client side, thus
    // being lenient.
    if ((datePattern == null || datePattern == _NO_JS_PATTERN)
        && getSecondaryPattern() != null)
    {
      int length = getSecondaryPattern().length() * 2 + 2;
      StringBuilder outBuffer = new StringBuilder(length);
      _escapePattern(outBuffer, getSecondaryPattern());
      return outBuffer.toString();
    }

    if (datePattern != null)
    {
      String secondaryPattern = getSecondaryPattern();

      if (datePattern != _NO_JS_PATTERN)
      {
        int length = datePattern.length() * 2 + 2;
        if (secondaryPattern != null)
          length = length + 3 + secondaryPattern.length() * 2;

        StringBuilder outBuffer = new StringBuilder(length);
        jsPattern = _getEscapedPattern(outBuffer, datePattern, secondaryPattern);
      } else
      {
        jsPattern = datePattern;
      }
    }
    return jsPattern;
  }

  // get the escaped form of the pattern
  private static void _escapePattern(StringBuilder buffer, String pattern)
  {
    buffer.append('\'');
    XhtmlUtils.escapeJS(buffer, pattern);
    buffer.append('\'');
  }

  private String _getHint()
  {
    String type = getType();
    if(type.equals("date"))
    {
      return getHintDate();
    }
    else if (type.equals("both"))
    {
      return getHintBoth();
    }
    else
    {
      return getHintTime();
    }
  }

  private static String _getEscapedPattern(StringBuilder buffer, String pattern,
      String secondaryPattern)
  {
    if (secondaryPattern != null)
      buffer.append('[');

    // get the escaped form of the date pattern
    _escapePattern(buffer, pattern);

    if (secondaryPattern != null)
    {
      buffer.append(",'");
      XhtmlUtils.escapeJS(buffer, secondaryPattern);
      buffer.append("']");
    }
    return buffer.toString();
  }

  private String _getLocaleString (FacesContext context)
  {
    Locale dateTimeConverterLocale = getLocale();
    if (dateTimeConverterLocale != null)
    {
      Locale defaultLocale = RenderingContext.getCurrentInstance()
          .getLocaleContext().getFormattingLocale();
      if (!(dateTimeConverterLocale.equals (defaultLocale)))
      {
        String loc = dateTimeConverterLocale.toString();
        StringBuffer sb = new StringBuffer (2 + loc.length());
        sb.append ("'");
        sb.append (loc);
        sb.append ("'");
        return (sb.toString());
      }
    }
    return "null";
  }

  // Bug 4570591
  // for now, we are disabling the client-side validation when the
  // locale is not the page's default locale.
  /*
   * This method returns true if the locale specified for DateTimeConverter is
   * different from the locale specified in Adf-faces-config or the client
   * locale. If both are same or if no locale is specified for
   * DateTimeConverter, then this returns false.
   */
  private boolean _isDifferentLocale()
  {
    Locale dateTimeConverterLocale = getLocale();
    if (dateTimeConverterLocale != null)
    {
      Locale defaultLocale = RenderingContext.getCurrentInstance()
          .getLocaleContext().getFormattingLocale();
      return !dateTimeConverterLocale.equals(defaultLocale);
    }

    return false;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger
      .createTrinidadLogger(DateTimeConverter.class);

  // RenderingContext key indicating the _dateFormat object
  // has been created
  private static final String _PATTERN_WRITTEN_KEY = "org.apache.myfaces.trinidadinternal.convert.DateTimeConverter._PATTERN_WRITTEN";

  // String indicating that NO_JS_PATTERN is available
  private static final String _NO_JS_PATTERN = new String();

  private static final String _GMT_PLUS = "GMT+";

  private static final String _GMT_MINUS = "GMT-";

  private static final int _MILLIS_PER_HOUR = 60 * 60 * 1000;

  private static final int _MILLIS_PER_MINUTE = 60 * 1000;

  private static final Collection<String> _IMPORT_NAMES = Collections
      .singletonList("TrDateTimeConverter()");

}
