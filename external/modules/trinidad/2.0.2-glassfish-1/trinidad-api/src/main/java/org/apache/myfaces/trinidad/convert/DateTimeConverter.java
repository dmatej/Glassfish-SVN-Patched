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

import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TimeZone;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFProperty;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.util.MessageFactory;


/**
 * <p>{@link Converter} implementation for <code>java.util.Date</code>
 * values. Converts an strings to and from java.util.Date objects.</p>
 *
 * The converter has additonal features than standard JSF
 * {@link javax.faces.convert.DateTimeConverter}.
 *
 * New dateStyle <code>shortish</code> has been introduced. Shortish is identical
 * to <code>short</code> but forces the year to be a full four digits.
 * If dateStyle is not set, then <code>dateStyle</code> defaults to
 * <code>shortish</code>.
 *
 *  <p>Timezone can be set per web-app in trinidad-config.xml configuration file.
 * If <code>timeZone</code> is not set on the converter, then timezone will be defaulted to the
 * value set in trinidad-config.xml configuration file. If it is not set in the
 * configuration file, then it will be defaulted to GMT.</p>
 *
 * <p>The converter always allows a level of <code>leniency</code> while converting
 * user input value to date to the following extent.
 * <ul>
 * <li>A converter with associated pattern 'MMM' for month, when attached to any
 * value holder, will accept values with month specified in the form 'MM' or 'M'
 * as valid.</li>
 * <li>Allows use of separators '-' or '.' or '/' irrespective of the separator
 * specified in the associated pattern.</li>
 * <li>The leniency is applicable to both 'pattern' and 'secondaryPattern'.</li>
 * </ul></p>
 * <p>
 * For example:</br>
 * When pattern on the converter is set to "MMM/d/yyyy" the following inputs
 * are tolerated as valid by the converter.</br>
 * <dl>
 * <dt>Jan/4/2004</dt>
 * <dt>Jan-4-2004</dt>
 * <dt>Jan.4.2004</dt>
 * <dt>01/4/2004</dt>
 * <dt>01-4-2004</dt>
 * <dt>01.4.2004</dt>
 * <dt>1/4/2004</dt>
 * <dt>1-4-2004</dt>
 * <dt>1.4.2004</dt>
 * </dl>
 * </p>
 *
 * The detail part of faces message,for conversion errors can be customized
 * by overriding the message associated with each CONVERT_[TYPE]_MESSAGE_ID.
 *
 * <p>The methods used for customizing the detail message associated with each id
 * is given below:</p>
 * <ol>
 * <li>{@link #CONVERT_DATE_MESSAGE_ID} - {@link #setMessageDetailConvertDate(String)}</li>
 * <li>{@link #CONVERT_TIME_MESSAGE_ID} - {@link #setMessageDetailConvertTime(String) }</li>
 * <li>{@link #CONVERT_BOTH_MESSAGE_ID} - {@link #setMessageDetailConvertBoth(String) }</li>
 * </ol> The custom messages can contain placeholders, which will be replaced with
 * values as specified in its corresponding message id.
 *
 * <p>The <code>getAsObject()</code> method parses a String into a
 * <code>java.util.Date</code>, according to the following algorithm:</p>
 * <ul>
 * <li>If the specified String is null, return
 *     a <code>null</code>.  Otherwise, trim leading and trailing
 *     whitespace before proceeding.</li>
 * <li>If the specified String - after trimming - has a zero length,
 *     return <code>null</code>.</li>
 * <li>If the <code>locale</code> property is not null,
 *     use that <code>Locale</code> for managing parsing.  Otherwise, use the
 *     <code>Locale</code> from the <code>UIViewRoot</code>.</li>
 * <li>If a <code>pattern</code> has been specified, its syntax must conform
 *     the rules specified by <code>java.text.SimpleDateFormat</code>.  Such
 *     a pattern will be used to parse, and the <code>type</code>,
 *     <code>dateStyle</code>, and <code>timeStyle</code> properties
 *     will be ignored.</li>
 * <li>If a <code>pattern</code> has not been specified, parsing will be based
 *     on the <code>type</code> property, which expects a date value, a time
 *     value, or both.  Any date and time values included will be parsed in
 *     accordance to the styles specified by <code>dateStyle</code> and
 *     <code>timeStyle</code>, respectively.</li>
 * <li>If conversion fails with <code>pattern</code> or <code>style</code>
 *     and if <code> secondaryPattern</code> is set, re parsers based on the
 *     <code>secondaryPattern</code>. Syntax for <code>secondaryPattern</code>
 *     must conform to the rules specified by
 *     <code>java.text.SimpleDateFormat</code>.</li>
 * <li>Parsing is lenient as outlined earlier and is not the same as setting
 *     leniency on <code>java.text.DateFormat</code>
 * </ul>
 *
 * <p>The <code>getAsString()</code> method expects a value of type
 * <code>java.util.Date</code> (or a subclass), and creates a formatted
 * String according to the following algorithm:</p>
 * <ul>
 * <li>If the specified value is null, return a zero-length String.</li>
 * <li>If the specified value is a String, return it unmodified.</li>
 * <li>If the <code>locale</code> property is not null,
 *     use that <code>Locale</code> for managing formatting.  Otherwise, use the
 *     <code>Locale</code> from the <code>UIViewRoot</code>.</li>
 * <li>If a <code>pattern</code> has been specified, its syntax must conform
 *     the rules specified by <code>java.text.SimpleDateFormat</code>. Such
 *     a pattern will be used to format, and the <code>type</code>,
 *     <code>dateStyle</code>, and <code>timeStyle</code> properties
 *     will be ignored.</li>
 * <li>If a <code>pattern</code> has not been specified, formatting will be
 *     based on the <code>type</code> property, which includes a date value,
 *     a time value, or both into the formatted String.  Any date and time
 *     values included will be formatted in accordance to the styles specified
 *     by <code>dateStyle</code> and <code>timeStyle</code>, respectively.</li>
 * <li><code>secondaryPattern</code> even if set is never used for formatting
 *     to a String</li>
 * </ul>
 *
 * @see #CONVERT_DATE_MESSAGE_ID
 * @see #CONVERT_TIME_MESSAGE_ID
 * @see #CONVERT_BOTH_MESSAGE_ID
 * @see java.text.DateFormat
 * @see java.text.SimpleDateFormat
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/convert/DateTimeConverter.java#0 $) $Date: 10-nov-2005.19:09:11 $
 */
@JSFConverter(configExcluded=true)
public class DateTimeConverter extends javax.faces.convert.DateTimeConverter
                               implements Converter, StateHolder

{

  /**
   * <p>Standard converter id for this converter.</p>
   */
  public static final String CONVERTER_ID = "org.apache.myfaces.trinidad.DateTime";

  /**
   * <p>The message identifier of the FacesMessage to be created if
   * the value cannot be converted to a date, when <code>pattern</code>
   * is null or not set and <code>type</code> is set to <code>'date'</code>.
   * Or when failures occurs when value cannot be converted to a date
   * based on the pattern set. The message format string for this message
   * may optionally include <code>{0}</code>, <code>{1}</code>, <code>{4}</code>
   * placeholdes, which will be replaced  by input value, component label
   * and example date based on the <code>pattern</code> or based on the
   * <code>style</code> when <code>type</code> is set to <code>'date'</code>.</p>
   */
  public static final String CONVERT_DATE_MESSAGE_ID =
      "org.apache.myfaces.trinidad.convert.DateTimeConverter.CONVERT_DATE";

  /**
   * <p>The message identifier of the FacesMessage to be created if
   * the value cannot be converted date time object, when <code>type</code>
   * is set to <code>'time'</code> and pattern is null or not set.
   * The message format string for this message may optionally include
   * <code>{0}</code>, <code>{1}</code>, <code>{4}</code>
   * placeholdes, which will be replaced  by input value, component label
   * and a time example, based on the <code>timeStyle</code>
   * set in the converter.</p>
   */
  public static final String CONVERT_TIME_MESSAGE_ID =
      "org.apache.myfaces.trinidad.convert.DateTimeConverter.CONVERT_TIME";

  /**
   * <p>The message identifier of the FacesMessage to be created if
   * the value cannot be converted to a date when <code>type</code>
   * is set to <code>'both'</code> and pattern is either null or not set. The
   *  message format string for this message may optionally include
   * <code>{0}</code>, <code>{1}</code>, <code>{4}</code>
   * placeholdes, which will be replaced  by input value, component label
   * and a date-time example, based on the <code>dateStyle</code> and
   * <code>timeStyle</code> set in the converter.</p>
   */
  public static final String CONVERT_BOTH_MESSAGE_ID =
      "org.apache.myfaces.trinidad.convert.DateTimeConverter.CONVERT_BOTH";

  /**
   * Creates a DateTimeConverter
   */
  public DateTimeConverter()
  {
  }

  /**
   * Creates a DateTimeConverter with the specified SimpleDateFormat format
   * pattern
   * @param pattern a primary pattern;  this will be used to format
   *        and parser strings.
   */
  public DateTimeConverter(String pattern)
  {
    this();
    setPattern(pattern);
  }

  /**
   * Creates a DateTimeConverter with the specified SimpleDateFormat format
   * pattern and a secondary pattern.
   * @param pattern a primary pattern;  this will be used to format
   *        and parser strings.
   * @param secondaryPattern a second pattern, which will be used
   *        as a second attempt to parse a string if the primary pattern or
   *        styles fail, but is never used for formatting strings.
   */
  public DateTimeConverter(String pattern, String secondaryPattern)
  {
     this(pattern);
     setSecondaryPattern(secondaryPattern);

  }

  /**
   * <p>Convert the specified string value, which is associated with
   * the specified {@link UIComponent}, into a java.util.Date object
   * based on the values set.</p>
   *
   * @param context {@link FacesContext} for the request being processed
   * @param component {@link UIComponent} with which this model object
   *  value is associated
   * @param value String value to be converted (may be <code>null</code>)
   *
   * @return <code>null</code> if the value to convert is <code>null</code>,
   *  otherwise return a java.util.Date object.
   *
   * @exception ConverterException if conversion cannot be successfully
   *  performed
   * @exception NullPointerException if <code>context</code> or
   *  <code>component</code> is <code>null</code>
   *
   * @exception IllegalArgumentException if the <code>value</code> is of
   * type other than {@link java.util.Date}, {@link java.lang.String}. The
   * <code>value</code> can be null.
   */
  @Override
  public Object getAsObject(
    FacesContext context,
    UIComponent component,
    String value)
  {
    Date date = _getParsedDate(context, component, value);
    if (date != null)
    {
      _fillTimePortion(context, component, date);
    }

    return date;
  }

  /**
   * <p>Convert the model Date object value, into a String based on the pattern
   *  or styles.</p>
   * @param context {@link FacesContext} for the request being processed
   * @param component {@link UIComponent} with which this model object
   *  value is associated
   * @param value Model object value to be converted
   *  (may be <code>null</code>)
   *
   * @return a zero-length String if value is <code>null</code>,
   *  otherwise the result of the conversion
   *
   * @exception ConverterException if conversion cannot be successfully
   *  performed
   * @exception NullPointerException if <code>context</code> or
   *  <code>component</code> is <code>null</code>
   */
  @Override
  public String getAsString(
    FacesContext context,
    UIComponent component,
    Object value
    )
  {
    if (context == null || component == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));

    if (null == value)
      return "";

    if (value instanceof String)
      return (String)value;

    if (!(value instanceof Date))
      throw new ClassCastException(_LOG.getMessage(
        "VALUE_IS_NOT_DATE_TYPE_IT_IS", new Object[]{value,value.getClass()}));

    DateFormat format = _getDateFormat(context, getPattern(), false, (Date)value);
    return format.format(value);
  }

 /**
  * <p>Custom error message to be used, for creating detail part of the {@link FacesMessage},
  * for values that cannot be converted to {@link java.util.Date} when the
  * <code>pattern / secondary pattern</code> is set or when <code>type</code>
  * is set to <code>'date'</code>.</p>
  * Overrides detail message identified by message id {@link #CONVERT_DATE_MESSAGE_ID}
  * @param convertDateMessageDetail custom error message.
  *
  */
  public void setMessageDetailConvertDate(String convertDateMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_DATE_MESSAGE_DETAIL_KEY, convertDateMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for values that cannot be converted to {@link java.util.Date} when
   * <code>pattern / secondary pattern</code> is set or
   * when <code>type</code> is set to <code>'date'</code>.</p>
   * @return custom error message that was set.
   * @see #setMessageDetailConvertDate(String)
   */
  @JSFProperty
  public String getMessageDetailConvertDate()
  {
    Object msg = _facesBean.getProperty(_CONVERT_DATE_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the {@link FacesMessage},
   * for time based value that cannot be converted to date
   * when <code>type</code> is set to <code>'time'</code>.</p>
   * Overrides detail message identified by message id {@link #CONVERT_TIME_MESSAGE_ID}
   * @param convertTimeMessageDetail custom error message.
   */
  public void setMessageDetailConvertTime(String convertTimeMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_TIME_MESSAGE_DETAIL_KEY, convertTimeMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for values that cannot be converted to {@link java.util.Date}
   * when <code>type</code> is set to <code>'time'</code>.
   * @return custom error message that was set.</p>
   * @see #setMessageDetailConvertTime(java.lang.String)
   */
  @JSFProperty
  public String getMessageDetailConvertTime()
  {
    Object msg =_facesBean.getProperty(_CONVERT_TIME_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the {@link FacesMessage},
   * for date-time based value that cannot be converted to {@link java.util.Date}
   * when <code>type</code> is set to <code>'both'</code>.</p>
   * Overrides detail message identified by message id {@link #CONVERT_BOTH_MESSAGE_ID}
   * @param convertBothMessageDetail custom error message.
   * @see #CONVERT_BOTH_MESSAGE_ID
   */
  public void setMessageDetailConvertBoth(String convertBothMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_BOTH_MESSAGE_DETAIL_KEY, convertBothMessageDetail);
  }

  /**
   * Return custom detail error message that was set for creating {@link FacesMessage},
   * for values that cannot be converted to {@link java.util.Date}
   * when <code>type</code> is set to <code>'both'</code>.
   * @return custom error message that was set.
   * @see #setMessageDetailConvertBoth(java.lang.String)
   */
  @JSFProperty
  public String getMessageDetailConvertBoth()
  {
     Object msg = _facesBean.getProperty(_CONVERT_BOTH_MESSAGE_DETAIL_KEY);
     return ComponentUtils.resolveString(msg);
  }

  /**
   * <p>Custom hintDate message.</p>
   * Overrides default hint message
   * @param hintDate Custom hint message.
   */
  public void setHintDate(String hintDate)
  {
    _facesBean.setProperty(_HINT_DATE_KEY, hintDate);
  }

  /**
   * <p>Return custom hintDate message.</p>
   * @return Custom hint message.
   * @see  #setHintDate(String)
   */
  public String getHintDate()
  {
    Object obj = _facesBean.getProperty(_HINT_DATE_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hintTime message.</p>
   * Overrides default hint message
   * @param hintTime Custom hint message.
   */
  public void setHintTime(String hintTime)
  {
    _facesBean.setProperty(_HINT_TIME_KEY, hintTime);
  }

  /**
   * <p>Return custom hintTime message.</p>
   * @return Custom hint message.
   * @see  #setHintTime(String)
   */
  public String getHintTime()
  {
    Object obj = _facesBean.getProperty(_HINT_TIME_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hintBoth message.</p>
   * Overrides default hint message
   * @param hintBoth Custom hint message.
   */
  public void setHintBoth(String hintBoth)
  {
    _facesBean.setProperty(_HINT_BOTH_KEY, hintBoth);
  }

  /**
   * <p>Return custom hintBoth message.</p>
   * @return Custom hint message.
   * @see  #setHintBoth(String)
   */
  public String getHintBoth()
  {
    Object obj = _facesBean.getProperty(_HINT_BOTH_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * Gets the existing date from the component.
   * This date will be used to fill in missing portions of the new date.
   * For example, if the new date is missing the time, the time portion
   * from the existing date will be used.
   * <P>
   * This implementation checks to see if the component is a ValueHolder, and
   * calls getValue() and returns the result if it is a Date instance.
   * @param component The component to get the existing date from.
   * @return null if there is no existing date.
   */
  protected Date getDate(FacesContext context, UIComponent component)
  {
    if (component instanceof ValueHolder)
    {
      Object value = ((ValueHolder)component).getValue();
      if(value instanceof Date)
      {
        return (Date)value;
      }
    }
    return null;
  }
  
  private Locale _extractConverterLocale(
    FacesContext context)
  {
    Locale locale = getLocale();
    if (null == locale)
    {
      RequestContext reqContext = RequestContext.getCurrentInstance();  
      if (reqContext != null)
      {
        locale = reqContext.getFormattingLocale();
      }
      if (locale == null)
      {
        locale = context.getViewRoot().getLocale();
      }
    }
    return locale;
  }

  private Date _getParsedDate(FacesContext context,
                              UIComponent component,
                              String value)
  {
    if (context == null || component == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));

    if (null == value)
      return null;

    value = value.trim();

    if ( 1 > value.length() )
      return null;

    try
    {
      String pattern = getPattern();
      if (pattern == null)
      {
        // get the pattern based on the style and type that has been set.
        DateFormat format = getDateFormat(context, null, true, null);
        if (format instanceof SimpleDateFormat)
        {
          pattern = ((SimpleDateFormat)format).toPattern();
        }
      }

      if (pattern != null)
      {
        return _doLenientParse(context, component, value, pattern);
      }
      else
      {
        // more unlikely that we will get null pattern here but just to be safe
        return _parse(context, component, value, null);
      }
    }
    catch (ConverterException ce)
    {
      try
      {
        // If the parsing fails with primary pattern or with the styles
        // then we try with the secondary pattern.
        String secPattern = getSecondaryPattern();
        if ( secPattern != null)
        {
          return _doLenientParse(context, component, value, secPattern);
        }
      }
      catch(ConverterException secondaryCe)
      {
        // either way we throw the first exception.
        ;
      }
      throw ce;
    }
  }

  // for fixing bug 4469819.
  /**
   * Fill in the time portion of the new date with the time from the previous
   * date value if the converter displays only date. For now, we are not
   * bothered about filling all the missing parts of the pattern. But in
   * future we would consider that.
   */
  private void _fillTimePortion(
    FacesContext context,
    UIComponent component,
    Date newDate)
  {
    // get the previous date value
    Date prevDate = getDate(context, component);

    // if the component doesn't have any date value before, return
    if (prevDate == null)
    {
      return;
    }

    // if the converter uses timePortion, then we need not do anything
    String pattern = getPattern();
    if (pattern == null && !"date".equals(getType()))
    {
      return;
    }

    // find out the missing time components from the pattern
    boolean fillMilliSeconds = true;
    boolean fillSeconds = true;
    boolean fillMinutes = true;
    boolean fillHour = true;

    if (pattern != null)
    {
      int patternLen = pattern.length();
      for (int currCharIndex = 0; currCharIndex < patternLen; currCharIndex++)
      {
        switch (pattern.charAt(currCharIndex))
        {
          case 'S':
            fillMilliSeconds = false;
            break;
          case 's':
            fillSeconds = false;
            break;
          case 'm':
            fillMinutes = false;
            break;
          case 'h':
          case 'H':
          case 'k':
          case 'K':
            fillHour = false;
            break;
        }
      }
    }

    // fill only if any of the time components are missing
    if ( fillMilliSeconds || fillSeconds || fillMinutes || fillHour )
    {
      TimeZone timeZone = _getTimeZone();

      // convert the previous date value wrt client's timeZone
      Calendar prevCal = Calendar.getInstance(timeZone);
      prevCal.setTime(prevDate);
      // convert the new date value wrt client's timeZone
      Calendar newCal = Calendar.getInstance(timeZone);
      newCal.setTime(newDate);

      // extract all the missing time portions from the previous date value
      // and set it to the new date value.
      if (fillMilliSeconds)
      {
        newCal.set(Calendar.MILLISECOND, prevCal.get(Calendar.MILLISECOND));
      }

      if (fillSeconds)
      {
        newCal.set(Calendar.SECOND, prevCal.get(Calendar.SECOND));
      }

      if(fillMinutes)
      {
        newCal.set(Calendar.MINUTE, prevCal.get(Calendar.MINUTE));
      }

      if(fillHour)
      {
        newCal.set(Calendar.HOUR_OF_DAY, prevCal.get(Calendar.HOUR_OF_DAY));
      }

      // modify the new date value.
      newDate.setTime(newCal.getTimeInMillis());
    }
  }

  private Date _parse(
    FacesContext context,
    UIComponent component,
    String value,
    String pattern
    )
  {
    DateFormat fmt = getDateFormat(context, pattern, true, null);
    try
    {
      return fmt.parse(value);

    } catch (ConverterException ce)
    {
      throw ce;
    }
    catch (ParseException pe)
    {
      Object[] params = _getPlaceHolderParameters(context, component, value);
      throw new ConverterException(getParseErrorMessage(context, component,
                                                        pattern, params),
           pe);
    }
  }

  /**
   * Does some more lenient parsing than the stric JSF standard wants.
   */
  private Date _doLenientParse(
    FacesContext context,
    UIComponent component,
    String value,
    String pattern
    )
  {
    // When a pattern (e.g. dd.MM.yyyy HH:mm' Uhr ') requires a whitespace
    // at the end, we should honor that. As the JSF spec (see http://bit.ly/kTelf)
    // wants the converter to trim leading/trailing whitespace, we have to append
    // one, if the pattern requires it at the end...
    // TODO at the beginning as well ?
    if(pattern.endsWith(" '"))
    {
      value += " ";
    }

    // do lenient parsing for the pattern supplied.
    // accept derived patterns during
    // parsing, allowing:
    // 01/13/99  --> 13-Jan-99
    // 03/Oct/99 --> 03-Oct-99
    // 03.Oct.99 --> 03-Oct-99

    ConverterException ce;
    try
    {
      return _parse(context, component, value, pattern);
    }
    catch (ConverterException convException)
    {
      // Let us save this exception to throw, if in case we have exhausted
      // all possible patterns
      ce = convException;

      List<String> patterns = new ArrayList<String>();
      patterns.add(pattern);

      Locale locale = _extractConverterLocale(context);
      // we apply some patterns for convenience reasons (see TRINIDAD-859, 1262)
      if (_CONVENIENCE_PATTERNS.containsKey(locale)) 
      {
        patterns.addAll(_CONVENIENCE_PATTERNS.get(locale));          
      }        
      List<String> lenientPatterns = new ArrayList<String>();
      for (String tmpPattern : patterns)
      {
        lenientPatterns.addAll(_getLenientPatterns(tmpPattern));
      }

      for (String lenientPattern : lenientPatterns)
      {
        try
        {
          return _parse(context, component, value, lenientPattern);
        }
        catch (ConverterException e)
        {
          // Just do nothing with the excpetion - we still need to evaluate
          // for other possible patterns, and we will throw the initially caught
          // exception, which will convey to the user the appropriate message.
          continue;
        }
      }
      throw ce;
    }
  }



  /**
   * <p>Set the <code>Locale</code> to be used when parsing or formatting
   * dates and times.  If set to <code>null</code>, the <code>Locale</code>
   * stored in the {@link javax.faces.component.UIViewRoot} for the current
   * request will be utilized.</p>
   *
   * @param locale The new <code>Locale</code> (or <code>null</code>)
   */
  @Override
  public void setLocale(Locale locale)
  {
    _facesBean.setProperty(_LOCALE_KEY, locale);
  }

 /**
  * <p>Return the <code>Locale</code> that was set.
  * If not explicitly set, the <code>Locale</code> stored
  * in the {@link javax.faces.component.UIViewRoot} for the current
  * request is used during call to <code>getAsObject</code> and
  * <code>getAsString</code>.</p>
  */
  @JSFProperty
  @Override
  public Locale getLocale()
  {
    Object locale = _facesBean.getProperty(_LOCALE_KEY);
    return ComponentUtils.resolveLocale(locale);
  }

  /**
   * <p>Set the format pattern to be used when formatting and parsing
   * dates and times.  Valid values are those supported by
   * <code>java.text.SimpleDateFormat</code>.
   * An invalid value will cause a {@link ConverterException} when
   * <code>getAsObject()</code> or <code>getAsString()</code> is called.</p>
   *
   * @param pattern The new format pattern
   */
  @Override
  public void setPattern(String pattern)
  {
    _facesBean.setProperty(_PATTERN_KEY, pattern);
  }

  /**
   * <p>Return the format pattern to be used when formatting and
   * parsing dates and times.</p>
   */
  @JSFProperty
  @Override
  public String getPattern()
  {
    Object patternObj = _facesBean.getProperty(_PATTERN_KEY);
    String pattern = ComponentUtils.resolveString(patternObj);

    if (pattern != null && pattern.trim().isEmpty())
    {
      return null;
    }

    return pattern;
  }

  /**
   * <p>Set the <code>TimeZone</code> used to interpret a time value.</p>
   *
   * @param timeZone The new time zone
   */
  @Override
  public void setTimeZone(TimeZone timeZone)
  {
    _facesBean.setProperty(_TIME_ZONE_KEY, timeZone);
  }

 /**
  * <p>Return the <code>TimeZone</code> that is used to interpret a time value.
  * If not explicitly set or if a null value is set, then during call to
  * <code>getAsObject</code> and <code>getAsString</code>, the time zone set
  * for the web-app is used. If time zone is not set for the web-app then
  * the default time zone of <code>GMT</code> is used.</p>
  */
  @JSFProperty
  @Override
  public TimeZone getTimeZone()
  {
    Object timeZone = _facesBean.getProperty(_TIME_ZONE_KEY);
    return ComponentUtils.resolveTimeZone(timeZone);
  }

  /**
  * <p>Set the type of value to be formatted or parsed.
  * Valid values are <code>both</code>, <code>date</code>, or
  * <code>time</code>.
  * An invalid value will cause a {@link IllegalStateException} when
  * <code>getAsObject()</code> or <code>getAsString()</code> is called.</p>
  *
  * @param type The new date style
  */
  @Override
  public void setType(String type)
  {
    _facesBean.setProperty(_TYPE_KEY, type);
  }

  /**
   * <p>Return the type of value to be formatted or parsed.
   * If not explicitly set, the default type, <code>date</code>
   * is returned.</p>
   */
  @JSFProperty(defaultValue="date")
  @Override
  public String getType()
  {
    Object type = _facesBean.getProperty(_TYPE_KEY);
    return ComponentUtils.resolveString(type, "date");
  }

  /**
   * <p>Set the style to be used to format or parse dates.  Valid values
   * are <code>default</code>, <code>shortish</code>
   * <code>short</code>, <code>medium</code>,
   * <code>long</code>, and <code>full</code>.
   * An invalid value will cause a {@link IllegalStateException} when
   * <code>getAsObject()</code> or <code>getAsString()</code> is called.</p>
   *
   * @param dateStyle The new style code
   */
  @Override
  public void setDateStyle(String dateStyle)
  {
    _facesBean.setProperty(_DATE_STYLE_KEY, dateStyle);
  }

  /**
   * <p>Return the style to be used to format or parse dates.  If not set,
   * the default value, <code>shortish</code>, is returned.</p>
   * @see #setDateStyle(java.lang.String)
   * @return date style
   */
  @JSFProperty(defaultValue="shortish")
  @Override
  public String getDateStyle()
  {
    Object dateStyle = _facesBean.getProperty(_DATE_STYLE_KEY);
    return ComponentUtils.resolveString(dateStyle, "shortish");
  }

  /**
   * <p>Set the style to be used to format or parse times.  Valid values
   * are <code>default</code>, <code>short</code>,
   * <code>medium</code>, <code>long</code>, and <code>full</code>.
   * An invalid value will cause a {@link IllegalStateException} when
   * <code>getAsObject()</code> or <code>getAsString()</code> is called.</p>
   *
   * @param timeStyle The new style code
   */
  @Override
  public void setTimeStyle(String timeStyle)
  {
    _facesBean.setProperty(_TIME_STYLE_KEY, timeStyle);
  }

  /**
   * <p>Return the style to be used to format or parse times.  If not set,
   * the default value, <code>short</code>, is returned.</p>
   */
  @JSFProperty(defaultValue="short")
  @Override
  public String getTimeStyle()
  {
    Object timeStyle = _facesBean.getProperty(_TIME_STYLE_KEY);
    return ComponentUtils.resolveString(timeStyle, "short");
  }

  /**
   * <p>Second pattern which will be used to parse string in
   * <code>getAsObject</code> if pattern or styles fail. But is never
   * used for formatting to string in <code>getAsString()</code>.</p>
   * @param secondaryPattern a second pattern which will be used
   *        as a second attempt to parse a string if the primary pattern or
   *        styles fail, but is never used for formatting strings.
   */
  public void setSecondaryPattern(String secondaryPattern)
  {
    _facesBean.setProperty(_SECONDARY_PATTERN_KEY, secondaryPattern);
  }

  /**
   * <p>Return the secondary pattern used to parse string when parsing by
   * pattern or style fails.</p>
   */
  @JSFProperty
  public String getSecondaryPattern()
  {
    Object secPattern = _facesBean.getProperty(_SECONDARY_PATTERN_KEY);
    return ComponentUtils.resolveString(secPattern);
  }

  @Override
  public boolean isTransient()
  {
    return _isTransient;
  }

  @Override
  public void setTransient(boolean isTransient)
  {
    _isTransient = isTransient;
  }

  @Override
  public Object saveState(FacesContext context)
  {
    return _facesBean.saveState(context);
  }

  @Override
  public void restoreState(FacesContext context, Object state)
  {
    _facesBean.restoreState(context, state);
  }

  /**
   * <p>Set the {@link ValueExpression} used to calculate the value for the
   * specified attribute if any.</p>
   *
   * @param name Name of the attribute for which to set a {@link ValueExpression}
   * @param expression The {@link ValueExpression} to set, or <code>null</code>
   *  to remove any currently set {@link ValueExpression}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   *            attribute of this converter
   */
  public void setValueExpression(String name, ValueExpression expression)
  {
    ConverterUtils.setValueExpression(_facesBean, name, expression) ;
  }


  /**
   * <p>Return the {@link ValueExpression} used to calculate the value for the
   * specified attribute name, if any.</p>
   *
   * @param name Name of the attribute or property for which to retrieve a
   *  {@link ValueExpression}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   * attribute of this converter
   */
  public ValueExpression getValueExpression(String name)
  {
    return ConverterUtils.getValueExpression(_facesBean, name);
  }

  /**
   * <p>Set the {@link ValueBinding} used to calculate the value for the
   * specified attribute if any.</p>
   *
   * @param name Name of the attribute for which to set a {@link ValueBinding}
   * @param binding The {@link ValueBinding} to set, or <code>null</code>
   *  to remove any currently set {@link ValueBinding}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   *            attribute of this converter
   * @deprecated
   */
  public void setValueBinding(String name, ValueBinding binding)
  {
    ConverterUtils.setValueBinding(_facesBean, name, binding) ;
  }


  /**
   * <p>Return the {@link ValueBinding} used to calculate the value for the
   * specified attribute name, if any.</p>
   *
   * @param name Name of the attribute or property for which to retrieve a
   *  {@link ValueBinding}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   * attribute of this converter
   * @deprecated
   */
  public ValueBinding getValueBinding(String name)
  {
    return ConverterUtils.getValueBinding(_facesBean, name);
  }

  /**
   * <p>Compares this DateTimeConverter with the specified Object for
   * equality.</p>
   * @param object  Object to which this DateTimeConverter is to be compared.
   * @return true if and only if the specified Object is a DateTimeConverter
   * and if all parameters are equal.
   */
  @Override
  public boolean equals(Object object)
  {
    if (this == object)
      return true;

    if(object instanceof DateTimeConverter)
    {
      DateTimeConverter other = (DateTimeConverter)object;

      if ( (isTransient() == other.isTransient())
           && ConverterUtils.equals(getDateStyle(), other.getDateStyle())
           && ConverterUtils.equals(getLocale(), other.getLocale())
           && ConverterUtils.equals(getPattern(), other.getPattern())
           && ConverterUtils.equals(getTimeStyle(), other.getTimeStyle())
           && ConverterUtils.equals(getTimeZone(), other.getTimeZone())
           && ConverterUtils.equals(getType(), other.getType())
           && ConverterUtils.equals(getSecondaryPattern(), other.getSecondaryPattern())
           && ConverterUtils.equals(getMessageDetailConvertDate(),
                                    other.getMessageDetailConvertDate())
           && ConverterUtils.equals(getMessageDetailConvertTime(),
                                    other.getMessageDetailConvertTime())
           && ConverterUtils.equals(getMessageDetailConvertBoth(),
                                    other.getMessageDetailConvertBoth())
         )
      {
        return true;
      }
    }
    return false;
  }

  /**
   * <p>Returns the hash code for this Converter.</p>
   * @return a hash code value for this object.
   */
  @Override
  public int hashCode()
  {
    int result = 17;
    result = result * 37 + (isTransient()? 1 : 0);
    result = result * 37 + _getHashValue(getDateStyle());
    result = result * 37 + _getHashValue(getLocale());
    result = result * 37 + _getHashValue(getPattern());
    result = result * 37 + _getHashValue(getTimeStyle());
    result = result * 37 + _getHashValue(getTimeZone());
    result = result * 37 + _getHashValue(getType());
    result = result * 37 + _getHashValue(getSecondaryPattern());
    result = result * 37 + _getHashValue(getMessageDetailConvertDate());
    result = result * 37 + _getHashValue(getMessageDetailConvertTime());
    result = result * 37 + _getHashValue(getMessageDetailConvertBoth());
    return result;
  }

  protected final DateFormat getDateFormat(
    FacesContext context,
    String pattern,
    boolean forParsing,
    Date    targetDate
    ) throws ConverterException
  {
    ConverterException exception = null;
    try
    {
      DateFormat format = _getDateFormat(context, pattern, forParsing, targetDate);
      return format;
    }
    catch (ConverterException ce)
    {
      exception = ce;
    }
    catch (Exception e)
    {
      exception = new ConverterException(e);
    }
    throw exception;
  }

  /**
   * Returns the TimeZone that will be set on DateFormat for formatting
   * and parsing the dates. By default, this just returns the specified
   * time zone, the one that is set on the DateTimeConverter or in the
   * Adf-Faces config.
   */
  protected TimeZone getFormattingTimeZone(TimeZone tZone)
  {
    return getFormattingTimeZone (tZone, null);
  }

  /**
   * Returns the timeZone for formatting and parsing the date. 
   * TRINIDAD-1512: In some cases,timezone varies depending on the targetDate, 
   * e.g. daylight savings.
   */
  protected TimeZone getFormattingTimeZone(TimeZone tZone, Date targetDate)
  {
    return tZone;
  }

  // This is used while displaying error message at the client side.
  // Identifies the pattern expected to be matched.
  private String[] _getExpectedPatterns(FacesContext context)
  {
    String pattern = getPattern();

    if ( pattern != null )
    {
      return _getAllowedPatterns(context, pattern, getSecondaryPattern());
    }
    else
    {
      String datePattern = null;

      try
      {
        DateFormat format  = getDateFormat(context, null,false, null);
        if ((format != null) && (format instanceof SimpleDateFormat))
        {
          datePattern = ((SimpleDateFormat)format).toPattern();
        }
      }
      catch (ConverterException ce)
      {
        // Do nothing here. Check to see if secondary pattern is available.
        ;
      }
      return _getAllowedPatterns(context, datePattern, getSecondaryPattern());
    }
  }

  protected final  FacesMessage getParseErrorMessage(
    FacesContext context,
    UIComponent component,
    String pattern,
    Object[] params
    )
  {
    // if pattern is set then - conversion would have been carried out using
    // the pattern or secondary pattern.
    String key = getViolationMessageKey(pattern);
    return _getConvertErrorFacesMessage(context, key, params, component);

  }

  protected final String getExample(FacesContext context)
  {
    String[] expectedPatterns = _getExpectedPatterns(context);

     assert((expectedPatterns != null) && (expectedPatterns.length >= 1));
     String example = expectedPatterns[0];
     return example;
  }

  private String[] _getAllowedPatterns(
    FacesContext context,
    String mainPattern,
    String secondaryPattern
    )
  {
    String[] patterns;

    if (mainPattern != null)
    {
      if (secondaryPattern != null)
      {
        patterns = new String[]{mainPattern, secondaryPattern};
      }
      else
      {
        patterns = new String[]{mainPattern};
      }
    }
    else
    {
      patterns = new String[]{secondaryPattern};
    }

    // Convert each pattern into an example
    for (int i = 0; i < patterns.length; i++)
    {
      patterns[i] = _getExample(context, patterns[i]);
    }

    return patterns;
  }

  /**
   * <p>Return the style constant for the specified style name.</p>
   * If invalid throw IllegalStateException.
   *
   * @param dateStyle Name of the date style for which to return a constant
   *
   */
  private static final int _getDateStyle(String dateStyle)
  {
    if (dateStyle.equals("shortish"))
    {
      return _SHORTISH;
    }
    else if (dateStyle.equals("default"))
    {
      return (DateFormat.DEFAULT);
    }
    else if (dateStyle.equals("short"))
    {
      return (DateFormat.SHORT);
    }
    else if (dateStyle.equals("medium"))
    {
      return (DateFormat.MEDIUM);
    }
    else if (dateStyle.equals("long"))
    {
      return (DateFormat.LONG);
    }
    else if (dateStyle.equals("full"))
    {
      return (DateFormat.FULL);
    }
    else
      throw new IllegalStateException(_LOG.getMessage(
        "INVALID_DATE_STYLE", dateStyle));
  }

  private static final int _getTimeStyle(String timeStyle)
  {
    if ("default".equals(timeStyle))
    {
      return (DateFormat.DEFAULT);
    }
    else if ("short".equals(timeStyle))
    {
      return (DateFormat.SHORT);
    }
    else if ("medium".equals(timeStyle))
    {
      return (DateFormat.MEDIUM);
    }
    else if ("long".equals(timeStyle))
    {
      return (DateFormat.LONG);
    }
    else if ("full".equals(timeStyle))
    {
      return (DateFormat.FULL);
    }
    else
      throw new IllegalStateException(_LOG.getMessage(
        "INVALID_TIME_STYLE", timeStyle));
  }

  /**
   * <p>The valid values for type are date,time and both. Any value other than this
   * would result in a ConverterException.</p>
   * @return type
   */
  private static int _getType(String type)
  {
    if ("date".equals(type))
      return _TYPE_DATE;
    else if ("time".equals(type))
      return _TYPE_TIME;
    else if ("both".equals(type))
      return _TYPE_BOTH;
    else
      throw new IllegalStateException(_LOG.getMessage(
        "INVALID_TYPE", type));
  }

  // Don't use this for Array Object and other objects which don't implement
  // their hashCode()
  private static int _getHashValue(Object obj)
  {
    return obj == null? 0 : obj.hashCode();
  }

  private static List<String> _getLenientPatterns(String pattern)
  {
    //Create patterns so as to be lenient.
    // allow for
    // 01/13/99  --> 13-Jan-99 [MMM -> MM], [MMM -> M]
    // Apply the below conversion to the above obtained patterns and the actual
    // patern
    // 03/Oct/99 --> 03-Oct-99
    // 03.Oct.99 --> 03-Oct-99

    List<String> patterns = new ArrayList<String>();
    // Don't forget to add the actual pattern.
    patterns.add(pattern);

    String[] leniencyApplicablePatterns = new String[1];
    leniencyApplicablePatterns[0] = pattern;

    if (pattern.indexOf("MMM") != -1)
    {
      leniencyApplicablePatterns = new String[3];
      leniencyApplicablePatterns[0] = pattern;

      String str1 = pattern.replaceAll("MMM", "MM");
      patterns.add(str1);
      leniencyApplicablePatterns[1] = str1;

      String str2 = pattern.replaceAll("MMM", "M");
      leniencyApplicablePatterns[2] = str2;
      patterns.add(str2);
    }

    // Apply the leninecy (German for leniency?) to the above obtained patterns which was obtained
    // after replacing MMM -> MM and MMM -> M and the actual pattern
    int len = leniencyApplicablePatterns.length;
    if (pattern.indexOf('/') != -1)
    {
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("/", "-"));
      
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("/", "."));
    }
    else if (pattern.indexOf('-') != -1)
    {
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("-", "/"));
      
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("-", "."));
    }
    else if (pattern.indexOf('.') != -1)
    {
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("\\.", "/"));
      
      for (int i = 0; i < len; i++)
        patterns.add(leniencyApplicablePatterns[i].replaceAll("\\.", "-"));
    }
      
    return patterns;
  }

  private Object[] _getPlaceHolderParameters(
    FacesContext context,
    UIComponent component,
    String value)
  {
     Object label = ConverterUtils.getComponentLabel(component);
     String example = getExample(context);
     Object[] params = {label, value, example};
     return params;
  }

  private Object _getRawConvertBothMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_BOTH_MESSAGE_DETAIL_KEY);
  }

  private Object _getRawConvertDateMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_DATE_MESSAGE_DETAIL_KEY);
  }

  private Object _getRawConvertTimeMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_TIME_MESSAGE_DETAIL_KEY);
  }

  private FacesMessage _getConvertErrorFacesMessage(
    FacesContext context,
    String key,
    Object[] params,
    UIComponent component
    )
  {
    Object msgPattern = getMessagePattern(context, key, params, component);
    return MessageFactory.getMessage(context, key, msgPattern,
                                     params, component);
  }
  
  private String _getExample(FacesContext context, String pattern)
  {
    DateFormat format = _getDateFormat(context, pattern, false, _EXAMPLE_DATE);
    return format.format(_EXAMPLE_DATE);
  }


  protected Object getMessagePattern(
      FacesContext context,
      String key,
      Object[] params,
      UIComponent component
      )
  {
    Object msgPattern;
    if (CONVERT_DATE_MESSAGE_ID.equals(key))
    {
      msgPattern = _getRawConvertDateMessageDetail();
    }
    else if (CONVERT_TIME_MESSAGE_ID.equals(key))
    {
      msgPattern = _getRawConvertTimeMessageDetail();
    }
    else if (CONVERT_BOTH_MESSAGE_ID.equals(key))
    {
      msgPattern = _getRawConvertBothMessageDetail();
    }
    else
    {
      // THIS CAN NEVER HAPPEN!
      throw new IllegalArgumentException(_LOG.getMessage(
        "ILLEGAL_MESSAGE_ID", key));
    }
      
    return msgPattern;
  }

  protected String getViolationMessageKey(String pattern)
  {
    String key = null;
    // if pattern is null, then use ViolationMessage based on type specified
    if (getPattern() == null || pattern == null)
    {
      String type = getType();
      if("date".equals(type))
      {
        key = CONVERT_DATE_MESSAGE_ID;
      }
      else if ("time".equals(type))
      {
        key = CONVERT_TIME_MESSAGE_ID;
      }
      else if ("both".equals(type))
      {
        key = CONVERT_BOTH_MESSAGE_ID;
      }
      else if (pattern == null)
      {
        // The chances of this happening is remote. If this was the case..
        // it should have happend during early stages of processing.
        throw new IllegalArgumentException(_LOG.getMessage(
          "ILLEGAL_ATTRIBUTE_TYPE_VALUE", type));
      }
    }

    if (key == null)
    {
      // if pattern is specified explicitly, then use this key
      key = CONVERT_DATE_MESSAGE_ID;
    }

    return key;
  }

  private SimpleDateFormat _getSimpleDateFormat(String pattern, Locale locale)
  {
    String variant = locale.getVariant();
    SimpleDateFormat sdf = null;

    if ((variant != null) && (variant.toUpperCase().startsWith("ORACLE")))
    {
      // This is a special Oracle format, we have to build a special formatter
      // using Oracle style resources.
      try
      {
        ResourceBundle oraElementsData =
          ResourceBundle.getBundle(_ORA_LOCALE_ELEMENTS_BASE, locale);

        DateFormatSymbols syms = new DateFormatSymbols(locale);

        String[] res = oraElementsData.getStringArray("AmPmMarkers");
        if (res != null)
          syms.setAmPmStrings(res);
        res = oraElementsData.getStringArray("Eras");
        if (res != null)
          syms.setEras(res);
        res = oraElementsData.getStringArray("MonthNames");
        if (res != null)
          syms.setMonths(res);
        res = oraElementsData.getStringArray("MonthAbbreviations");
        if (res != null)
          syms.setShortMonths(res);
        res = oraElementsData.getStringArray("DayAbbreviations");
        if (res != null)
          syms.setShortWeekdays(res);
        res = oraElementsData.getStringArray("DayNames");
        if (res != null)
          syms.setWeekdays(res);
        sdf = new SimpleDateFormat(pattern, syms);
      }
      catch (MissingResourceException e)
      {
        // the Oracle resource bundle must be screwed up, just use the default
        sdf = new SimpleDateFormat(pattern, locale);
      }
    }
    else
      sdf = new SimpleDateFormat(pattern, locale);

    return sdf;
  }

  /**
   * Returns a SimpleDateFormat based on the passed in SimpleDateFormat that
   * uses at least 4 digit years if it uses years at all.
   * <p>
   * If <code>format</code> already uses at least 4 digit years, then
   * the <code>format</code> instance will be returned from this function
   * unchanged.
   */
  private SimpleDateFormat _get4YearFormat(
    SimpleDateFormat format,
    Locale           loc
    )
  {
    String formatPattern = format.toPattern();

    //
    // search through the formatPattern for a less than 4 year pattern
    //
    int patternLen = formatPattern.length();

    int firstYIndex = -1;
    int yCount = 0;
    boolean inQuotes = false;

    int currCharIndex = 0;

    for (; currCharIndex < patternLen; currCharIndex++)
    {
      char currChar = formatPattern.charAt(currCharIndex);

      switch (currChar)
      {
        case 'y':
        if (!inQuotes)
        {
          // save the location of the first y
          if (firstYIndex < 0)
            firstYIndex = currCharIndex;

          yCount++;
        }
        break;

        case '\'':
        if (inQuotes)
        {
          int nextCharIndex = currCharIndex + 1;

          if ((nextCharIndex < patternLen) &&
              ('\'' == formatPattern.charAt(nextCharIndex)))
          {
            // this is just an escaped quote, so ignore
            currCharIndex++;
          }
          else
          {
            // no longer quoted
            inQuotes = false;
          }
        }
        else
        {
          // we're now in quotes
          inQuotes = true;
        }
        // fall through

        default:
        {
          // we only replace the first set of years
          // FIXME: this break does nothing, so commenting out;  is
          // it really logic that needs to be fixed?
          /*
          if ((yCount > 0) && (yCount < 4))
          {
            break;
          }
          */
        }
      }
    }

    // we have some years, but not enough, so add enough y's to the current
    // y's to get us to 4
    // =-= bts should we actually be guaranteeing 4 digit years?  Maybe we
    //         should guarantee that we use the same year format as the
    //         MEDIUM format.
    if ((yCount > 0) && (yCount < 4))
    {
      StringBuffer newFormatPattern = new StringBuffer(patternLen + 4 - yCount);

      // append everything from the orginal string before the first y
      if (firstYIndex > 0)
      {
        newFormatPattern.append(formatPattern.substring(0, firstYIndex));
      }

      // add in the correct number of y's
      for (; yCount < 4; yCount++)
      {
        newFormatPattern.append('y');
      }

      // append everything from the orginal string after the last y
      if (firstYIndex < patternLen)
      {
        newFormatPattern.append(formatPattern.substring(firstYIndex,
                                                        patternLen));
      }

      // create the new format
      //format = new SimpleDateFormat(newFormatPattern.toString(), loc);
      format = _getSimpleDateFormat(newFormatPattern.toString(), loc);
    }

    return format;
  }

  private DateFormat _getDateFormat(
    FacesContext context,
    String pattern,
    boolean forParsing,
    Date    targetDate
    )
  {
    Locale locale = _extractConverterLocale(context);
    TimeZone tZone = _getTimeZone();

    DateFormat format = _getCachedFormat(locale, tZone, pattern);

    if (format != null)
    {
      format.setTimeZone(tZone);
    }
    else
    {
      // create a format off of the styles.
      // We will change shortish to short only at place where it is required.
      // Otherwise we may end up throwing convert exception for case where
      // dateStyle is invalid. So evaluating only at the required place.
      if ( null == pattern || "".equals(pattern))
      {
        int type = _getType(getType());

        if (type == _TYPE_DATE || type == _TYPE_BOTH)
        {
          int actualDateStyle = _getDateStyle(getDateStyle());
          int dateStyle = _changeShortishAsShortIfNeeded(actualDateStyle);

          if (type == _TYPE_DATE)
          {
            format = DateFormat.getDateInstance(dateStyle, locale);
          }
          else
          {
            int timeStyle = _getTimeStyle(getTimeStyle());
            format = DateFormat.getDateTimeInstance(dateStyle, timeStyle, locale);
          }
        }
        if (type == _TYPE_TIME)
        {
          int timeStyle = _getTimeStyle(getTimeStyle());
          format = DateFormat.getTimeInstance(timeStyle, locale);
        }
      }
      else
      {
        // create a format off of the pattern
        format = _getSimpleDateFormat(pattern, locale);
      }

      if (format instanceof SimpleDateFormat)
      {
        SimpleDateFormat simpleFormat = (SimpleDateFormat)format;
        
        if (!forParsing)
        {  
          // make sure that we have a 4 digit year for "shortish"
          // dates
          // DO NOT CHANGE THE FOLLOWING LINE to "dateStyle";  this
          // must be retrieved from the instance variable!  (See above)
          // and we need to apply shortish only if it is of date type or
          // type is date and time.
          if (null == pattern && "shortish".equals(getDateStyle()) )
          {
            int type = _getType(getType());
        
            if (type == _TYPE_DATE || type == _TYPE_BOTH )
            {
              simpleFormat = _get4YearFormat(simpleFormat, locale);
              format = simpleFormat;
            }
          }
        }//end-if for formatting
        else
        {
          Calendar cal;
          RequestContext reqContext = RequestContext.getCurrentInstance();
          
          if (reqContext == null)
          {
            cal = null;
        
            if(_LOG.isWarning())
            {
              _LOG.warning("NO_REQUESTCONTEXT_TWO_DIGIT_YEAR_START_DEFAULT");
            }
          }
          else
          {
            cal = new GregorianCalendar(reqContext.getTwoDigitYearStart(), 0, 0);
          }
          
          if (cal != null)
            simpleFormat.set2DigitYearStart(cal.getTime());
        }//end-if for parsing
      }//end-if using SimpleDateFormat

      // Bug 2002065
      format.setLenient(false);

      // Bug 2317641
      // include timezone in date format
      if (tZone != null)
      {
        TimeZone formatTZone = getFormattingTimeZone(tZone, targetDate);
        format.setTimeZone(formatTZone);
      }

      // cache the format
      _cacheFormat(format, locale, tZone, pattern);
    }

    return format;
  }

  private int _changeShortishAsShortIfNeeded(int dateStyle)
  {
    if (dateStyle == _SHORTISH)
      dateStyle = DateFormat.SHORT;
    return dateStyle;
  }

  // 1. For the given locale get the timeZone map
  // 2. For the given timeZone get the pattern,type map
  // 3. For the given pattern and type, dateStyle, timeStyle from the
  // InfoHolderMap- get the date format.
  private DateFormat _getCachedFormat(
    Locale locale,
    TimeZone tZone,
    String pattern
    )
  {
    // See _cacheFormat()
    return null;
  }

  // caching is based on pattern, type, dateStyle and timeStyle
  private void _cacheFormat(
    DateFormat format,
    Locale locale,
    TimeZone tZone,
    String pattern
    )
  {
    // We formerly attempted to cache the SimpleDateFormat in
    // a global, static HashMap.  However, this is invalid:  SimpleDateFormats
    // are not threadsafe, and can only be cached within a thread.  This
    // architecture must be revisited and implemented in a thread-safe fashion.
  }

  private TimeZone _getTimeZone()
  {
    TimeZone tZone = getTimeZone();

    if (tZone == null)
    {
      RequestContext context = RequestContext.getCurrentInstance();
      if (context == null)
      {
        _LOG.warning("NO_REQUESTCONTEXT_TIMEZONE_DEFAULT");
      }
      else
      {
        tZone = context.getTimeZone();
      }

      // If RequestContext is null or if it returns a null,
      // then set it to the default time zone which is GMT time zone
      if (tZone == null)
      {
        tZone = _DEFAULT_TIME_ZONE;
      }
    }

    return tZone;
  }

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _DATE_STYLE_KEY
    = _TYPE.registerKey("dateStyle", String.class, "shortish");

  private static final PropertyKey _LOCALE_KEY
    = _TYPE.registerKey("locale", Locale.class);

  private static final PropertyKey _PATTERN_KEY
    = _TYPE.registerKey("pattern", String.class);

  private static final PropertyKey _SECONDARY_PATTERN_KEY
    = _TYPE.registerKey("secondaryPattern", String.class);

  private static final PropertyKey _TIME_STYLE_KEY
    = _TYPE.registerKey("timeStyle", String.class, "short");

  private static final PropertyKey _TIME_ZONE_KEY
    = _TYPE.registerKey("timeZone", TimeZone.class);

  private static final PropertyKey  _TYPE_KEY
    = _TYPE.registerKey("type", String.class, "date");

  private static final PropertyKey _CONVERT_DATE_MESSAGE_DETAIL_KEY
    = _TYPE.registerKey("messageDetailConvertDate", String.class);

  private static final PropertyKey _CONVERT_TIME_MESSAGE_DETAIL_KEY
    = _TYPE.registerKey("messageDetailConvertTime", String.class);

  private static final PropertyKey _CONVERT_BOTH_MESSAGE_DETAIL_KEY
    = _TYPE.registerKey("messageDetailConvertBoth", String.class);
  
  private static final PropertyKey  _HINT_DATE_KEY =
    _TYPE.registerKey("hintDate", String.class);

  private static final PropertyKey  _HINT_TIME_KEY =
    _TYPE.registerKey("hintTime", String.class);

  private static final PropertyKey  _HINT_BOTH_KEY =
    _TYPE.registerKey("hintBoth", String.class);

  private FacesBean _facesBean = ConverterUtils.getFacesBean(_TYPE);

  private boolean _isTransient;

  private static final TimeZone _DEFAULT_TIME_ZONE = TimeZone.getTimeZone("GMT");

  private static final int _SHORTISH       = -2;

  private static final int _TYPE_DATE      = 0;

  private static final int _TYPE_TIME      = 2;

  private static final int _TYPE_BOTH      = 4;

  private static final String _ORA_LOCALE_ELEMENTS_BASE =
    "org.apache.myfaces.trinidad.resource.LocaleElements";

  private static final TrinidadLogger _LOG  = TrinidadLogger.createTrinidadLogger(DateTimeConverter.class);
  private static final Date _EXAMPLE_DATE;
  /**
   * All entries added to this map MUST also be added to the client map:
   * trinidad-impl\src\main\javascript\META-INF\adf\jsLibs\DateFormat.js->_CONVENIENCE_PATTERNS
   * (in TrDateTimeConverter.prototype._initConveniencePatterns)
   */
  private static final Map<Locale, List<String>> _CONVENIENCE_PATTERNS = 
    new HashMap<Locale, List<String>>();
  private static final List<String> _US_CONVENIENCE_PATTERNS =
    Arrays.asList("MMMM dd, yy", "MMMM/dd/yy", "dd-MMMM-yy");

  static
  {
    Calendar dateFactory = Calendar.getInstance();
    dateFactory.set(1998, 10, 29, 15, 45);
    _EXAMPLE_DATE = dateFactory.getTime();
    
    // All entries added to this map MUST also be added to the client map:
    // trinidad-impl\src\main\javascript\META-INF\adf\jsLibs\DateFormat.js->_CONVENIENCE_PATTERNS
    // (in TrDateTimeConverter.prototype._initConveniencePatterns)
    _CONVENIENCE_PATTERNS.put(Locale.US, _US_CONVENIENCE_PATTERNS);  
  }
}
