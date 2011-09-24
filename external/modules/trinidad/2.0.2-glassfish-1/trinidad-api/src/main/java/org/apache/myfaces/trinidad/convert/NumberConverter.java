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

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParsePosition;

import java.util.Currency;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import java.util.regex.Matcher;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
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
 * This is an extension of the standard JSF {@link javax.faces.convert.NumberConverter}
 * The converter provides all the standard functionality
 * of the default NumberConverter and is strict while converting to object.
 *
 * JSF {@link javax.faces.convert.NumberConverter} is lenient and will convert
 * values like 22.22.2 or 22ABC to valid number 22.22 and 22 respectively,
 * here it would result in a conversion failure and would throw
 * ConverterException.
 *
 * If <code>number grouping separator, decimal separator</code>
 * is configured in <code>trinidad-config.xml</code> file,
 * it will be used during call to <code>getAsObject()</code> and
 * <code>getAsString()</code> for parsing and formatting. If it has not been set,
 * <code>number grouping separator, decimal separator</code> is
 * defaulted based on the locale.</p>
 *
 * <p>If <code>currencyCode</code> is set on the converter then it will be used.
 * Else uses the <code>currencyCode</code> set on <code>trinidad-config.xml</code>
 * file. If it is not set, then it is defaulted based on the locale. The
 * value registered in trinidad-config.xml is obtained using
 * api from {@link org.apache.myfaces.trinidad.context.RequestContext}.</p>
 *
 * <p>Since Apache Trinidad is compatible only with JDK 1.4 and higher versions,
 * the <code>currencyCode</code> gets preference over <code>currencySymbol</code>
 * See RI's {@link javax.faces.convert.NumberConverter} for the way in which
 * <code>currencyCode</code> and <code>currencySymbol</code> gets preference for
 * different version of JDK.
 *
 *<p>The <code>detail</code> part of the {@link FacesMessage} can be customized.
 * For each message id there is a corresponding
 * setter method, which provides for message customization. The customized
 * messages can contain placeholders as specified in the documentation
 * for its corresponding message id.</p>
 *
 * <p>Example: to customize the message for invalid input values, which will result
 * in conversion error containing {@link #CONVERT_NUMBER_MESSAGE_ID}, the following,
 * can be done.</p>
 * <code>
 * String convertNumberMessageDetail = "{0}" in "{1}" is not valid age.</code>
 *
 * //  Note that, the string can contain placeholders and it will be replaced
 * //  appropriately as specified in the documentation for the corresponding
 * //  message id.
 *
 * <code>setMessageDetailConvertNumber(convertNumberMessageDetail);</code>
 *
 * This way user can override detail part of the {@link FacesMessage} for
 * different conversion errors that occur for wrong values, that arise
 * during conversion.
 *
 * There is a one to one mapping for message customization which is as given below. <p>The methods used for customizing the detail message associated with each id
 * is given below:</p>
 * <ol><code>
 * <li>{@link #CONVERT_PATTERN_MESSAGE_ID} - {@link #setMessageDetailConvertPattern(String)}</li>
 * <li>{@link #CONVERT_NUMBER_MESSAGE_ID} -  {@link #setMessageDetailConvertNumber(String)}</li>
 * <li>{@link #CONVERT_CURRENCY_MESSAGE_ID} - {@link #setMessageDetailConvertCurrency(String)}</li>
 * <li>{@link #CONVERT_PERCENT_MESSAGE_ID} - {@link #setMessageDetailConvertPercent(String)}</li>
 * </code></ol>The custom messages can contain placeholders, which will be replaced with
 * values as specified in its corresponding message id.
 *
 * <p>
 * This NumberConverter is automatically registered under the standard
 * converter ID, and therefore will be used when the
 * <code>&lt;f:convertNumber&gt;</code> tag is used.
 *
 * @see org.apache.myfaces.trinidad.context.RequestContext
 *
 * <p>
 */
@JSFConverter(configExcluded=true)
public class NumberConverter extends javax.faces.convert.NumberConverter
{

  /**
   * <p>The standard converter id for this converter.</p>
   */
    public static final String CONVERTER_ID = "org.apache.myfaces.trinidad.Number";

  /**
   * <p>The message identifier of the {@link FacesMessage}
   * to be created if the input value does not match the specified pattern.
   * The message format string for this message may optionally
   * include <code>{0}</code>, <code>{1}</code> and <code>{4}</code>
   * placeholders, which will be replaced with the input value,
   * label associated with the component and the pattern respectively.</p>
   */
   public static final String CONVERT_PATTERN_MESSAGE_ID =
    "org.apache.myfaces.trinidad.convert.NumberConverter.CONVERT_PATTERN";

  /**
   * <p>The message identifier of the {@link FacesMessage}
   * to be created if the input value is not a valid number.  The message format
   * string for this message may optionally include <code>{0}</code> and
   * <code>{1}</code> placeholders, which will be replaced with
   * the input value and label associated with the component
   * respectively.</p>
   */
   public static final String CONVERT_NUMBER_MESSAGE_ID =
    "org.apache.myfaces.trinidad.convert.NumberConverter.CONVERT_NUMBER";

  /**
   * <p>The message identifier of the {@link FacesMessage}
   * to be created if the input value is not a valid number when <code>type</code>
   * is set to <code>'currency'</code>. The message format
   * string for this message may optionally include <code>{0}</code> and
   * <code>{1}</code> placeholders, which will be replaced with
   * the input value and label associated with the component
   * respectively.</p>
   */
   public static final String CONVERT_CURRENCY_MESSAGE_ID =
    "org.apache.myfaces.trinidad.convert.NumberConverter.CONVERT_CURRENCY";

   /**
   * <p>The message identifier of the {@link FacesMessage}
   * to be created if the input value is not a valid number when <code>type</code>
   * is set to <code>'percent'</code>. The message format
   * string for this message may optionally include <code>{0}</code> and
   * <code>{1}</code> placeholders, which will be replaced with
   * the input value and label associated with the component
   * respectively.</p>
   */
   public static final String CONVERT_PERCENT_MESSAGE_ID =
    "org.apache.myfaces.trinidad.convert.NumberConverter.CONVERT_PERCENT";


  //Converter interface implementation
  /**
   * Performs strict conversion of string to number.
   * Values having more than one decimal seprator like <code>22.22.22</code>
   * and values of the form <code>22ABC, 22%ABC</code> will result in
   * {@link javax.faces.convert.ConverterException}.
   */
  @Override
  public Object getAsObject(
    FacesContext context,
    UIComponent component,
    String value)
  {
    if (null == context || null == component)
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));
    }

    if (null == value)
      return null;

    value = value.trim();
    if (value.length() < 1)
      return null;

    String pattern = getPattern();
    String type = getType();

    if (null == pattern && null == type)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "EITHER_PATTERN_OR_TYPE_MUST_SPECIFIED"));
    }

    RequestContext reqCtx = RequestContext.getCurrentInstance();
    Locale locale = _getLocale(reqCtx, context);

    NumberFormat fmt = _getNumberFormat(pattern, type, locale, reqCtx);
    
    DecimalFormat df = (DecimalFormat)fmt;
    df.setParseBigDecimal(true); // TODO What does this do?
    DecimalFormatSymbols dfs = df.getDecimalFormatSymbols();
    
    // We change the grouping_separator b/c TRINIDAD-849
    // source is this JDK bug: 4510618.
    boolean changed = false;
    if (dfs.getGroupingSeparator() == '\u00a0')
    {
      // In some locales, such as fr_FR, the grouping separator is '\u00a0', a 
      // non-breaking space. However, users will normally enter a regular space 
      // character into an input field, so in order for the input to be parsed 
      // correctly, we set the grouping separator to a regular space character. 
      dfs.setGroupingSeparator(' ');
      df.setDecimalFormatSymbols(dfs);
      
      // In the (rare) case that the user actually enters a non-breaking space, 
      // we replace it with a regular space. This should be fine, since the 
      // percent format for fr_FR is " %" (regular space followed by percent).
      value = value.replace('\u00a0', ' ');
      
      changed = true;
    }
    
    ParsePosition pp = new ParsePosition(0);  
    Number num = (Number)fmt.parseObject(value, pp);   
    
    // The following determines whether the percent/currency symbol was left off.
    int typeIdx = _getType(pattern, type);
    if (num == null && (typeIdx == _CURRENCY_TYPE || typeIdx == _PERCENT_TYPE))
    {
      // For parsing 'value' as a Number when the percent/currency symbol is left off.
      NumberFormat nfmt = NumberFormat.getNumberInstance(locale);
      DecimalFormat ndf = (DecimalFormat)nfmt;
      ndf.setParseBigDecimal(true); // TODO What does this do?
      DecimalFormatSymbols ndfs = null;
      
      if (changed)
      {
        ndfs = ndf.getDecimalFormatSymbols();
        ndfs.setGroupingSeparator(' ');
        ndf.setDecimalFormatSymbols(ndfs);
      }
      
      // Assume the percent/currency symbol was left off, in which case we should 
      // be able to parse 'value' as a Number.
      // An error occured, so the index of pp should still be 0.
      num = (Number)nfmt.parseObject(value, pp);      
      if (typeIdx == _PERCENT_TYPE && num != null)
        num = num.doubleValue() / 100.0;
    }
    
    // Change it back, since we could have been handed a cached reference. This 
    // may not be thread-safe, but it probably doesn't have to be.
    if (changed)
    {
      dfs.setGroupingSeparator('\u00a0');
      df.setDecimalFormatSymbols(dfs);
    }

    if (pp.getIndex() != value.length())
    {
      // According to the comments in 
      // trinidad-api\src\main\xrts\org\apache\myfaces\trinidad\resource\MessageBundle.xrts,
      // the substitution parameters are supposed to be:
      // {0} the label that identifies the component
      // {1} value entered by the user
      Object label = ConverterUtils.getComponentLabel(component);
      Object[] params = null;
      
      if (typeIdx == _PATTERN_TYPE)
      {
        // We call this since the pattern may contain the generic currency sign, which we don't 
        // want to display to the user.
        pattern = getLocalizedPattern(context, pattern, dfs);
        
        params = new Object[] {label, value, pattern};
      }
      else if (typeIdx == _NUMBER_TYPE)
      {
        params = new Object[] {label, value};
      }
      else if (typeIdx == _CURRENCY_TYPE)
      {
        params = new Object[] {label, value, fmt.format(_EXAMPLE_CURRENCY)};
      }
      else if (typeIdx == _PERCENT_TYPE)
      {
        params = new Object[] {label, value, fmt.format(_EXAMPLE_PERCENT)};
      }
        
      throw new ConverterException(
        getConvertMessage(context, component, value, params));
    }

    // if we set setParseIntegerOnly(isIntegerOnly()) - This may result in
    // the formatter stopping to parse after the first decimal point.
    // that is number of value 222.22 which is legitimate, hence our test would
    // fail. hence we did not do the following
    // fmt.setParseIntegerOnly(isIntegerOnly());
    // We allow the value to be totally parsed and if the user has set
    // to integer only. We will return the long value from the number object
    // we have in hand.
    if (isIntegerOnly())
      return Long.valueOf(num.longValue());

    return num;
  }

  /**
   *
   * @param context {@link FacesContext} for the request being processed
   * @param component {@link UIComponent} with which this model object
   *        value is associated.
   * @param value Model object value to be converted (may be <code>null</code>)
   *
   * @return a zero-length String if value is <code>null</code>,
   *         if the passed value is a String, it's returned unchanged,
   *         otherwise String representation for the number object based on the
   *         attributes set.
   * @exception ConverterException if conversion cannot be successfully
   *            performed
   * @exception NullPointerException if <code>context</code> or
   *            <code>component</code> is <code>null</code>
   * @exception IllegalArgumentException if the <code>value</code> is not of
   *            type other than {@link java.lang.Number}, {@link java.lang.String}.
   *            <code>value</code> can be null.
   */
  @Override
  public String getAsString(
    FacesContext context,
    UIComponent component,
    Object value)
  {
    if ( null == context || null == component )
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));
    }

    if(value == null)
     return "";

    if(value instanceof String)
      return (String)value;

    if (!(value instanceof Number))
      throw new IllegalArgumentException(_LOG.getMessage(
        "VALUE_NOT_JAVA_LANG_NUMBER_TYPE"));

    String pattern = getPattern();
    String type    = getType();

    if (null == pattern && null == type)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "EITHER_PATTERN_OR_TYPE_MUST_SPECIFIED"));
    }


    RequestContext reqCtx = RequestContext.getCurrentInstance();
    Locale locale  = _getLocale(reqCtx, context);

    NumberFormat formatter = _getNumberFormat(pattern, type, locale, reqCtx);

    _setFormatProperties(formatter);

    if("currency".equals(type))
    {
      _setCurrencyFormattingProperties(reqCtx, formatter);
    }

    return formatter.format(value);
  }

  @Override
  public void restoreState(
    FacesContext context,
    Object state)
  {
    _facesBean.restoreState(context, state);
  }

  @Override
  public Object saveState(FacesContext context)
  {
    return _facesBean.saveState(context);
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
   * Custom error message to be used, for creating detail part of the {@link FacesMessage},
   * message, when <code>value</code> cannot be converted to a number,
   * based on the <code>pattern</code> set.
   * Overrides detail message identified by message id {@link #CONVERT_PATTERN_MESSAGE_ID}
   * @param convertPatternMessageDetail Custom error message.
   */
  public void setMessageDetailConvertPattern(String convertPatternMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_PATTERN_MESSAGE_DETAIL_KEY, convertPatternMessageDetail);
  }

  /**
   * Custom detail error message that was set for creation of {@link FacesMessage}
   * when conversion fails for values that does not match the pattern set.
   * @return Custom error message.
   * @see #setMessageDetailConvertPattern(String)
   *
   */
  @JSFProperty
  public String getMessageDetailConvertPattern()
  {
    Object msg = _facesBean.getProperty(_CONVERT_PATTERN_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the {@link FacesMessage},
   * when <code>value</code> cannot be converted to a number,
   * when <code>type</code> is set to <code>'number'</code> and
   * <code>pattern</code> is null or not set.</p>
   * Overrides detail message identified by message id {@link #CONVERT_NUMBER_MESSAGE_ID}
   * @param convertNumberMessageDetail custom error message.
   */
  public void setMessageDetailConvertNumber(String convertNumberMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_NUMBER_MESSAGE_DETAIL_KEY, convertNumberMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * when conversion fails for values, when <code>type</code> is set to <code>'number'</code> and
   * <code>pattern</code> is null or not set.</p>
   * @return Custom error message.
   * @see #setMessageDetailConvertNumber(String)
   */
  @JSFProperty
  public String getMessageDetailConvertNumber()
  {
    Object msg = _facesBean.getProperty(_CONVERT_NUMBER_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }

  /**
   * Custom error message to be used, for creating detail part of the
   * {@link FacesMessage},  when <code>value</code>
   * cannot be converted to a number, when <code>type</code> is set to
   * <code>'currency'</code> and <code>pattern</code> is null or not set.</p>
   * Overrides detail message identified by message id {@link #CONVERT_CURRENCY_MESSAGE_ID}.
   * @param convertCurrencyMessageDetail custom error message.
   *
   */
  public void setMessageDetailConvertCurrency(String convertCurrencyMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_CURRENCY_MESSAGE_DETAIL_KEY,convertCurrencyMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * when conversion fails for values, when <code>type</code> is set to
   * <code>'currency'</code> and <code>pattern</code> is null or not set.</p>
   * @return Custom error message.
   * @see #setMessageDetailConvertCurrency(String)
   */
  @JSFProperty
  public String getMessageDetailConvertCurrency()
  {
    Object msg = _facesBean.getProperty(_CONVERT_CURRENCY_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }

  /**
   * Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when <code>value</code> cannot be converted to a
   * number, when <code>type</code> is set to <code>'percent'</code> and
   * <code>pattern</code> is null or not set.</p>
   * Overrides detail message identified by message id {@link #CONVERT_PERCENT_MESSAGE_ID}
   * @param convertPercentMessageDetail custom error message.
   */
  public void setMessageDetailConvertPercent(String convertPercentMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_PERCENT_MESSAGE_DETAIL_KEY, convertPercentMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * when conversion fails for values, when <code>value</code> cannot be converted to a
   * number, when <code>type</code> is set to <code>'percent'</code>
   * and <code>pattern</code> is null or not set.</p>
   * @return Custom error message.
   * @see #setMessageDetailConvertPercent(String)
   */
  @JSFProperty
  public String getMessageDetailConvertPercent()
  {
    Object msg = _facesBean.getProperty(_CONVERT_PERCENT_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(msg);
  }
  
  /**
   * <p>Custom hintPattern message.</p>
   * Overrides default hint message
   * @param hintPattern Custom hint message.
   */
  public void setHintPattern(String hintPattern)
  {
    _facesBean.setProperty(_HINT_PATTERN_KEY, hintPattern);
  }

  /**
   * <p>Return custom hintPattern message.</p>
   * @return Custom hint message.
   * @see  #setHintPattern(String)
   */
  public String getHintPattern()
  {
    Object obj = _facesBean.getProperty(_HINT_PATTERN_KEY);
    return ComponentUtils.resolveString(obj);
  }

  

  @Override
  public void setCurrencyCode(String currencyCode)
  {
    _facesBean.setProperty(_CURRENCY_CODE_KEY, currencyCode);
  }

  @JSFProperty
  @Override
  public String getCurrencyCode()
  {
    Object currCode = _facesBean.getProperty(_CURRENCY_CODE_KEY);
    return ComponentUtils.resolveString(currCode);
  }

  @Override
  public void setCurrencySymbol(String currencySymbol)
  {
    _facesBean.setProperty(_CURRENCY_SYMBOL_KEY, currencySymbol);
  }

  @JSFProperty
  @Override
  public String getCurrencySymbol()
  {
    Object currSymbol = _facesBean.getProperty(_CURRENCY_SYMBOL_KEY);
    return ComponentUtils.resolveString(currSymbol);
  }

  @Override
  public void setGroupingUsed(boolean groupingUsed)
  {
    Boolean grpUsed = _getBooleanValue(groupingUsed);
    _facesBean.setProperty(_GROUPING_USED_KEY, grpUsed);
  }

  @JSFProperty(defaultValue="true")
  @Override
  public  boolean isGroupingUsed()
  {
    Object grpUSed = _facesBean.getProperty(_GROUPING_USED_KEY);
    return ComponentUtils.resolveBoolean(grpUSed, true);
  }

  @Override
  public void setIntegerOnly(boolean integerOnly)
  {
    _facesBean.setProperty(_INTEGER_ONLY_KEY, _getBooleanValue(integerOnly));
  }

  @JSFProperty(defaultValue="false")
  @Override
  public boolean isIntegerOnly()
  {
    Object isInt = _facesBean.getProperty(_INTEGER_ONLY_KEY);
    return ComponentUtils.resolveBoolean(isInt, false);
  }

  /**
   * <p>Set the <code>Locale</code> to be used when parsing numbers.
   * If set to <code>null</code>, the <code>Locale</code> stored in the
   * {@link javax.faces.component.UIViewRoot} for the current request
   * will be utilized.</p>
   *
   * @param locale The new <code>Locale</code> (or <code>null</code>)
   */
  @Override
  public void setLocale(Locale locale)
  {
    _facesBean.setProperty(_LOCALE_KEY, locale);
  }
  /**
   * <p>Return the <code>Locale</code> that was set, returns null if it was not set,
   * while faces RI returns the <code>Locale</code> set on the view root if the locale
   * is null.
   * If this value is <code>null</code>, the <code>Locale</code> stored
   * in the {@link javax.faces.component.UIViewRoot} for the current request
   * will be utilized during parsing.</p>
   */
  @JSFProperty
  @Override
  public Locale getLocale()
  {
    Object locale = _facesBean.getProperty(_LOCALE_KEY);
    return ComponentUtils.resolveLocale(locale);
  }

  // All these overrides are mainly to identify whether these were set or nor in
  // the first place

  @Override
  public void setMaxFractionDigits(int maxFractionDigits)
  {
    _facesBean.setProperty(_MAX_FRACTION_DIGITS_KEY, _getIntValue(maxFractionDigits));
  }

  @JSFProperty
  @Override
  public int getMaxFractionDigits()
  {
    Object value = _facesBean.getProperty(_MAX_FRACTION_DIGITS_KEY);
    return ComponentUtils.resolveInteger(value);
  }

  @Override
  public void setMaxIntegerDigits(int maxIntegerDigits)
  {
    _facesBean.setProperty(_MAX_INTEGER_DIGITS_KEY, _getIntValue(maxIntegerDigits));
  }

  @JSFProperty
  @Override
  public int getMaxIntegerDigits()
  {
    Object value = _facesBean.getProperty(_MAX_INTEGER_DIGITS_KEY);
    return ComponentUtils.resolveInteger(value);
  }

  @Override
  public void setMinFractionDigits(int minFractionDigits)
  {
    _facesBean.setProperty(_MIN_FRACTION_DIGITS_KEY, _getIntValue(minFractionDigits));
  }

  @JSFProperty
  @Override
  public int getMinFractionDigits()
  {
    Object value = _facesBean.getProperty(_MIN_FRACTION_DIGITS_KEY);
    return ComponentUtils.resolveInteger(value);
  }

  @Override
  public void setMinIntegerDigits(int minIntegerDigits)
  {
    _facesBean.setProperty(_MIN_INTEGER_DIGITS_KEY, _getIntValue(minIntegerDigits));
  }

  @JSFProperty
  @Override
  public int getMinIntegerDigits()
  {
    Object value = _facesBean.getProperty(_MIN_INTEGER_DIGITS_KEY);
    return ComponentUtils.resolveInteger(value);
  }

  @Override
  public void setPattern(String pattern)
  {
    _facesBean.setProperty(_PATTERN_KEY, pattern);
  }

  @JSFProperty
  @Override
  public String getPattern()
  {
    Object pattern = _facesBean.getProperty(_PATTERN_KEY);
    return ComponentUtils.resolveString(pattern, true);
  }

  /**
   * If <code>pattern</code> contains the generic currency sign, this method will replace it 
   * with the localized currency symbol (if one exists). 
   * @param context the FacesContext
   * @param pattern the pattern to be localized
   * @param dfs the DecimalFormatSymbols; if null, will be constructed from the <code>context</code>
   * @return
   */
  public String getLocalizedPattern(FacesContext context, String pattern, DecimalFormatSymbols dfs)
  {
    if (pattern == null)
      return null;
    
    // If the pattern contains the generic currency sign, replace it with the localized 
    // currency symbol (if one exists), so that when the pattern is displayed (such as in an error 
    // message), it is more meaningful to the user.
    // If the pattern contains double international currency symbol, replace it with the international currency symbol. 
    // For an explanation of this behavior, see section "Special Pattern Characters" at: 
    // http://java.sun.com/javase/6/docs/api/java/text/DecimalFormat.html
    // The unicode for the international currency symbol is: \u00A4
    // The xml hex is        : &#xA4;
    int idx = pattern.indexOf('\u00A4');
    if (idx == -1)
      return pattern;
    
    if (dfs == null)
    {
      String type = getType();
      RequestContext reqCtx = RequestContext.getCurrentInstance();
      Locale locale = _getLocale(reqCtx, context);
      NumberFormat fmt = _getNumberFormat(pattern, type, locale, reqCtx);
      DecimalFormat df = (DecimalFormat) fmt;
      dfs = df.getDecimalFormatSymbols();
    }
    
    if (idx + 1 < pattern.length() && pattern.charAt(idx + 1) == '\u00A4')
    {
      // Matcher.quoteReplacement ensures that the replacement string is properly escaped.
      String symbol = dfs.getInternationalCurrencySymbol();
      if (symbol.length() > 0)
        pattern = pattern.replaceAll(new String(new char[] {'\u00A4', '\u00A4'}), Matcher.quoteReplacement(symbol));
    }
    else
    {
      // Matcher.quoteReplacement ensures that the replacement string is properly escaped.
      String symbol = dfs.getCurrencySymbol();
      if (symbol.length() > 0)
        pattern = pattern.replaceAll(new String(new char[] {'\u00A4'}), Matcher.quoteReplacement(symbol));
    }
    
    return pattern;
  }

  @Override
  public void setType(String type)
  {
    _facesBean.setProperty(_TYPE_KEY, type);
  }

  @JSFProperty(defaultValue="number")
  @Override
  public String getType()
  {
    Object type = _facesBean.getProperty(_TYPE_KEY);
    return ComponentUtils.resolveString(type, "number");
  }

  /**
   * Returns the hash code for this Converter.
   */
  @Override
  public int hashCode()
  {
    int result = 17;
    result = result * 37 + _getHashValue(getLocale());
    result = result * 37 + _getHashValue(getCurrencyCode());
    result = result * 37 + _getHashValue(getCurrencySymbol());
    result = result * 37 + _getHashValue(getType());
    result = result * 37 + _getHashValue(getPattern());
    result = result * 37 + getMaxFractionDigits();
    result = result * 37 + getMaxIntegerDigits();
    result = result * 37 + getMinFractionDigits();
    result = result * 37 + getMinIntegerDigits();
    result = result * 37 + (isGroupingUsed() ? 1: 0);
    result = result * 37 + (isIntegerOnly()? 1: 0);
    result = result * 37 + (isTransient() ? 1: 0);
    result = result * 37 + _getHashValue(getMessageDetailConvertPattern());
    result = result * 37 + _getHashValue(getMessageDetailConvertNumber());
    result = result * 37 + _getHashValue(getMessageDetailConvertCurrency());
    result = result * 37 + _getHashValue(getMessageDetailConvertPercent());
    return result;
  }

  /**
   * <p>Compares this NumberConverter with the specified Object for equality.</p>
   */
  @Override
  public boolean equals(Object numberConverter)
  {
    if (this == numberConverter)
      return true;

    if (numberConverter instanceof NumberConverter)
    {

      NumberConverter nConv = (NumberConverter) numberConverter;

      return
        getMaxFractionDigits() == nConv.getMaxFractionDigits()  &&
        getMaxIntegerDigits()  == nConv.getMaxIntegerDigits()   &&
        getMinFractionDigits() ==  nConv.getMinFractionDigits() &&
        getMinIntegerDigits()  ==  nConv.getMinIntegerDigits()  &&
        isTransient() == nConv.isTransient() &&
        isGroupingUsed() ==  nConv.isGroupingUsed() &&
        isIntegerOnly()  == nConv.isIntegerOnly()   &&
        ConverterUtils.equals(getType(), nConv.getType()) &&
        ConverterUtils.equals(getLocale(), nConv.getLocale()) &&
        ConverterUtils.equals(getCurrencyCode(), nConv.getCurrencyCode()) &&
        ConverterUtils.equals(getCurrencySymbol(), nConv.getCurrencySymbol()) &&
        ConverterUtils.equals(getPattern(), nConv.getPattern()) &&
        ConverterUtils.equals(getMessageDetailConvertPattern(),
                              nConv.getMessageDetailConvertPattern()) &&
        ConverterUtils.equals(getMessageDetailConvertNumber(),
                              nConv.getMessageDetailConvertNumber())  &&
        ConverterUtils.equals(getMessageDetailConvertCurrency(),
                              nConv.getMessageDetailConvertCurrency())&&
        ConverterUtils.equals(getMessageDetailConvertPercent(),
                              nConv.getMessageDetailConvertPercent());

    }
    return false;
  }

  private static int _getHashValue(Object obj)
  {
    return obj == null? 0 : obj.hashCode();
  }

  private static DecimalFormatSymbols _getCachedDecimalFormatSymbol(Locale locale)
  {
    synchronized(_SYMBOLS_LOCK)
    {

      Object dfs = _patternFormatSymbolsHolder.get(locale);

      // make sure to return a clone so that it can be mutated at the point
      // of use based on the setting in the RequestContext
      if (dfs != null)
        return (DecimalFormatSymbols) ((DecimalFormatSymbols) dfs).clone();
      else
        return null;
    }
  }

  private static void _cacheDecimalFormatSymbols(
    Locale locale,
    DecimalFormatSymbols symbols)
  {
    synchronized(_SYMBOLS_LOCK)
    {
      // -= Simon Lessard =- That if looks paranoid, the map get instanciated 
      //                     during static initialization and is never set to 
      //                     null
       if (_patternFormatSymbolsHolder == null)
        _patternFormatSymbolsHolder = new HashMap<Locale, DecimalFormatSymbols>();
       else
       // to clone here or at the point of creation??
      // FIXME: Is that line really supposed to go in the else????
      _patternFormatSymbolsHolder.put(locale, (DecimalFormatSymbols)symbols.clone());
    }
  }

  private static Boolean _getBooleanValue(boolean value)
  {
    return (value ? Boolean.TRUE : Boolean.FALSE);
  }

  private static Integer _getIntValue(int value)
  {
    return Integer.valueOf(value);
  }

  private NumberFormat _getCachedNumberFormat(
    String pattern,
    String type,
    Locale locale)
  {
    synchronized(_TYPE_LOCK)
    {
      // get the map for the appropriate type('number','currency', 'percent') or
      // based on diff patterns - pattern1.. pattern2 for diff locales.
      String key = ((pattern != null) ? pattern : type);
      Map<Locale, NumberFormat> nfMap = _numberFormatHolder.get(key);

      if (nfMap == null)
        return null;
      else
      {
        NumberFormat nf = nfMap.get(locale);
        if (nf != null)
          return (NumberFormat) nf.clone();
      }
      return null;
    }
  }

  private void _cacheNumberFormat(
    NumberFormat format,
    String pattern,
    String type,
    Locale locale)
  {
    synchronized(_TYPE_LOCK)
    {
      // -= Simon Lessard =- That if looks paranoid, the map get instanciated 
      //                     during static initialization and is never set to 
      //                     null
      if (_numberFormatHolder == null)
        _numberFormatHolder = new HashMap<String, Map<Locale, NumberFormat>>();

      else
      {
        // The key could have either been the type based on which formats are
        // stored or it can be based on the pattern also.
        String key = ((pattern != null) ? pattern : type);

        Map<Locale, NumberFormat> nfMap = _numberFormatHolder.get(key);

        // if we have not cached any NumberFormat for this type, then create a
        // map for that type and add to it based on the locale
        if (nfMap == null)
        {
          nfMap = new HashMap<Locale, NumberFormat>();
          _numberFormatHolder.put(key, nfMap);
          
        }
        // add this based on the type ('number','currency','percent') or
        // pattern1, pattern2.. patternN to the main holder
        nfMap.put(locale, (NumberFormat)format.clone());
      }
    }
  }

  private NumberFormat _getNumberFormat(
    String pattern,
    String type,
    Locale locale,
    RequestContext reqCtx
    )
  {
    NumberFormat nfmt;

    int formatType = _getType(pattern, type);

    nfmt = _getCachedNumberFormat(pattern, type, locale);

    if (nfmt == null)
    {
      nfmt = _getNumberFormatter(formatType, pattern, locale);

     _cacheNumberFormat(nfmt,pattern, type, locale);
    }

    if (nfmt instanceof DecimalFormat)
    {
      DecimalFormat dfmt = (DecimalFormat)nfmt;

      // what we get here is a shallow copy. cloned DFS
      DecimalFormatSymbols dfSymbols = dfmt.getDecimalFormatSymbols();

      _setUpDecimalSymbolFormatProperties(dfSymbols, reqCtx, locale);

      //since we get a shallow copy - setting it again after modification.
      ((DecimalFormat) nfmt).setDecimalFormatSymbols(dfSymbols);
    }
    else
    {
      if(_LOG.isWarning())
      {
        _LOG.warning("Failed to get hold of DecimalFormat for type: +" + type + "\n" +
                     "decimal separator,"         +
                     "number grouping separator," +
                     "currency code"              +
                     "will be defaulted based on locale " + locale.toString());
     }
    }
    return nfmt;
  }

  private void _setUpDecimalSymbolFormatProperties(
    DecimalFormatSymbols symbols,
    RequestContext reqCtx,
    Locale locale
    )
  {
    if (reqCtx != null)
    {
      char ch = (char) 0;

      if ((ch = reqCtx.getDecimalSeparator()) != (char)0)
        symbols.setDecimalSeparator(ch);

      if ((ch = reqCtx.getNumberGroupingSeparator()) != (char)0)
        symbols.setGroupingSeparator(ch);

    }
    else
    {
      if (_LOG.isWarning())
      {
        _LOG.warning("NULL_REQUESTCONTEXT", locale.toString());
        }
    }
  }


   // Configure the specified NumberFormat  based on the
   // formatting properties that have been set.
  private void _setFormatProperties(NumberFormat formatter) {

    formatter.setGroupingUsed(isGroupingUsed());

    if (isMaximumFractionDigitsSet())
    {
        formatter.setMaximumFractionDigits(getMaxFractionDigits());
    }

    if (isMaximumIntegerDigitsSet())
    {
      formatter.setMaximumIntegerDigits(getMaxIntegerDigits());
    }

    if (isMinimumFractionDigitsSet())
    {
      formatter.setMinimumFractionDigits(getMinFractionDigits());
    }

    if (isMinimumIntegerDigitsSet())
    {
      formatter.setMinimumIntegerDigits(getMinIntegerDigits());
    }
  }

  private void _setCurrencyInformation(
    RequestContext context,
    DecimalFormatSymbols symbols)
  {
    String currencyCode = _getCurrencyCode(context);

    // currencyCode is set we honour currency code.
    if (currencyCode != null)
    {
      symbols.setCurrency(Currency.getInstance(currencyCode));
      return;
    }

    if (getCurrencySymbol() != null)
    {
      symbols.setCurrencySymbol(getCurrencySymbol());

       // Loggin at level INFO - shows up by default - so use fine.
      _LOG.fine("Using currency symbol as currecny code evaluates to null");
    }
    // currency symbol will now default based on the locale.
  }

  private NumberFormat _getNumberFormatter(
    int formatType,
    String pattern,
    Locale locale)
  {
    NumberFormat nfmt;

    if(_PATTERN_TYPE == formatType)
    {
      DecimalFormatSymbols symbols = _getCachedDecimalFormatSymbol(locale);

      if (null == symbols)
      {
         symbols = new DecimalFormatSymbols(locale);
         // cache this - It is cloned while caching.
        _cacheDecimalFormatSymbols(locale, symbols);
      }
      nfmt = new DecimalFormat(pattern, symbols);
    }

    else if(_NUMBER_TYPE == formatType)
    {
       nfmt = NumberFormat.getNumberInstance(locale);
    }
    else if(_CURRENCY_TYPE == formatType)
    {
      nfmt = NumberFormat.getCurrencyInstance(locale);
    }
    else if(_PERCENT_TYPE == formatType)
    {
      nfmt = NumberFormat.getPercentInstance(locale);
    }
    else
    {
      // never expected to happen
      assert (formatType > _PATTERN_TYPE || formatType < _NUMBER_TYPE) : "invalid type" ;
      nfmt = null;
    }
    return nfmt;
  }

  private Object _getRawConvertCurrencyMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_CURRENCY_MESSAGE_DETAIL_KEY);
  }

  private Object _getRawConvertNumberMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_NUMBER_MESSAGE_DETAIL_KEY);
  }

  private Object _getRawConvertPatternMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_PATTERN_MESSAGE_DETAIL_KEY);
  }

  private Object _getRawConvertPercentMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_PERCENT_MESSAGE_DETAIL_KEY);
  }

  protected final FacesMessage getConvertMessage(
    FacesContext context,
    UIComponent component,
    String inputValue,
    Object[] params
    )
  {
    int type = _getType(getPattern(), getType());
    Object convMsgDet;
    String msgId;

    // Always check for pattern first.
    if (_PATTERN_TYPE == type)
    {
      convMsgDet = _getRawConvertPatternMessageDetail();
      msgId = CONVERT_PATTERN_MESSAGE_ID;
    }
    else if(_NUMBER_TYPE == type)
    {
      convMsgDet = _getRawConvertNumberMessageDetail();
      msgId = CONVERT_NUMBER_MESSAGE_ID;
    }
    else if(_CURRENCY_TYPE == type)
    {
      convMsgDet = _getRawConvertCurrencyMessageDetail();
      msgId = CONVERT_CURRENCY_MESSAGE_ID;
    }
    else if(_PERCENT_TYPE == type)
    {
      convMsgDet = _getRawConvertPercentMessageDetail();
      msgId = CONVERT_PERCENT_MESSAGE_ID;
    }
    else
    {
      throw new IllegalArgumentException("Invalid type: " + getType());
    }
    
    return MessageFactory.getMessage(context, 
                                     msgId, 
                                     convMsgDet, 
                                     params, 
                                     component);
  }

  private Locale _getLocale(RequestContext rc, FacesContext context)
  {
    Locale locale = getLocale();
    if (locale == null )
    {
      locale = rc.getFormattingLocale();
      if (locale == null)
      {
        locale = context.getViewRoot().getLocale();
      }
    }

    return locale;
  }

  private String _getCurrencyCode(
    RequestContext context
    )
  {
    String currencyCode = getCurrencyCode();
    if (currencyCode == null)
    {
      if (context != null)
      {
        currencyCode = context.getCurrencyCode();
      }
      else
      {
        _LOG.warning("NULL_REQUEST_CONTEXT_UNABLE_GET_CURRENCY_CODE");
      }
    }

    return currencyCode;
  }

  // applied only while formatting
  private void _setCurrencyFormattingProperties(
    RequestContext context,
    NumberFormat numberFormatter
   )
  {
    // Useless if... should be instanceof DecimalFormat
    // Potential ClassCastException before the change
    //if (numberFormatter instanceof NumberFormat)
    if (numberFormatter instanceof DecimalFormat)
    {
      DecimalFormat dfmt = (DecimalFormat)numberFormatter;
      DecimalFormatSymbols symbols = dfmt.getDecimalFormatSymbols();
      _setCurrencyInformation(context, symbols);
      dfmt.setDecimalFormatSymbols(symbols);
    }
    else
    {  //string cat at compile time.
      _LOG.warning("NUMBER_NOT_DECIMALFORMAT_IGNORE_CURRENCY");
    }
  }

  private static int _getType(String pattern, String type)
  {
    // check for pattern should be done first.
    if (pattern != null)
      return _PATTERN_TYPE;

    else if ("number".equals(type))
     return _NUMBER_TYPE;

    else if ("currency".equals(type))
     return _CURRENCY_TYPE;

    else if ("percent".equals(type))
     return _PERCENT_TYPE;
    else
      throw new IllegalArgumentException(_LOG.getMessage(
        "NOT_VALID_TYPE", type));
  }
 
 /**
   * Return true if the maximum fraction digits have been set. If not set, return false;
   * @return true, if the maximum fraction digits have been set. 
   */
  public boolean isMaximumFractionDigitsSet()
  {
    return _facesBean.getProperty(_MAX_FRACTION_DIGITS_KEY) != null;
  }

 /**
   * Return true if the minimum fraction digits have been set. If not set, return false;
   * @return true, if the minimum fraction digits have been set. 
   */
  public boolean isMinimumFractionDigitsSet()
  {
    return _facesBean.getProperty(_MIN_FRACTION_DIGITS_KEY) != null;
  }

 /**
   * Return true if the maximum integer digits have been set. If not set, return false;
   * @return true, if the maximum integer digits have been set. 
   */
  public boolean isMaximumIntegerDigitsSet()
  {
    return _facesBean.getProperty(_MAX_INTEGER_DIGITS_KEY) != null;
  }

 /**
   * Return true if the minimum integer digits have been set. If not set, return false;
   * @return true, if the minimum integer digits have been set. 
   */
  public boolean isMinimumIntegerDigitsSet()
  {
    return _facesBean.getProperty(_MIN_INTEGER_DIGITS_KEY) != null;
  }

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _CONVERT_CURRENCY_MESSAGE_DETAIL_KEY
   = _TYPE.registerKey("messageDetailConvertCurrency", String.class);

  private static final PropertyKey _CONVERT_NUMBER_MESSAGE_DETAIL_KEY
   = _TYPE.registerKey("messageDetailConvertNumber", String.class);

  private static final PropertyKey _CONVERT_PATTERN_MESSAGE_DETAIL_KEY
   = _TYPE.registerKey("messageDetailConvertPattern", String.class);

  private static final PropertyKey _CONVERT_PERCENT_MESSAGE_DETAIL_KEY
   = _TYPE.registerKey("messageDetailConvertPercent", String.class);
  
  private static final PropertyKey  _HINT_PATTERN_KEY =
    _TYPE.registerKey("hintPattern", String.class);

  private static final PropertyKey _CURRENCY_CODE_KEY
   = _TYPE.registerKey("currencyCode", String.class);

  private static final PropertyKey _CURRENCY_SYMBOL_KEY
   = _TYPE.registerKey("currencySymbol", String.class);

  private static final PropertyKey _GROUPING_USED_KEY
   = _TYPE.registerKey("groupingUsed", boolean.class, Boolean.TRUE);

  private static final PropertyKey _INTEGER_ONLY_KEY
   =  _TYPE.registerKey("integerOnly", boolean.class, Boolean.FALSE);

  private static final PropertyKey _LOCALE_KEY
   = _TYPE.registerKey("locale", Locale.class);

  private static final PropertyKey _MAX_FRACTION_DIGITS_KEY
   =  _TYPE.registerKey("maxFractionDigits", int.class);

  private static final PropertyKey _MAX_INTEGER_DIGITS_KEY
   = _TYPE.registerKey("maxIntegerDigits", int.class);

  private static final PropertyKey _MIN_FRACTION_DIGITS_KEY
   = _TYPE.registerKey("minFractionDigits", int.class);

  private static final PropertyKey _MIN_INTEGER_DIGITS_KEY
   = _TYPE.registerKey("minIntegerDigits", int.class);

  private static final PropertyKey _PATTERN_KEY
   = _TYPE.registerKey("pattern", String.class);

  private static final PropertyKey  _TYPE_KEY
   = _TYPE.registerKey("type", String.class, "numeric");

  private FacesBean _facesBean = ConverterUtils.getFacesBean(_TYPE);

  private static TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(NumberConverter.class);


  // This is a Map which will hold type and patterns as the key and then
  // hold corresponding  number formats based on the locale. The keys of this
  // map are 'number', 'percent', 'currency' and differernt patterns which the
  // user could have used during the entire application. Each key will
  // hold a Map as its value.
  // This Map (value part of the type) will hold locale as its key and Number
  // formats as its values.
  private static Map<String, Map<Locale, NumberFormat>> _numberFormatHolder = 
    new HashMap<String, Map<Locale, NumberFormat>>();

  // This is map to hold DecimalFormatSymbols when the converter is used,
  // by specifying a pattern. When a pattern is specified we take care of
  // creating the DecimalFormatSymbols. We are caching decimal format symbols
  // based on the locale so that we can make use of it, when a new number
  // converters is instantiated and used based on pattern and not by type.
  private static Map<Locale, DecimalFormatSymbols> _patternFormatSymbolsHolder = 
    new HashMap<Locale, DecimalFormatSymbols>();

  private static final Object _TYPE_LOCK = new Object();

  private static final Object _SYMBOLS_LOCK = new Object();

  private static final int _NUMBER_TYPE   = 1;

  private static final int _CURRENCY_TYPE = 2;

  private static final int _PERCENT_TYPE  = 3;

  private static final int _PATTERN_TYPE  = 4;
  
  private static final Number _EXAMPLE_PERCENT;

  private static final Number _EXAMPLE_CURRENCY;

  static
  {
    _EXAMPLE_PERCENT = 0.3423d;
    _EXAMPLE_CURRENCY = 10250;
  }
}
