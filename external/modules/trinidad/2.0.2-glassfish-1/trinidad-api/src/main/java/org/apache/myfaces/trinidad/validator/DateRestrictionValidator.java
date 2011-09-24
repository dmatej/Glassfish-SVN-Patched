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
package org.apache.myfaces.trinidad.validator;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.DateTimeConverter;
import javax.faces.el.ValueBinding;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFProperty;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFValidator;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.DateListProvider;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.util.MessageFactory;

@JSFValidator(configExcluded=true)
public class DateRestrictionValidator implements Validator, StateHolder {


  public static final String VALIDATOR_ID = "org.apache.myfaces.trinidad.DateRestriction";
  
  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the valid days  value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{3}</code> placeholders,
   * which will be replaced by user input, component label and configured
   * days value.</p>
   */
  public static final String DAY_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.DAY";

  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the valid month value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{3}</code> placeholders,
   * which will be replaced by user input, component label and configured
   * month value.</p>
   */
  public static final String MONTH_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.MONTH";

  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the valid weekdays value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{3}</code> placeholders,
   * which will be replaced by user input, component label and configured
   * weekdays value.</p>
   */
  public static final String WEEKDAY_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.WEEKDAY";

  /**
   * Construct a {@link Validator} with no preconfigured limits.
   */
  public DateRestrictionValidator()
  {
    super();
    _initMaps();
  }

  @JSFProperty
  public final String[] getInvalidMonths()
  {
    return (String[]) _facesBean.getProperty(_INVALID_MONTHS);
  }

  public final void setInvalidMonths(String[] invalidMonths)
  {
    _facesBean.setProperty(_INVALID_MONTHS, invalidMonths);
  }
  
  @JSFProperty
  public final String[] getInvalidDaysOfWeek()
  {
    return (String[]) _facesBean.getProperty(_INVALID_DAYS_OF_WEEK);  }

  public final void setInvalidDaysOfWeek(String[] invalidDaysOfWeek)
  {
    _facesBean.setProperty(_INVALID_DAYS_OF_WEEK, invalidDaysOfWeek);
  }
  
  @JSFProperty
  public final DateListProvider getInvalidDays()
  {
    return (DateListProvider)_facesBean.getProperty(_INVALID_DAYS);
  }

  public final void setInvalidDays(DateListProvider invalidDays)
  {
    _facesBean.setProperty(_INVALID_DAYS, invalidDays);
  }
  
  @JSFProperty
  public final String getMessageDetailInvalidMonths()
  {
    Object messageDetailInvalidMonths = _facesBean.getProperty(_INVALID_MONTHS_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(messageDetailInvalidMonths);
  }

  public final void setMessageDetailInvalidMonths(String invalidMonths)
  {
    _facesBean.setProperty(_INVALID_MONTHS_MESSAGE_DETAIL_KEY, invalidMonths);
  }
  
  @JSFProperty
  public final String getMessageDetailInvalidDaysOfWeek()
  {
    Object messageDetailInvalidDaysOfWeek = _facesBean.getProperty(_INVALID_DAYS_OF_WEEK_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(messageDetailInvalidDaysOfWeek);
  }

  public final void setMessageDetailInvalidDaysOfWeek(String invalidDaysOfWeek)
  {
    _facesBean.setProperty(_INVALID_DAYS_OF_WEEK_MESSAGE_DETAIL_KEY, invalidDaysOfWeek);
  }
  
  @JSFProperty
  public final String getMessageDetailInvalidDays()
  {
    Object messageDetailInvalidDays = _facesBean.getProperty(_INVALID_DAYS_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(messageDetailInvalidDays);
  }

  public final void setMessageDetailInvalidDays(String invalidDays)
  {
    _facesBean.setProperty(_INVALID_DAYS_MESSAGE_DETAIL_KEY, invalidDays);
  }

  /**
   * <p>Custom hint invalidDaysOfWeek message.</p>
   * Overrides default hint message
   * @param hintWeek Custom hint message.
   */
  public void setHintInvalidDaysOfWeek(String hintWeek)
  {
    _facesBean.setProperty(_HINT_WEEK_KEY, hintWeek);
  }

  /**
   * <p>Return custom hint invalidDaysOfWeek message.</p>
   * @return Custom hint message.
   * @see  #setHintInvalidDaysOfWeek(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHintInvalidDaysOfWeek()
  {
    Object obj = _facesBean.getProperty(_HINT_WEEK_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hint invalidMonths message.</p>
   * Overrides default hint message
   * @param hintMonth Custom hint message.
   */
  public void setHintInvalidMonths(String hintMonth)
  {
    _facesBean.setProperty(_HINT_MONTH_KEY, hintMonth);
  }

  /**
   * <p>Return custom hint invalidMonths message.</p>
   * @return Custom hint message.
   * @see  #setHintInvalidMonths(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHintInvalidMonths()
  {
    Object obj = _facesBean.getProperty(_HINT_MONTH_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * @exception IllegalArgumentException if <code>value</code> is not of type
   * {@link java.util.Date}
   */
  public void validate(
    FacesContext context,
    UIComponent  component,
    Object       value) throws ValidatorException
  {
    if ((context == null) || (component == null))
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));
    }

    if (value != null)
    {
      Calendar calendar = getCalendar();
      calendar.setTime(getDateValue(value));
      Date convertedDate = calendar.getTime();
      
      String weekday = _dayMap.get(calendar.get(Calendar.DAY_OF_WEEK));
      if (_getInvalidDaysOfWeek().contains(weekday))
      {
        throw new ValidatorException(
            _getWrongWeekDayMessage(context, component, value, weekday));
      }
      
      String month = _monthMap.get(calendar.get(Calendar.MONTH));
      if ( _getInvalidMonths().contains(month))
      {
        throw new ValidatorException(
            _getWrongMonthMessage(context, component, value, month));
      }
      
      DateListProvider dlp = getInvalidDays();
      List<Date> dates = null;
      if (dlp != null)
      {
        dates = dlp.getDateList(context, calendar, calendar.getTime(), calendar.getTime());
      }
      
      if(dates!=null)
      {
        for (Date date : dates)
        {
          //range is only one submitted day...
          if(!date.before(convertedDate) && !date.after(convertedDate))
          {
            throw new ValidatorException(
                _getWrongDayMessage(context, component, value, date));
          }
        }
      }
    }
  }

  //  StateHolder Methods
  public Object saveState(FacesContext context)
  {
    return _facesBean.saveState(context);
  }


  public void restoreState(FacesContext context, Object state)
  {
    _facesBean.restoreState(context, state);
  }

  @JSFProperty(istransient=true, tagExcluded=true)
  public boolean isTransient()
  {
    return (_transientValue);
  }


  public void setTransient(boolean transientValue)
  {
    _transientValue = transientValue;
  }
  //  End of StateHolder Methods


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
    ValidatorUtils.setValueExpression(_facesBean, name, expression) ;
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
    return ValidatorUtils.getValueExpression(_facesBean, name);
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
   *            attribute of this validator
   * @deprecated
   */
  public void setValueBinding(String name, ValueBinding binding)
  {
    ValidatorUtils.setValueBinding(_facesBean, name, binding) ;
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
   * attribute of this validator
   * @deprecated
   */
  public ValueBinding getValueBinding(String name)
  {
    return ValidatorUtils.getValueBinding(_facesBean, name);
  }

  @Override
  public boolean equals(Object o)
  {
    if ( o instanceof DateRestrictionValidator)
    {
      DateRestrictionValidator that = (DateRestrictionValidator)o;

      if ( _transientValue == that._transientValue &&
           (ValidatorUtils.equals(getInvalidDays(), that.getInvalidDays())) &&
           (ValidatorUtils.equals(getInvalidDaysOfWeek(), that.getInvalidDaysOfWeek())) &&
           (ValidatorUtils.equals(getInvalidMonths(), that.getInvalidMonths())) &&
           (ValidatorUtils.equals(getMessageDetailInvalidDays(),
                                   that.getMessageDetailInvalidDays())) &&
           (ValidatorUtils.equals(getMessageDetailInvalidDaysOfWeek(),
                                   that.getMessageDetailInvalidDaysOfWeek())) &&
           (ValidatorUtils.equals(getMessageDetailInvalidMonths(),
                                   that.getMessageDetailInvalidMonths()))
          )
      {
        return true;
      }
    }
    return false;
  }

  @Override
  public int hashCode()
  {
    int result = 17;
    Object days = getInvalidDays();
    Object daysOfWeek = getInvalidDaysOfWeek();
    Object month = getInvalidMonths();
    Object msgDetDays = getMessageDetailInvalidDays();
    Object msgDetDaysOfWeek = getMessageDetailInvalidDaysOfWeek();
    Object msgDetMonth = getMessageDetailInvalidMonths();

    result = 37 * result + ( days == null ? 0 : days.hashCode());
    result = 37 * result + ( daysOfWeek == null ? 0 : daysOfWeek.hashCode());
    result = 37 * result + ( month == null ? 0 : month.hashCode());
    result = 37 * result + ( _transientValue ? 0 : 1);
    result = 37 * result + ( msgDetDays == null ? 0: msgDetDays.hashCode());
    result = 37 * result + ( msgDetDaysOfWeek == null ? 0: msgDetDaysOfWeek.hashCode());
    result = 37 * result + ( msgDetMonth == null ? 0: msgDetMonth.hashCode());
    return result;
  }


  private Converter _getConverter(
    FacesContext context,
    UIComponent component)
  {
    Converter converter = null;
    if (component instanceof ValueHolder)
    {
      converter = ((ValueHolder) component).getConverter();
    }

    if (converter == null)
    {
      // Use the DateTimeConverter's CONVERTER_ID, not Date.class,
      // because there is in fact not usually a converter registered
      // at Date.class
      converter = context.getApplication().createConverter(
                      DateTimeConverter.CONVERTER_ID);
    }

    assert(converter != null);

    return converter;
  }

  
  protected Calendar getCalendar()
  {
    TimeZone tz = null;
    RequestContext rctx = RequestContext.getCurrentInstance();
    if (rctx != null)
    {
      tz = rctx.getTimeZone();
    }
    else
    {
      tz = TimeZone.getDefault();
    }
    return Calendar.getInstance(tz);

  }
  
  /**
   * Parses the already converted value to a <code>java.util.Date</code>.
   * @param value converted value
   * @return fulltyped <code>java.util.Date</code>
   * @throws IllegalArgumentException
   */
  protected static Date getDateValue(
      Object value) throws IllegalArgumentException
    {
      if (value instanceof Date)
      {
        return ( (Date)value );
      }

      throw new IllegalArgumentException(_LOG.getMessage(
        "VALUE_IS_NOT_DATE_TYPE"));
    }

  private FacesMessage _getWrongWeekDayMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object weekday)
  { 
      Converter converter = _getConverter(context, component);

      Object cValue = _getConvertedValue(context, component, converter, value);
      Object cWeekday   = _getConvertedValue(context, component, converter, weekday);

      Object msg   = _getRawInvalidDaysOfWeekMessageDetail();
      Object label = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, cValue, cWeekday};

      return MessageFactory.getMessage(context, WEEKDAY_MESSAGE_ID,
                                        msg, params, component);
  }
  private Object _getRawInvalidDaysOfWeekMessageDetail()
  {
    return _facesBean.getRawProperty(_INVALID_DAYS_OF_WEEK_MESSAGE_DETAIL_KEY);
  }

  private FacesMessage _getWrongMonthMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object weekday)
  { 
      Converter converter = _getConverter(context, component);

      Object cValue = _getConvertedValue(context, component, converter, value);
      Object cWeekday   = _getConvertedValue(context, component, converter, weekday);

      Object msg   = _getRawInvalidMonthMessageDetail();
      Object label = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, cValue, cWeekday};

      return MessageFactory.getMessage(context, MONTH_MESSAGE_ID,
                                        msg, params, component);
  }
  private Object _getRawInvalidMonthMessageDetail()
  {
    return _facesBean.getRawProperty(_INVALID_MONTHS_MESSAGE_DETAIL_KEY);
  }

  private FacesMessage _getWrongDayMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object day)
  { 
      Converter converter = _getConverter(context, component);

      Object cValue = _getConvertedValue(context, component, converter, value);
      Object cDay   = _getConvertedValue(context, component, converter, day);

      Object msg   = _getRawInvalidDaysMessageDetail();
      Object label = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, cValue, cDay};

      return MessageFactory.getMessage(context, DAY_MESSAGE_ID,
                                        msg, params, component);
  }
  private Object _getRawInvalidDaysMessageDetail()
  {
    return _facesBean.getRawProperty(_INVALID_DAYS_MESSAGE_DETAIL_KEY);
  }

  private Object _getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Converter    converter,
    Object       value)
  {
    return converter.getAsString(context, component, value);
  }

  private final Set<String> _getInvalidMonths()
  {
    Set<String> monthSet = new HashSet<String>();
    String[] month = getInvalidMonths();
    if(month != null){
      
      for (int i = 0; i < month.length; i++)
      {
        monthSet.add(month[i].toLowerCase());
      }
    }
      
    return monthSet;
  }

  private final Set<String> _getInvalidDaysOfWeek()
  {
    Set<String> daysOfWeekSet = new HashSet<String>();
    String[] daysOfWeek = getInvalidDaysOfWeek();
    if(daysOfWeek != null){
      
      for (int i = 0; i < daysOfWeek.length; i++)
      {
        daysOfWeekSet.add(daysOfWeek[i].toLowerCase());
      }
    }
      
    return daysOfWeekSet;
  }

  private void _initMaps()
  {
    _dayMap = new HashMap<Integer, String>();
    _dayMap.put(Calendar.SUNDAY, "sun");
    _dayMap.put(Calendar.MONDAY, "mon");
    _dayMap.put(Calendar.TUESDAY, "tue");
    _dayMap.put(Calendar.WEDNESDAY, "wed");
    _dayMap.put(Calendar.THURSDAY, "thu");
    _dayMap.put(Calendar.FRIDAY, "fri");
    _dayMap.put(Calendar.SATURDAY, "sat");
    
    _monthMap = new HashMap<Integer, String>();
    _monthMap.put(Calendar.JANUARY, "jan");
    _monthMap.put(Calendar.FEBRUARY, "feb");
    _monthMap.put(Calendar.MARCH, "mar");
    _monthMap.put(Calendar.APRIL, "apr");
    _monthMap.put(Calendar.MAY, "may");
    _monthMap.put(Calendar.JUNE, "jun");
    _monthMap.put(Calendar.JULY, "jul");
    _monthMap.put(Calendar.AUGUST, "aug");
    _monthMap.put(Calendar.SEPTEMBER, "sep");
    _monthMap.put(Calendar.OCTOBER, "oct");
    _monthMap.put(Calendar.NOVEMBER, "nov");
    _monthMap.put(Calendar.DECEMBER, "dec");
  }
  
  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _INVALID_MONTHS =
    _TYPE.registerKey("invalidMonths", String[].class);

  private static final PropertyKey _INVALID_DAYS_OF_WEEK =
    _TYPE.registerKey("invalidDaysOfWeek", String[].class);

  private static final PropertyKey _INVALID_DAYS =
    _TYPE.registerKey("invalidDays", DateListProvider.class);

  private static final PropertyKey _INVALID_MONTHS_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailInvalidMonths", String.class);

  private static final PropertyKey _INVALID_DAYS_OF_WEEK_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailInvalidDaysOfWeek", String.class);

  private static final PropertyKey _INVALID_DAYS_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailInvalidDays", String.class);

  private static final PropertyKey  _HINT_WEEK_KEY =
    _TYPE.registerKey("hintWeek", String.class);

  private static final PropertyKey  _HINT_MONTH_KEY =
    _TYPE.registerKey("hintMonth", String.class);

  private FacesBean _facesBean = ValidatorUtils.getFacesBean(_TYPE);

  private boolean _transientValue = false;
  
  private Map<Integer, String> _dayMap = null;
  private Map<Integer, String> _monthMap = null;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(DateRestrictionValidator.class);

}