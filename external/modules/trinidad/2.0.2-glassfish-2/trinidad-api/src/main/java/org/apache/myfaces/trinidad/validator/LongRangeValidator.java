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

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;


import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFProperty;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFValidator;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidad.util.MessageFactory;

/**
 * <p>Implementation for <code>java.lang.Long</code> values.</p>
 *
 */
@JSFValidator(configExcluded=true)
public class LongRangeValidator extends javax.faces.validator.LongRangeValidator
{
  
  public static final String VALIDATOR_ID = "org.apache.myfaces.trinidad.LongRange";

  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the maximum value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{3}</code> placeholders,
   * which will be replaced by user input, component label and configured
   * maximum value.</p>
   */
  public static final String MAXIMUM_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.LongRangeValidator.MAXIMUM";

  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the minimum value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{2}</code> placeholders, which will be replaced
   * by user input, component label and configured minimum value.</p>
   */
  public static final String MINIMUM_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.LongRangeValidator.MINIMUM";


  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the maximum or minimum value check fails, and both
   * the maximum and minimum values for this validator have been set.
   * The message format string for this message may optionally include
   * <code>{0}</code>, <code>{1}</code>, <code>{2}</code> and <code>{3}</code>
   * placeholders, which will be replaced by user input, component label,
   * configured minimum value and configured maximum value.</p>
   */
  public static final String NOT_IN_RANGE_MESSAGE_ID =
      "org.apache.myfaces.trinidad.validator.LongRangeValidator.NOT_IN_RANGE";

  /**
   * <p>The message identifier of the FacesMessage to be created if
   * the value cannot be converted to an integer
   */
  public static final String CONVERT_MESSAGE_ID =
      "org.apache.myfaces.trinidad.convert.LongConverter.CONVERT";


  /**
   * Construct a {@link Validator} with no preconfigured limits.
   */
  public LongRangeValidator()
  {
    super();
  }

  /**
   * Construct a {@link Validator} with the specified preconfigured
   * limit.
   *
   * @param maximum Maximum value to allow
   */
  public LongRangeValidator(long maximum)
  {
    super(maximum);
  }

  /**
   * Construct a {@link Validator} with the specified preconfigured
   * limits.
   *
   * @param maximum Maximum value to allow
   * @param minimum Minimum value to allow
   *
   */
  public LongRangeValidator(long maximum, long minimum)
  {
    super(maximum, minimum);
  }
  
  /**
   * Return the maximum value to be enforced by this {@link
   * Validator} or null if it has not been
   * set.
   */
  @JSFProperty
  @Override
  public long getMaximum()
  {
    Object maxLong = _facesBean.getProperty(_MAXIMUM_KEY);
    if(maxLong == null)
      maxLong = _MAXIMUM_KEY.getDefault();
    return ComponentUtils.resolveLong(maxLong);
  }

  /**
   * Set the maximum value to be enforced by this {@link Validator}.
   *
   * @param maximum The new maximum value
   *
   */
  @Override
  public void setMaximum(long maximum)
  {
    _facesBean.setProperty(_MAXIMUM_KEY, Long.valueOf(maximum));
  }


  /**
   * Return the minimum value to be enforced by this {@link
   * Validator}, or null if it has not been
   * set.
   */
  @JSFProperty
  @Override
  public long getMinimum()
  {
    Object minLong = _facesBean.getProperty(_MINIMUM_KEY);
    if(minLong == null)
      minLong = _MINIMUM_KEY.getDefault();
    return ComponentUtils.resolveLong(minLong);
  }

  /**
   * Set the minimum value to be enforced by this {@link Validator}.
   *
   * @param minimum The new minimum value
   *
   */
  @Override
  public void setMinimum(long minimum)
  {
    _facesBean.setProperty(_MINIMUM_KEY, Long.valueOf(minimum));
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value exceeds the maximum value set.</p>
   * Overrides detail message identified by message id {@link #MAXIMUM_MESSAGE_ID}
   * @param maximumMessageDetail Custom error message.
   */
  public void setMessageDetailMaximum(String maximumMessageDetail)
  {
    _facesBean.setProperty(_MAXIMUM_MESSAGE_DETAIL_KEY, maximumMessageDetail);
  }

  /**
   *  <p>Return custom detail error message that was set for creating {@link FacesMessage},
   *  for cases where input value exceeds the <code>maximum</code> value set.</p>
   * @return Custom error message.
   * @see #setMessageDetailMaximum(String)
   */
  @JSFProperty
  public String getMessageDetailMaximum()
  {
    Object maxMsgDet = _facesBean.getProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(maxMsgDet);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value is less the set
   * <code>minimum</code> value.</p>
   * Overrides detail message identified by message id {@link #MINIMUM_MESSAGE_ID}
   * @param minimumMessageDetail Custom error message.
   */
  public void setMessageDetailMinimum(String minimumMessageDetail)
  {
    _facesBean.setProperty(_MINIMUM_MESSAGE_DETAIL_KEY, minimumMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for cases where, input value is less than the <code>minimum</code> value set.</p>
   * @return Custom error message.
   * @see #setMessageDetailMinimum(String)
   */
  @JSFProperty
  public String getMessageDetailMinimum()
  {
    Object minMsgDet = _facesBean.getProperty(_MINIMUM_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(minMsgDet);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value is not with in the range,
   * when <code>minimum</code> and <code>maximum</code> is set.</p>
   * Overrides detail message identified by message id {@link #NOT_IN_RANGE_MESSAGE_ID}
   * @param notInRangeMessageDetail Custom error message.
   */
  public void setMessageDetailNotInRange(String notInRangeMessageDetail)
  {
    _facesBean.setProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY, notInRangeMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for cases where, input value exceeds the <code>maximum</code> value and is
   * less than the <code>minimum</code> value set.</p>
   * @return Custom error message.
   * @see #setMessageDetailNotInRange(String)
   */
  @JSFProperty
  public String getMessageDetailNotInRange()
  {
    Object notInRngMsg = _facesBean.getProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(notInRngMsg);
  }

  /**
   * <p>Custom hint maximum message.</p>
   * Overrides default hint message
   * @param hintMaximum Custom hint message.
   */
  public void setHintMaximum(String hintMaximum)
  {
    _facesBean.setProperty(_HINT_MAXIMUM_KEY, hintMaximum);
  }

  /**
   * <p>Return custom hint maximum message.</p>
   * @return Custom hint message.
   * @see  #setHintMaximum(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHintMaximum()
  {
    Object obj = _facesBean.getProperty(_HINT_MAXIMUM_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hint minimum message.</p>
   * Overrides default hint message
   * @param hintMinimum Custom hint message.
   */
  public void setHintMinimum(String hintMinimum)
  {
    _facesBean.setProperty(_HINT_MINIMUM_KEY, hintMinimum);
  }

  /**
   * <p>Return custom hint minimum message.</p>
   * @return Custom hint message.
   * @see  #setHintMinimum(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHintMinimum()
  {
    Object obj = _facesBean.getProperty(_HINT_MINIMUM_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hint notInRange message.</p>
   * Overrides default hint message
   * @param hintNotInRange Custom hint message.
   */
  public void setHintNotInRange(String hintNotInRange)
  {
    _facesBean.setProperty(_HINT_NOT_IN_RANGE, hintNotInRange);
  }

  /**
   * <p>Return custom hint notInRange message.</p>
   * @return Custom hint message.
   * @see  #setHintNotInRange
   */
  public String getHintNotInRange()
  {
    Object obj = _facesBean.getProperty(_HINT_NOT_IN_RANGE);
    return ComponentUtils.resolveString(obj);
  }
  
  @Override
  public void validate(
    FacesContext context,
    UIComponent component,
    Object value
    ) throws ValidatorException
  {
    if ((context == null) || (component == null))
    {
      throw new NullPointerException();
    }

    if (value == null)
    {
      return;
    }

    try
    {
      long longValue = _convertValueToLong(value);
      long min = getMinimum();
      long max = getMaximum();
      
      if(isMaximumSet() && isMinimumSet())
      {
        if(longValue<min || longValue>max)
        {
          throw new ValidatorException(
            _getNotInRangeMessage(context, component, value, min, max));
        }
      }
      
      if(isMaximumSet())
      {
        if (longValue > max)
        {
          throw new ValidatorException(
            _getMaximumMessage(context, component, value, IntegerUtils.getString(max)));
        }
      }
      
      if(isMinimumSet())
      {
        if (longValue < min)
        {
            throw new ValidatorException(
              _getMinimumMessage(context, component, value, IntegerUtils.getString(min)));
        }
      }
    }
    catch (NumberFormatException nfe)
    {
      FacesMessage msg = _getNotCorrectType(context);
      throw new ValidatorException(msg);
    }
  }

  //  StateHolder Methods
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
  
  @JSFProperty(istransient=true, tagExcluded=true)
  @Override
  public boolean isTransient()
  {
    return (_transientValue);
  }


  @Override
  public void setTransient(boolean transientValue)
  {
    _transientValue = transientValue;
  }

  protected boolean isMaximumSet()
  {
    return _facesBean.getProperty(_MAXIMUM_KEY) != null;
  }

  protected boolean isMinimumSet()
  {
    return _facesBean.getProperty(_MINIMUM_KEY) != null;
  }

  private FacesMessage _getNotCorrectType(
    FacesContext context)
  {
    return MessageFactory.getMessage(context, CONVERT_MESSAGE_ID);
  }
  
  /**
   * Try to call longValue() from java.lang.Number. Since not all
   * "number" implement java.lang.Number, we try to call string
   * and parse the String representation of the "number". If that fails
   * we aren't working with a number so we throw a NumberFormatException  
   * 
   * @param value the number value
   * @return parsed number value
   * @throws NumberFormatException if the value is not a number
   */
  private long _convertValueToLong(Object value) throws NumberFormatException
  {
    if(value instanceof Number)
    {
      return ((Number)value).longValue();
    }
    else
    {
      return Long.parseLong(value.toString());
    }
  }

  private FacesMessage _getNotInRangeMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object min,
      Object max)
    {
      Object msg   = _getRawNotInRangeMessageDetail();
      Object label = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, value, min, max};

      return MessageFactory.getMessage(context, NOT_IN_RANGE_MESSAGE_ID,
                                        msg, params, component);
    }


    
    private Object _getRawNotInRangeMessageDetail()
    {
      return _facesBean.getRawProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY);
    }


    private FacesMessage _getMaximumMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object max)
    {

      Object msg   = _getRawMaximumMessageDetail();
      Object label = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, value, max};

      return MessageFactory.getMessage(context,
                                       MAXIMUM_MESSAGE_ID,
                                       msg,
                                       params,
                                       component);
    }

    private Object _getRawMaximumMessageDetail()
    {
      return _facesBean.getRawProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
    }

    private FacesMessage _getMinimumMessage(
      FacesContext context,
      UIComponent component,
      Object value,
      Object min)
    {
      Object msg      = _getRawMinimumMessageDetail();
      Object label    = ValidatorUtils.getComponentLabel(component);

      Object[] params = {label, value, min};

      return MessageFactory.getMessage(context, MINIMUM_MESSAGE_ID,
                                       msg, params, component);
    }

    private Object _getRawMinimumMessageDetail()
    {
      return _facesBean.getRawProperty(_MINIMUM_MESSAGE_DETAIL_KEY);
    }

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _MINIMUM_KEY =
    _TYPE.registerKey("minimum", Long.class,
                                 // Don't rely on autoboxing: there's a method overload
                                 Long.valueOf(Long.MIN_VALUE));

  private static final PropertyKey _MAXIMUM_KEY =
    _TYPE.registerKey("maximum", Long.class,
                                 // Don't rely on autoboxing: there's a method overload
                                 Long.valueOf(Long.MAX_VALUE));

  private static final PropertyKey _MAXIMUM_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailMaximum", String.class);

  private static final PropertyKey _MINIMUM_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailMinimum", String.class);

  private static final PropertyKey _NOT_IN_RANGE_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailNotInRange", String.class);

  private static final PropertyKey  _HINT_MAXIMUM_KEY =
    _TYPE.registerKey("hintMaximum", String.class);

  private static final PropertyKey  _HINT_MINIMUM_KEY =
    _TYPE.registerKey("hintMinimum", String.class);

  private static final PropertyKey  _HINT_NOT_IN_RANGE =
    _TYPE.registerKey("hintNotInRange", String.class);

  private FacesBean _facesBean = ValidatorUtils.getFacesBean(_TYPE);

  private boolean _transientValue = false;
}
