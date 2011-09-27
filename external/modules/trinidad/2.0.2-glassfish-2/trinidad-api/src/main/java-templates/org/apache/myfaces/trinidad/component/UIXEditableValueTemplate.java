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
package org.apache.myfaces.trinidad.component;

import java.util.Iterator;

import javax.el.ValueExpression;

import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PostValidateEvent;
import javax.faces.event.PreValidateEvent;
import javax.faces.event.ValueChangeEvent;
import javax.faces.render.Renderer;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import javax.validation.Validation;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.LabeledFacesMessage;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.util.Reportable;


/**
 * Base class for components that have a value.
 * <p>
 * @version $Name:  $ ($Revision: 1084651 $) $Date: 2011-03-23 10:48:07 -0700 (Wed, 23 Mar 2011) $
 * @todo Autogen more of the properties, getters and setters
 */
abstract public class UIXEditableValueTemplate
  extends UIXValue implements EditableValueHolder
{
/**/  static public final FacesBean.Type TYPE = new FacesBean.Type(
/**/    UIXValue.TYPE);

  static public final PropertyKey VALIDATORS_KEY =
    TYPE.registerKey("validators", Validator[].class, PropertyKey.CAP_LIST);

  static public final String REQUIRED_MESSAGE_ID =
    "org.apache.myfaces.trinidad.UIXEditableValue.REQUIRED";
  static public final String CONVERSION_MESSAGE_ID =
    "org.apache.myfaces.trinidad.UIXEditableValue.CONVERSION";
  static public final String TRINIDAD_BEAN_VALIDATION_AVAILABLE =
    "org.apache.myfaces.trinidad.UIXEditableValue.BEAN_VALIDATION_AVAILABLE";
  static public final String VALIDATE_EMPTY_FIELDS_PARAM_NAME =
    "org.apache.myfaces.trinidad.UIXEditableValue.VALIDATE_EMPTY_FIELDS";

  /** -=matzew=- According to http://wiki.java.net/bin/view/Projects/Jsf2MR1ChangeLog
   * this constant will be made public on UIInput with JSF 2.1. For now we have to have
   * it here as a private one...
   **/
  static private final String JSF_SPEC_EMPTY_VALUES_AS_NULL_PARAM_NAME =
    "javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL";

  // our own cache key...
  static public final String TRINIDAD_EMPTY_VALUES_AS_NULL_PARAM_NAME =
    "org.apache.myfaces.trinidad.UIXEditableValue.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL";

  /**
   * Convenience method to reset this component's value to an
   * uninitialized state, by resetting the local value and
   * submitted values to null (ensuring that {@link #isLocalValueSet}
   * is false), and setting "valid" to true.
   */
  public void resetValue()
  {
    setValue(null);
    setSubmittedValue(null);
    setLocalValueSet(false);
    setValid(true);
  }


  // ----------------------------------------------------- Validators Methods



  public void addValidator(Validator validator)
  {
    if (validator == null)
      throw new NullPointerException();

    getFacesBean().addEntry(VALIDATORS_KEY, validator);
  }


  public Validator[] getValidators()
  {
    return (Validator[]) getFacesBean().getEntries(VALIDATORS_KEY,
                                                   Validator.class);
  }

  public void removeValidator(Validator validator)
  {
    getFacesBean().removeEntry(VALIDATORS_KEY, validator);
  }


  /**
   */
  public void validate(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    // Submitted value == null means "the component was not submitted
    // at all";  validation should not continue

    Object submittedValue = getSubmittedValue();
    if (submittedValue == null)
      return;

    // From the SPEC:
    // If the javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL context parameter value is
    // true (ignoring case), and getSubmittedValue() returns a zero-length String call
    // setSubmittedValue(null) and continue processing using null as the current submitted value
    //
    // TODO: -> SPEC ISSUE (matzew)  setSubmittedValue(null) is wrong, so we do not follow the spec here...
    if (shouldInterpretEmptyStringSubmittedValuesAsNull(context) && _isEmptyString(submittedValue))
    {
      submittedValue = null;
    }

    Object newValue = null;
    try
    {
      newValue = getConvertedValue(context, submittedValue);
    }
    catch (ConverterException ce)
    {
      _addConversionErrorMessage(context, ce, submittedValue);
      setValid(false);
    }

    validateValue(context, newValue);

    // If our value is valid, store the new value, erase the
    // "submitted" value, and emit a ValueChangeEvent if appropriate
    if (isValid())
    {
      Object previous = getValue();
      setSubmittedValue(null);
      if (compareValues(previous, newValue))
      {
        setValue(newValue);
        queueEvent(new ValueChangeEvent(this, previous, newValue));
      }
    }
  }


  /**
   * In addition to to the default
   * {@link javax.faces.component.UIComponent#broadcast}
   * processing, pass the {@link ValueChangeEvent} being broadcast to the
   * method referenced by <code>valueChangeListener</code> (if any).
   *
   * @param event {@link FacesEvent} to be broadcast
   *
   * @exception AbortProcessingException Signal the JavaServer Faces
   *  implementation that no further processing on the current event
   *  should be performed
   * @exception IllegalArgumentException if the implementation class
   *  of this {@link FacesEvent} is not supported by this component
   * @exception NullPointerException if <code>event</code> is
   * <code>null</code>
   */
  @Override
  public void broadcast(FacesEvent event)
        throws AbortProcessingException
  {
    // Perform standard superclass processing
    super.broadcast(event);

    if (event instanceof ValueChangeEvent)
    {
      broadcastToMethodBinding(event, getValueChangeListener());
    }
  }


  /**
   * In addition to the standard <code>processDecodes</code> behavior
   * inherited from {@link UIXComponentBase}, calls
   * <code>validate()</code> if the the <code>immediate</code>
   * property is true.  Iif the component is invalid afterwards or
   * a <code>RuntimeException</code> is thrown, calls
   * {@link FacesContext#renderResponse}.
   */
  @Override
  public void processDecodes(FacesContext context)
  {
    if (!isValid())
    {
      // An exception could occur during normal bean attribute level
      // validation in update_model phase. When it happens, the component
      // will have an invalid local value, and LOCAL_VALUE_SET remains
      // true since we want the invalid value to be shown to end user
      // to make corrections. But we don't want the invalid state affects
      // the next request, so we clear the local value and LOCAL_VALUE_SET
      // property here. While on the other hand, we should not clear the
      // state when the component is valid, to avoid accidentally clearing
      // data that other components might depend on.

      setValue(null);
      setLocalValueSet(false);
    }
    setValid(true);

    // Skip processing if our rendered flag is false
    if (!isRendered())
      return;

    pushComponentToEL(context, this);
    try
    {
      super.processDecodes(context);

      if (isImmediate())
        _executeValidate(context);
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  @Override
  public void processUpdates(FacesContext context)
  {
    // Skip processing if our rendered flag is false
    if (!isRendered())
      return;

    pushComponentToEL(context, this);
    try
    {
      super.processUpdates(context);

      // Process this component itself
      updateModel(context);
    }
    finally
    {
      popComponentFromEL(context);
    }

    if (!isValid())
    {
      context.renderResponse();
    }
  }

  @Override
  public void processValidators(FacesContext context)
  {
    // Skip processing if our rendered flag is false
    if (!isRendered())
      return;

    pushComponentToEL(context, this);
    try
    {
      super.processValidators(context);

      if (!isImmediate())
        _executeValidate(context);
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  // TODO Better error messages when update model fails.
  public void updateModel(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isValid() || !isLocalValueSet())
      return;

    ValueExpression expression = getFacesBean().getValueExpression(VALUE_KEY);
    if (expression == null)
      return;

    try
    {
      Object localValue = getLocalValue();
      expression.setValue(context.getELContext(), localValue);
      setValue(null);
      setLocalValueSet(false);
      if (_LOG.isFiner())
      {
        _LOG.finer("Wrote value {0} to model {1} in component {2}",
                   new Object[]{localValue,
                                expression.getExpressionString(),
                                this});
      }
    }
    catch (RuntimeException e)
    {
      // exceptions at this point can occur during normal
      // bean attribute level validation:
      if (_LOG.isFine())
      {
        _LOG.fine("Error updating expression ({0})",
                    expression.getExpressionString());
        _LOG.fine(e);
      }

      setValid(false);

      // don't report the exception if the exception is a Reportable instance and tells so
      boolean shouldReportMessage = (e instanceof Reportable) ?
                                    ((Reportable) e).shouldReportMessage() :
                                    true;

      if (shouldReportMessage)
      {
        FacesMessage message = MessageFactory.getMessage(e);
        message = _wrapMessage(message);
        context.addMessage(getClientId(context), message);
      }
    }
  }

  /**
   */
  @SuppressWarnings("unchecked")
  protected void validateValue(FacesContext context, Object newValue)
  {
    if (!isValid())
      return;

    // If our value is empty, check the required property
    boolean isEmpty = isEmpty(newValue);
    if (isEmpty && isRequired())
    {
      FacesMessage message = _getRequiredFacesMessage(context);
      context.addMessage(getClientId(context), message);
      setValid(false);
    }

    // If our value is not empty, OR we should do empty field validation, call all validators
    if (!isEmpty || shouldValidateEmptyFields(context))
    {
      Iterator<Validator> validators = (Iterator<Validator>)getFacesBean().entries(VALIDATORS_KEY);
      while (validators.hasNext())
      {
        Validator validator = validators.next();
        try
        {
          validator.validate(context, this, newValue);
        }
        catch (ValidatorException ve)
        {
          // If the validator throws an exception, we're
          // invalid, and we need to add a message
          setValid(false);
          FacesMessage message = ve.getFacesMessage();
          if (message != null)
          {
            message.setSeverity(FacesMessage.SEVERITY_ERROR);
            message = _wrapMessage(message);
            context.addMessage(getClientId(context), message);
          }
        }
      }

      MethodBinding validatorBinding = getValidator();
      if (validatorBinding != null)
      {
        try
        {
          validatorBinding.invoke(context,
                                  new Object[] { context, this, newValue});
        }
        catch (EvaluationException ee)
        {
          Throwable cause = ee.getCause();
          if (cause instanceof ValidatorException)
          {
            ValidatorException ve = (ValidatorException) cause;

            // If the validator throws an exception, we're
            // invalid, and we need to add a message
            setValid(false);
            FacesMessage message = ve.getFacesMessage();
            if (message != null)
            {
              message.setSeverity(FacesMessage.SEVERITY_ERROR);
              message = _wrapMessage(message);
              context.addMessage(getClientId(context), message);
            }
          }
          else
          {
            // Otherwise, rethrow the EvaluationException
            throw ee;
          }
        }
      }
    }
  }


  protected String getRequiredMessageKey()
  {
    return REQUIRED_MESSAGE_ID;
  }

  /**
   *
   */
  protected Object getConvertedValue(
    FacesContext context,
    Object       submittedValue) throws ConverterException
  {
    Renderer renderer = getRenderer(context);
    Object newValue = null;

    if (_LOG.isFine())
    {
      _LOG.fine("Converting from " + submittedValue + "(" +
                submittedValue.getClass() + ")");
    }

    if (renderer != null)
    {
      newValue = renderer.getConvertedValue(context, this,
                                            submittedValue);
      if (_LOG.isFine())
      {
        _LOG.fine("Renderer " + renderer + " returned value " + newValue + "(" +
                  ((newValue != null) ? newValue.getClass().getName() : "null") + ")");
      }
    }
    else if (submittedValue instanceof String)
    {
      // If there's no Renderer, and we've got a String,
      // run it through the Converter (if any)
      Converter converter = _getConverterWithType(context);
      if (converter != null)
      {
        newValue = converter.getAsObject(context, this,
                                         (String) submittedValue);
      }
      else
      {
        newValue = submittedValue;
      }
    }
    else
    {
      newValue = submittedValue;
    }

    return newValue;
  }

 /**
   * <p>Return <code>true</code> if the new value is different from the
   * previous value.</p>
   *
   * @param previous old value of this component (if any)
   * @param value new value of this component (if any)
   */
  protected boolean compareValues(Object previous, Object value)
  {
    // handle cases where previous value was empty
    if (previous == null || "".equals(previous)) // bug 4268807
      return !(value == null || "".equals(value));

    boolean isNotEqual = !previous.equals(value);

    // Handle objects whose comparable() implementation is inconsistent with equals().
    // if not equal we will also check compareTo if the data implements Comparable.
    // An example of why we need this is for a case where the data is bigdecimal,
    // because bigdecimal remembers formatting information like scale. So 2.0 is not equal to 2.00
    // in bigdecimal, but when you use compareTo 2.0 and 2.00 are equal.
    // See Issue TRINIDAD-1489 for test case
    if (isNotEqual && value instanceof Comparable && previous.getClass().equals(value.getClass()))
    {
      int compareTo = ((Comparable)previous).compareTo(value);
      isNotEqual = (compareTo != 0);
    }

    return isNotEqual;
  }

  /**
   * <p>Return <code>true</code> if the value is empty.</p>
   */
  protected boolean isEmpty(Object value)
  {
    if (value == null)
      return true;

    return ((value instanceof String) &&
            (((String) value).trim().length() == 0));
  }

  /**
   * <p>Return <code>true</code> if the value is an empty <code>String</code>.</p>
   */
  private boolean _isEmptyString(Object value)
  {
    return ((value instanceof String) && (((String) value).length() == 0));
  }

  /**
   * Checks if the <code>validate()</code> should interpret an empty
   * submitted value should be handle as <code>NULL</code>
   *
   * @return a (cached) boolean to identify the interpretation as null
   */
  public static boolean shouldInterpretEmptyStringSubmittedValuesAsNull(FacesContext context)
  {
    ExternalContext ec = context.getExternalContext();
    Boolean interpretEmptyStringAsNull = (Boolean)ec.getApplicationMap().get(TRINIDAD_EMPTY_VALUES_AS_NULL_PARAM_NAME);

    // not yet cached...
    if (interpretEmptyStringAsNull == null)
    {
      // parses the web.xml to get the "javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL" value
      String param = ec.getInitParameter(JSF_SPEC_EMPTY_VALUES_AS_NULL_PARAM_NAME);

      // evaluate the context parameter
      interpretEmptyStringAsNull = "true".equalsIgnoreCase(param);

      // cache the parsed value
      ec.getApplicationMap().put(TRINIDAD_EMPTY_VALUES_AS_NULL_PARAM_NAME, interpretEmptyStringAsNull);
    }

    return interpretEmptyStringAsNull;
  }

  /**
   * Checks if the <code>validateValue()</code> should handle
   * empty field validation (part of BeanValidation and JSF 2.0).
   *
   * @return a (cached) boolean to identify empty field validation
   */
  public static boolean shouldValidateEmptyFields(FacesContext context)
  {
    ExternalContext ec = context.getExternalContext();
    Boolean shouldValidateEmptyFields = (Boolean)ec.getApplicationMap().get(VALIDATE_EMPTY_FIELDS_PARAM_NAME);

    // not yet cached...
    if (shouldValidateEmptyFields == null)
    {
      // From the JSF 2.0 specification:
      // The implementation must obtain the init parameter Map  from the ExternalContext and inspect the value
      // for the key given by the value of the symbolic constant VALIDATE_EMPTY_FIELDS_PARAM_NAME.
      String param = ec.getInitParameter(UIInput.VALIDATE_EMPTY_FIELDS_PARAM_NAME);

      // If there is no value under that key, use the same key and look in the
      // application map from the ExternalContext.
      if (param == null)
      {
        param = (String) ec.getApplicationMap().get(UIInput.VALIDATE_EMPTY_FIELDS_PARAM_NAME);
      }

      // null means the same as auto (see SPEC on page 11-5)
      if (param == null)
      {
        param = "auto";
      }
      else
      {
        // The environment variables are case insensitive...
        param = param.toLowerCase();
      }

      if (param.equals("auto") && _isBeanValidationAvailable(context))
      {
        shouldValidateEmptyFields = Boolean.TRUE;
      }
      else
      {
        // "true".equalsIgnoreCase(param) is faster than Boolean.valueOf()
        shouldValidateEmptyFields = "true".equalsIgnoreCase(param);
      }

      // cache the parsed value
      ec.getApplicationMap().put(VALIDATE_EMPTY_FIELDS_PARAM_NAME, shouldValidateEmptyFields);
    }

    return shouldValidateEmptyFields;
  }

  /**
   * This boolean indicates if Bean Validation is present.
   *
   * @return a (cached) boolean to identify if bean validation is present
   */
  private static boolean _isBeanValidationAvailable(FacesContext context)
  {
    ExternalContext ec = context.getExternalContext();
    Boolean couldLoadBeanValidationAPI = (Boolean) ec.getApplicationMap().get(TRINIDAD_BEAN_VALIDATION_AVAILABLE);

    // not yet cached...
    if (couldLoadBeanValidationAPI == null)
    {
      try
      {
        couldLoadBeanValidationAPI = Boolean.valueOf(ClassLoaderUtils.loadClass("javax.validation.Validation") != null);

        if (couldLoadBeanValidationAPI)
        {
          try
          {
            // Trial-error approach to check for Bean Validation impl existence.
            Validation.buildDefaultValidatorFactory().getValidator();
          }
          catch (Exception validationException)
          {
            // SPEC section 3.5.6.2:
            // TODO do a i18n version of the error msg
            throw new FacesException("A ValidatorFactory can not be retrieved", validationException);
          }
        }
      }
      catch (ClassNotFoundException cnfe)
      {
        // SPEC section 3.5.6.2:
        // if a Bean Validation provider is not present, bean validation is disabled
        // TODO need a better warning (i18n) here, which has more information
        _LOG.warning("A Bean Validation provider is not present, therefore bean validation is disabled");
        couldLoadBeanValidationAPI = Boolean.FALSE;
      }

      // cache the parsed value
      ec.getApplicationMap().put(TRINIDAD_BEAN_VALIDATION_AVAILABLE, couldLoadBeanValidationAPI);
    }

    return couldLoadBeanValidationAPI;
  }

  /**
   * Executes validation logic.
   */
  private void _executeValidate(FacesContext context)
  {
    Application application = context.getApplication();
    application.publishEvent(context, PreValidateEvent.class, UIComponent.class, this);
    try
    {
      validate(context);
    }
    catch (RuntimeException e)
    {
      context.renderResponse();
      throw e;
    }
    finally
    {
      application.publishEvent(context, PostValidateEvent.class, UIComponent.class, this);
    }

    if (!isValid())
    {
      context.renderResponse();
    }
  }


  // We currently use 'label' for the validation failed message
  private Object _getLabel()
  {
    Object o = getAttributes().get("label");
    if (o == null)
      o = getValueExpression("label");

    return o;
  }

  private Object _getRequiredMessageDetail()
  {
    Object o = getAttributes().get("requiredMessageDetail");
      if (o == null)
       o = getValueExpression("requiredMessageDetail");

    return o;
  }

  private FacesMessage _getRequiredFacesMessage(FacesContext context)
  {
    Object customMessageDetail = _getRequiredMessageDetail();
    FacesMessage message;
    Object label = _getLabel();

    // if message is null then a custom message was not set.
    message = MessageFactory.getMessage(context,
                                        getRequiredMessageKey(),
                                        customMessageDetail,
                                        new Object[]{label},
                                        label);
    return message;
  }

  private void _addConversionErrorMessage(
     FacesContext       context,
     ConverterException ce,
     Object             value)
  {
    FacesMessage message = ce.getFacesMessage();

    if (message == null)
    {
      Object label = _getLabel();
      message = MessageFactory.getMessage(context,
                                          CONVERSION_MESSAGE_ID,
                                          new Object[]{label, value,
                                                       ce.getMessage()},
                                          label);
    }
    else
    {
      message = _wrapMessage(message);
    }

    context.addMessage(getClientId(context), message);
  }

  private Converter _getConverterWithType(FacesContext context)
  {
    Converter converter = getConverter();
    if (converter != null)
    {
      return converter;
    }

    ValueExpression valueExpression = getValueExpression("value");
    if (valueExpression == null)
    {
      return null;
    }

    Class<?> converterType = valueExpression.getType(context.getELContext());
    // if converterType is null, String, or Object, assume
    // no conversion is needed
    if (converterType == null ||
        converterType == String.class ||
        converterType == Object.class)
    {
      return null;
    }

    // if getType returns a type for which we support a default
    // conversion, acquire an appropriate converter instance.
    try
    {
      Application application = context.getApplication();
      return application.createConverter(converterType);
    }
    catch (Exception e)
    {
      return null;
    }
  }

  private FacesMessage _wrapMessage(FacesMessage original)
  {
    if (original instanceof LabeledFacesMessage)
      return original;

    return new FacesMessageWrapper(original, _getLabel());
  }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(UIXEditableValue.class);
}
