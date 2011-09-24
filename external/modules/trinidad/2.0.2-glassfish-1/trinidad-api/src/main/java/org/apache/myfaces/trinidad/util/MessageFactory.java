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
package org.apache.myfaces.trinidad.util;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.el.ValueExpression;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Factory class to return {@link FacesMessage} objects.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/MessageFactory.java#0 $) $Date: 30-nov-2005.11:48:33 $
 */
public final class MessageFactory
{
  private MessageFactory()
  {
  }

  /**
   * Creates a FacesMessage for the given Throwable.
   * The severity is {@link FacesMessage#SEVERITY_ERROR}
   * @param error The root cause of this Exception will be used.
   */
  public static FacesMessage getMessage(Throwable error)
  {
    _LOG.fine(error);

    Throwable unwrap = ComponentUtils.unwrap(error);
    String detail = unwrap.getLocalizedMessage();
    if (detail == null)
    {
      // this is unusual. It is probably an unexpected RT error
      // in the framework. log at warning level:
      detail = unwrap.getClass().getName();
      _LOG.warning(error);
    }
    // bug 4733165:
    FacesMessage message =
      new FacesMessage(FacesMessage.SEVERITY_ERROR, null, detail);
    return message;
  }

  /**
   * Creates a FacesMessage containing formatted text and
   * a severity of SEVERITY_ERROR.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object[] parameters
    )
  {
      return  getMessage(context, FacesMessage.SEVERITY_ERROR,
                         messageId, parameters, null);
  }

  /**
   * Creates a FacesMessage containing formatted text and
   * a severity of SEVERITY_ERROR.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @param component The component generating the message
   *                  (allows label tracking)
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object[] parameters,
    UIComponent component
    )
  {
    return _createFacesMessage(context, FacesMessage.SEVERITY_ERROR, messageId,
                               parameters, _getLabel(component));
  }


  /**
   * Creates a FacesMessage containing formatted text and
   * a severity of SEVERITY_ERROR.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @param label The label of the component generating the message
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object[] parameters,
    Object label
    )
  {
    return _createFacesMessage(context, FacesMessage.SEVERITY_ERROR, messageId,
                               parameters, label);
  }

  /**
   * Creates a FacesMessage containing formatted text.
   *
   * @param context faces context
   * @param severity the message severity
   * @param messageId the bundle key for the translated string
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    FacesMessage.Severity severity,
    String messageId,
    Object[] parameters
    )
  {
      return  getMessage(context, severity,
                         messageId, parameters, null);
  }

  /**
   * Creates a FacesMessage containing formatted text.
   *
   * @param context faces context
   * @param severity the message severity
   * @param messageId the bundle key for the translated string
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @param component The component generating the message
   *                  (allows label tracking)
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    FacesMessage.Severity severity,
    String messageId,
    Object[] parameters,
    UIComponent component
    )
  {
    return _createFacesMessage(context, severity, messageId,
                               parameters, _getLabel(component));
  }


 /**
   * Creates a FacesMessage without any parameters, and
   * a severity of SEVERITY_ERROR.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId
    )
  {
    return getMessage(context, messageId, (Object[]) null, (UIComponent) null);
  }

 /**
   * Returns the localized string
   * @param context
   * @param messageId
   * @return String
   */
  public static String getString(
    FacesContext context,
    String messageId
    )
    {
      return  LocaleUtils.__getSummaryString(context, messageId);
    }


 /**
   * Creates a FacesMessage without any parameters, and
   * a severity of SEVERITY_ERROR.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param component The component generating the message
   *                  (allows label tracking)
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    UIComponent component
    )
  {
    return getMessage(context, messageId, (Object[]) null, component);
  }

  /**
   * Creates a FacesMessage using a single parameter.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param parameter parameter to be substituted for "{0}"
   * @param label the label of the creating component
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object parameter,
    Object label
    )
  {
    return getMessage(context, messageId, new Object[]{parameter}, label);
  }

  /**
   * Creates a FacesMessage using a single parameter.
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param parameter parameter to be substituted for "{0}"
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object parameter
    )
  {
    return getMessage(context, messageId, new Object[]{parameter});
  }

  /**
   * <p>Gets the translation summary and detail text from the message bundle.
   * If <code>customMessagePattern</code> is set, then it is used as the
   * detail part of the faces message. The summary and detail string are
   * formatted based on the supplied <code>parameters</code>. Returns a
   * FacesMessage using the formatted summary and detail message with
   * severity set to error.</p>
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param customMessagePattern Custom error message. It can also contain
   *        placeholders which will be formatted with the supplied parameters.
   *        This customizes the detail part of the {@link FacesMessage}.
   *        If value is null. Then picksup translation summary and detail from
   *        the bundle, which is then formatted and used in construction of faces
   *        message.
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object customMessagePattern,
    Object[] parameters)
  {
    return getMessage(context, messageId, customMessagePattern,
                      parameters, null);
  }

  /**
   * <p>Gets the translation summary and detail text from the message bundle.
   * If <code>customMessagePattern</code> is set, then it is used as the
   * detail part of the faces message. The summary and detail string are
   * formatted based on the supplied <code>parameters</code>. Returns a
   * FacesMessage using the formatted summary and detail message with
   * severity set to error.</p>
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param customMessagePattern Custom error message. It can also contain
   *        placeholders which will be formatted with the supplied parameters.
   *        This customizes the detail part of the {@link FacesMessage}.
   *        If value is null. Then picksup translation summary and detail from
   *        the bundle, which is then formatted and used in construction of faces
   *        message.
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @param component The component generating the message
   *                  (allows label tracking)
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object customMessagePattern,
    Object[] parameters,
    UIComponent component)
  {
    return getMessage(context, messageId, customMessagePattern,
                      parameters, _getLabel(component));
  }

  /**
   * <p>Gets the translation summary and detail text from the message bundle.
   * If <code>customMessagePattern</code> is set, then it is used as the
   * detail part of the faces message. The summary and detail string are
   * formatted based on the supplied <code>parameters</code>. Returns a
   * FacesMessage using the formatted summary and detail message with
   * severity set to error.</p>
   *
   * @param context faces context
   * @param messageId the bundle key for the translated string
   * @param customMessagePattern Custom error message. It can also contain
   *        placeholders which will be formatted with the supplied parameters.
   *        This customizes the detail part of the {@link FacesMessage}.
   *        If value is null. Then picksup translation summary and detail from
   *        the bundle, which is then formatted and used in construction of faces
   *        message.
   * @param parameters parameters to be substituted in the placeholders
   *        of the translated string.
   * @param label the label of the creating component
   * @return a FacesMessage object
   */
  public static FacesMessage getMessage(
    FacesContext context,
    String messageId,
    Object customMessagePattern,
    Object[] parameters,
    Object label)
  {
    if (null != customMessagePattern)
    {
      String summary = LocaleUtils.__getSummaryString(context, messageId);

      ErrorMessages msgs = _getErrorMessage(summary, customMessagePattern,
                                            parameters);

      return _createFacesMessage(FacesMessage.SEVERITY_ERROR, msgs, label);
    }

    return _createFacesMessage(context, FacesMessage.SEVERITY_ERROR, messageId,
                               parameters, label);
  }

  private static FacesMessage _createFacesMessage(
    FacesContext context,
    FacesMessage.Severity severity,
    String messageId,
    Object[] parameters,
    Object label
    )
  {
    ErrorMessages errMsgs
      = LocaleUtils.__getErrorMessages(context, messageId);

    ErrorMessages formattedErrMsgs
      = _getBindingOrFormattedErrorMessages(errMsgs, parameters);
    return _createFacesMessage(severity, formattedErrMsgs, label);
  }

//  private static FacesMessage _createFacesMessage(
//    FacesMessage.Severity severity,
//    ErrorMessages messageStrings,
//    UIComponent component
//    )
//  {
//    return _createFacesMessage(severity, messageStrings, _getLabel(component));
//  }

  private static FacesMessage _createFacesMessage(
    FacesMessage.Severity severity,
    ErrorMessages messageStrings,
    Object label
    )
  {
    if (messageStrings instanceof BindingErrorMessages)
    {
       return new BindingFacesMessage(severity, messageStrings, label);
    }

    String summary = messageStrings.getMessage();
    String detail  = messageStrings.getDetailMessage();

    return new LabeledFacesMessage(severity, summary, detail, label);
  }

  private static Object[] _getProcessedBindings(
    FacesContext facesContext,
    Object[] parameters)
  {
    FacesContext context = facesContext;
    Object[] resolvedParameters = new Object[parameters.length];
    for (int i = 0; i < parameters.length; i++)
    {
      Object o = parameters[i];
      if (o instanceof ValueExpression)
      {
        if (context == null)
          context = FacesContext.getCurrentInstance();
        o = ((ValueExpression) o).getValue(context.getELContext());
      }

      resolvedParameters[i] = o;
    }
    return resolvedParameters;
  }

  private static String _getFormattedString(String pattern, Object parameters[])
  {
    if (parameters == null)
      return pattern;

    FastMessageFormat formatter = new FastMessageFormat(pattern);
    String fmtedMsgStr = formatter.format(parameters);
    return fmtedMsgStr;
  }

  private static boolean _containsBinding(Object[] parameters)
  {
    if (parameters == null)
      return false;

    for (int i = 0; i < parameters.length; i++)
    {
      if (parameters[i] instanceof ValueExpression)
        return true;
    }

    return false;
  }

  //A Factory
  private static ErrorMessages _getErrorMessage(
    String summary,
    Object customMessagePattern,
    Object[] parameters)
  {
    _assertIsValidCustomMessageType(customMessagePattern);
    boolean isCustomMsgValueBound = (customMessagePattern instanceof ValueExpression);
    boolean containsBinding = _containsBinding(parameters);
    if (isCustomMsgValueBound || containsBinding)
    {
      if (!(isCustomMsgValueBound))
      {
        String customMesg = (String)customMessagePattern;
        return new BindingErrorMessages(summary, customMesg, parameters);
      }
      else
      {
        ValueExpression customMessage = (ValueExpression)customMessagePattern;
        return new CustomDetailErrorMessage(summary, customMessage,
                                            parameters, containsBinding);
      }
    }
    else
    {
      String detailMsgPattern = (String)customMessagePattern;
      ErrorMessages errorMsg = new FormattedErrorMessages(summary,
                                                        detailMsgPattern,
                                                        parameters);
      return errorMsg;
    }
  }

  private static Object _getLabel(UIComponent component)
  {
    Object o = null;
    if (component != null)
    {
      o = component.getAttributes().get("label");
      if (o == null)
        o = component.getValueExpression ("label");
    }
    return o;
  }

  private static void _assertIsValidCustomMessageType(Object customMessagePattern)
  {
    if (!(customMessagePattern instanceof ValueExpression  ||
         customMessagePattern instanceof String))
         throw new IllegalArgumentException(_LOG.getMessage(
           "CUSTOM_MESSAGE_SHOULD_BE_VALUEBINDING_OR_STRING_TYPE"));
  }

  private static ErrorMessages _getBindingOrFormattedErrorMessages(
    ErrorMessages unFormattedErrorMessages,
    Object[] parameters)
  {

    if (!_containsBinding(parameters))
      return new FormattedErrorMessages(unFormattedErrorMessages.getMessage(),
                                        unFormattedErrorMessages.getDetailMessage(),
                                        parameters);
    else
      return new BindingErrorMessages(unFormattedErrorMessages.getMessage(),
                                      unFormattedErrorMessages.getDetailMessage(),
                                      parameters);
  }

  private static class BindingFacesMessage extends LabeledFacesMessage
  {
    public BindingFacesMessage(FacesMessage.Severity severity,
                               ErrorMessages messageStrings,
                               Object label)
    {
      super(severity, null, null, label);
      _messageStrings = messageStrings;
    }

    @Override
    public String getDetail()
    {
      return _messageStrings.getDetailMessage();
    }

    @Override
    public String getSummary()
    {
      return _messageStrings.getMessage();
    }

    public void setDetail()
    {
      throw new UnsupportedOperationException();
    }

    public void setSummary()
    {
      throw new UnsupportedOperationException();
    }

    private final ErrorMessages _messageStrings;
    private static final long serialVersionUID = 1L;
  }

  private static class BindingErrorMessages extends ErrorMessages
  {
    BindingErrorMessages(
      String messageFormat, String detailMessageFormat, Object[] parameters)
    {
      super(messageFormat, detailMessageFormat);
      _parameters = parameters;
      if (parameters == null)
        throw new NullPointerException();
    }

    @Override
    public String getMessage()
    {
      String pattern = super.getMessage();
      return _getFormattedString(pattern, getResolvedParameters());
    }

    @Override
    public String getDetailMessage()
    {
      String pattern = super.getDetailMessage();
      return _getFormattedString(pattern, getResolvedParameters());
    }

    protected final Object[] getResolvedParameters()
    {
      return _getProcessedBindings(null, _parameters);
    }

    protected final Object[] getParameters()
    {
      return _parameters;
    }

    private Object[] _parameters;
    private static final long serialVersionUID = 1L;
  }

  // Though it may not be exactly correct to extend BindingErrorMessages
  // as parameters might not have value binding. This inheritance keeps
  // it simple.
  private static class CustomDetailErrorMessage extends BindingErrorMessages
  {

    CustomDetailErrorMessage(
      String messageFormat,
      ValueExpression customDetailErrorMessage,
      Object[] parameters,
      boolean hasBoundParameters
    )
    {
      super(messageFormat, null, parameters);
      _customDetailErrorMessage = customDetailErrorMessage;
      _hasBoundParameters = hasBoundParameters;
    }

    // Currently only detail message can be customized. So we override the
    // detail message. If summary is to be overridden we have to do the
    // same to it also.
    @Override
    public String getDetailMessage()
    {
      FacesContext context    = FacesContext.getCurrentInstance();
      String detailMsgPattern = (String)_customDetailErrorMessage.getValue(context.getELContext());
      if(detailMsgPattern == null)
      {
        // Set a default message that might get used by FacesException
        // constructor for example. This will often happen because
        // ValidatorException constructor will call this method to
        // get the exception message for its parent. So by default
        // we'll use the EL String.
        // Note that by default
        detailMsgPattern = _customDetailErrorMessage.getExpressionString();

        // Since that string will get parsed by FastMessageFormat, the { }
        // of the EL must be escaped
        //detailMsgPattern = '\'' + detailMsgPattern + '\'';

        // No need to format this string, just return it here.
        return detailMsgPattern;
      }

      Object[] params = super.getParameters();

      if (_hasBoundParameters)
         params = getResolvedParameters();

      return _getFormattedString(detailMsgPattern, params);
    }

    private ValueExpression _customDetailErrorMessage;
    private boolean _hasBoundParameters;
    private static final long serialVersionUID = 1L;
  }

  private static class FormattedErrorMessages extends ErrorMessages
  {
    FormattedErrorMessages(String summary, String detail, Object[] parameters)
    {
      super(summary, detail);
      _parameters = parameters;
    }

    @Override
    public String getMessage()
    {
      return _getFormattedString(super.getMessage(), _parameters);
    }

    @Override
    public String getDetailMessage()
    {
      return _getFormattedString(super.getDetailMessage(), _parameters);
    }

    Object[] _parameters;
    private static final long serialVersionUID = 1L;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(MessageFactory.class);
}
