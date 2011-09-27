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

import java.io.UnsupportedEncodingException;

import java.nio.charset.IllegalCharsetNameException;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
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
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Validator for validating the byte length of strings.
 * If encoding is not specified, defaults to ISO-8859-1
 */
/**
 * <p><strong>ByteLengthValidator</strong> is a {@link Validator} that checks
 * the value of the corresponding component for its byte length for the set
 * character encoding. The following algorithm is implemented:</p>
 * <ul>
 * <li>If the passed value is <code>null</code>, exit immediately.</li>
 * <li>If the current component value is not of String type throw
 *      IllegalArgumentException
 * <li>If both <code>encoding</code> and <code>maximum</code> property
 *     has been configured on this {@link Validator}, check the component
 *     value byte length against the maximum. If the component value byte length
 *     is greater than this specified maximum, throw a {@link ValidatorException}
 *     containing a MAXIMUM_MESSAGE_ID message.</li>
 * <li>If only <code>maximum</code> property has been configured on this
 *     {@link Validator}, check the component value against
 *     this limit defaulting the encoding to be <code>iso-8859-1</code>.
 *     If the component value length is greater than the specified maximum,
 *     throw a {@link ValidatorException} containing a MAXIMUM_MESSAGE_ID
 *     message.</li>
 * <li>If a <code>encoding</code> property has been configured on this
 *     {@link Validator}, and if it is not a valid Java encoding, then throws a
 *     {@link java.nio.charset.IllegalCharsetNameException}</li>
 *
 * <li>If  <code>maximumMessageDetail</code> is set, it is used for constructing
 *     faces message, when validation fails. The message can contain placeholders
 *     which will be replaced as specified in {@link #MAXIMUM_MESSAGE_ID}
 * </li>
 * </ul>
 * @see #setMessageDetailMaximum(String)
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/validator/ByteLengthValidator.java#0 $) $Date: 10-nov-2005.19:08:32 $
 */
@JSFValidator(configExcluded=true)
public class ByteLengthValidator  implements StateHolder, Validator
{

  /**
   * <p>The message identifier of the {@link FacesMessage} to be created if
   * the maximum byte length check fails.  The message format string for this
   * message may optionally include  <code>{0}</code>, <code>{1}</code> and
   * <code>{2}</code> placeholders, which will be replaced by input value, label
   * associated with the component and the maximum bytes respectively.<p>
   */
  public static final String MAXIMUM_MESSAGE_ID =
    "org.apache.myfaces.trinidad.validator.ByteLengthValidator.MAXIMUM";

  /**
   *  <p>Standard validator id for this validator.</p>
   */
  public static final String VALIDATOR_ID = "org.apache.myfaces.trinidad.ByteLength";


  /**
   * <p>Construct a {@link Validator} with <code>iso-8859-1</code> as the encoding
   * and <code>zero</code> as the maximum bytes allowed.</p>
   */
  public ByteLengthValidator()
  {
    setEncoding("iso-8859-1");
  }

  /**
   * <p>Construct a {@link Validator} with the specified preconfigured
   * values.</p>
   * @param maximum the maximum number of bytes allowed.
   * @param encoding the Java character set encoding. This must be
   *        an encoding supported by Java.
   */
  public ByteLengthValidator(int maximum, String encoding)
  {
    super();
    setMaximum(maximum);
    setEncoding(encoding);
  }

  /**
   * <p>Set the character encoding for this {@link Validator}.</p>
   *
   * @param encoding The character encoding.
   */
  public void setEncoding(String encoding)
  {
    _facesBean.setProperty(_ENCODING_KEY, encoding);
  }

  /**
   * <p>Return the character encoding set for this {@link Validator} or
   * <code>iso-8859-1</code> if it has not been set.</p>
   */
  @JSFProperty(defaultValue="iso-8859-1")
  public String getEncoding()
  {
    Object encoding = _facesBean.getProperty(_ENCODING_KEY);
    return ComponentUtils.resolveString(encoding);
  }

  /**
   * <p>Set the maximum bytes to be enforced by this {@link Validator}.</p>
   *
   * @param maximum The new maximum value
   *
   */
  public void setMaximum(int maximum)
  {
    _facesBean.setProperty(_MAXIMUM_KEY, Integer.valueOf(maximum));
  }

  /**
   * <p>Return the maximum bytes to be enforced by this {@link
   * Validator} or <code>zero</code> if it has not been
   * set.</p>
   */
  @JSFProperty
  public int getMaximum()
  {
    return ComponentUtils.resolveInteger(_facesBean.getProperty(_MAXIMUM_KEY));
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage},  when users input exceeds the maximum byte length.</p>
   * Overrides detail message identified by message id {@link #MAXIMUM_MESSAGE_ID}
   * @param maximumMessageDetail Custom error message.
   */
  public void setMessageDetailMaximum(String maximumMessageDetail)
  {
    _facesBean.setProperty(_MAXIMUM_MESSAGE_DETAIL_KEY, maximumMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for values that exceeds the maximum byte length.</p>
   * @return Custom error message.
   * @see  #setMessageDetailMaximum(String)
   */
  @JSFProperty
  public String getMessageDetailMaximum()
  {
    Object obj = _facesBean.getProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom hint message.</p>
   * Overrides default hint message
   * @param hintMaximum Custom hint message.
   */
  public void setHintMaximum(String hintMaximum)
  {
    _facesBean.setProperty(_HINT_MAXIMUM_KEY, hintMaximum);
  }

  /**
   * <p>Return custom hint message.</p>
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
   * <p>Validates unless it is too long, in which case throws
   * ValidatorException.</p>
   * @exception ValidatorException if validation fails
   * @exception NullPointerException if <code>context</code>
   * @exception IllegalCharsetNameException if <code>encoding</code> is
   * @exception IllegalArgumentException if <code>value</code> is not of type
   *            {@link java.lang.String}
   * <code>unsupported</code>
   *  or <code>component</code> is <code>null</code>
   */
  public void validate(
    FacesContext context,
    UIComponent component,
    Object value
    ) throws ValidatorException
  {

    if ((context == null) || (component == null))
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));
    }

    if (value != null)
    {
      ValidatorUtils.assertIsString(value,
                                    "'value' is not of type java.lang.String.");
      String theValue  = (String)value;

      int maxBytes = getMaximum();
      try
      {
        byte[] bytes = theValue.getBytes(getEncoding());
        if (bytes.length > maxBytes)
          throw new ValidatorException(
            getLengthValidationFailureMessage(context, component, theValue));

      }
      catch (UnsupportedEncodingException uee)
      {
        throw new IllegalCharsetNameException(_LOG.getMessage(
          "ENCODING_NOT_SUPPORTED_BY_JVM", getEncoding()));
      }
    }
  }

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
    return (_isTransient);
  }

  public void setTransient(boolean transientValue)
  {
    _isTransient = transientValue;
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

  /**
   * <p>Compares this ByteLengthValidator with the specified Object for
   * equality.</p>
   * @param object  Object to which this ByteLengthValidator is to be compared.
   * @return true if and only if the specified Object is a ByteLengthValidator
   * and if the values encoding, maximum and transient are equal.
   */
  @Override
  public boolean equals(Object object)
  {

    if (this == object)
      return true;

    if ( object instanceof ByteLengthValidator )
    {
      ByteLengthValidator other = (ByteLengthValidator) object;
      String encoding = getEncoding();
      String otherEncoding = other.getEncoding();
      String otherMsgMaxDet = other.getMessageDetailMaximum();
      String msgMaxDet = getMessageDetailMaximum();

      if ( this.isTransient() == other.isTransient() &&
            ValidatorUtils.equals(encoding, otherEncoding) &&
            ValidatorUtils.equals(msgMaxDet, otherMsgMaxDet) &&
            (getMaximum() == other.getMaximum())
         )
      {
        return true;
      }
    }
    return false;
 }

 /**
  * <p>Returns the hash code for this Validator.</p>
  * @return a hash code value for this object.
  */
  @Override
  public int hashCode()
  {
    int result = 17;
    String maximumMsgDet = getMessageDetailMaximum();
    String encoding = getEncoding();
    result = 37 * result + (encoding == null? 0 : encoding.hashCode());
    result = 37 * result + (_isTransient ? 0 : 1);
    result = 37 * result + getMaximum();
    result = 37 * result + (maximumMsgDet == null? 0 : maximumMsgDet.hashCode());
    return result;
  }

  /**
   * The {@link FacesMessage} to be returned if byte length validation fails.
   * @param context Faces context
   * @param value   The value entered / set by the user on the component
   * @return error message when the length exceeds the maximum byte length set.
   */
  protected FacesMessage getLengthValidationFailureMessage(
    FacesContext context,
    UIComponent component,
    String value
    )
  {
    Object label = ValidatorUtils.getComponentLabel(component);

    Object maxMesgDetail = _getRawMaximumMessageDetail();
    String maximumBytes  = String.valueOf(getMaximum());

    Object[] params = { label, value, maximumBytes};

    FacesMessage msg = MessageFactory.getMessage(context,
                                                 MAXIMUM_MESSAGE_ID,
                                                 maxMesgDetail,
                                                 params,
                                                 component);
    return msg;
  }

  private Object _getRawMaximumMessageDetail()
  {
    return _facesBean.getRawProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
  }

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _ENCODING_KEY =
    _TYPE.registerKey("encoding", String.class, "iso-8859-1");

  private static final PropertyKey _MAXIMUM_KEY =
    _TYPE.registerKey("maximum", int.class, 0);

  private static final PropertyKey  _MAXIMUM_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("messageDetailMaximum", String.class);

  private static final PropertyKey  _HINT_MAXIMUM_KEY =
    _TYPE.registerKey("hintMaximum", String.class);

  private FacesBean _facesBean = ValidatorUtils.getFacesBean(_TYPE);

  private boolean _isTransient = false;


  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ByteLengthValidator.class);
}
