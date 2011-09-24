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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

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
<p><strong>RegExpValidator</strong> is a {@link javax.faces.validator.Validator} that checks
 * the value of the corresponding component against specified pattern
 * using Java regular expression syntax.
 *
 * The regular expression syntax accepted by the RegExpValidator class is
 * same as mentioned in class {@link java.util.regex.Pattern} in package
 * <code>java.util.regex</code>. The following algorithm is implemented:</p>
 *
 * <ul>
 * <li>If the passed value is <code>null</code>, exit immediately.</li>
 *
 * <li>If a <code>pattern</code> property has been configured on this
 *     {@link javax.faces.validator.Validator}, check the component value against this pattern.
 *     If value does not match pattern throw a {@link ValidatorException}
 *     containing a NO_MATCH_MESSAGE_ID message.
 *     If <code>noMatchMessageDetail</code> is set, it is used for constructing faces
 *     message. The message can contain placeholders which will be replaced as
 *     specified in {@link #NO_MATCH_MESSAGE_ID}</li>
 * </ul>
 * @see #setMessageDetailNoMatch(String)
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/validator/RegExpValidator.java#0 $) $Date: 10-nov-2005.19:08:34 $
 */
@JSFValidator(configExcluded=true)
public class RegExpValidator implements StateHolder, Validator
{
  /**
   * <p>Standard validator id for this validator.</p>
   */
  public static final String VALIDATOR_ID = "org.apache.myfaces.trinidad.RegExp";

  /**
   * <p>The message identifier of the {@link FacesMessage}
   * to be created if the match fails.  The message format
   * string for this message may optionally include a <code>{0}</code>,
   * <code>{1}</code> and <code>{4}</code> placeholders, which will be replaced
   * input value, label associated with the component and pattern respectively.</p>
   */
  public static final String NO_MATCH_MESSAGE_ID
    = "org.apache.myfaces.trinidad.validator.RegExpValidator.NO_MATCH";

  /**
   * <p>Construct a RegExpValidator with no preconfigured pattern.</p>
   */
  public RegExpValidator()
  {
    super();
  }

  /**
   * <p>Construct a RegExpValidator with preconfigured pattern.</p>
   */
  public RegExpValidator(String pattern)
  {
    setPattern(pattern);
  }

  /**
   * @exception ValidatorException if validation fails
   * @exception NullPointerException if <code>context</code>
   * or <code>component</code> or <code>pattern</code> is <code>null</code>
   * @exception IllegalArgumentException if <code>value</code> is not of type
   * {@link java.lang.String}
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

    if ( value != null)
    {
      ValidatorUtils.assertIsString(value,
                                    "'value' is not of type java.lang.String.");

      if (getPattern() == null)
        throw new NullPointerException(_LOG.getMessage(
          "NULL_REGEXP_PATTERN"));

      // compile the regular expression if we haven't already.
      // we cache the compiled regular expression because we can't cache
      // the RE object, as it isn't thread safe.
      if (_compiled == null)
      {
        try
        {
          _compiled = Pattern.compile(getPattern());
        }
        catch (PatternSyntaxException pse)
        {
          // compilation choked
          throw pse;
        }
      }
      String theValue = (String)value;
      Matcher matcher = _compiled.matcher(theValue);
      // the matched string has to be the same as the input
      if (! matcher.matches())
      {
        throw new ValidatorException(_getNoMatchFoundMessage(context,
                                                             component,
                                                             theValue));
      }
    }
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

  public Object saveState(FacesContext context)
  {
    return _facesBean.saveState(context);
  }

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

  /**
   * <p>Compares this PatternValidator with the specified Object for
   * equality.</p>
   * @param object  Object to which this PatternValidator is to be compared.
   * @return true if and only if the specified Object is a PatternValidator
   * and if the values pattern and transient are equal.
   */
  @Override
  public boolean equals(Object object)
  {
    if (this == object)
      return true;

    if ( object instanceof RegExpValidator )
    {
      RegExpValidator other = (RegExpValidator) object;

      if ( this.isTransient() == other.isTransient() &&
           ValidatorUtils.equals(getPattern(), other.getPattern()) &&
           ValidatorUtils.equals(getMessageDetailNoMatch(),
                                   other.getMessageDetailNoMatch())
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
    String pattern = getPattern();
    String noMesgDetail = getMessageDetailNoMatch();
    result = 37 * result + (pattern == null? 0 : pattern.hashCode());
    result = 37 * result + (isTransient() ? 0 : 1);
    result = 37 * result + (noMesgDetail == null ? 0 : noMesgDetail.hashCode());
    return result;
  }

  /**
   * <p>Custom hint message.</p>
   * Overrides default hint message
   * @param hintPattern Custom hint message.
   */
  public void setHint(String hintPattern)
  {
    _facesBean.setProperty(_HINT_PATTERN_KEY, hintPattern);
  }

  /**
   * <p>Return custom hint message.</p>
   * @return Custom hint message.
   * @see  #setHint(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHint()
  {
    Object obj = _facesBean.getProperty(_HINT_PATTERN_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Set the pattern value to be enforced by this {@link
   * Validator}
   * @param pattern to be enforced.
   */
  public void setPattern(String pattern)
  {
    String prevPattern = getPattern();
    if ((prevPattern != null) && prevPattern.equals(pattern))
      return;
    if ((prevPattern == null) && (pattern == null))
      return;

    _facesBean.setProperty(_PATTERN_KEY, pattern);
    _compiled = null;
  }

  /**
   * <p>Return the pattern value to be enforced by this {@link
   * Validator}
   */
  @JSFProperty
  public String getPattern()
  {
    Object obj = _facesBean.getProperty(_PATTERN_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when value does not match the specified pattern.</p>
   * Overrides detail message identified by message id {@link  #NO_MATCH_MESSAGE_ID}
   * @param noMatchMessageDetail
   */
  public void setMessageDetailNoMatch(String noMatchMessageDetail)
  {
    _facesBean.setProperty(_NO_MATCH_MESSAGE_DETAIL_KEY, noMatchMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating faces message,
   * for values that do not match the specified pattern.</p>
   * @return Custom error message
   * @see #setMessageDetailNoMatch(String)
   */
  @JSFProperty
  public String getMessageDetailNoMatch()
  {
    Object obj = _facesBean.getProperty(_NO_MATCH_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(obj);
  }

  /**
   * @todo custom message should be evaluated lazily and then be used for
   * displaying message.
   */
  private FacesMessage _getNoMatchFoundMessage(
    FacesContext context,
    UIComponent component,
    String value)
  {
    Object noMatchMsgDet = _getRawNoMatchMessageDetail();
    Object label = ValidatorUtils.getComponentLabel(component);
    Object[] params = {label, value, getPattern()};

    FacesMessage msg =
        MessageFactory.getMessage(context, NO_MATCH_MESSAGE_ID,
                                  noMatchMsgDet, params, label);
      return msg;
  }

  private Object _getRawNoMatchMessageDetail()
  {
    return _facesBean.getRawProperty(_NO_MATCH_MESSAGE_DETAIL_KEY);
  }


  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _PATTERN_KEY
    = _TYPE.registerKey("pattern", String.class);

  private static final PropertyKey _NO_MATCH_MESSAGE_DETAIL_KEY
    = _TYPE.registerKey("messageDetailNoMatch", String.class);

  private static final PropertyKey  _HINT_PATTERN_KEY =
    _TYPE.registerKey("hint", String.class);

  private FacesBean _facesBean = ValidatorUtils.getFacesBean(_TYPE);

  private transient Pattern _compiled;

  private boolean _isTransient = false;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RegExpValidator.class);
}
