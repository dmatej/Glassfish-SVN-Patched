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
package org.apache.myfaces.trinidad.webapp;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.el.MethodExpression;
import javax.el.ValueExpression;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.webapp.UIComponentELTag;

import javax.servlet.jsp.JspException;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Subclass of UIComponentTag to add convenience methods,
 * and optimize where appropriate.
 */
abstract public class UIXComponentELTag extends UIComponentELTag
{
  public UIXComponentELTag()
  {
  }

  public void setAttributeChangeListener(MethodExpression attributeChangeListener)
  {
    _attributeChangeListener = attributeChangeListener;
  }

  @Override
  public int doStartTag() throws JspException
  {
    int retVal = super.doStartTag();

    //pu: There could have been some validation error during property setting
    //  on the bean, this is the closest opportunity to burst out.
    if (_validationError != null)
      throw new JspException(_validationError);

    return retVal;
  }

  @Override
  protected final void setProperties(UIComponent component)
  {
    if (component instanceof UIViewRoot)
    {
      throw new IllegalStateException(
         "<f:view> was not present on this page; tag " + this +
         "encountered without an <f:view> being processed.");
    }

    super.setProperties(component);

    UIXComponent uixComponent = (UIXComponent) component;

    if (_attributeChangeListener != null)
    {
      uixComponent.setAttributeChangeListener(_attributeChangeListener);
    }

    setProperties(uixComponent.getFacesBean());
  }

  protected void setProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, expression.getValue(null));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.lang.String[].  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of strings.
   * Null values are ignored.
   */
  protected void setStringArrayProperty(
    FacesBean       bean,
    PropertyKey     key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, _parseNameTokens(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.util.List<java.lang.String>.  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of strings.
   * Null values are ignored.
   */
  protected void setStringListProperty(
    FacesBean       bean,
    PropertyKey     key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, 
                       _parseNameTokensAsList(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.util.Set<java.lang.String>.  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of strings.
   * Null values are ignored.
   */
  protected void setStringSetProperty(
    FacesBean       bean,
    PropertyKey     key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, 
                       _parseNameTokensAsSet(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.lang.Number.  If the value
   * is an EL expression, it will be stored as a ValueBinding.
   * Otherwise, it will parsed with Integer.valueOf() or Double.valueOf() .
   * Null values are ignored.
   */
  protected void setNumberProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Object value = expression.getValue(null);
      if (value != null)
      { 
        if (value instanceof Number)
        {
          bean.setProperty(key, value);
        }
        else
        {
          String valueStr = value.toString();
          if(valueStr.indexOf('.') == -1)
            bean.setProperty(key, Integer.valueOf(valueStr));
          else
            bean.setProperty(key, Double.valueOf(valueStr));
        }
      }
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type int[].  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of ints.
   * Null values are ignored.
   */
  protected void setIntArrayProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Object value = expression.getValue(null);
      if (value != null)
      {
        String[] strings = _parseNameTokens(value);
        final int[] ints;
        if (strings != null)
        {
          try
          {
            ints = new int[strings.length];
            for(int i=0; i<strings.length; i++)
            {
              int j = Integer.parseInt(strings[i]);
              ints[i] = j;
            }
          }
          catch (NumberFormatException e)
          {
            _LOG.severe("CANNOT_CONVERT_INTO_INT_ARRAY",value);
            _LOG.severe(e);
            return;
          }
        }
      }
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.util.Date.  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as an ISO 8601 date (yyyy-MM-dd).
   * Null values are ignored.
   */
  protected void setDateProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, _parseISODate(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

    /**
   * Set a property of type java.util.Date.  If the value
   * is an EL expression, it will be stored as a ValueBinding.
   * Otherwise, it will parsed as an ISO 8601 date (yyyy-MM-dd)
   * and the time components (hour, min, second, millisecond) maximized.
   * Null values are ignored.
   */
    protected void setMaxDateProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Date d = _parseISODate(expression.getValue(null));
      Calendar c = Calendar.getInstance();
      TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
      if (tz != null)
        c.setTimeZone(tz);
      c.setTime(d);
      // Original value had 00:00:00 for hours,mins, seconds now maximize those
      // to get the latest time value for the date supplied.
      c.set (Calendar.HOUR_OF_DAY, 23);
      c.set (Calendar.MINUTE, 59);
      c.set (Calendar.SECOND, 59);
      c.set (Calendar.MILLISECOND, 999);
      bean.setProperty(key, c.getTime());
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  protected void setProperties(FacesBean bean)
  {
    // Could be abstract, but it's easier to *always* call super.setProperties(),
    // and perhaps we'll have something generic in here, esp. if we take
    // over "rendered" from UIComponentTag
  }

  /**
   * Sets any fatal validation error that could have happened during property
   *  setting. If this is set, tag execution aborts with a JspException at the
   *  end of doStartTag().
   * @param validationError
   */
  protected void setValidationError(String validationError)
  {
    _validationError = validationError;
  }

  /**
   * Parse a string into a java.util.Date object.  The
   * string must be in ISO 9601 format (yyyy-MM-dd).
   */
  static private final Date _parseISODate(Object o)
  {
    if (o == null)
      return null;

    String stringValue = o.toString();
    try
    {
      return _getDateFormat().parse(stringValue);
    }
    catch (ParseException pe)
    {
      _LOG.info("CANNOT_PARSE_VALUE_INTO_DATE", stringValue);
      return null;
    }
  }

  /**
   * Parses a whitespace separated series of name tokens.
   * @param stringValue the full string
   * @return an array of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   * @todo Move to utility function somewhere (ADF Share?)
   */
  static private final String[] _parseNameTokens(Object o)
  {
    List<String> list = _parseNameTokensAsList (o);

    if (list == null)
      return null;

    return list.toArray(new String[list.size()]);
  }

  static private final List<String> _parseNameTokensAsList (Object o)
  {
    if (o == null)
      return null;

    String stringValue = o.toString();
    ArrayList<String> list = new ArrayList<String>(5);

    int     length = stringValue.length();
    boolean inSpace = true;
    int     start = 0;
    for (int i = 0; i < length; i++)
    {
      char ch = stringValue.charAt(i);

      // We're in whitespace;  if we've just departed
      // a run of non-whitespace, append a string.
      // Now, why do we use the supposedly deprecated "Character.isSpace()"
      // function instead of "isWhitespace"?  We're following XML rules
      // here for the meaning of whitespace, which specifically
      // EXCLUDES general Unicode spaces.
      if (Character.isWhitespace(ch))
      {
        if (!inSpace)
        {
          list.add(stringValue.substring(start, i));
          inSpace = true;
        }
      }
      // We're out of whitespace;  if we've just departed
      // a run of whitespace, start keeping track of this string
      else
      {
        if (inSpace)
        {
          start = i;
          inSpace = false;
        }
      }
    }

    if (!inSpace)
      list.add(stringValue.substring(start));

    if (list.isEmpty())
      return null;

    return list;
  }

  static private final Set<String> _parseNameTokensAsSet (Object o)
  {
    List<String> list = _parseNameTokensAsList(o);

    if (list == null)
      return null;
    else
      return new HashSet(list);
  }

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(UIXComponentELTag.class);

  // We rely strictly on ISO 8601 formats
  private static DateFormat _getDateFormat()
  {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
    if (tz != null)
      sdf.setTimeZone(tz);
    return sdf;
  }

  //  No more used anywhere in Trinidad code, so deprecate since 2.0.x.
  @Deprecated
  public static final String DOCUMENT_CREATED_KEY = "org.apache.myfaces.trinidad.DOCUMENTCREATED";

  private MethodExpression  _attributeChangeListener;
  private String            _validationError;
  
}
