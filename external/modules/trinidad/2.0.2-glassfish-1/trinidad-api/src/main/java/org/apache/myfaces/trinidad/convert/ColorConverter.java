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

import java.awt.Color;

import java.text.ParseException;

import javax.el.ValueExpression;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFProperty;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * <p>Converters string to Color object and vice versa based on
 * the patterns and the transparency set.</p>
 *
 * Pattern format for colors:
 *
 * <p>
 * <strong>Color Format Syntax:</strong>
 * <p>
 * To specify the color format use a <em>color pattern</em> string.
 * In this pattern, all ASCII letters are reserved as pattern letters,
 * which are defined as the following:
 * <blockquote>
 * <pre>
 * Symbol   Meaning                 Presentation        Example
 * ------   -------                 ------------        -------
 * r        red component           (Number)            242
 * g        green component         (Number)            242
 * b        blue component          (Number)            242
 * a        alpha component         (Number)            255
 * R        red component           (Hex)               F2
 * G        green component         (Hex)               F2
 * B        blue component          (Hex)               F2
 * A        alpha component         (Hex)               FF
 * '        escape for text         (Delimiter)
 * ''       single quote            (Literal)           '
 * </pre>
 * </blockquote>
 * <p>
 * <strong>Examples:</strong>
 * <blockquote>
 * <pre>
 * Format Pattern                         Result
 * --------------                         -------
 * "#RRGGBB"                         ->>  #6609CC
 * "rrr,ggg,bbb"                     ->>  102,009,204
 * "t"                               ->>  Transparent (when alpha is zero)
 * </pre>
 * </blockquote>
 * <p>
 * If patterns is not set then it defaults to patterns  "#RRGGBB", "r,g,b"
 * The first pattern is special - it is always used for formatting color values.
 * For default case, getAsString() will use "#RRGGBB" format to represent the color.
 * Object as String.</p>
 *
 * <p>The <code>getAsObject()</code> method parses a String into a {@link java.awt.Color},
 * according to the following algorithm:</p>
 * <ul>
 * <li>If the specified String is null, return
 *     a <code>null</code>.  Otherwise, trim leading and trailing
 *     whitespace before proceeding.</li>
 * <li>If the specified String - after trimming - has a zero length,
 *     return <code>null</code>.</li>
 * <li>Parses the trimmed string as mentioned in the documentation,
 *     If the string does not match the patterns specified  throw
 *     {@link ConverterException} containing a {@link #CONVERT_MESSAGE_ID} message.
 *     The detail message of {@link #CONVERT_MESSAGE_ID} can be overridden
 *     by setting a custom error message, by calling
 *     method <code>setMessageDetailConvert()</code>. The custom message
 *     can contain placeholders as specified in {@link #CONVERT_MESSAGE_ID}.
 *     The placeholders will be replaced by appropriate values as mentioned
 *     in {@link #CONVERT_MESSAGE_ID}</li>
 * </ul>
 *
 * <p>The <code>getAsString()</code> method expects a value of type
 * {@link Color} (or a subclass), and creates a formatted
 * String according to the following algorithm:</p>
 * <ul>
 * <li>If the specified value is null, return a zero-length String.</li>
 * <li>If the specified value is a String, return it unmodified.</li>
 * <li>return the string representation of color based on the first pattern.</li>
 * </ul>
 *
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/convert/ColorConverter.java#0 $) $Date: 10-nov-2005.19:09:09 $
 */
@JSFConverter(configExcluded=true)
public class ColorConverter implements Converter, StateHolder
{

  /**
   * <p>Standard converter id for this converter.</p>
   */
  public static final String CONVERTER_ID = "org.apache.myfaces.trinidad.Color";

 /**
   * <p>The message identifier of the {@link FacesMessage} to be created when
   * input value cannot be converterd to color based on the patterns set.
   * The message format string for this message may optionally include a
   * <code>{0}</code>, <code>{1}</code>, <code>{4}</code> placeholdes,
   * which will be replaced  by input value, component label  and the pattern
   * set in the converter.</p>
   */
    public static final String CONVERT_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ColorConverter.CONVERT";

 /**
   * <p>The string identifier for the "Transparent" option 
   * set in the converter.</p>
   */
    private static final String TRANSPARENT =
        "org.apache.myfaces.trinidad.convert.ColorConverter.TRANSPARENT";

  // Deprecated array:  arrays should never be public constants,
  // as they are mutable
  /**
   * @deprecated use getDefaultColorFormatPatterns()
   */
  @Deprecated
  public static final String[] DEFAULT_COLOR_FORMAT_PATTERNS = 
    getDefaultColorFormatPatterns();


  /**
   * <p>Returns the default patterns to be used if the pattern if not supplied.
   * The default patterns is <code>"#RRGGBB", "r,g,b"</code>
   * The first pattern is special, it is always used for formatting color
   * values</p>
   */
  public static final String[] getDefaultColorFormatPatterns()
  {
    return new String[]
    {
      "#RRGGBB",
      "r,g,b"
    };
  }
 
  /**
   * <p>Construct a ColorConverter with preconfigured values.</p>
   * @param patterns The set of R,G,B format patterns that
   *                 are accepted by this Converter.  The first
   *                 pattern is special - it is always used for
   *                 formatting color values.
   * @param allowsTransparent Indicates whether or not transparent
   *                 colors are considered valid.
   */
  public ColorConverter(
    String[] patterns,
    boolean  allowsTransparent
    )
  {
    if (patterns == null)
    {
      _facesBean.setProperty(_PATTERNS_KEY, getDefaultColorFormatPatterns());
    }
    else
    {
      _facesBean.setProperty(_PATTERNS_KEY, patterns);
    }
    setTransparentAllowed(allowsTransparent);
  }

  /**
   * <p>Construct a ColorConverter with the default values.
   * The defualt patterns being "#RRGGBB", "r,g,b" and allowsTransparent is set
   * to false.</p>
   */
  public ColorConverter()
  {
    this(null, false);
  }

  /**
   * <p>Convert the specified string value, which is associated with
   * the specified {@link UIComponent}, into a Color object
   * based on the patterns set.</p>
   *
   * @param context {@link FacesContext} for the request being processed
   * @param component {@link UIComponent} with which this model object
   *  value is associated
   * @param value String value to be converted (may be <code>null</code>)
   *
   * @return <code>null</code> if the value to convert is <code>null</code>,
   *  otherwise return a Color object.
   *
   * @exception ConverterException if conversion cannot be successfully
   *  performed
   * @exception NullPointerException if <code>context</code> or
   *  <code>component</code> is <code>null</code>
   *
   */
  public Object getAsObject(
    FacesContext context,
    UIComponent component,
    String value)
  {
    if (context == null || component == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));

    if (value == null)
      return null;

    value = value.trim();

    if (0 == value.length())
     return null;

    try
    {
      return _parseString(context, value);
    }
    catch (ParseException pe)
    {
      throw new ConverterException(
                         _getConvertMessage(context, component, value,
                                            getPatterns()));
    }
  }

  /**
   * <p>Return a String representation for the Color object based on the first
   * pattern in the given patterns.</p>
   *
   * @param context {@link FacesContext} for the request being processed
   * @param component {@link UIComponent} with which this model object
   *        value is associated.
   * @param value Model object value to be converted (may be <code>null</code>)
   *
   * @return a zero-length String if value is <code>null</code>,
   *  otherwise String representation for the Color object based on the first
   * pattern in the specified patterns.
   *
   * @exception ConverterException if conversion cannot be successfully
   *  performed
   * @exception NullPointerException if <code>context</code> or
   *  <code>component</code> is <code>null</code>
   * @exception IllegalArgumentException if the <code>value</code> is not of
   * type other than {@link java.awt.Color}, {@link java.lang.String} or
   * <code>value</code> can be null.
   *
   */
  public String getAsString(
    FacesContext context,
    UIComponent  component,
    Object       value)
  {
    if (context == null || component == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT_OR_UICOMPONENT"));

    if (value == null)
      return "";

     // if the incoming value is String then just return it.
     if (value instanceof String)
       return (String) value;


    if (value instanceof Color)
    {
      return _formatObject(context, (Color)value);
    }
    else throw new IllegalArgumentException("'value' is not of type java.awt.Color'");
  }

  /**
   * <p>Set if localized transparent text should be supported by this
   * converter.</p>
   * @param isTransparentAllowed
   */
  public void setTransparentAllowed(
    boolean isTransparentAllowed)
  {
    Boolean isAllowed = (isTransparentAllowed ? Boolean.TRUE : Boolean.FALSE);
    _facesBean.setProperty(_TRANSPARENT_ALLOWED_KEY, isAllowed);
  }

  /**
   * <p>Set the R,G, B patterns, based on the patterns set, Color object is
   * created during call to getAsObject(FacesContext,UIComponent, String),
   * while based on the first pattern which is at index 0, the String
   * representation for the object is determined with call to
   * getAsString(FacesContext, UIComponent, Object). <code>null</code>
   * value for patterns result in IllegalArgumentException.</p>
   * @param patterns
   * @throws IllegalArgumentException if a value of pattern is null or if an invalid pattern is passed.
   */
  public void setPatterns(String[] patterns) throws IllegalArgumentException
  {
     if (null == patterns || patterns.length == 0)
     {
       throw new IllegalArgumentException(_LOG.getMessage(
         "PATTERN_MUST_HAVE_VALUE"));
     }
     String[] newPatterns = new String[patterns.length];
     System.arraycopy(patterns, 0, newPatterns, 0, patterns.length);
     _facesBean.setProperty(_PATTERNS_KEY, newPatterns);
  }

  /**
   * <p>Retrun the patterns set for this converter.</p>
   * @return patterns
   */
  @JSFProperty
  public String[] getPatterns()
  {
    return ComponentUtils.resolveStringArray(_facesBean.getProperty(_PATTERNS_KEY));
  }

  /**
   * <p>Return if localized transparent text should be supported by this
   * converter.</p>
   */
  @JSFProperty(defaultValue="false")
  public boolean isTransparentAllowed()
  {
    return ComponentUtils.resolveBoolean(_facesBean.getProperty(_TRANSPARENT_ALLOWED_KEY));
  }

  /**
   * <p>Compares this ColorConverter with the specified Object for equality.</p>
   * @param obj  Object to which this ColorConverter is to be compared.
   * @return true if and only if the specified Object is a ColorConverter
   * and if the values patterns, transparentAllowed and transient are equal.
   */
  @Override
  public boolean equals(Object obj)
  {
    if ( null == obj)
      return false;

    if (this == obj)
      return true;

    if (!(obj instanceof ColorConverter))
      return false;

    ColorConverter other = (ColorConverter)obj;

    return ( ( this.isTransient() == other.isTransient() ) &&
             ( this.isTransparentAllowed() == other.isTransparentAllowed()) &&
             ( _isEqualPatterns(other.getPatterns())) &&
             ( ConverterUtils.equals(getMessageDetailConvert(),
                                     other.getMessageDetailConvert()))
            );
  }

  /**
   * <p>Returns the hash code for this converter.</p>
   * @return a hash code value for this object.
   */
  @Override
  public int hashCode()
  {
    int result = 17;
    result = 37 * result + (isTransient() ? 1 : 0);
    result = 37 * result + (isTransparentAllowed() ? 1 : 0 );
    String[] patterns = getPatterns();
    for (int i = 0; i < patterns.length; i++)
    {
      result = 37 * result + patterns[i].hashCode();
    }
    String convMsgDet = getMessageDetailConvert();
    result = result * 37 + (convMsgDet == null ? 0 : convMsgDet.hashCode());
    return result;
  }

  public boolean isTransient()
  {
    return _isTransient;
  }

  public void setTransient(boolean isTransient)
  {
    _isTransient = isTransient;
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
   * Custom error message to be used, for creating detail part of
   * the faces message, when <code>value</code> cannot be converted
   * to {@link java.awt.Color}. Overrides the detail message identified by
   * {@link #CONVERT_MESSAGE_ID}
   * @param convertMessageDetail Custom error message.
   * @see #CONVERT_MESSAGE_ID
   */
  public void setMessageDetailConvert(String convertMessageDetail)
  {
    _facesBean.setProperty(_CONVERT_MESSAGE_DETAIL_KEY, convertMessageDetail);
  }

  /**
   * Return custom detail error message that was set for creating faces message,
   * for values that cannot be converted to {@link java.awt.Color}
   * @return Custom error message.
   * @see #setMessageDetailConvert(String)
   */
  @JSFProperty
  public String getMessageDetailConvert()
  {
    return ComponentUtils.resolveString(_facesBean.getProperty(_CONVERT_MESSAGE_DETAIL_KEY));
  }

  /**
   * <p>Custom hint message.</p>
   * Overrides default hint message
   * @param hintFormat Custom hint message.
   */
  public void setHint(String hintFormat)
  {
    _facesBean.setProperty(_HINT_FORMAT_KEY, hintFormat);
  }

  /**
   * <p>Return custom hint message.</p>
   * @return Custom hint message.
   * @see  #setHint(String)
   */
  @JSFProperty(tagExcluded=true)
  public String getHint()
  {
    Object obj = _facesBean.getProperty(_HINT_FORMAT_KEY);
    return ComponentUtils.resolveString(obj);
  }

  protected String getTransparentString(FacesContext context)
  {
    String msg = MessageFactory.getString(context, TRANSPARENT);
    return msg;
  }

  /**
   * <p>Returns the value as a Color.</p>
   */
  private Object _parseString(
    FacesContext  context,
    String        colorString
    ) throws ParseException
  {
    boolean isTrans = isTransparentAllowed();
    if (isTrans &&
        colorString != null &&
        colorString.equals(getTransparentString(context)))
      return _TRANSPARENT_COLOR;

    ParseException pe = null;

    String[] thePatterns =  getPatterns();
    for (int i=0; i < thePatterns.length; i++)
    {
      try
      {
        Object  value = _getColorFormat(thePatterns[i]).parse(colorString);
        return value;
      }
      catch (ParseException e)
      {
        // ignore, try lenient patterns
        pe = e;
      }
    }
    // throw the last parse exception
    if (pe != null)
      throw pe;

    return null;
  }

  private String _formatObject(
    FacesContext context,
    Color color
    )
  {
    if (color != null)
    {
       boolean isTrans = isTransparentAllowed();
      if (isTrans && color.getAlpha() == 0)
        return getTransparentString(context);

      return _getFormattingColorFormat().format(color);
    }
    else
    {
      return null;
    }
  }

  private ColorFormat _getFormattingColorFormat()
  {
    // Use the first pattern for formatting
    return _getColorFormat(_getOutputPattern());
  }

  private ColorFormat _getColorFormat(
    String pattern
    )
  {
    return new RGBColorFormat(pattern);
  }

  private String _getOutputPattern()
  {
    String[] patterns = getPatterns();
    return patterns[0];
  }

  private Object _getRawConvertMessageDetail()
  {
    return _facesBean.getRawProperty(_CONVERT_MESSAGE_DETAIL_KEY);
  }

  private boolean _isEqualPatterns(String[] patterns)
  {
    String[]  thisPattern = getPatterns();
    if (null == thisPattern  && null == patterns)
      return true;

    if ((thisPattern == null  && patterns != null) ||
          (patterns == null && thisPattern != null) ||
            (thisPattern.length != patterns.length))
      return false;

    for (int i = 0; i < thisPattern.length; i++)
    {
      if (!thisPattern[i].equals(patterns[i]))
        return false;
    }
    return true;
  }

  private FacesMessage _getConvertMessage(
    FacesContext context,
    UIComponent  component,
    String       value,
    String[]       patternsArray
    )
  {
    Object noMatchMsgDet = _getRawConvertMessageDetail();

    Object label = ConverterUtils.getComponentLabel(component);

    StringBuffer patterns = new StringBuffer();
    for (int i = 0; i < patternsArray.length ; i++)
    {
      patterns.append(patternsArray[i]);
      patterns.append(' ');
    }

    Object[] params = {label, value, patterns, null, null};

    FacesMessage msg = MessageFactory.getMessage(context,
                                                 CONVERT_MESSAGE_ID,
                                                 noMatchMsgDet,
                                                 params,
                                                 component);
    return  msg;
  }


  private static final Color  _TRANSPARENT_COLOR = new Color(0,0,0,0);

  /**
   * Identifies if this class is to be persisted
   */
  private boolean _isTransient;

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  /**
   * Should allow color to be transparent
   */
  private static final PropertyKey _TRANSPARENT_ALLOWED_KEY
    = _TYPE.registerKey("transparentAllowed", Boolean.class, Boolean.FALSE);

  private static final PropertyKey _PATTERNS_KEY
    = _TYPE.registerKey("patterns", String[].class,
                        getDefaultColorFormatPatterns());

  private static final PropertyKey _CONVERT_MESSAGE_DETAIL_KEY
    = _TYPE.registerKey("messageDetailConvert", String.class);

  private static final PropertyKey  _HINT_FORMAT_KEY =
    _TYPE.registerKey("hint", String.class);

  private FacesBean _facesBean = ConverterUtils.getFacesBean(_TYPE);

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ColorConverter.class);
}
