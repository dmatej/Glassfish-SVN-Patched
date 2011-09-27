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
package org.apache.myfaces.trinidadinternal.convert;

import java.math.BigInteger;
import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import javax.faces.convert.ConverterException;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.MessageFactory;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * Private utilities for working with converters
 */
public class ConverterUtils
{
  private ConverterUtils()
  {
  }


  /**
   * This is integer in the mathematical sense, not the java sense.
   * This method is called when a ConverterException has already occurred
   */
  public static final ConverterException getIntegerConverterException(
    FacesContext       context,
    UIComponent        component,
    ConverterException originalCE,
    String             value,
    String             convertMessageId,
    String             maxMessageId,
    String             maxString,
    String             minMessageId,
    String             minString
  )
  {
    // value should never be null, but just in case check for it
    if (value != null)
    {
      value = value.trim();
      try
      {
        // if this throws an NFE then value isn't an integer
        /*BigInteger bi = */new BigInteger(value);

        // value is a number but it's out of range. See if it's a
        // positive or negative number.
        boolean isNegative = false;
        if (value.startsWith("-"))
          isNegative = true;

        if (isNegative)
        {

          return createConverterException(context,
                                          component,
                                          minMessageId,
                                          value,
                                          minString);
        }
        else
        {

          return createConverterException(context,
                                                        component,
                                                        maxMessageId,
                                                        value,
                                                        maxString);
        }
      }
      catch (NumberFormatException nfe)
      {

        return createConverterException(context,
                                         component,
                                         convertMessageId,
                                         value);
      }

    }

    return originalCE;
  }
   public static final ConverterException createConverterException(
     FacesContext context,
     UIComponent  component,
     String       messageId,
     String       value
   )
   {
     return createConverterException(context, component,
                                     messageId, value, null);
   }

   public static final ConverterException createConverterException(
     FacesContext context,
     UIComponent  component,
     String       messageId,
     String       value,
     String       param
   )
   {
      Object label = _getLabel(component);
      FacesMessage message = MessageFactory.getMessage(context,
                                          messageId,
                                          new Object[]{label, value, param},
                                          label);
      return new ConverterException(message);
   }


  // We currently use 'label' for the validation failed message
  private static final Object _getLabel(
    UIComponent component
  )
  {
    Object o = component.getAttributes().get("label");
    if (o == null)
      o = component.getValueBinding("label");

    return o;
  }

  /**
   * Create a converter for a type.
   */
  static public Converter createConverter(
    FacesContext context,
    Class<?>     converterType)
  {
    // Don't bother for Objects;  note that the 1.1_01 RI
    // returns null, but the spec requires a FacesException, and MyFaces
    // correctly implements that.
	  
	// https://issues.apache.org/jira/browse/TRINIDAD-1117
	// Note - JSF 1.2 allows converter for String: https://javaserverfaces-spec-public.dev.java.net/issues/show_bug.cgi?id=131   
    if (converterType == null ||
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
    catch (FacesException e)
    {
      _LOG.warning("CANNOT_CREATE_CONVERTER_LIKELY_BECAUSE_NO_CONVERTER_REGISTERED", converterType.toString());
      return null;
    }
  }

  /**
   * @deprecated method needs to an overhaul
   */
  @Deprecated
  public static String getClientValidation(
    FacesContext context,
    UIComponent component,
    String maxId,
    String minId,
    String defaultId,
    String maxVal,
    String minVal,
    String type)
  {
    return _getClientConversion(context, component, maxId, minId, defaultId,
                                maxVal, minVal, false, type);
  }

  public static String getClientConversion(
    FacesContext context,
    UIComponent component,
    String maxId,
    String minId,
    String defaultId,
    String maxVal,
    String minVal)
  {
    return _getClientConversion(context, component, maxId, minId, defaultId,
                                maxVal, minVal, true, null);
  }

  private static String _getClientConversion(
    FacesContext context,
    UIComponent component,
    String maxId,
    String minId,
    String defaultId,
    String maxVal,
    String minVal,
    boolean isConverter,
    String validatorType)
  {
    StringBuilder outBuffer = new StringBuilder(250);

    if (isConverter)
      outBuffer.append("new TrNumberConverter(");
    else
      outBuffer.append("new " + validatorType + "(");

    outBuffer.append("{LV:'");
    FacesMessage maxMessage =
      MessageFactory.getMessage(context, maxId,
                                new Object[]{"{0}", "{1}", maxVal});

    outBuffer.append(XhtmlLafUtils.escapeJS(maxMessage.getDetail()));

    outBuffer.append("',LV_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(maxMessage.getSummary()));    
    outBuffer.append("',MV:'");

    FacesMessage minMessage =
      MessageFactory.getMessage(context, minId,
                                new Object[]{"{0}", "{1}", minVal});

    outBuffer.append(XhtmlLafUtils.escapeJS(minMessage.getDetail()));
    
    outBuffer.append("',MV_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(minMessage.getSummary()));  

    outBuffer.append("',D:'");

    FacesMessage defaultMessage =
      MessageFactory.getMessage(context, defaultId,
                                new Object[]{"{0}", "{1}"});

    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getDetail())); 
    
    outBuffer.append("',D_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getSummary()));

    outBuffer.append("'},null,0,");
    outBuffer.append(maxVal);
    outBuffer.append(',');
    outBuffer.append(minVal);
    outBuffer.append(")");

    return outBuffer.toString();
  }

  /**
   * @deprecated not used currently
   * @param context
   * @param component
   * @param defaultId
   * @return
   */
  @Deprecated
  public static String getClientConversion(
    FacesContext context,
    UIComponent component,
    String defaultId)
  {
    StringBuilder outBuffer = new StringBuilder(250);

      outBuffer.append("new TrNumberConverter(");

    outBuffer.append("{D:'");

    FacesMessage defaultMessage =
      MessageFactory.getMessage(context, defaultId,
                                new Object[]{"{0}", "{1}"});

    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getDetail()));
    outBuffer.append("',D_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getSummary()));
    outBuffer.append("'})");

    return outBuffer.toString();
  }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ConverterUtils.class);
}
