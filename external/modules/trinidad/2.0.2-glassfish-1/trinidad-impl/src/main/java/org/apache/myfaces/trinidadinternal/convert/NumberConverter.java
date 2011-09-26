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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

@JSFConverter(
        name="tr:convertNumber",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.Number",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.convert.ConvertNumberTag")
public class NumberConverter extends org.apache.myfaces.trinidad.convert.NumberConverter
                   implements ClientConverter
{
  public NumberConverter()
  {
  }
  
  @Override
  public Object getAsObject(
    FacesContext context,
    UIComponent component,
    String value)
    throws ConverterException
  {
    Object number = super.getAsObject(context, component, value);
    if (number == null) // bug 4137626
      return null;

    // this is causing issue TRINIDAD-690    
    number = 
      DateTimeConverter.__typeConvert(context, this, component, value, number);
    return number;
  }
  
  @Override
  public String getAsString(
    FacesContext context, 
    UIComponent component,
    Object value)
    throws ConverterException 
  {
    if (value == null)
      return null;

    GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
    // we support other types of numbers, like oracle.jbo.domain.Number:
    if ((!(value instanceof Number)) && fac.isConvertible(value, Number.class))
    {
      value = fac.convert(value, Number.class);
    }
    // bug 4214147:
    return super.getAsString(context, component, value);
  }

  public String getClientConversion(FacesContext context, UIComponent component)
  {
    String hintPattern = this.getHintPattern();
    String messageDetailConvertNumber   = this.getMessageDetailConvertNumber();
    String messageDetailConvertPercent  = this.getMessageDetailConvertPercent();
    String messageDetailConvertCurrency = this.getMessageDetailConvertCurrency();
    Map<String, String> cMessages = null;
    if(hintPattern != null || messageDetailConvertNumber != null || messageDetailConvertPercent != null || messageDetailConvertCurrency != null)
    {
      cMessages = new HashMap<String, String>();
      cMessages.put("hintPattern", hintPattern);
      cMessages.put("number", messageDetailConvertNumber);
      cMessages.put("percent", messageDetailConvertPercent);
      cMessages.put("currency", messageDetailConvertCurrency);
    }
    
    return _getTrNumberConverter(context, component, cMessages);
  }

  public Collection<String> getClientImportNames()
  {
    // Load the library for the different locale, so that the locale elements
    // are available for the client converter and validator.
    if (_isDifferentLocale())
    {
      ArrayList<String> names = new ArrayList<String>(2);
      names.add("TrNumberConverter()");
      // Load NamedLocaleInfoScriptlet  "LocaleInfo_<locale>"
      String sloc = getLocale().toString();
      StringBuffer sb = new StringBuffer(11 + sloc.length());
      sb.append("LocaleInfo_");
      sb.append(sloc);
      names.add(sb.toString());
      return names;
    }   

    return _IMPORT_NAMES;
  }

  public String getClientLibrarySource(FacesContext context)
  {
    return null;
  }

  public String getClientScript(FacesContext context, UIComponent component)
  {
    return null;
  }
  
  /**
   * Helper method, that creates an Object array, which contains all
   * required constructor parameters for the TrNumberConverter class.
   * 
   * TrNumberConverter takes several arguments, like pattern or type.
   * It also takes some arguments that are only useful, when displaying
   * formatted numbers, like currencyCode or maximumIntegerDigits.
   * 
   */
  private Object[] _getClientConstructorParams(FacesContext context, Map<?, ?> messages)
  {
    Object[] params;
    boolean formating = _formatingAttributesSet();

    if(formating)
      params = new Object[12];
    else
      params = new Object[4];

    // We call this since the pattern may contain the generic currency sign, which we don't 
    // want to display to the user.
    params[0] = getLocalizedPattern(context, getPattern(), null);

    params[1] = this.getType();
    params[2] = this.getLocale() != null ? this.getLocale().toString() : null;
    params[3] = messages;
    
    //TODO we don't really need these attributes all the time,
    //only if specified.
    if(formating)
    {
      params[4] = this.isIntegerOnly();
      params[5] = this.isGroupingUsed();
      params[6] = this.getCurrencyCode();
      params[7] = this.getCurrencySymbol();
      params[8] = this.isMaximumFractionDigitsSet() ? this.getMaxFractionDigits() : null;
      params[9] = this.isMaximumIntegerDigitsSet() ? this.getMaxIntegerDigits() : null;
      params[10] = this.isMinimumFractionDigitsSet() ? this.getMinFractionDigits() : null;
      params[11] = this.isMinimumIntegerDigitsSet() ? this.getMinIntegerDigits() : null;
    }

    return params;
  }
  
  /*
   * checks if the attributes, that are interesting for
   * formating only are applied.
   */
  private boolean _formatingAttributesSet()
  {
    return (this.getCurrencyCode()!=null ||
      this.getCurrencySymbol() != null ||
      this.isGroupingUsed() != true ||
      this.isIntegerOnly() != false ||
      this.isMaximumFractionDigitsSet() ||
      this.isMaximumIntegerDigitsSet() ||
      this.isMinimumFractionDigitsSet() ||
      this.isMinimumIntegerDigitsSet());
  }
  
  private String _getTrNumberConverter(
      FacesContext context,
      UIComponent  component,
      Map<?, ?>    messages)
    {
      StringBuilder outBuffer = new StringBuilder(250);
      outBuffer.append("new TrNumberConverter(");

      Object[] params = _getClientConstructorParams(context, messages);
      for (int i = 0; i < params.length; i++)
      {
        try
        {
          JsonUtils.writeObject(outBuffer, params[i], false); 
        } 
        catch (Exception e)
        {
          outBuffer.append("null");
        }
        if (i < params.length-1)
          outBuffer.append(',');
      }
      outBuffer.append(')');
      return outBuffer.toString();
    }
  
  /*
   * This method returns true if the locale specified for NumberConverter is
   * different from the locale specified in Adf-faces-config or the client
   * locale. If both are same or if no locale is specified for
   * NumberConverter, then this returns false.
   */
  private boolean _isDifferentLocale()
  {
    Locale converterLocale = getLocale();
    if (converterLocale != null)
    {
      Locale defaultLocale =
        RenderingContext.getCurrentInstance().getLocaleContext().getFormattingLocale();
      return !converterLocale.equals(defaultLocale);
    }

    return false;
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(NumberConverter.class);

  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
}
