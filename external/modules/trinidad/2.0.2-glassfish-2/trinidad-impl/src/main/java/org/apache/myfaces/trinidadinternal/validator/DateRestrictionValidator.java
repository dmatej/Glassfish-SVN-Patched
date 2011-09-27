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

package org.apache.myfaces.trinidadinternal.validator;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFValidator;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 *
 */
@JSFValidator(
        name="tr:validateDateRestriction",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.DateRestriction",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.validator.ValidateDateRestrictionTag")
public class DateRestrictionValidator extends org.apache.myfaces.trinidad.validator.DateRestrictionValidator
                                       implements ClientValidator
{
  public DateRestrictionValidator()
  {
    super();
  }

  @Override
  public void validate(
    FacesContext context,
    UIComponent  component,
    Object       value) throws ValidatorException
  {
    if (value == null)
      return;
    
    if (!(value instanceof Date))
    {
      GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
      value = fac.convert(value, Date.class);
    }
    super.validate(context, component, value);
  }
  
  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }


  /**
   * @todo this should have not_in_range messages, not just max and min!
   * @todo Format these numbers properly.
   */
  public String getClientValidation(
    FacesContext context,
    UIComponent component)
  {
    String[] weekdaysValue = getInvalidDaysOfWeek();
    String weekdaysValues = null;
    StringBuilder sb1 = new StringBuilder();
    
    String[] monthValue = getInvalidMonths();
    String monthValues = null;
    StringBuilder sb2 = new StringBuilder();
    monthValues = sb2.toString();

    try
    {
      JsonUtils.writeObject(sb1, weekdaysValue, false);
      weekdaysValues = sb1.toString();
      JsonUtils.writeObject(sb2, monthValue, false);
      monthValues = sb2.toString();
    }
    catch (IOException e)
    {
      weekdaysValues  = "null";
      monthValues  = "null";
    }
    
    
    String messageDetailDaysOfWeek = this.getMessageDetailInvalidDaysOfWeek();
    String messageDetailMonth = this.getMessageDetailInvalidMonths();
    String hintWeek = this.getHintInvalidDaysOfWeek();
    String hintMonth = this.getHintInvalidMonths();

    
    Map<String, String> cMessages = null;
    if(messageDetailDaysOfWeek != null || messageDetailMonth != null || hintWeek != null || hintMonth != null)
    {
      cMessages = new HashMap<String, String>();
      cMessages.put("days", messageDetailDaysOfWeek);
      cMessages.put("month", messageDetailMonth);
      cMessages.put("hintWeek", hintWeek);
      cMessages.put("hintMonth", hintMonth);
    }

    return _getTrDateRestrictionValidator(context, component, weekdaysValues, monthValues, cMessages);
  }
  
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private static String _getTrDateRestrictionValidator(
      FacesContext context,
      UIComponent component,
      String weekdaysValues,
      String monthValues,
      Map<String, String> messages)
  {
    StringBuilder outBuffer = new StringBuilder(31 + weekdaysValues.length() + monthValues.length());
    outBuffer.append("new TrDateRestrictionValidator(");
    outBuffer.append(weekdaysValues);
    outBuffer.append(',');
    outBuffer.append(monthValues);
    outBuffer.append(',');
    if(messages == null)
    {
      outBuffer.append("null");
    }
    else
    {
      try
      {
        JsonUtils.writeMap(outBuffer, messages, false);
      }
      catch (IOException e)
      {
        outBuffer.append("null");
      }
    }
    outBuffer.append(')');

    return outBuffer.toString();
  }
  
  
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
}