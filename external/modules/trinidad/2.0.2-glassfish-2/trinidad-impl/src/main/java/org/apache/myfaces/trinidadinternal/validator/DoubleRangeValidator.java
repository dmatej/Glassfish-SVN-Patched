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
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFValidator;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 * <p>Implementation for <code>java.lang.Long</code> values.</p>
 *
 */
@JSFValidator(
        name="tr:validateDoubleRange",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.DoubleRange",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.validator.ValidateDoubleRangeTag")
public class DoubleRangeValidator extends org.apache.myfaces.trinidad.validator.DoubleRangeValidator
                                implements ClientValidator
{

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
    double max = getMaximum();
    double min = getMinimum();
    String maxStr = max == Double.MAX_VALUE ? "null" : Double.toString(max);
    String minStr = min == Double.MIN_VALUE ? "null" : Double.toString(min);
    
    String messageDetailMax = this.getMessageDetailMaximum();
    String messageDetailMin = this.getMessageDetailMinimum();
    String messageDetailRange = this.getMessageDetailNotInRange();
    String hintMax = this.getHintMaximum();
    String hintMin = this.getHintMinimum();
    String hintRange = this.getHintNotInRange();
    
    Map<String, String> cMessages = null;
    if(messageDetailMax != null || messageDetailMin != null || messageDetailRange != null || hintMax != null || hintMin != null|| hintRange != null)
    {
      cMessages = new HashMap<String, String>();
      cMessages.put("max", messageDetailMax);
      cMessages.put("min", messageDetailMin);
      cMessages.put("range", messageDetailRange);
      cMessages.put("hintMax", hintMax);
      cMessages.put("hintMin", hintMin);
      cMessages.put("hintRange", hintRange);
    }
    
    return  _getTrRangeValidator(context, component, maxStr, minStr, cMessages);
  }
  
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private static String _getTrRangeValidator(
      FacesContext context,
      UIComponent component,
      String max,
      String min,
      Map<String, String> messages)
  {
    StringBuilder outBuffer = new StringBuilder();
    outBuffer.append("new TrRangeValidator(");
    outBuffer.append(max);
    outBuffer.append(',');
    outBuffer.append(min);
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