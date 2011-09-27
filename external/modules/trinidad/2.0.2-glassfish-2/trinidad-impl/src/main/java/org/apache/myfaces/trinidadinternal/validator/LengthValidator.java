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
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 * <p>Implementation for length of <code>java.lang.String</code> values.</p>
 *
 */
@JSFValidator(
        name="tr:validateLength",
        bodyContent="empty",
        id="org.apache.myfaces.trinidad.Length",
        tagClass="org.apache.myfaces.trinidadinternal.taglib.validator.ValidateLengthTag")
public class LengthValidator extends org.apache.myfaces.trinidad.validator.LengthValidator
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
   * @todo Format these numbers properly.
   */
  public String getClientValidation(
    FacesContext context,
    UIComponent component)
  {
    int max = getMaximum();
    int min = getMinimum();
    // If min is specified, and max is not, it implies that max is Integer.MAX_VALUE
    if (min > 0 && max == 0)
    {
      max = Integer.MAX_VALUE;
    }

    // Only pass down the messages that are relevant to this
    // validator instance, based on the min and max
    String detailKey = null;
    String hintKey = null;
    String detail = null;
    String hint = null;
    if (min > 0)
    {
      if (max != Integer.MAX_VALUE)
      {
        detailKey = "range";
        hintKey = "hintRange";
        if (min == max)
        {
          detail  = getMessageDetailExact();
          hint = getHintExact();
        }
        else
        {
          detail  = getMessageDetailNotInRange();
          hint = getHintNotInRange();
        }
      }
      else
      {
        detailKey = "min";
        hintKey = "hintMin";
        detail = getMessageDetailMinimum();
        hint = getHintMinimum();
      }
    }
    else
    {
      detailKey = "max";
      hintKey = "hintMax";
      detail = getMessageDetailMaximum();
      hint = getHintMaximum();
    }

    Map<String, String> cMessages = null;
    if ((detail != null) || (hint != null))
    {
      cMessages = new HashMap<String, String>();
      if (detail != null)
        cMessages.put(detailKey, detail);
      if (hint != null)
        cMessages.put(hintKey, hint);
    }

    return _getTrLengthValidator(context, component, max, min, cMessages);

  }

  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }

  private static String _getTrLengthValidator(
    FacesContext context,
    UIComponent component,
    int         max,
    int         min,
    Map<String, String> messages)
  {
    StringBuilder outBuffer = new StringBuilder();
    outBuffer.append("new TrLengthValidator(");
    if (max == Integer.MAX_VALUE)
      outBuffer.append("null");
    else
      outBuffer.append(IntegerUtils.getString(max));
    outBuffer.append(',');
    outBuffer.append(IntegerUtils.getString(min));
    if(messages == null)
    {
      outBuffer.append(')');
    }
    else
    {
      outBuffer.append(',');
      try
      {
        JsonUtils.writeMap(outBuffer, messages, false);
      }
      catch (IOException e)
      {
        outBuffer.append("null");
      }
      outBuffer.append(')');
    }

    return outBuffer.toString();
  }

  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrLengthValidator()" );

}
