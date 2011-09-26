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

import java.util.Collection;
import java.util.Collections;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFConverter;
import org.apache.myfaces.trinidad.convert.ClientConverter;

/**
 * <p>Implementation for <code>java.lang.Byte</code> values.</p>
 *
 *
 */
@JSFConverter(id="javax.faces.Byte")
public class ByteConverter extends javax.faces.convert.ByteConverter
                           implements ClientConverter
{


    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value is greater than Byte.MAX_VALUE.
     * The message format string for this
     * message may optionally include a <code>{2}</code> placeholder, which
     * will be replaced by Byte.MAX_VALUE.</p>
     */
    public static final String MAXIMUM_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ByteConverter.MAXIMUM";

    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value is less than Byte.MIN_VALUE.
     * The message format string for this
     * message may optionally include a <code>{2}</code> placeholder, which
     * will be replaced by Byte.MIN_VALUE.</p>
     */
    public static final String MINIMUM_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ByteConverter.MINIMUM";

    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value cannot be converted to an integer
     */
    public static final String CONVERT_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ByteConverter.CONVERT";

  @Override
  public Object getAsObject(
    FacesContext context, 
    UIComponent component,
    String value) 
  {
    try
    {
      return super.getAsObject(context, component, value);
    }
    catch(ConverterException ce)
    {
  
      throw ConverterUtils.getIntegerConverterException(context, 
                                                        component, 
                                                        ce,
                                                        value,
                                                        CONVERT_MESSAGE_ID,
                                                        MAXIMUM_MESSAGE_ID,
                                                        _BYTE_MAX,
                                                        MINIMUM_MESSAGE_ID,
                                                        _BYTE_MIN);
    }     
    
  }
  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }

  /**
   * @todo translations
   * @param context
   * @return
   */
  public String getClientConversion(
    FacesContext context,
    UIComponent component)
  {
    return _getTrByteConverter(context, component, _BYTE_MAX, _BYTE_MIN);
  }

  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  private static String _getTrByteConverter(
      FacesContext context,
      UIComponent component,
      String maxVal,
      String minVal)
    {
      StringBuilder outBuffer = new StringBuilder(250);

      outBuffer.append("new TrByteConverter(");

      outBuffer.append("null,null,0,");
      outBuffer.append(maxVal);
      outBuffer.append(',');
      outBuffer.append(minVal);
      outBuffer.append(")");

      return outBuffer.toString();
    }

  private static final String _BYTE_MAX = Byte.toString(Byte.MAX_VALUE);
  private static final String _BYTE_MIN = Byte.toString(Byte.MIN_VALUE);
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
}
