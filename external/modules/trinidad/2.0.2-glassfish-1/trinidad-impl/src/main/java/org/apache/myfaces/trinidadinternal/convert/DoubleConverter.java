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
 * <p>Implementation for <code>java.lang.Double</code> values.</p>
 *
 */
@JSFConverter(id="javax.faces.Double")
public class DoubleConverter extends javax.faces.convert.DoubleConverter
                             implements ClientConverter
{
    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value cannot be converted
     */
    public static final String CONVERT_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.DoubleConverter.CONVERT";

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
      throw ConverterUtils.createConverterException(context, 
                                                         component,
                                                         CONVERT_MESSAGE_ID, 
                                                         value);                                                      
    }     
    
  }


  public String getClientScript(
   FacesContext context,
   UIComponent component)
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
    return _getTrDoubleConverter(context, component);
  }

  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private String _getTrDoubleConverter(
      FacesContext context,
      UIComponent component)
    {
      return "new TrDoubleConverter()";
    }

  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );

}
