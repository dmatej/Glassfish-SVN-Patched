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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreResponseStateManager;

/**
 * 
 */ 
public class FormUtils
{
  @SuppressWarnings("unchecked")
  static public boolean wasSubmitted(
    FacesContext context,
    UIComponent component)
  {
    Map<String, String> paramMap = 
      context.getExternalContext().getRequestParameterMap();
    
    Object formName = paramMap.get(CoreResponseStateManager.FORM_FIELD_NAME);
    boolean submitted = false;
    
    if ( formName != null )  
      submitted = formName.equals(component.getClientId(context));

    return submitted;
  }
}
