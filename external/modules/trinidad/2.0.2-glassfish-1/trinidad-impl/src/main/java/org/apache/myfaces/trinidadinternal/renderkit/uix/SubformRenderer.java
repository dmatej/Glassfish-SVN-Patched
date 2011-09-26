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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.component.UIXSubform;

/**
 * Renderer for Subform component.
 */
public class SubformRenderer extends Renderer
{
  @SuppressWarnings("unchecked")
  static public List<String> getSubformList(
    FacesContext context,
    boolean      isDefault,
    boolean      createIfNeeded)
  {
    String key = isDefault ? _DEFAULT_LIST_KEY : _LIST_KEY;
    Map<String, Object> requestMap = 
      context.getExternalContext().getRequestMap();
    
    List<String> list = (List<String>) requestMap.get(key);
    if ((list == null) && createIfNeeded)
    {
      list = new ArrayList<String>();
      requestMap.put(key, list);
    }
    
    return list;
  }

  @Override
  public void encodeBegin(FacesContext context, UIComponent component)
  {
    boolean isDefault = ((UIXSubform) component).isDefault();
    String clientId = component.getClientId(context);
    getSubformList(context, false, true).add(clientId);
    if (isDefault)
      getSubformList(context, true, true).add(clientId);
  }

  @Override
  public boolean getRendersChildren()
  {
    return false;
  }
  
  static private final String _DEFAULT_LIST_KEY = 
    "org.apache.myfaces.trinidadinternal.DefaultSubForms";
  static private final String _LIST_KEY = 
    "org.apache.myfaces.trinidadinternal.SubForms";
}
