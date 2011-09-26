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
package org.apache.myfaces.trinidadinternal.uinode.bind;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue that returns the clientId of the component.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/ClientIdBoundValue.java#0 $) $Date: 10-nov-2005.18:49:31 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ClientIdBoundValue implements BoundValue
{
  /**
   * Creates a ClientIdBoundValue.
   * @param component the UIComponent
   */
  public ClientIdBoundValue(UIComponent component)
  {
    this(component, false);
  }

  /**
   * Creates a ClientIdBoundValue.
   * @param component the UIComponent
   * @param onlyIfIdSet if true, ClientIdBoundValue will
   *   return null when "id" is null.
   */
  public ClientIdBoundValue(UIComponent component,
                            boolean onlyIfIdSet)
  {
    if (component == null)
      throw new NullPointerException();

    _component = component;
    _onlyIfIdSet = onlyIfIdSet;
  }



  public Object getValue(UIXRenderingContext context)
  {
    if (_onlyIfIdSet)
    {
      String key = CoreOutputText.PARTIAL_TRIGGERS_KEY.getName();
      if (_component.getAttributes().get(key) == null)
      {
        // there were no partialTriggers set on the component. Therefore
        // it is safe to hide the id. see bug 4039431:
        String id = _component.getId();
        if (id == null)
          return null;
  
        if (id.startsWith(UIViewRoot.UNIQUE_ID_PREFIX))
          return null;
      }
    }

    FacesContext fContext = (context == null) ? 
      FacesContext.getCurrentInstance() : context.getFacesContext();
    
    return _component.getClientId(fContext);
  }

  private final UIComponent _component;
  private final boolean     _onlyIfIdSet;
}
