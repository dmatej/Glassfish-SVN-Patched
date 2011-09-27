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

import org.apache.myfaces.trinidad.component.UIXNavigationHierarchy;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue that returns the clientId of the component.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/MenuSelectedValueBoundValue.java#0 $) $Date: 10-nov-2005.18:50:06 $
 * @todo if menu and process end up same component family this can be simplified
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class MenuSelectedValueBoundValue implements BoundValue
{
   public MenuSelectedValueBoundValue(
    UIComponent component
    )
  {
    _menuComponent = (UIXNavigationHierarchy)component;
  }
  
  public Object getValue(UIXRenderingContext context)
  {
      
    UIComponent stamp = getStamp(_menuComponent);
    
    if(stamp != null)
    { 
      // Save the current key  
      Object oldPath =_menuComponent.getRowKey();
      boolean isNewPath = setNewPath(_menuComponent);
      if (isNewPath)
      {
        String clientId = stamp.getClientId(context.getFacesContext());
        _menuComponent.setRowKey(oldPath);
        return clientId;
      }
    }
    
    return null;
  }
  
  abstract protected boolean setNewPath(
    UIXNavigationHierarchy  menuComponent
  );
  
  
  abstract protected UIComponent getStamp(
    UIXNavigationHierarchy   menuComponent
    );
  
  
  UIXNavigationHierarchy _menuComponent;
}
