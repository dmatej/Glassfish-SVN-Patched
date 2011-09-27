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
package org.apache.myfaces.trinidad.change;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Base class for specialized Change that when applied will add a component
 *  instance to the component tree.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/AddComponentChange.java#0 $) $Date: 10-nov-2005.19:09:55 $
 */
abstract public class AddComponentChange extends ComponentChange
{
  protected AddComponentChange(
    UIComponent component)
  {
    if (component == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "COMPONENT_REQUIRED"));
    
    _proxy = new ChangeComponentProxy(FacesContext.getCurrentInstance(), 
                                      component);
  }
  
  /**
   * Returns the component that is to be added either as a child or a facet 
   *  while applying this Change. Returns <code>null</code> if the component cannot be 
   *  successfully re-constructed.
   */
  public UIComponent getComponent()
  {
    return _proxy.createComponent();
  }

  private final ChangeComponentProxy _proxy;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    AddComponentChange.class);
  private static final long serialVersionUID = 1L;
}