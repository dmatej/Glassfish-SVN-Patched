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

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ComponentChange;
import org.apache.myfaces.trinidad.change.DocumentChange;

/**
 * Base class for ChangeManager implementations that wrap other ChangeManagers.
 */
abstract public class ChangeManagerWrapper extends ChangeManager
{
  /**
   * Delegates the addComponentChange() implementation to the wrapped
   * ChangeManager.
   */
  @Override
  public void addComponentChange(
    FacesContext context,
    UIComponent component,
    ComponentChange change)
  {
    getWrappedChangeManager().addComponentChange(context, component, change);
  }

  /**
   * Delegates the addDocumentChange() implementation to the wrapped
   * ChangeManager.
   */
  @Override
  public void addDocumentChange(
    FacesContext context,
    UIComponent component,
    DocumentChange change)
  {
    getWrappedChangeManager().addDocumentChange(context, component, change);
  }

  /**
   * Delegates the applyComponentChangesForCurrentView() implementation to 
   * the wrapped ChangeManager.
   */
  @Override
  public void applyComponentChangesForCurrentView(FacesContext context)
  {
    getWrappedChangeManager().applyComponentChangesForCurrentView(context);
  }

  /**
   * Delegates the applyComponentChangesForSubtree() implementation to the 
   * wrapped ChangeManager.
   */
  @Override
  public void applyComponentChangesForSubtree(FacesContext context, NamingContainer root)
  {
    getWrappedChangeManager().applyComponentChangesForSubtree(context, root);
  }

  /**
   * Delegates the applySimpleComponentChanges() implementation to the wrapped
   * ChangeManager.
   */
  @Override
  public void applySimpleComponentChanges(FacesContext context, UIComponent component)
  {
    getWrappedChangeManager().applySimpleComponentChanges(context, component);
  }

  /**  
   * Returns the wrapped ChangeManager.
   */  
  abstract protected ChangeManager getWrappedChangeManager();
}
