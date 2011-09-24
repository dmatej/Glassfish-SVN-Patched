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

/**
 * An ChangeManager implementation that is all a no-op.
 *
 */
public class NullChangeManager extends ChangeManager
{
  /**
   * {@inheritDoc}
   */
  @Override
  public void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    // do nothing
  }

  /**
   * {@inheritDoc}
   * @param facesContext The FacesContext instance for the current request.
   */
  @Override
  public void applyComponentChangesForCurrentView(FacesContext facesContext)
  {
    //no-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void applyComponentChangesForSubtree(
    FacesContext facesContext,
    NamingContainer root
    )
  {
    //no-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void applySimpleComponentChanges(FacesContext context, UIComponent component)
  {
    //no-op
  }
}
