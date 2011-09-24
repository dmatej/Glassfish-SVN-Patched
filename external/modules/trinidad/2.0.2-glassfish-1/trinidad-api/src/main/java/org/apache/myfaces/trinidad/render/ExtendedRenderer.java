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
package org.apache.myfaces.trinidad.render;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

/**
 * TraversingRenderer enhances the general component renderer contract, to
 * control all child component hierarchy traversal and local client id
 * generation.
 *
 */
abstract public class ExtendedRenderer extends Renderer
                                       implements LifecycleRenderer
{
  /**
   * Indicates that this Renderer takes responsibility for encoding
   * its child components.
   *
   * @return true, always
   */
  @Override
  public final boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Decodes a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  @SuppressWarnings("unchecked")
  public boolean decodeChildren(
    FacesContext context,
    UIComponent  component)
  {
    return false;
  }

  /**
   * Validates a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  @SuppressWarnings("unchecked")
  public boolean validateChildren(
    FacesContext context,
    UIComponent  component)
  {
    return false;
  }

  /**
   * Updates a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  @SuppressWarnings("unchecked")
  public boolean updateChildren(
    FacesContext context,
    UIComponent  component)
  {
    return false;
  }
}
