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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * Interface implemented to apply the visitor pattern to a set of components where the
 * components are not necessarily siblings of each other.  When <code>processComponent</code> is
 * called, the component instance is guaranteed to be in the correct context.
 * <p>
 * Instances of this interface are parameterized by the type of the callbackContext the implementor
 * expects.  The typical usage is that the implementor creates and instance of the desired context
 * and calls a helper method on a different object (for example
 * <code>UIXComponent.processComponent</code> with the desired ComponentProcessor and context
 * instances to actually perform the iteration.
 * <p>
 * Because the ComponentProcessor can't look ahead, more complex iteration tasks, such as
 * laying out and rendering components based on the number of components to render may
 * require multiple-passes--one to collect the layout information into the callbackContext using
 * one ComponentProcessor implementation and a second pass using a different ComponentProcessor
 * to actually perform the layout using the information calculated in the first pass.
 * @see FlattenedComponent
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
 */
public interface ComponentProcessor<T>
{
  /**
   * Processes a component
   * @param context The current FacesContext
   * @param cpContext context represtinging the current component iteration state
   * @param component Component to process on this iteration
   * @param callbackContext ComponentProcessor-specific context
   * @throws IOException if processing resulted in an IOException
   */
  public void processComponent(
    FacesContext context,
    ComponentProcessingContext cpContext,
    UIComponent component,
    T callbackContext) throws IOException;
}
