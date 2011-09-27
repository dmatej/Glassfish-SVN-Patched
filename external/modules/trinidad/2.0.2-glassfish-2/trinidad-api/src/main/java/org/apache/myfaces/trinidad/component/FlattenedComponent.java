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

import javax.faces.context.FacesContext;

/**
 * Interface implemented by Components that don't render any content themselves but rather set
 * up context before asking their children to render themselves.  Implementing this interface
 * enables Renderers for components that contain instances of the FlattenedComponents to
 * visit each descendant in  a flattened view of their children, recursively including any
 * FlattenedComponent treating all of these descendants as direct children.
 * <p>
 * A good indicator that a
 * component should implement FlattenedComponent is that the component doesn't delegate to a
 * Renderer, but rather renders itself.
 * 
 * @see ComponentProcessor
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
 * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
 */
public interface FlattenedComponent
{
  /**
   * Set up the component context, process all of the renderable children of this component,
   * and the restore the previous context, returning <code>true</code> if any of the children
   * were processed.
   * <p>
   * The context set up and tear down to perform is identical to that which the component
   * would perform when handling rendering or implementing <code>invokeOnComponent</code>
   * <p>
   * To handle actually processing the children, the component will typically delegate to one
   * of the two <code>UIXComponent.processFlattenedChildren</code> helpers.  If the component only
   * processes a single child, as UIXSwitcher does, it will call the version taking a single child
   * as the argument.  If it processes all of its children as UIXIterator and UIXGroup do, it
   * will call <code>getChildren</code> and pass the List&lt;UIComponent> to the version accepting
   * an Iterable&lt;UIComponent>.
   * <p>
   * This method should only be called if <code>FlattenedComponent.isFlatteningChildren</code>
   * returns <code>true</code>.  If called when <code>FlattenedComponent.isFlatteningChildren</code>
   * is <code>false</code> the behavior is undefined and the implementation may throw an
   * IllegalStateException.
   * <p>
   * This method may only be called when the FlattenedComponent is in the correct context
   * to process itself.
   * @param context Current FacesContext
   * @param cpContext ComponentProcesingContext represetning the current child iteration state
   * @param childProcessor ComponentProcessor to call for each flattened child
   * @param callbackContext childProcessor-specific context to be passed on each call to the
   * childProcessor
   * @return <code>true</code> if this FlattenedComponent actually processed any children
   * @throws IOException if an error occurs while processing children
   * @throws IllegalStateException if called when <code>isFlatteningChildren()</code> is
   * <code>false</code>.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see #isFlatteningChildren
   */  
  public <S> boolean processFlattenedChildren(
   FacesContext context,
   ComponentProcessingContext cpContext,
   ComponentProcessor<S> childProcessor,
   S callbackContext) throws IOException;

  /**
   * Returns <code>true</code> if this FlattenedComponent is currently flattening its children. This
   * allows a FlattenedComponent instance to flatten or not flatten its children as it sees fit.
   * <p>
   * It is illegal to call <code>processFlattenedChildren</code> on a FlattenedComponent that
   * has returned <code>false</code> from <code>isFlatteningChildren</code>.
   * @param context FacesContext
   * @return <code>true</code> if this FlattenedComponent is currently flattening its children
   * @see #processFlattenedChildren
   */  
  public boolean isFlatteningChildren(FacesContext context);
}
