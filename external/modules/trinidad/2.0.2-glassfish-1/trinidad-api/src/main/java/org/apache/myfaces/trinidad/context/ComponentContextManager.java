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
package org.apache.myfaces.trinidad.context;

import java.util.Iterator;

import javax.faces.context.FacesContext;


/**
 * Interface that controls access to the stack of component context changes. This class
 * allows components to push changes onto a stack that must be rolled back in order to perform
 * an {@link javax.faces.component.UIComponent#invokeOnComponent} or
 * {@link javax.faces.component.UIComponent#visitTree} call in the proper context.
 * <p>This functionality allows changes that only should apply when a component is currently
 * processing its lifecycle to be temporarily suspended. For example, a component that alters
 * the EL context should undo and changes when the component is not processing. An example of
 * this is the Trinidad table that injects "var" and "varStatus" variables into the EL context
 * while the table is iterating. By saving off these variables and restoring them, the EL will
 * be correct should code perform an invoke on component or visit tree call, reentering the
 * component tree from the view root.</p>
 */
public abstract class ComponentContextManager
{
  /**
   * Push a change onto the stack.
   *
   * @param change The change to push
   */
  public abstract void pushChange(ComponentContextChange change);

  /**
   * Remove the latest change from the stack.
   *
   * @return the change that has been removed from the stack
   * @throws IllegalStateException if an attempt is made to pop an empty stack
   */
  public abstract ComponentContextChange popChange()
    throws IllegalStateException;

  /**
   * Get the latest change on the stack without removing it.
   *
   * @return the latest change or null if none
   */
  public abstract ComponentContextChange peekChange();

  /**
   * Suspend the entire stack of context changes. Used by the document to clear all changes
   * during an invoke on component or visit tree call.
   *
   * @param facesContext The faces context
   * @return an object to use to pass back to the {@link #resume} method to resume the suspend
   * changes at a later time
   */
  public abstract SuspendedContextChanges suspend(FacesContext facesContext);

  /**
   * Suspend the changes on the stack to an arbitrary point. This method is useful should a
   * component need to back out changes only to a certain point. For example, a compound
   * component may need to back out any changes to be able to evaluate EL in the context
   * of the defining page.
   *
   * @param facesContext the faces context
   * @param callback a callback interface used to determine how far back to suspend.
   * @return an object to use to pass back to the {@link #resume} method to resume the suspend
   * changes at a later time
   */
  public abstract SuspendedContextChanges partialSuspend(
    FacesContext    facesContext,
    SuspendCallback callback);

  /**
   * Resume a set of suspended changes.
   *
   * @param facesContext the faces context
   * @param suspendedChanges a set of changes that have been previously suspended
   * @return Iterator of changes that were resumed
   * during the call (iterates in the earliest to most recent order).
   */
  public abstract Iterator<ComponentContextChange> resume(
    FacesContext            facesContext,
    SuspendedContextChanges suspendedChanges);
}
