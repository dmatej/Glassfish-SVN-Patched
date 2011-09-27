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
package org.apache.myfaces.trinidad.event;

import java.util.EventListener;

import javax.faces.context.ExternalContext;

/**
 * <p>
 * A listener called when the Lifecyle of a Window changes.
 * </p>
 * <p>
 * Window listeners may be registered automatically by adding a file
 * containing the names of the classes implementing the WindowLifecycleListener in a file named
 * <code>org.apache.myfaces.trinidad.event.WindowLifecycleListener</code> inside of
 * the <code>META_INF/services</code> directory or manually by calling
 * <code>WindowManager.addWindowLifecycleListener</code>
 * @see org.apache.myfaces.trinidad.context.WindowManager
 */
public interface WindowLifecycleListener extends EventListener
{
  /**
   * <p>
   * Called when the LifecycleState of a Window changes.
   * </p>
   * <p>
   * The current lifecycle state of a Window is the framework's best guess and may not be accurate.
   * In particular, the last remaining open window may never move into the <code>CLOSED</code> state
   * once it has moved into the <code>UNLOADED</code> state.  In addition, no Window lifecycle events
   * are delivered if the Session ceases to exist.
   * </p>
   * <p>
   * The FacesContext may not be available at the time that this event is delivered.
   * </p>
   * @param extContext ExternalContext available for this event
   * @param event WindowLifecycleEvent indicating the cause of the change to the Window's
   * LifecycleState
   */
  public abstract void processWindowLifecylce(
    ExternalContext extContext, WindowLifecycleEvent event);
}
