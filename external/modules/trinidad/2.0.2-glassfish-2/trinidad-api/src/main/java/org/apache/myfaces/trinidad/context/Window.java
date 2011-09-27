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

import java.io.Serializable;

import java.util.Map;

/**
 * Represents a Window in the current user's Session.  Windows are created and vended
 * by the Session's WindowManager and the Window for the current request is
 * available from <code>WindowManager.getCurrentWindow</code>
 * @see WindowManager#getCurrentWindow
 */
public abstract class Window implements Serializable
{
  /**
   * <p>
   * Represents the current state of the Window.  Windows start out <code>OPEN</code>,
   * when the current window's document is being unloaded, they move to the <code>UNLOADING</code>
   * state and then either move back to the <code>OPEN</code> state if the Window's content
   * is populated with a new document from the same application, or to the <code>CLOSED</code>
   * state if it is not.
   * </p><p>
   * This represents the framework's best guess at the current status of the Window.
   * </p>
   */
  public enum LifecycleState
  {
    /** The Window is currently open */
    OPEN,

    /** The Window is being unloaded */
    UNLOADING,

    /** The Window is believed to be closed, either because the window was explicitly closed
     *  or because the window is suspected to have been closed
     */
    CLOSED
  }
 
  /**
   * Represents how the window is used in the application
   */
  public enum Usage
  {
    /** Used as a top-level application window */
    FRAME,

    /** Used as a dialog */
    DIALOG
  }
 
  /**
   * @return The unique identifier for this Window within the Session
   */
  public abstract String getId();

  /**
   * @return The current state of the Window
   */
  public abstract LifecycleState getLifecycleState();

  /**
   * Returns the Usage of the Window--either a top-level frame or a dialog
   * @return how the window is used
   */
  public abstract Usage getUsage();
  
  /**
   * @return <code>true</code> if the window's document hasn't been rendered since the Window
   * was created.
   */
  public abstract boolean isNew();

  /**
   * Returns the Map for storing data associated with this Window object.  If the environment is
   * configured for fail-over, the contents of this Map must be Serializable.
   * @return The client data storage Map.
   */
  public abstract Map<String, Object> getWindowMap();

  private static final long serialVersionUID = 1L;
}