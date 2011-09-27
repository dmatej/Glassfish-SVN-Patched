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

import java.io.IOException;

import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.event.WindowLifecycleListener;

/**
 * <p>
 * Manages the set of Windows currently in the Session and allows listeners on the Windows'
 * lifecycles to be registered.
 * </p>
 * @see org.apache.myfaces.trinidad.context.RequestContext#getWindowManager
 */
abstract public class WindowManager
{
  /**
   * @param extContext ExternalContext so that the WindowManager may be called before the
   * FacesContext is available
   * @return The Window that contains the document making the current request
   */
  public abstract Window getCurrentWindow(ExternalContext extContext);
  
  /**
   * @param extContext ExternalContext so that the WindowManager may be called before the
   * FacesContext is available
   * @return The Unmodifiable Map of WindowIds to Windows
   */
  public abstract Map<String, ? extends Window> getWindows(ExternalContext extContext);

  /**
   * <p>
   * Registers a listener that will be informed of changes to the Lifecylce state of any of
   * the known Windows.
   * </p>
   * <p>
   * WindowLifecycleListener may be registered automatically by adding a file
   * containing the names of the classes implementing the WindowListener in a file named
   * <code>org.apache.myfaces.trinidad.event.WindowListener</code> inside of
   * the <code>META_INF/services</code> directory.
   * </p>
   * @param extContext ExternalContext so that the WindowManager may be called before the
   * FacesContext is available
   * @param windowListener
   */
  public abstract void addWindowLifecycleListener(
    ExternalContext extContext, WindowLifecycleListener windowListener);

  /**
   * Removes a listener that will be informed of changes to the Lifecylce state of any of
   * the known Windows
   * @param extContext ExternalContext so that the WindowManager may be called before the
   * FacesContext is available
   * @param windowListener
   */
  public abstract void removeWindowLifecycleListener(
    ExternalContext extContext, WindowLifecycleListener windowListener);

  /**
   * Performs any necessary action to embed the current window identifier into the output
   * @param context FacesContext to use to write the output
   * @throws IOException if an output exception occurs
   */
  public abstract void writeState(FacesContext context) throws IOException;
}
