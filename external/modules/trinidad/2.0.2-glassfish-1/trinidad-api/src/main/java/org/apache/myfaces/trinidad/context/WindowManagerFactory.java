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

import javax.faces.context.ExternalContext;

/**
 * <p>
 * Application-scoped factory for creating per-Session WindowManager instances.  It is the
 * WindowManagerFactory implementation's responsibility to ensure that only one
 * WindowManager instance is created per-session.  The WindowManagerFactory is also responsible
 * for ensuring that any mutable state in the WindowManager instances will be successfully failed
 * over.
 * </p>
 * <p>
 * The factory is usually specified by placing the name of the WindowManagerFactory
 * implementation class in a file named
 * <code>org.apache.myfaces.trinidad.context.WindowManagerFactory</code>
 * in the <code>META-INF/services</code> directory
 * </p>
 * @see org.apache.myfaces.trinidad.context.WindowManager
 * @see org.apache.myfaces.trinidad.context.RequestContext#getWindowManager
 */
abstract public class WindowManagerFactory
{
  /**
   * Returns the WindowManager to use for this session, creating a new instance if one doesn't
   * already exist.
   * @param extContext ExternalContext
   * @return WindowManager to use for this Session
   */
  public abstract WindowManager getWindowManager(ExternalContext extContext);
}
