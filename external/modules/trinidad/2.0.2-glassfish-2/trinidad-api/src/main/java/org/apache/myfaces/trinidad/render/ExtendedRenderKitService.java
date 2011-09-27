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

import java.io.IOException;
import javax.faces.context.FacesContext;

/**
 * <p>
 * Service implemented by RenderKits that provide further
 * support for per-page rendering actions, like including
 * scripts global to a page or short-circuiting rendering
 * altogether.  Developers should always retrieve
 * an instance using {@link org.apache.myfaces.trinidad.util.Service#getService},
 * passing in the current RenderKit, or with 
 * {@link org.apache.myfaces.trinidad.util.Service#getRenderKitService},
 * </p>
 * <p><b>Example:</b>  The following code will add a script
 *  to be rendered during the following request:
 * <pre>
 *    ExtendedRenderKitService service = 
 *      Service.getRenderKitService(facesContext, ExtendedRenderKitService.class);
 *    service.addScript(facesContext, "alert('foo');");
 * </pre>
 * </p>
 */
public interface ExtendedRenderKitService
{
  /**
   * Adds a script for execution during rendering.
   */
  public void addScript(FacesContext context, String script);

  /**
   * Output any needed scripts required by the RenderKit
   * for this page.
   */
  public void encodeScripts(
    FacesContext context) throws IOException;

  /**
   * Called to short-circuit rendering the view.
   * A ViewHandler should call this method before
   * rendering the view (for example, before forwarding to a JSP),
   * and if it returns true, do nothing further.
   */
  public boolean shortCircuitRenderView(
    FacesContext context) throws IOException;

  public boolean isStateless(
    FacesContext context);

  /**
   * Called when the encoding of a page begins.
   */
  public void encodeBegin(FacesContext context) throws IOException;

  /**
   * Called when the encoding of a page ends, if there were no exceptions.
   */
  public void encodeEnd(FacesContext context) throws IOException;

  /**
   * Called when the encoding of a page completes, whether or not there
   * were exceptions.
   */
  public void encodeFinally(FacesContext context);
}
