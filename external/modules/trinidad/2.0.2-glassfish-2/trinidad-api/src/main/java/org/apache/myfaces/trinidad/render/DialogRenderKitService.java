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

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

/**
 * Interface to be implemented by RenderKits that support
 * the launching of dialogs.  The
 * {@link org.apache.myfaces.trinidad.component.UIXCommand UIXCommand} components
 * will not support launching dialogs unless the render kit provides an
 * implementation of this API.  Developers should always retrieve
 * an instance using {@link org.apache.myfaces.trinidad.util.Service#getService},
 * passing in the current RenderKit.
 * </p>
 * <pre>
 *    RenderKit rk = facesContext.getRenderKit();
 *    DialogService service = (DialogRenderKitService)
 *      Service.getService(rk, DialogRenderKitService.class);
 * </pre>
 *
 */
public interface DialogRenderKitService
{
  /**
   * Launch a dialog, if possible given the current agent's capabilities.
   * If the dialog cannot be launched, return false, indicating that
   * the caller should fallback to launching a simple subprocess.
   * The dialog may not be launched immediately;  this method
   * may result in simply queuing up behavior for the Render Response
   * phase, when {@link ExtendedRenderKitService#encodeScripts} can output
   * markup that will actually launch the dialog.
   *
   * @param context the current FacesContext
   * @param source the source component
   * @param targetRoot the UIViewRoot that should be displayed
   * @param processParameters a set of parameters to populate the
   *   newly created pageFlowScope
   * @param useWindow if true, try to show the dialog in a separate
   *   window.  The meaning of this parameter can be interpreted
   *   by the RenderKit, and does not necessarily have to map
   *   to a full window.
   * @param windowProperties a map of UI parameters used to
   *   modify the dialog.  The list of property names that are
   *   supported will depend on the <code>RenderKit</code>, but
   *   common examples include "width" and "height".
   * @return true if launching the dialog was handled by this service, false
   *   if it could not be, in which case ADF Faces will fall back on
   *   default dialog functionality.
   */
  public boolean launchDialog(
    FacesContext       context,
    UIViewRoot         targetRoot,
    UIComponent        source,
    Map<String,Object> processParameters,
    boolean            useWindow,
    Map<String,Object> windowProperties);

  /**
   * Called to return from a dialog.
   * @param context the current FacesContext
   * @param returnValue the value being returned from the dialog
   */
  public boolean returnFromDialog(
    FacesContext context,
    Object       returnValue);

  /**
   * Returns true if the RenderKit is aware that a dialog has
   * returned, and the given source component was responsible
   * for launching that dialog.
   *
   * @param context the current FacesContext
   * @param source the source component
   */
  public boolean isReturning(
    FacesContext context,
    UIComponent  source);
}
