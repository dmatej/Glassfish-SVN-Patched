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

import javax.faces.FacesException;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

/**
 * An InternalView is a view ID that is handled internally to the webapp,
 * without (necessarily) dispatching to an external resource.  This
 * can also be used to register pseudo-URLs that 
 * <p>
 * InternalViews should be registered with a .properties-syntax file at
 * <code>/META-INF/org.apache.myfaces.trinidad.render.InternalView.properties</code>
 */
public abstract class InternalView
{  
  /**
   * Creates the UIViewRoot.
   * <p>
   * Unlike ViewHandler.createView(), null is an acceptable return value -
   * it indicates that a default, empty UIViewRoot for this viewId should be
   * created on behalf of the InternalView, which might populate the
   * view during renderView().
   */
  abstract public UIViewRoot createView(FacesContext context, String viewId);

  /**
   * Restores the UIViewRoot;  return null if no view should be returned.
   */
  abstract public UIViewRoot restoreView(FacesContext context, String viewId);
  
  /**
   * Renders the view.
   */
  abstract public void renderView(
    FacesContext context, 
    UIViewRoot   viewToRender) throws IOException, FacesException;
  

  /**
   * Return true if this view is stateless;  which, by default, it is.
   * Stateless views will have no state saved during Render Response. 
   * restoreView() will still be called, so a stateless view can
   * still process postback by returning a populated component tree
   * from restoreView().
   */
  public boolean isStateless(FacesContext context, String viewId)
  {
    return true;
  }
}

