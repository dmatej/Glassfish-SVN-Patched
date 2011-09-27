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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * <h4> Utility class to pass selected status of Links during the process of
 * rendering in the rendering context. </h4>
 * This can be used by all renderers. But it is necessary that all clients
 * save the stauts before setting the selected status and restore it back once
 * its off with its rendering. Thus once the completion of the children are over
 * the status in the parent nodes remains unaffected. If not done diligently
 * could cause problem. Since saving is done on the  context, there
 * could be performance problem.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/LinkUtils.java#0 $) $Date: 10-nov-2005.18:54:00 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class LinkUtils implements UIConstants
{
  private LinkUtils()
  {
  }

  /**
   * This method returns the status of selection.
   * This method is normally used by parent renderers to query the status
   * and store its initial status and then changes the status of this
   * property for resolving the selected link.
   * Then delegates call to the child renderers
   * once its over sets it back to the initial state.
   * So parent renderers if they are going to change the status they should
   * follow the given fashion
   * <pre>
   * 1. Store the inital-status - by calling  isSelected(RenderingContext)
   * 2. Change the stauts if necessary using setSelected(RenderingContext,boolean)
   * 3. Could delegate call to child with the rendering context
   * 4. Then reset the selection status to the initial status calling
   *    setSelected(RenderingContext,initial-status)
   * </pre>
   * @see #setSelected(UIXRenderingContext,boolean)
   * @param context The rendering context
   * @return true if selected else false
   */
  public static boolean isSelected(
    UIXRenderingContext context
  )
  {
    return Boolean.TRUE.equals(
                         context.getProperty( MARLIN_NAMESPACE,
                                             _LINK_STATUS_SELECTED_PROPERTY));
  }

  /**
   * Set the status of selection, should be done with caution.
   * This is later retreived while rendering. Ensure to get and  store the
   * initial status and reset it back to that initial status.
   * @param context The rendering context
   * @param selected selected status of the link.
   * @see #isSelected(UIXRenderingContext)
   */
  public static void setSelected(
    UIXRenderingContext context,
    boolean          selected
  )
  {
    context.setProperty( MARLIN_NAMESPACE,
                         _LINK_STATUS_SELECTED_PROPERTY,
                         Boolean.valueOf(selected));
  }

  /**
   * Called by link containers prior to rendering their children
   * in order to suppress the rendering of the default link
   * style class (.OraLink).  Most link containers (like tabBar,
   * globalHeader) provide their own style classes - the default
   * OraLink style class ends up getting in the way.
   *
   * Important: Each call to startDefaultStyleClassDisabled()
   * must be followed by a matching call to endDefaultStyleClassDisabled().
   */
  public static void startDefaultStyleClassDisabled(UIXRenderingContext context)
  {   
    RenderingContext arc = RenderingContext.getCurrentInstance();
    ((CoreRenderingContext) arc).setDefaultLinkStyleDisabled(true);
  }

  /**
   * Called by link containers that have called
   * startDefaultStyleClassDisabled().
   */
  public static void endDefaultStyleClassDisabled(UIXRenderingContext context)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    ((CoreRenderingContext) arc).setDefaultLinkStyleDisabled(false);
  }

  /**
   * Called by base.desktop.LinkRenderer to detect whether default
   * style class rendering is currently disabled.
   */
  public static boolean isDefaultStyleClassDisabled(UIXRenderingContext context)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    return ((CoreRenderingContext) arc).isDefaultLinkStyleDisabled();
  }


  /**
   * The key which is stored in the rendering context. The status is set as
   * the value for this key.
   */
   private static final Object _LINK_STATUS_SELECTED_PROPERTY = new Object();

}
