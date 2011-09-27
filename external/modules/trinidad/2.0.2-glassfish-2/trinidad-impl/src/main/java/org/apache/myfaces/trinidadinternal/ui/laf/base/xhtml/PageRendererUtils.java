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

import org.apache.myfaces.trinidad.component.UIXHierarchy;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/PageRendererUtils.java#0 $) $Date: 10-nov-2005.18:54:08 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageRendererUtils 
{
  
  public static final Object getFocusPath(
    UIXRenderingContext context
  )
  {
    return context.getProperty(UIConstants.MARLIN_NAMESPACE, 
                                      _FOCUS_PATH_KEY);
  }  
  
  static final void setFocusPath(
    UIXRenderingContext context,
    Object             focusPath
  )
  {
    context.setProperty(UIConstants.MARLIN_NAMESPACE, 
                        _FOCUS_PATH_KEY, 
                        focusPath);
  }

  /**
   * 
   * @param component 
   * @param focusPath 
   * @param startDepth 
   * @return whether or not a path was set.
   */
  public static final boolean setNewPath(
    UIXRenderingContext context, 
    UIXHierarchy    component,
    int              startDepth
  )
  {
    Object focusPath = getFocusPath(context);
    return ModelRendererUtils.setNewPath(component, startDepth, focusPath);
  }  

  /**
   * Checks to see whether the globalHeader is empty (contains no
   * indexed children).
   */
  public static final boolean isEmpty(
    UIXRenderingContext context,
    UIXHierarchy    component,
    int              startDepth
    )
  {                   
    Object focusPath = getFocusPath(context);
    return ModelRendererUtils.isEmpty(component, startDepth, focusPath);
  }  
  private static Object _FOCUS_PATH_KEY = new Object();
}
