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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * Utility class to pass information about headers
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/HeaderUtils.java#0 $) $Date: 10-nov-2005.18:55:17 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class HeaderUtils implements UIConstants
{
  private HeaderUtils()
  {
  }

  public static Object getHeaderID(
    UIXRenderingContext context
  )
  {    
    return BaseDesktopUtils.getRenderingProperty(context, _HEADER_ID_PROPERTY);
  }

  public static void setHeaderID(
    UIXRenderingContext context,
    Object           id
  )
  {
    BaseDesktopUtils.setRenderingProperty(context, _HEADER_ID_PROPERTY, id);
  }

  // RenderingContext property used to pass the HeaderBean's ID 
  // in to the HideShowBean
  private static final Object _HEADER_ID_PROPERTY = new Object();
}
