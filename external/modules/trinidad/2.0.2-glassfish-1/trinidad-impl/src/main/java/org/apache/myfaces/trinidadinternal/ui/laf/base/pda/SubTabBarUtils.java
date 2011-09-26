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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SubTabBarUtils implements UIConstants
{
  public static Object getSelectedIndex(
    UIXRenderingContext context
    )
  {
    return context.getProperty( MARLIN_NAMESPACE,
                                _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY);
  }
  
  public static void setSelectedIndex(
    UIXRenderingContext context,
    Object selectedIndex)
  {
    context.setProperty( MARLIN_NAMESPACE,
                         _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY,
                         selectedIndex);
  }

  private static final Object _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY = new Object();
}
