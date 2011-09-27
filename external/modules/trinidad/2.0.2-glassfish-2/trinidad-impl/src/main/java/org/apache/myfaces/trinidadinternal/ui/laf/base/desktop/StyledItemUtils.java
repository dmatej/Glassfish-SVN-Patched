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
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * Utility class to pass selected information to a styledItem.
 * Normally used by a styledList to tell a styledItem it is selected.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/StyledItemUtils.java#0 $) $Date: 10-nov-2005.18:56:17 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class StyledItemUtils implements UIConstants
{
  private StyledItemUtils()
  {
  }

  public static boolean isSelected(
    UIXRenderingContext context,
    UINode styledItemNode
  )
  {
    boolean isSelected = Boolean.TRUE.equals(styledItemNode.getAttributeValue(
      context, SELECTED_ATTR));
    //If node's selected attribute is not true see if selectedIndex on styledList
    //  means this item is selected (this info by now is a context property)
    if (!isSelected)
    {
      isSelected = Boolean.TRUE.equals(
        context.getProperty(MARLIN_NAMESPACE, _STYLED_ITEM_SELECTED_PROPERTY));
    }
    return isSelected;
  }

  public static void setSelected(
    UIXRenderingContext context,
    boolean          selected
  )
  {
    context.setProperty( MARLIN_NAMESPACE,
                         _STYLED_ITEM_SELECTED_PROPERTY,
                         Boolean.valueOf(selected));
  }

   private static final Object _STYLED_ITEM_SELECTED_PROPERTY = new Object();
}
