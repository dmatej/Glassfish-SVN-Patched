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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * GlobalButtonBarRenderer for the desktop implementation of the
 * Simple Look And Feel.
 *
 * This is an extension of the Base Look And Feel global button bar which
 * adds the following customizable icon:
 *
 * <ul>
 * <li>globalButtonBarSeparator: The separator bewteen global buttons
 *                                    items.
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/GlobalButtonBarRenderer.java#0 $) $Date: 10-nov-2005.18:51:22 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalButtonBarRenderer
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.GlobalButtonBarRenderer
  implements SimpleDesktopConstants
{
  /**
   * Override of renderBetweenIndexedChildren() which
   * renders the separator Icon.
   */
  @Override
  protected void renderBetweenNodes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // If we have a separator icon, render it in a table
    // cell between the other children
    Icon icon = _getSeparatorIcon(context);

    if (icon != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(TABLE_DATA_ELEMENT, null);

      // Render the icon
      FacesContext fContext = context.getFacesContext();
      RenderingContext arc = RenderingContext.getCurrentInstance();
      OutputUtils.renderIcon(fContext, arc, icon, "", null);

      writer.endElement(TABLE_DATA_ELEMENT);
    }
  }


  // Returns the separator Icon to render between global buttons
  private static Icon _getSeparatorIcon(
    UIXRenderingContext context
    )
  {
    // First, check to see if the Icon has been stored
    // as a local property
    Object localValue = context.getLocalProperty(0,
                                                 _SEPARATOR_ICON_KEY,
                                                 _NULL_ICON);
    if (localValue != _NULL_ICON)
      return (Icon)localValue;

    // If we didn't find a local property, get the Icon from
    // the Skin
    Skin skin = context.getSkin();
    Icon icon = skin.getIcon(AF_MENU_BUTTONS_SEPARATOR_ICON_NAME);

    // Stash the Icon away so that we don't have to
    // look it up in the laf again next time.
    context.setLocalProperty(_SEPARATOR_ICON_KEY, icon);

    // Return the icon
    return icon;
  }

  // Key for local property which holds the separator Icon
  private static final Object _SEPARATOR_ICON_KEY = new Object();

  // Placeholder for null Icon local property
  private static final Object _NULL_ICON = new Object();
}
