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
package org.apache.myfaces.trinidadinternal.renderkit.core.skin;

import java.io.IOException;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * Utilities for the Core Skins.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/CoreSkinUtils.java#0 $) $Date: 10-nov-2005.19:02:51 $
 */
public class CoreSkinUtils 
{

  // Renders an icon as a background
  // If possible, use the method that takes FacesContext and AdfRenderingContext
  // This method derives those objects, so if you have them already, use the
  // other method.
  public static void __renderBackgroundIcon(
    UIXRenderingContext context, 
    Icon icon)
    throws IOException
  {
    if (icon != null)
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fContext = context.getFacesContext();
      __renderBackgroundIcon(fContext, arc, icon);
    }
  }

  // Renders an icon as a background
  // This is more optimized than the method that takes a RenderingContext
  public static void __renderBackgroundIcon(
    FacesContext        fContext, 
    RenderingContext arc, 
    Icon                icon)
    throws IOException
  {
    if (icon != null)
    {

      // Make sure that our icon has a URI
      Object uri = icon.getImageURI(fContext, arc);
      if (uri == null)
        return;

      ResponseWriter writer = fContext.getResponseWriter();

      writer.writeAttribute("background", uri, null);
    }
  }
  
  private CoreSkinUtils() {}

  /**
   * Registers a set of org.apache.myfaces.trinidadinternal.skin.icon.Icon objects
   * on the specified Skin. The icons
   * array contains alternating pairs of icon name and Icon objects.
   */
  public static void registerIcons(Skin skin, Object[] icons)
  {
    for (int i = 0; i < icons.length; i += 2)
    {
      String iconName = (String) icons[i];
      Icon icon = (Icon) icons[i + 1];

      skin.registerIcon(iconName, icon);
    }
  }
}
