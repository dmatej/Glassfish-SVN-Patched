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

import java.awt.Color;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * ResetButtonRenderer for Simple Look And Feel.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/ResetButtonRenderer.java#0 $) $Date: 10-nov-2005.18:51:24 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ResetButtonRenderer 
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.ResetButtonRenderer
{
  // Note: We just extend the BLAF version of the ResetButtonRenderer
  // because it is convenient.  At some point we should probably either
  // refactor BLAF's button rendering code into a base class - or
  // perhaps make BLAF an LookAndFeelExtension of SLAF.  For now,
  // we'll just list with the BLAF dependency.

  /**
   * Tests whether the button should be rendered as an image.
   */
  @Override
  protected boolean doRenderImageContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Check with superclass first (screen reader mode).
    if (!super.doRenderImageContent(context, node))
      return false;

    // We only render buttons as images if we have all
    // of the button icons. 
    return SimpleButtonUtils.doRenderImageButton(context);
  }

  /**
   * Creates the ImageProviderRequest to use when looking up the
   * button image.
   */ 
  @Override
  protected ImageProviderRequest createImageProviderRequest(
    UIXRenderingContext context,
    Object       name,
    Object       text,
    Color        foreground,
    Color        background,
    Color        surroundingColor,
    FontProxy    font,
    boolean      disabled,
    boolean      textAntialias,
    boolean      startRounded,
    boolean      endRounded,
    char         accessKey
    )
  {
    return SimpleButtonUtils.createButtonRequest(
                        context,
                        (name != null)
                          ? name.toString()
                          : null,
                        (text != null)
                          ? text.toString()
                          : null,
                        foreground,
                        background,
                        surroundingColor,
                        font,
                        disabled,
                        textAntialias,
                        accessKey);
  }

  /**
   * Returns the name of the server-side style for styling
   * button text.
   */
  @Override
  protected String getServerStyleName(
    UIXRenderingContext context,
    UINode           node,
    boolean          disabled
    )
  {
    return SimpleButtonUtils.getButtonStyleName(disabled);
  }
}
