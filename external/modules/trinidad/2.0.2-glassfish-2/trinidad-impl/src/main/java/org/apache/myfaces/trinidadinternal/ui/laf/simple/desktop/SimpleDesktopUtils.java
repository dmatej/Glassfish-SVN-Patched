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

import org.apache.myfaces.trinidadinternal.ui.UIConstants;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelExtension;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelScorer;
import org.apache.myfaces.trinidadinternal.ui.laf.NameOnlyScorer;

import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopUtils;

/**
 * Utilities for the desktop implementation of the Simple
 * Look And Feel. Currently the Look And Feel is used only for choosing
 * renderers. We stripped out the styles, icons, translations, and properties,
 * since we now use a Skin to customize that. The goal is to get rid of the 
 * LookAndFeel code altogether.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/SimpleDesktopUtils.java#0 $) $Date: 10-nov-2005.18:51:27 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SimpleDesktopUtils extends BaseDesktopUtils
  implements SimpleDesktopConstants
{

  /**
   * Registers the desktop implementation of the Simple
   * Look And Feel with the specified LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    // The Simple Look And Feel is an extension of the
    // Base Look And Feel.  Get our base LAF from the 
    // LookAndFeelManager.
    LookAndFeel baseLAF = manager.getLookAndFeelById(
                               BaseDesktopConstants.BASE_DESKTOP_ID);

    // The Base LAF should always be pre-registered, but just to 
    // be extra safe...
    if (baseLAF == null)
    {
      // No need to log an error message - but an assert
      // should catch our attention
      assert false;

      // We can't create SLAF without the Base LAF
      return;
    }

    // Create a LookAndFeelExtension for SLAF
    LookAndFeelExtension simpleLAF = 
      new LookAndFeelExtension(baseLAF, SIMPLE_DESKTOP_ID, _SIMPLE_FAMILY);

    // Register custom Renderers
    _registerRenderers(simpleLAF);

    // Create the LookAndFeelScorer
    LookAndFeelScorer baseScorer = manager.getLookAndFeelScorer(baseLAF);
    LookAndFeelScorer scorer = new NameOnlyScorer(_SIMPLE_FAMILY, baseScorer);

    // Finally, register the LAF with the LookAndFeelManager
    manager.registerLookAndFeel(scorer, simpleLAF);
  }


  // Register the SLAF Renderers
  private static void _registerRenderers(
    LookAndFeelExtension laf
    )
  {
    _registerRenderer(laf, UIConstants.GLOBAL_BUTTON_BAR_NAME);
    _registerRenderer(laf, UIConstants.GLOBAL_HEADER_NAME);
    _registerRenderer(laf, UIConstants.MESSAGE_BOX_NAME);
    _registerRenderer(laf, UIConstants.PAGE_MENU_BAR_NAME);
    _registerRenderer(laf, UIConstants.PAGE_MENU_BUTTONS_NAME);
    _registerRenderer(laf, UIConstants.SIDE_BAR_NAME);
    _registerRenderer(laf, UIConstants.TAB_BAR_NAME);
    _registerRenderer(laf, UIConstants.BUTTON_NAME);
    _registerRenderer(laf, UIConstants.SUBMIT_BUTTON_NAME);

  }

  private static void _registerRenderer(
    LookAndFeelExtension laf, 
    String               localName
    )
  {
    char initialChar = Character.toUpperCase(localName.charAt(0));
    String className = initialChar + localName.substring(1) + "Renderer";

    laf.registerRenderer(UIConstants.MARLIN_NAMESPACE,
                         localName,
                         null,       // =-ags just default facet for now?
                         _PACKAGE_PREFIX + className);
  }



  private SimpleDesktopUtils() {}


  // SLAF family name
  private static final String _SIMPLE_FAMILY = "simple";

  // Package prefix for SLAF package
  private static final String _PACKAGE_PREFIX = 
    "org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop.";
}
