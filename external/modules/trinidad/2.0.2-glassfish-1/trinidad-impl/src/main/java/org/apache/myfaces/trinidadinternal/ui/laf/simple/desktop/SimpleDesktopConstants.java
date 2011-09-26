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

import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopConstants;

/**
 * Constants used by the desktop implementation of the Simple
 * Skin
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/SimpleDesktopConstants.java#0 $) $Date: 10-nov-2005.18:51:26 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface SimpleDesktopConstants extends BaseDesktopConstants
{
  /**
   * The id for the desktop implementation of the 
   * Simple Skin.
   */
  public static final String SIMPLE_DESKTOP_ID = "simple.desktop";  


  // menuTabs Icons
  static final String AF_MENU_TABS_ENABLED_START_ICON_NAME = 
    "af|menuTabs::enabled-start-icon";
  static final String AF_MENU_TABS_SELECTED_START_ICON_NAME = 
    "af|menuTabs::selected-start-icon";
  static final String AF_MENU_TABS_ENABLED_END_ICON_NAME = 
    "af|menuTabs::enabled-end-icon";
  static final String AF_MENU_TABS_SELECTED_END_ICON_NAME = 
    "af|menuTabs::selected-end-icon";
  static final String AF_MENU_TABS_ENABLED_JOIN_ICON_NAME = 
    "af|menuTabs::enabled-join-icon";
  static final String AF_MENU_TABS_SELECTED_ENABLED_JOIN_ICON_NAME = 
    "af|menuTabs::selected-enabled-join-icon";
  static final String AF_MENU_TABS_ENABLED_SELECTED_JOIN_ICON_NAME = 
    "af|menuTabs::enabled-selected-join-icon";
  static final String AF_MENU_TABS_ENABLED_BACKGROUND_ICON_NAME = 
    "af|menuTabs::enabled-background-icon";
  static final String AF_MENU_TABS_SELECTED_BACKGROUND_ICON_NAME = 
    "af|menuTabs::selected-background-icon";
 
  // menuBar Icons
  static final String AF_MENU_BAR_START_ICON_NAME = 
    "af|menuBar::start-icon";
  static final String AF_MENU_BAR_END_ICON_NAME = 
    "af|menuBar::end-icon";
  static final String AF_MENU_BAR_LEADING_SEPARATOR_ICON_NAME = 
    "af|menuBar::leading-separator-icon";
  static final String AF_MENU_BAR_TRAILING_SEPARATOR_ICON_NAME = 
    "af|menuBar::trailing-separator-icon";
  static final String AF_MENU_BAR_BACKGROUND_ICON_NAME = 
    "af|menuBar::background-icon";

  // GlobalButtonBar Icons
  static final String AF_MENU_BUTTONS_SEPARATOR_ICON_NAME = 
    "af|menuButtons::separator-icon";

  // messages Icons
  static final String AF_MESSAGES_TOP_START_ICON_NAME = 
    "af|messages::top-start-icon";
  static final String AF_MESSAGES_TOP_END_ICON_NAME = 
    "af|messages::top-end-icon";
  static final String AF_MESSAGES_TOP_BACKGROUND_ICON_NAME = 
    "af|messages::top-background-icon";
  static final String AF_MESSAGES_BOTTOM_START_ICON_NAME = 
    "af|messages::bottom-start-icon";
  static final String AF_MESSAGES_BOTTOM_END_ICON_NAME = 
    "af|messages::bottom-end-icon";
  static final String AF_MESSAGES_BOTTOM_BACKGROUND_ICON_NAME = 
    "af|messages::bottom-background-icon";
  static final String AF_MESSAGES_START_BACKGROUND_ICON_NAME = 
    "af|messages::start-background-icon";
  static final String AF_MESSAGES_END_BACKGROUND_ICON_NAME = 
    "af|messages::end-background-icon";

  // panelSideBar Icons
  static final String AF_PANEL_SIDE_BAR_TOP_START_ICON_NAME = 
    "af|panelSideBar::top-start-icon";
  static final String AF_PANEL_SIDE_BAR_TOP_END_ICON_NAME = 
    "af|panelSideBar::top-end-icon";
  static final String AF_PANEL_SIDE_BAR_TOP_BACKGROUND_ICON_NAME = 
    "af|panelSideBar::top-background-icon";
  static final String AF_PANEL_SIDE_BAR_BOTTOM_START_ICON_NAME = 
    "af|panelSideBar::bottom-start-icon";
  static final String AF_PANEL_SIDE_BAR_BOTTOM_END_ICON_NAME = 
    "af|panelSideBar::bottom-end-icon";
  static final String AF_PANEL_SIDE_BAR_BOTTOM_BACKGROUND_ICON_NAME = 
    "af|panelSideBar::bottom-background-icon";
  static final String AF_PANEL_SIDE_BAR_START_BACKGROUND_ICON_NAME = 
    "af|panelSideBar::start-background-icon";
  static final String AF_PANEL_SIDE_BAR_END_BACKGROUND_ICON_NAME = 
    "af|panelSideBar::end-background-icon";


  // Button Icons
  static final String BUTTON_START_ICON_NAME = 
    "AFButtonStartIcon";
  static final String BUTTON_END_ICON_NAME = 
    "AFButtonEndIcon";
  static final String BUTTON_TOP_BACKGROUND_ICON_NAME = 
    "AFButtonTopBackgroundIcon";
  static final String BUTTON_BOTTOM_BACKGROUND_ICON_NAME = 
    "AFButtonBottomBackgroundIcon";

  static final String BUTTON_DISABLED_START_ICON_NAME = 
    "AFButtonDisabledStartIcon";
  static final String BUTTON_DISABLED_END_ICON_NAME = 
    "AFButtonDisabledEndIcon";
  static final String BUTTON_DISABLED_TOP_BACKGROUND_ICON_NAME = 
    "AFButtonDisabledTopBackgroundIcon";
  static final String BUTTON_DISABLED_BOTTOM_BACKGROUND_ICON_NAME = 
    "AFButtonDisabledBottomBackgroundIcon";
  
  // Style classes

  // menuBar style classes
  public static final String AF_MENU_BAR_EMPTY_STYLE_CLASS = 
    "af|menuBar::empty";
  public static final String AF_MENU_BAR_BODY_STYLE_CLASS = 
    "af|menuBar::body";
  public static final String AF_MENU_BAR_TITLE_STYLE_CLASS = 
    "af|menuBar::title";
  
    
  // tr:messages style classes
  // when we combine base and simple renders, this will move up to 
  // BaseDesktopConstants. In fact, all the styles in this class will.
    
   
  // style that is on the td that surrounds the entire message box
  public static final String AF_MESSAGES_BODY_STYLE_CLASS = 
    "af|messages::body";

    
  // tr:panelSideBar style classes
  public static final String AF_PANEL_SIDE_BAR_BODY_STYLE_CLASS = 
    "af|panelSideBar::body";
}
