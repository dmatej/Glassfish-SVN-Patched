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

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;

/**
 * Constants used throughout the HTML Laf rendering.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/BaseDesktopConstants.java#0 $) $Date: 10-nov-2005.18:55:08 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface BaseDesktopConstants extends XhtmlLafConstants
{
  /**
   * The id for the desktop implementation of the
   * Base Look And Feel.
   */
  public static final String BASE_DESKTOP_ID = "base.desktop";


  public static final String __SORTABLE_STYLED_TEXT_NAME =
    "sortableStyledText";


  // Icon names

  // (menuBar)
  // This is the only thing customizable in base.desktop.GlobalHeaderRenderer
  // more things are customizable in simple.desktop.
  // =-=jmw some day we can combine the renderers, when we combine the
  // BaseDesktop/SimpleDesktop skins.
  // menuBar is not customizable for pda or for oracle.desktop.
  // From the renderered output it looks like oracle.desktop can be
  // simulated by customizing simple.desktops renderer.
  public static final String AF_MENU_BAR_SEPARATOR_ICON_NAME =
    "af|menuBar::separator-icon";


  // Shuttle Icons
  // (selectManyShuttle and selectOrderShuttle)
  // simple/oracle use the same base.desktop renderer now that I fixed it.
  // shuttle is not supported in pda.

  // Shuttle Move
  public static final String SHUTTLE_MOVE_ICON_ALIAS_NAME =
    "AFShuttleMoveIcon";
  public static final String AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME =
    "af|selectManyShuttle::move-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME =
    "af|selectOrderShuttle::move-icon";

  // Shuttle Move All
  public static final String SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME =
    "AFShuttleMoveAllIcon";
  public static final String AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME =
    "af|selectManyShuttle::move-all-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME =
    "af|selectOrderShuttle::move-all-icon";

 // Shuttle Remove
  public static final String SHUTTLE_REMOVE_ICON_ALIAS_NAME =
    "AFShuttleRemoveIcon";
  public static final String AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME =
    "af|selectManyShuttle::remove-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME =
    "af|selectOrderShuttle::remove-icon";

  // Shuttle Remove All
  public static final String SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME =
    "AFShuttleRemoveAllIcon";
  public static final String AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME =
    "af|selectManyShuttle::remove-all-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME =
    "af|selectOrderShuttle::remove-all-icon";

  // selectOrderShuttle's reordering icon keys
  public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME =
    "af|selectOrderShuttle::reorder-top-icon";

  public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME =
    "af|selectOrderShuttle::reorder-up-icon";

  public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME =
    "af|selectOrderShuttle::reorder-down-icon";

  public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME =
    "af|selectOrderShuttle::reorder-bottom-icon";

  // TabBar style classes
  public static final String AF_MENU_TABS_STYLE_CLASS =
    "af|menuTabs";
  public static final String AF_MENU_TABS_SELECTED_STYLE_CLASS =
    "af|menuTabs::selected";
  public static final String AF_MENU_TABS_ENABLED_STYLE_CLASS =
    "af|menuTabs::enabled";
  public static final String AF_MENU_TABS_DISABLED_STYLE_CLASS =
    "af|menuTabs::disabled";
  public static final String AF_MENU_TABS_SEPARATOR_STYLE_CLASS =
    "af|menuTabs::separator";


  // MenuBar style classes
  public static final String AF_MENU_BAR_STYLE_CLASS =
    "af|menuBar";
  public static final String AF_MENU_BAR_SELECTED_STYLE_CLASS =
    "af|menuBar::selected";
  public static final String AF_MENU_BAR_ENABLED_STYLE_CLASS =
    "af|menuBar::enabled";
  public static final String AF_MENU_BAR_DISABLED_STYLE_CLASS =
    "af|menuBar::disabled";
  public static final String AF_MENU_BAR_SEPARATOR_STYLE_CLASS =
    "af|menuBar::separator";

  public static final String AF_SELECT_MANY_SHUTTLE_PB_CONTENT_STYLE_CLASS =
    "af|selectManyShuttle::box-content";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_CONTENT_STYLE_CLASS =
    "af|selectOrderShuttle::box-content";

    
  public static final String AF_SELECT_MANY_SHUTTLE_BOTTOM_START_ICON_NAME =        
    "af|selectManyShuttle::bottom-start-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_BOTTOM_END_ICON_NAME =          
    "af|selectManyShuttle::bottom-end-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_BOTTOM_BG_ICON_NAME =          
    "af|selectManyShuttle::bottom-background-icon";    
  public static final String AF_SELECT_MANY_SHUTTLE_TOP_START_ICON_NAME =          
    "af|selectManyShuttle::top-start-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_TOP_END_ICON_NAME =            
    "af|selectManyShuttle::top-end-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_TOP_BG_ICON_NAME =                
    "af|selectManyShuttle::top-background-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_START_BG_ICON_NAME =                    
    "af|selectManyShuttle::start-background-icon";
  public static final String AF_SELECT_MANY_SHUTTLE_END_BG_ICON_NAME =                        
    "af|selectManyShuttle::end-background-icon";      
    
  public static final String AF_SELECT_ORDER_SHUTTLE_BOTTOM_START_ICON_NAME =        
    "af|selectOrderShuttle::bottom-start-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_BOTTOM_END_ICON_NAME =          
    "af|selectOrderShuttle::bottom-end-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_BOTTOM_BG_ICON_NAME =          
    "af|selectOrderShuttle::bottom-background-icon";    
  public static final String AF_SELECT_ORDER_SHUTTLE_TOP_START_ICON_NAME =          
    "af|selectOrderShuttle::top-start-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_TOP_END_ICON_NAME =            
    "af|selectOrderShuttle::top-end-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_TOP_BG_ICON_NAME =                
    "af|selectOrderShuttle::top-background-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_START_BG_ICON_NAME =                    
    "af|selectOrderShuttle::start-background-icon";
  public static final String AF_SELECT_ORDER_SHUTTLE_END_BG_ICON_NAME =                        
    "af|selectOrderShuttle::end-background-icon";       
    
  // showOneTab style/icon constants
  public static final String AF_SELECT_ONE_TAB_SEPARATOR_ICON_NAME =
    "af|panelTabbed::separator-icon";
    

  // ColorField style classes
  public static final String AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_STYLE_CLASS =
    "af|inputColor::swatch-overlay";

  // Header style classes
  // if icon attribute is set, this is the style for it.
  // I wouldn't normally have style in the name, but I don't want it to be
  // confused with a icon.
  public static final String AF_PANEL_HEADER_ICON_STYLE_CLASS =
    "af|panelHeader::icon-style";

  // SortableHeader style classes
  public static final String SORTABLE_HEADER_SORT_ICON_STYLE_CLASS =
    "AFSortableHeaderSortIcon";

  // Separator style classes
  public static final String AF_SEPARATOR_STYLE_CLASS = 
    "af|separator";

  //messages
  // style that is on the outer table
  public static final String AF_MESSAGES_STYLE_CLASS =
    "af|messages";
   // style that is on the div that surrounds the message.
  public static final String AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS =
    "af|messages::message-text";
  // style that is on the ol or div around the list of messages.
  public static final String AF_MESSAGES_LIST_STYLE_CLASS =
    "af|messages::list";

  public static final String FOOTER_STYLE_CLASS = 
    "p_OraFooter";
  public static final String FOOTER_BOTTOM_STYLE_CLASS = 
    "p_OraFooterBottom";
  public static final String CONTENT_FOOTER_BOTTOM_STYLE_CLASS = 
    "p_OraContentFooterBottom";
  public static final String CONTENT_FOOTER_CHILDREN_STYLE_CLASS = 
    "p_OraContentFooterChildren";
  public static final String CONTENT_FOOTER_START_STYLE_CLASS = 
    "p_OraContentFooterStart";
  public static final String CONTENT_FOOTER_RULE_STYLE_CLASS = 
    "p_OraContentFooterRule";
  public static final String NAV_1_STYLE_CLASS = 
    "p_OraNav1";

  public static final String SIDE_BAR_MIN_WIDTH_STYLE_CLASS = 
    "p_OraSideBarMinWidth";
  public static final String HEADER_NEST_STYLE_CLASS = 
    "p_OraHeaderNest";
}
