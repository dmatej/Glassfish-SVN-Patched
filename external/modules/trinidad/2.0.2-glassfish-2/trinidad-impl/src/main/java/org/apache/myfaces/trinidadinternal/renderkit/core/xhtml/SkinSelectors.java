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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

/**
 * This class contains all skin selectors used by the various Trinidad
 * components.
 * 
 */
public final class SkinSelectors
{
  public static final String DEFAULT_NAMESPACE = "af";
  public static final String ICON_SUFFIX       = "-icon";
  public static final String STATE_PREFIX      = "p_AF";
  
  private SkinSelectors(){}

  /* The selectors are sorted by component, alphabetically.
   * 
   * The selectors not linked to any specific components like aliases are
   * presented before any component specific selectors.
   * 
   * The selector regarding the style classes are presented first, the
   * selectors concerning the icons are presented after.
   */
  //                                                                         //
  //                                                                         //
  // =============================== Aliases =============================== //
  //                                                                         //
  //                                                                         //
  // ========================== AF Style classes =========================== //
  public static final String AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS =
    "AFComponentMessageCell";
  public static final String AF_CONTENT_CELL_STYLE_CLASS =
    "AFContentCell";
  public static final String AF_DATA_TEXT_STYLE_CLASS =
    "AFDataText";
  public static final String AF_FIELD_TEXT_DISABLED_STYLE_CLASS =
    "AFFieldTextDisabled";
  public static final String AF_FIELD_TEXT_STYLE_CLASS =
    "AFFieldText";
  public static final String AF_LABEL_TEXT_STYLE_CLASS =
    "AFLabelText";
  public static final String AF_ACCESSKEY_STYLE_CLASS =
   "AFAccessKeyStyle";
  public static final String AF_LINKACCESSKEY_STYLE_CLASS =
   "AFLinkAccessKeyStyle";
  public static final String ERROR_ICON_STYLE_CLASS =
    "AFErrorIconStyle";
  public static final String INFO_ICON_STYLE_CLASS =
    "AFInfoIconStyle";
  public static final String WARNING_ICON_STYLE_CLASS =
    "AFWarningIconStyle";
  public static final String QUICK_SELECT_DISABLED_ICON_STYLE_CLASS =
    "AFQuickSelectDisabledIconStyle";
  public static final String QUICK_SELECT_ICON_STYLE_CLASS =
    "AFQuickSelectIconStyle";
  public static final String REQUIRED_ICON_STYLE_CLASS =
    "AFRequiredIconStyle";
  public static final String SORTABLE_HEADER_SORT_ICON_STYLE_CLASS =
    "AFSortableHeaderSortIcon";
  
  // ========================== Ora style classes ========================== //
  // These styles are for large record sets, which we do not have in Trinidad
  // yet. When we do, should these be customizable styles? If so, the names
  // will have to change to be like af|treeTable::nav-row-ilink
  public static final String HGRID_LOCATOR_HEADER_STYLE =
    "OraHGridLocatorHeader";
  public static final String HGRID_NAV_ROW_ALINK_STYLE_CLASS =
    "OraHGridNavRowActiveLink";
  public static final String HGRID_NAV_ROW_ILINK_STYLE_CLASS =
    "OraHGridNavRowInactiveLink";
  // used to show the tips on select* fields
  public static final String INLINE_ERROR_TEXT_STYLE_CLASS =
    "OraInlineErrorText";
  public static final String INLINE_INFO_TEXT_STYLE_CLASS =
    "OraInlineInfoText";
  public static final String LINK_DISABLED_STYLE_CLASS =
    "OraLinkDisabled";
  public static final String LINK_STYLE_CLASS =
    "OraLink";
  public static final String LINK_TEXT_STYLE_CLASS =
    "OraLinkText";

  // FIXME: get rid of these!
  public static final String NAV_BAR_ALINK_STYLE_CLASS =
    "OraNavBarActiveLink";
  public static final String NAV_BAR_ILINK_STYLE_CLASS =
    "OraNavBarInactiveLink";
  public static final String NAV_BAR_VIEW_STYLE_CLASS =
    "OraNavBarViewOnly";
  public static final String PAGE_STAMP_TEXT_STYLE_CLASS =
    "OraPageStampText";
  public static final String TABLE_BAND_SELECT_CELL_STYLE =
    "OraTableCellSelectBand";
  public static final String TABLE_BORDER_0001_STYLE =
    "OraTableBorder0001";
  public static final String TABLE_BORDER_0010_STYLE =
    "OraTableBorder0010";
  public static final String TABLE_BORDER_0011_STYLE =
    "OraTableBorder0011";
  public static final String TABLE_BORDER_0100_STYLE =
    "OraTableBorder0100";
  public static final String TABLE_BORDER_0101_STYLE =
    "OraTableBorder0101";
  public static final String TABLE_BORDER_0110_STYLE =
    "OraTableBorder0110";
  public static final String TABLE_BORDER_0111_STYLE =
    "OraTableBorder0111";
  public static final String TABLE_BORDER_1000_STYLE =
    "OraTableBorder1000";
  public static final String TABLE_BORDER_1001_STYLE =
    "OraTableBorder1001";
  public static final String TABLE_BORDER_1010_STYLE =
    "OraTableBorder1010";
  public static final String TABLE_BORDER_1011_STYLE =
    "OraTableBorder1011";
  public static final String TABLE_BORDER_1100_STYLE =
    "OraTableBorder1100";
  public static final String TABLE_BORDER_1101_STYLE =
    "OraTableBorder1101";
  public static final String TABLE_BORDER_1110_STYLE =
    "OraTableBorder1110";
  public static final String TABLE_BORDER_1111_STYLE =
    "OraTableBorder1111";
  public static final String TABLE_SELECT_CELL_STYLE =
    "OraTableCellSelect";
  public static final String TIP_TEXT_STYLE_CLASS =
    "OraTipText";
  public static final String TIP_LABEL_STYLE_CLASS  =
    "OraTipLabel";


  // ========================= Other private style classes ========================= //
  public static final String COLOR_FIELD_SWATCH_STYLE_CLASS =
    "p_OraColorFieldSwatch";
  public static final String DISABLED_STYLE_CLASS =
    "p_OraDisabled";
  public static final String HEADER_NEST_STYLE_CLASS =
    "p_OraHeaderNest";
  public static final String HIDDEN_LABEL_STYLE_CLASS =
    "p_OraHiddenLabel";
  public static final String HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS =
    "p_OraHideShowDisclosedSymbol";
  public static final String IN_CONTEXT_TEXT_STYLE_CLASS =
    "p_InContextBrandingText";
  public static final String PROCESS_STATUS_STYLE_CLASS =
    "p_OraProcessingStatus";
  public static final String QUICK_SEARCH_BOX_STYLE_CLASS =
    "p_OraQuickSearchBox";
  public static final String STATE_DISABLED =
    STATE_PREFIX + "Disabled";
  public static final String STATE_READ_ONLY =
    STATE_PREFIX + "ReadOnly";
  public static final String STATE_REQUIRED =
    STATE_PREFIX + "Required";
  public static final String P_AF_DISABLED =
    STATE_DISABLED;

  // ================================ Icons ================================ //
  public static final String BUSY_ICON_ALIAS_NAME =
    "AFBusyIcon";
  public static final String DETAIL_DISCLOSED_ICON_ALIAS_NAME =
    "AFDetailDisclosedIcon";
  public static final String DETAIL_UNDISCLOSED_ICON_ALIAS_NAME =
    "AFDetailUndisclosedIcon";
  public static final String ERROR_ANCHOR_ICON_ALIAS_NAME =
    "AFErrorAnchorIcon";
  public static final String ERROR_ICON_ALIAS_NAME =
    "AFErrorIcon";
  public static final String HEADER_ERROR_ICON_ALIAS_NAME =
    "AFHeaderErrorIcon";
  public static final String HEADER_CONFIRMATION_ICON_ALIAS_NAME =
    "AFHeaderConfirmationIcon";
  public static final String HEADER_INFO_ICON_ALIAS_NAME =
    "AFHeaderInfoIcon";
  public static final String HEADER_WARNING_ICON_ALIAS_NAME =
    "AFHeaderWarningIcon";
  public static final String INFO_ANCHOR_ICON_ALIAS_NAME =
    "AFInfoAnchorIcon";
  public static final String INFO_ICON_ALIAS_NAME =
    "AFInfoIcon";
  public static final String INSTRUCTION_TEXT_STYLE_CLASS =
    "AFInstructionText";
  public static final String PATH_SEPARATOR_ICON_ALIAS_NAME =
    "AFPathSeparatorIcon";
  public static final String QUICK_SELECT_DISABLED_ICON_NAME =
    "AFQuickSelectDisabledIcon";
  public static final String QUICK_SELECT_ICON_NAME =
    "AFQuickSelectIcon";
  public static final String READY_ICON_ALIAS_NAME =
    "AFReadyIcon";
  public static final String REQUIRED_ICON_ALIAS_NAME =
    "AFRequiredIcon";
  public static final String SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME =
    "AFShuttleMoveAllIcon";
  public static final String SHUTTLE_MOVE_ICON_ALIAS_NAME =
    "AFShuttleMoveIcon";
  public static final String SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME =
    "AFShuttleRemoveAllIcon";
  public static final String SHUTTLE_REMOVE_ICON_ALIAS_NAME =
    "AFShuttleRemoveIcon";
  public static final String WARNING_ANCHOR_ICON_ALIAS_NAME  =
    "AFWarningAnchorIcon";
  public static final String WARNING_ICON_ALIAS_NAME =
    "AFWarningIcon";

  
  //                                                                         //
  //                                                                         //
  // ========================= Lightweight Dialog=========================== //
  //                                                                         //
  //                                                                         //
  private static final String AF_DIALOG_ROOT_STYLE_CLASS = "af|dialog";

  public static final String AF_DIALOG_CONTAINER_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::container";
  public static final String AF_DIALOG_CONTENT_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::content";
  public static final String AF_DIALOG_TITLEBAR_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::title-bar";
  public static final String AF_DIALOG_TITLE_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::title-text";
  public static final String AF_DIALOG_CLOSE_ICON_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::close-icon";
  public static final String AF_DIALOG_BLOCKED_AREA_STYLE_CLASS =
    AF_DIALOG_ROOT_STYLE_CLASS + "::blocked-area";

  //                                                                         //
  //                                                                         //
  // =========================== tr:breadCrumbs ============================ //
  //                                                                         //
  //                                                                         //
  
  // ============================ Style classes ============================ //
  public static final String AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS =
    "af|breadCrumbs::selected-step";
  public static final String AF_NAVIGATION_PATH_STEP_STYLE_CLASS =
    "af|breadCrumbs::step";
  public static final String AF_NAVIGATION_PATH_STYLE_CLASS =
    "af|breadCrumbs";
  
  // ================================ Icons ================================ //
  public static final String AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME =
    "af|breadCrumbs::separator-icon";

  
  //                                                                         //
  //                                                                         //
  // ============================ tr:chooseDate ============================ //
  //                                                                         //
  //                                                                         //
  
  // ============================ Style classes ============================ //
  public static final String AF_CHOOSE_DATE_CONTENT_STYLE_CLASS =
    "af|chooseDate::content";
  public static final String AF_CHOOSE_DATE_DISABLED_STYLE_CLASS =
    "af|chooseDate::disabled";
  public static final String AF_CHOOSE_DATE_ENABLED_STYLE_CLASS =
    "af|chooseDate::enabled";
  public static final String AF_CHOOSE_DATE_HEADER_STYLE_CLASS =
    "af|chooseDate::header";
  public static final String AF_CHOOSE_DATE_NAV_STYLE_CLASS =
    "af|chooseDate::nav";
  public static final String AF_CHOOSE_DATE_SELECTED_STYLE_CLASS =
    "af|chooseDate::selected";
  public static final String AF_CHOOSE_DATE_TITLE_STYLE_CLASS =
    "af|chooseDate::title";
  
  // ================================ Icons ================================ //
  public static final String AF_CHOOSE_DATE_NEXT_ICON_NAME =
   "af|chooseDate::next-icon";
  public static final String AF_CHOOSE_DATE_NEXT_DISABLED_ICON_NAME =
    "af|chooseDate::next-disabled-icon";
  public static final String AF_CHOOSE_DATE_PREV_ICON_NAME =
    "af|chooseDate::prev-icon";
  public static final String AF_CHOOSE_DATE_PREV_DISABLED_ICON_NAME =
    "af|chooseDate::prev-disabled-icon";
  
  
  //                                                                         //
  //                                                                         //
  // ============================== tr:column ============================== //
  //                                                                         //
  //                                                                         //
  
  // ============================ Style classes ============================ //
  public static final String AF_COLUMN_CELL_ICON_BAND_STYLE =
    "af|column::cell-icon-format-band";
  public static final String AF_COLUMN_CELL_ICON_FORMAT_STYLE =
    "af|column::cell-icon-format";
  public static final String AF_COLUMN_CELL_NUMBER_BAND_STYLE =
    "af|column::cell-number-band";
  public static final String AF_COLUMN_CELL_NUMBER_STYLE =
    "af|column::cell-number";
  public static final String AF_COLUMN_CELL_TEXT_BAND_STYLE =
    "af|column::cell-text-band";
  public static final String AF_COLUMN_CELL_TEXT_STYLE =
    "af|column::cell-text";
  public static final String AF_COLUMN_HEADER_ICON_STYLE =
    "af|column::header-icon-format";
  public static final String AF_COLUMN_HEADER_NUMBER_STYLE =
    "af|column::header-number";
  public static final String AF_COLUMN_HEADER_TEXT_STYLE =
    "af|column::header-text";
  public static final String AF_COLUMN_ROW_HEADER_TEXT_STYLE =
    "af|column::row-header-text";
  public static final String AF_COLUMN_SORTABLE_HEADER_ICON_STYLE_CLASS =
    "af|column::sortable-header-icon-format";
  public static final String AF_COLUMN_SORTABLE_HEADER_NUMBER_STYLE_CLASS =
    "af|column::sortable-header-number";
  public static final String AF_COLUMN_SORTABLE_HEADER_STYLE_CLASS =
    "af|column::sortable-header-text";
  public static final String AF_COLUMN_TOTAL_NUMBER_STYLE =
    "af|column::total-number";
  public static final String AF_COLUMN_TOTAL_TEXT_STYLE =
    "af|column::total-text";
  public static final String AF_COLUMN_SORTED_HEADER_STYLE_CLASS =
    "af|column::sorted-header-text";
  public static final String AF_COLUMN_SORTED_HEADER_NUMBER_STYLE_CLASS =
    "af|column::sorted-header-number";
  public static final String AF_COLUMN_SORTED_HEADER_ICON_STYLE_CLASS =
    "af|column::sorted-header-icon-format";
  
  // ================================ Icons ================================ //
  public static final String AF_COLUMN_SORTED_ASCEND_ICON_NAME =
    "af|column::sort-ascend-icon";
   public static final String AF_COLUMN_SORTED_DESCEND_ICON_NAME =
    "af|column::sort-descend-icon";
   public static final String AF_COLUMN_UNSORTED_ICON_NAME =
    "af|column::unsorted-icon";

   //                                                                         //
   //                                                                         //
   // ============================ tr:commandButton ========================= //
   //                                                                         //
   //                                                                         //

   // ============================ Style classes ============================ //
  public static final String AF_COMMAND_BUTTON_STYLE_CLASS =
    "af|commandButton";
   

   //                                                                         //
   //                                                                         //
   // ============================ tr:goButton ========================= //
   //                                                                         //
   //                                                                         //

   // ============================ Style classes ============================ //
  public static final String AF_GO_BUTTON_STYLE_CLASS =
    "af|goButton";

   //                                                                         //
   //                                                                         //
   // ============================ tr:resetButton ========================= //
   //                                                                         //
   //                                                                         //

   // ============================ Style classes ============================ //
  public static final String AF_RESET_BUTTON_STYLE_CLASS =
    "af|resetButton";

   //                                                                         //
   //                                                                         //
   // ============================ tr:inputColor ============================ //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_STYLE_CLASS =
     "af|inputColor::swatch-overlay";
   
   // ================================ Icons ================================ //
   // Doesn't render in pda. Not supported in pda, that's why.
   // Defined in BaseDesktopSkin and OracleDesktopSkinExtension
   // not sure under what circumstances the code runs that renders this.
   public static final String AF_SELECT_INPUT_COLOR_LAUNCH_ICON_NAME =
     "af|inputColor::launch-icon";
   // Doesn't render in pda. Not supported in pda, that's why.
   // Defined in BaseDesktopSkin and OracleDesktopSkinExtension
   public static final String AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_NAME =
     "af|inputColor::swatch-overlay-icon";
   
   
   //                                                                         //
   //                                                                         //
   // ============================ tr:inputDate ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_SELECT_INPUT_DATE_CONTENT_STYLE_CLASS =
     "af|inputDate::content";
   public static final String AF_SELECT_INPUT_DATE_DISABLED_STYLE_CLASS =
     "af|inputDate::disabled";
   public static final String AF_SELECT_INPUT_DATE_ENABLED_STYLE_CLASS =
     "af|inputDate::enabled";
   public static final String AF_SELECT_INPUT_DATE_HEADER_STYLE_CLASS =
     "af|inputDate::header";
   public static final String AF_SELECT_INPUT_DATE_NAV_STYLE_CLASS =
     "af|inputDate::nav";
   public static final String AF_SELECT_INPUT_DATE_SELECTED_STYLE_CLASS =
     "af|inputDate::selected";
   public static final String AF_SELECT_INPUT_DATE_TITLE_STYLE_CLASS =
     "af|inputDate::title";
   
   // ================================ Icons ================================ //
   // this renders a button that launches the modal date picker.
   public static final String AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME =
     "af|inputDate::launch-icon";
   public static final String AF_SELECT_INPUT_DATE_PREV_DISABLED_ICON_NAME =
     "af|inputDate::prev-disabled-icon";
   public static final String AF_SELECT_INPUT_DATE_PREV_ICON_NAME =
     "af|inputDate::prev-icon";
   public static final String AF_SELECT_INPUT_DATE_NEXT_DISABLED_ICON_NAME =
     "af|inputDate::next-disabled-icon";
   public static final String AF_SELECT_INPUT_DATE_NEXT_ICON_NAME =
     "af|inputDate::next-icon";

   
   //                                                                         //
   //                                                                         //
   // ======================== tr:inputListOfValues ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   // used by base.desktop and base.pda LovButtonRenderer
   // I don't see a button renderer for pda. It uses LovFieldRenderer,
   // and there is no LovFieldRenderer for pda (a comment says it is in dev)
   // , so it picks up base.xhtml's
   // and that renders null for the icon.
   public static final String AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME =
     "af|inputListOfValues::button-icon";
   
   
   //                                                                         //
   //                                                                         //
   // ======================== tr:inputNumberSpinbox ======================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_CELL =
     "af|inputNumberSpinbox::increment-cell";
   public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_CELL =
     "af|inputNumberSpinbox::decrement-cell";
   
   // ================================ Icons ================================ //
   public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_ICON_NAME =
     "af|inputNumberSpinbox::increment-icon";
   public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_ICON_NAME =
     "af|inputNumberSpinbox::decrement-icon";
   public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_DISABLED_ICON_NAME =
     "af|inputNumberSpinbox::increment-disabled-icon";
   public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_DISABLED_ICON_NAME =
     "af|inputNumberSpinbox::decrement-disabled-icon";

   
   //                                                                         //
   //                                                                         //
   // ============================= tr:menuBar ============================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   // This is the only thing customizable in base.desktop.GlobalHeaderRenderer
   // more things are customizable in simple.desktop.
   // =-=jmw some day we can combine the renderers, when we combine the
   // BaseDesktop/SimpleDesktop skins.
   // menuBar is not customizable for pda or for oracle.desktop.
   // From the renderered output it looks like oracle.desktop can be
   // simulated by customizing simple.desktops renderer.
   public static final String AF_MENU_BAR_STYLE_CLASS =
     "af|menuBar";
   public static final String AF_MENU_BAR_DISABLED_STYLE_CLASS =
     "af|menuBar::disabled";
   public static final String AF_MENU_BAR_ENABLED_STYLE_CLASS =
     "af|menuBar::enabled";
   public static final String AF_MENU_BAR_EMPTY_STYLE_CLASS =
     "af|menuBar::empty";
   public static final String AF_MENU_BAR_BODY_STYLE_CLASS =
     "af|menuBar::body";
   public static final String AF_MENU_BAR_TITLE_STYLE_CLASS =
     "af|menuBar::title";
   public static final String AF_MENU_BAR_SELECTED_STYLE_CLASS =
     "af|menuBar::selected";
   public static final String AF_MENU_BAR_SEPARATOR_STYLE_CLASS =
     "af|menuBar::separator";
   
   // ================================ Icons ================================ //
   public static final String AF_MENU_BAR_SEPARATOR_ICON_NAME =
     "af|menuBar::separator-icon";

   
   //                                                                         //
   //                                                                         //
   // ============================ tr:menuButtons =========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_MENU_BUTTONS_IMAGE_STYLE_CLASS =
     "af|menuButtons::icon-style";
   public static final String AF_MENU_BUTTONS_TEXT_STYLE_CLASS =
     "af|menuButtons::text";
   public static final String AF_MENU_BUTTONS_TEXT_SELECTED_STYLE_CLASS =
     "af|menuButtons::text-selected";
   public static final String AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS =
     "af|menuButtons::text-disabled";

   
   //                                                                         //
   //                                                                         //
   // ============================ tr:menuChoice ============================ //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_MENU_CHOICE_LABEL_STYLE_CLASS =
     "af|menuChoice::label";

   
   //                                                                         //
   //                                                                         //
   // ============================= tr:menuTabs ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
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

   //                                                                         //
   //                                                                         //
   // ============================= tr:messages ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_MESSAGES_STYLE_CLASS =
     "af|messages";
   public static final String AF_MESSAGES_BODY_STYLE_CLASS =
     "af|messages::body";
   public static final String AF_MESSAGES_ERROR_STYLE_CLASS =
     "af|messages::error";
   public static final String AF_MESSAGES_HEADER_STYLE_CLASS =
     "af|messages::header";
   public static final String AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS =
     "af|messages::message-text";
   public static final String AF_MESSAGES_LIST_STYLE_CLASS =
     "af|messages::list";
   public static final String AF_MESSAGES_LIST_SINGLE_STYLE_CLASS =
     "af|messages::list-single";
   
   // ================================ Icons ================================ //
   public static final String AF_MESSAGES_ERROR_ICON_NAME =
     "af|messages::error-icon";
   public static final String AF_MESSAGES_WARNING_ICON_NAME =
     "af|messages::warning-icon";
   public static final String AF_MESSAGES_INFO_ICON_NAME =
     "af|messages::info-icon";
   public static final String AF_MESSAGES_CONFIRMATION_ICON_NAME =
     "af|messages::confirmation-icon";

   // ================================ Frame ================================ //
   public static final String AF_MESSAGES_TOP_START_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::top-start";
   public static final String AF_MESSAGES_TOP_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::top";
   public static final String AF_MESSAGES_TOP_END_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::top-end";
   public static final String AF_MESSAGES_START_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::start";
   public static final String AF_MESSAGES_END_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::end";
   public static final String AF_MESSAGES_BOTTOM_START_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::bottom-start";
   public static final String AF_MESSAGES_BOTTOM_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::bottom";
   public static final String AF_MESSAGES_BOTTOM_END_STYLE_CLASS =
     AF_MESSAGES_STYLE_CLASS + "::bottom-end";
    
   //                                                                         //
   //                                                                         //
   // ========================== tr:navigationPane ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_NAVIGATION_LEVEL_BAR_ACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::bar-active-disabled";
   public static final String AF_NAVIGATION_LEVEL_BAR_ACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::bar-active-enabled";
   public static final String AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS =
     "af|navigationPane::bar-content";
   public static final String AF_NAVIGATION_LEVEL_BAR_INACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::bar-inactive-disabled";
   public static final String AF_NAVIGATION_LEVEL_BAR_INACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::bar-inactive-enabled";
   public static final String AF_NAVIGATION_LEVEL_BAR_SEPARATOR_STYLE_CLASS =
     "af|navigationPane::bar-separator";
   public static final String AF_NAVIGATION_LEVEL_BAR_STYLE_CLASS =
     "af|navigationPane::bar";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::buttons-active-disabled";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::buttons-active-enabled";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS =
     "af|navigationPane::buttons-content";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::buttons-inactive-disabled";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::buttons-inactive-enabled";
   public static final String AF_NAVIGATION_LEVEL_BUTTONS_SEPARATOR_STYLE_CLASS =
     "af|navigationPane::buttons-separator";
   public static final String AF_NAVIGATION_LEVEL_CHOICE_BUTTON_STYLE_CLASS =
     "af|navigationPane::choice-button";
   public static final String AF_NAVIGATION_LEVEL_CHOICE_LABEL_STYLE_CLASS =
     "af|navigationPane::choice-label";
   public static final String AF_NAVIGATION_LEVEL_CHOICE_OPTIONS_STYLE_CLASS =
     "af|navigationPane::choice-options";
   public static final String AF_NAVIGATION_LEVEL_LIST_ACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::list-active-disabled";
   public static final String AF_NAVIGATION_LEVEL_LIST_ACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::list-active-enabled";
   public static final String AF_NAVIGATION_LEVEL_LIST_BULLET_STYLE_CLASS =
     "af|navigationPane::list-bullet";
   public static final String AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS =
     "af|navigationPane::list-content";
   public static final String AF_NAVIGATION_LEVEL_LIST_INACTIVE_DISABLED_STYLE_CLASS =
     "af|navigationPane::list-inactive-disabled";
   public static final String AF_NAVIGATION_LEVEL_LIST_INACTIVE_ENABLED_STYLE_CLASS =
     "af|navigationPane::list-inactive-enabled";
   public static final String AF_NAVIGATION_LEVEL_STYLE_CLASS =
     "af|navigationPane";
   
   
   //                                                                         //
   //                                                                         //
   // ======================== tr:navigationPaneTabs ======================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_NAVIGATION_LEVEL_TABS_ACTIVE_STYLE_CLASS =
     "af|navigationPane::tabs-active";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_CONTENT_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-end-content";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_JOIN_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-end-join";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-end";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_CONTENT_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-mid-content";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-mid";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-start";
   public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_CONTENT_STYLE_CLASS =
     "af|navigationPane::tabs-bottom-start-content";
   public static final String AF_NAVIGATION_LEVEL_TABS_END_JOIN_TO_INACTIVE_STYLE_CLASS =
     "af|navigationPane::tabs-end-join-to-inactive";
   public static final String AF_NAVIGATION_LEVEL_TABS_END_STYLE_CLASS =
     "af|navigationPane::tabs-end";
   public static final String AF_NAVIGATION_LEVEL_TABS_INACTIVE_STYLE_CLASS =
     "af|navigationPane::tabs-inactive";
   public static final String AF_NAVIGATION_LEVEL_TABS_MID_STYLE_CLASS =
     "af|navigationPane::tabs-mid";
   public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_ACTIVE_STYLE_CLASS =
     "af|navigationPane::tabs-start-join-from-active";
   public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_INACTIVE_STYLE_CLASS =
     "af|navigationPane::tabs-start-join-from-inactive";
   public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_STYLE_CLASS =
     "af|navigationPane::tabs-start-join";
   public static final String AF_NAVIGATION_LEVEL_TABS_START_STYLE_CLASS =
     "af|navigationPane::tabs-start";
   public static final String AF_NAVIGATION_LEVEL_TABS_STYLE_CLASS =
     "af|navigationPane::tabs";

   
   //                                                                         //
   //                                                                         //
   // ========================== tr:navigationTree ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   public static final String AF_NAVIGATION_TREE_DISCLOSED_ICON_NAME =
     "af|navigationTree::disclosed-icon";
   public static final String AF_NAVIGATION_TREE_UNDISCLOSED_ICON_NAME =
     "af|navigationTree::undisclosed-icon";

   
   //                                                                         //
   //                                                                         //
   // ========================== tr:outputDocument ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_OUTPUT_DOCUMENT_STYLE_CLASS =
     "af|outputDocument";
   public static final String AF_OUTPUT_DOCUMENT_PARAGRAPH_STYLE_CLASS =
     "af|outputDocument::paragraph";
   public static final String AF_OUTPUT_DOCUMENT_SEPARATOR_STYLE_CLASS =
     "af|outputDocument::separator";
   public static final String AF_OUTPUT_DOCUMENT_TITLE_STYLE_CLASS =
     "af|outputDocument::title";

   
   //                                                                         //
   //                                                                         //
   // ========================== tr:panelAccordion ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // FIXME: Name inconsistency, should be _PANEL_ACCORDION
   public static final String AF_PANELACCORDION_STYLE_CLASS =
     "af|panelAccordion";
   public static final String AF_PANELACCORDION_CONTENT_STYLE_CLASS =
     "af|panelAccordion::content";
   public static final String AF_PANELACCORDION_HEADER_COLLAPSED_STYLE_CLASS =
     "af|panelAccordion::header-collapsed";
   public static final String AF_PANELACCORDION_HEADER_DISABLED_STYLE_CLASS =
     "af|panelAccordion::header-disabled";
   public static final String AF_PANELACCORDION_HEADER_EXPANDED_STYLE_CLASS =
     "af|panelAccordion::header-expanded";
   public static final String AF_PANELACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
     "af|panelAccordion::title-disabled-link";
   public static final String AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS =
     "af|panelAccordion::title-link";
   public static final String AF_PANELACCORDION_TOOLBAR_STYLE_CLASS =
     "af|panelAccordion::toolbar";

   
   //                                                                         //
   //                                                                         //
   // ============================= tr:panelBorderLayout ==================== //
   //                                                                         //
   //                                                                         //
   public static final String AF_PANEL_BORDER_POSITIONED_ROOT_STYLE_CLASS =
     "af|panelBorderLayout-positioned";
   public static final String AF_PANEL_BORDER_POSITIONED_TOP_STYLE_CLASS =
     "af|panelBorderLayout-positioned::top";
   public static final String AF_PANEL_BORDER_POSITIONED_INNER_TOP_STYLE_CLASS =
     "af|panelBorderLayout-positioned::inner-top";
   public static final String AF_PANEL_BORDER_POSITIONED_BOTTOM_STYLE_CLASS =
     "af|panelBorderLayout-positioned::bottom";
   public static final String AF_PANEL_BORDER_POSITIONED_INNER_BOTTOM_STYLE_CLASS =
     "af|panelBorderLayout-positioned::inner-bottom";
   public static final String AF_PANEL_BORDER_POSITIONED_START_STYLE_CLASS =
     "af|panelBorderLayout-positioned::start";
   public static final String AF_PANEL_BORDER_POSITIONED_INNER_START_STYLE_CLASS =
     "af|panelBorderLayout-positioned::inner-start";
   public static final String AF_PANEL_BORDER_POSITIONED_END_STYLE_CLASS =
     "af|panelBorderLayout-positioned::end";
   public static final String AF_PANEL_BORDER_POSITIONED_INNER_END_STYLE_CLASS =
     "af|panelBorderLayout-positioned::inner-end";
   public static final String AF_PANEL_BORDER_POSITIONED_CENTER_STYLE_CLASS =
     "af|panelBorderLayout-positioned::center";
   public static final String AF_PANEL_BORDER_POSITIONED_INNER_CENTER_STYLE_CLASS =
     "af|panelBorderLayout-positioned::inner-center";

   //                                                                         //
   //                                                                         //
   // ============================= tr:panelBox ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // Root class
   public static final String AF_PANEL_BOX_ROOT_STYLE_CLASS =
     "af|panelBox";
   // panelBox parts
   public static final String AF_PANEL_BOX_BODY_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::body";
   public static final String AF_PANEL_BOX_CONTENT_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::content";
   public static final String AF_PANEL_BOX_HEADER_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::header";
   // panelBox backgrounds
   public static final String AF_PANEL_BOX_DARK_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::dark";
   public static final String AF_PANEL_BOX_LIGHT_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::light";
   public static final String AF_PANEL_BOX_MEDIUM_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::medium";
   public static final String AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::transparent";
   // panelBox container
   public static final String AF_PANEL_BOX_TOP_START_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::top-start";
   public static final String AF_PANEL_BOX_TOP_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::top";
   public static final String AF_PANEL_BOX_TOP_END_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::top-end";
   public static final String AF_PANEL_BOX_START_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::start";
   public static final String AF_PANEL_BOX_END_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::end";
   public static final String AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::bottom-start";
   public static final String AF_PANEL_BOX_BOTTOM_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::bottom";
   public static final String AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS =
     AF_PANEL_BOX_ROOT_STYLE_CLASS + "::bottom-end";

   //                                                                         //
   //                                                                         //
   // ============================= tr:panelCaptionGroup ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // Root class
   public static final String AF_PANEL_CAPTION_GROUP_ROOT_STYLE_CLASS =
     "af|panelCaptionGroup";
   // panelBox parts
   public static final String AF_PANEL_CAPTION_GROUP_CAPTION_STYLE_CLASS =
     AF_PANEL_CAPTION_GROUP_ROOT_STYLE_CLASS + "::caption";

   //                                                                         //
   //                                                                         //
   // ========================= tr:panelButtonBar ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // Root class
   public static final String AF_PANEL_BUTTON_BAR_STYLE_CLASS =
     "af|panelButtonBar";


   //                                                                         //
   //                                                                         //
   // ========================= tr:panelFormLayout ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_PANEL_FORM_COLUMN_STYLE_CLASS =
     "af|panelFormLayout::column";
   public static final String AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS =
     "af|panelFormLayout::content-cell";
   public static final String AF_PANEL_FORM_LABEL_CELL_STYLE_CLASS =
     "af|panelFormLayout::label-cell";
   public static final String AF_PANEL_FORM_LABEL_STACKED_CELL_STYLE_CLASS =
     "af|panelFormLayout::label-stacked-cell";
   public static final String AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS =
     "af|panelFormLayout::message-cell";
   public static final String AF_PANEL_FORM_SEPARATOR_STYLE_CLASS =
     "af|panelFormLayout::separator";
   public static final String AF_PANEL_FORM_STYLE_CLASS =
     "af|panelFormLayout";

   
   //                                                                         //
   //                                                                         //
   // =========================== tr:panelHeader ============================ //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // if icon attribute is set, this is the style for it.
   // I wouldn't normally have style in the name, but I don't want it to be
   // confused with a icon.
   public static final String AF_PANEL_HEADER_ERROR_STYLE_CLASS =
     "af|panelHeader::error";
   public static final String AF_PANEL_HEADER_ICON_STYLE_CLASS =
     "af|panelHeader::icon-style";
   public static final String AF_PANEL_HEADER_STYLE_CLASS =
     "af|panelHeader";
   
   // ================================ Icons ================================ //
   public static final String AF_PANEL_HEADER_CONFIRMATION_ICON_NAME =
     "af|panelHeader::confirmation-icon";
   public static final String AF_PANEL_HEADER_ERROR_ICON_NAME =
     "af|panelHeader::error-icon";
   public static final String AF_PANEL_HEADER_INFO_ICON_NAME =
     "af|panelHeader::info-icon";
   public static final String AF_PANEL_HEADER_WARNING_ICON_NAME =
     "af|panelHeader::warning-icon";
   public static final String AF_PANEL_HEADER_PROCESSING_ICON_NAME =
     "af|panelHeader::processing-icon";

  //                                                                         //
  //                                                                         //
  // =========================== tr:panelAndLabelMessage ====================//
  //                                                                         //
  //
  // ============================ Style classes ============================ //
  public static final String AF_PANEL_LABEL_AND_MESSAGE_HELP_FACET_STYLE_CLASS =
    "af|panelLabelAndMessage::help-facet";
  //
  //                                                                        //
  //                                                                         //

  //                                                                         //
  //                                                                         //
  // ======================== tr:panelGroupLayout ========================== //
  //                                                                         //
  //                                                                         //
  
  // ============================ Style classes ============================ //
  public static final String AF_PANEL_GROUP_LAYOUT_STYLE_CLASS =
    "af|panelGroupLayout";
  
  
   //                                                                         //
   //                                                                         //
   // ============================ tr:panelList ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_PANEL_LIST_STYLE_CLASS =
     "af|panelList";
   
   
   //                                                                         //
   //                                                                         //
   // ============================ tr:panelPage ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS =
     "af|panelPage::copyright";
   public static final String AF_PANEL_PAGE_PRIVACY_STYLE_CLASS =
     "af|panelPage::privacy";
   public static final String AF_PANEL_PAGE_ABOUT_STYLE_CLASS =
     "af|panelPage::about";
   
   
   //                                                                         //
   //                                                                         //
   // =========================== tr:panelSideBar =========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_PANEL_SIDE_BAR_STYLE_CLASS =
     "af|panelSideBar";
   public static final String AF_PANEL_SIDE_BAR_BODY_STYLE_CLASS =
     "af|panelSideBar::body";
   
   //                                                                         //
   //                                                                         //
   // =========================== tr:panelTabbed ============================ //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   public static final String AF_SELECT_ONE_TAB_SEPARATOR_ICON_NAME =
     "af|panelTabbed::separator-icon";

   //                                                                         //
   //                                                                         //
   // =========================== tr:panelTip =============================== //
   //                                                                         //
   //                                                                         //

  public static final String AF_PANEL_TIP_STYLE_CLASS = "af|panelTip";
  public static final String AF_PANEL_TIP_LABEL_STYLE_CLASS = "af|panelTip::label";
  public static final String AF_PANEL_TIP_CONTENT_STYLE_CLASS = "af|panelTip::content";

  //                                                                         //
  //                                                                         //
  // =========================== tr:panelPopup ============================= //
  //                                                                         //
  //                                                                         //
  private static final String AF_PANEL_POPUP_ROOT_STYLE_CLASS = "af|panelPopup";

  public static final String AF_PANEL_POPUP_LINK_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::link";
  public static final String AF_PANEL_POPUP_ICON_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::icon-style";
  public static final String AF_PANEL_POPUP_TRIGGER_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::trigger";
  public static final String AF_PANEL_POPUP_CONTAINER_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::container";
  public static final String AF_PANEL_POPUP_CONTENT_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::content";
  public static final String AF_PANEL_POPUP_TITLEBAR_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::title-bar";
  public static final String AF_PANEL_POPUP_TITLE_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::title-text";
  public static final String AF_PANEL_POPUP_CLOSE_ICON_STYLE_CLASS =
    AF_PANEL_POPUP_ROOT_STYLE_CLASS + "::close-icon";

   //                                                                         //
   //                                                                         //
   // ======================== tr:progressIndicator ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   public static final String AF_PROGRESS_INDICATOR_INDETERMINATE_ICON_NAME =
     "af|progressIndicator::indeterminate-icon";

   
   //                                                                         //
   //                                                                         //
   // ====================== tr:selectBooleanCheckbox ======================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   public static final String AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_CHECKED_ICON_NAME =
     "af|selectBooleanCheckbox::disabled-checked-icon";
   public static final String AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_UNCHECKED_ICON_NAME =
     "af|selectBooleanCheckbox::disabled-unchecked-icon";
   public static final String AF_SELECT_BOOLEAN_CHECKBOX_READONLY_CHECKED_ICON_NAME =
     "af|selectBooleanCheckbox::read-only-checked-icon";
   public static final String AF_SELECT_BOOLEAN_CHECKBOX_READONLY_UNCHECKED_ICON_NAME =
     "af|selectBooleanCheckbox::read-only-unchecked-icon";
   // -= Simon =-
   // FIXME: Check if those selectors are ok. Constant names suggest radio button
   //        while selector suggest checkbox
   public static final String AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME =
     "af|selectBooleanRadio::disabled-selected-icon";
   public static final String AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME =
     "af|selectBooleanRadio::disabled-unselected-icon";
   public static final String AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME =
     "af|selectBooleanRadio::read-only-selected-icon";
   public static final String AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME =
     "af|selectBooleanRadio::read-only-unselected-icon";

   
   //                                                                         //
   //                                                                         //
   // ======================== tr:selectManyShuttle ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   


  // Shuttle Icons
  // (selectManyShuttle and selectOrderShuttle)
  // simple/oracle use the same base.desktop renderer now that I fixed it.
  // shuttle is not supported in pda.
  public static final String AF_SELECT_MANY_SHUTTLE_PB_BODY_STYLE_CLASS =
    "af|selectManyShuttle::box-body";

  public static final String AF_SELECT_MANY_SHUTTLE_PB_STYLE_CLASS =
    "af|selectManyShuttle::box";

  public static final String AF_SELECT_MANY_SHUTTLE_PB_TOP_START_STYLE_CLASS =
    "af|selectManyShuttle::box-top-start";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_TOP_STYLE_CLASS =
    "af|selectManyShuttle::box-top";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_TOP_END_STYLE_CLASS =
    "af|selectManyShuttle::box-top-end";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_START_STYLE_CLASS =
    "af|selectManyShuttle::box-start";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_CONTENT_STYLE_CLASS =
    "af|selectManyShuttle::box-content";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_END_STYLE_CLASS =
    "af|selectManyShuttle::box-end";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_START_STYLE_CLASS =
    "af|selectManyShuttle::box-bottom-start";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_STYLE_CLASS =
    "af|selectManyShuttle::box-bottom";
  public static final String AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_END_STYLE_CLASS =
    "af|selectManyShuttle::box-bottom-end";

   // ================================ Icons ================================ //

  // FIXME: change to af|selectManyShuttle::header
  public static final String SHUTTLE_HEADER_STYLE_CLASS = "OraShuttleHeader";


   public static final String AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME =
     "af|selectManyShuttle::move-all-icon";
   public static final String AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME =
     "af|selectManyShuttle::move-icon";
   public static final String AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME =
     "af|selectManyShuttle::remove-all-icon";
   public static final String AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME =
     "af|selectManyShuttle::remove-icon";

   
   //                                                                         //
   //                                                                         //
   // ======================== tr:selectOrderShuttle ======================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   

  public static final String AF_SELECT_ORDER_SHUTTLE_PB_STYLE_CLASS =
    "af|selectOrderShuttle::box";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_BODY_STYLE_CLASS =
    "af|selectOrderShuttle::box-body";

  public static final String AF_SELECT_ORDER_SHUTTLE_PB_TOP_START_STYLE_CLASS =
    "af|selectOrderShuttle::box-top-start";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_TOP_STYLE_CLASS =
    "af|selectOrderShuttle::box-top";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_TOP_END_STYLE_CLASS =
    "af|selectOrderShuttle::box-top-end";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_START_STYLE_CLASS =
    "af|selectOrderShuttle::box-start";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_CONTENT_STYLE_CLASS =
    "af|selectOrderShuttle::box-content";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_END_STYLE_CLASS =
    "af|selectOrderShuttle::box-end";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_START_STYLE_CLASS =
    "af|selectOrderShuttle::box-bottom-start";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_STYLE_CLASS =
    "af|selectOrderShuttle::box-bottom";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_END_STYLE_CLASS =
    "af|selectOrderShuttle::box-bottom-end";

   // ================================ Icons ================================ //
   public static final String AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME =
     "af|selectOrderShuttle::move-all-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME =
     "af|selectOrderShuttle::move-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME =
     "af|selectOrderShuttle::remove-all-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME =
     "af|selectOrderShuttle::remove-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME =
     "af|selectOrderShuttle::reorder-top-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME =
     "af|selectOrderShuttle::reorder-up-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME =
     "af|selectOrderShuttle::reorder-down-icon";
   public static final String AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME =
     "af|selectOrderShuttle::reorder-bottom-icon";

   //                                                                         //
   //                                                                         //
   // ======================= tr:selectRangeChoiceBar ======================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   
   // ================================ Icons ================================ //
   public static final String AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME =
     "af|selectRangeChoiceBar::prev-icon";
   public static final String AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME =
     "af|selectRangeChoiceBar::next-icon";
   public static final String AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME =
     "af|selectRangeChoiceBar::prev-disabled-icon";
   public static final String AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME =
     "af|selectRangeChoiceBar::next-disabled-icon";


   //                                                                         //
   //                                                                         //
   // ============================ tr:separator ============================ //
   //                                                                         //
   //                                                                         //
   public static final String AF_SEPARATOR_STYLE_CLASS =
     "af|separator";
    
   //                                                                         //
   //                                                                         //
   // ============================ tr:showDetail ============================ //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_SHOW_DETAIL_PROMPT_DISCLOSED_STYLE_CLASS =
     "af|showDetail::prompt-disclosed";
   public static final String AF_SHOW_DETAIL_PROMPT_UNDISCLOSED_STYLE_CLASS =
     "af|showDetail::prompt-undisclosed";
   public static final String AF_SHOW_DETAIL_PROMPT_LINK_STYLE_CLASS =
     "af|showDetail::prompt-link";
   public static final String AF_SHOW_DETAIL_DISCLOSURE_ICON_LINK_STYLE_CLASS =
     "af|showDetail::disclosure-icon-link";
   
     
   //                                                                         //
   //                                                                         //
   // ============================ tr:showDetailItem ============================ //
   //                                                                         //
   //                                                                         //   
   public static final String AF_SHOW_DETAIL_ITEM_SELECTED =
     "af|showDetailItem::selected";
    
   // ================================ Icons ================================ //
   public static final String AF_SHOW_DETAIL_DISCLOSED_ICON_NAME =
     "af|showDetail::disclosed-icon";
   public static final String AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME =
     "af|showDetail::undisclosed-icon";

   // The Webkit browser that runs in Nokia platform doesn't support Unicode 
   // characters that display icons, so we need to render text-icons for
   // Nokia.
   // Ideally, we should be able to use above skinning-keys and switch it to 
   // text-icons for Nokia platform, but we don't have such mechanism in Trinidad
   // right now. Hence, I have created new skinning-keys for Nokia.
   public static final String AF_SHOW_DETAIL_DISCLOSED_ICON_NAME_FOR_NOKIA_S60 =
     "af|showDetail::nokia-disclosed-icon";
   public static final String AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME_FOR_NOKIA_S60 =
     "af|showDetail::nokia-undisclosed-icon";
   
   //                                                                         //
   //                                                                         //
   // ========================= tr:showDetailHeader ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_SHOW_DETAIL_HEADER_STYLE_CLASS =
     "af|showDetailHeader";

   // ================================ Icons ================================ //
   public static final String AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME =
     "af|showDetailHeader::disclosed-icon";
   public static final String AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME =
     "af|showDetailHeader::undisclosed-icon";


  
   //                                                                         //
   //                                                                         //
   // ======================== tr:singleStepButtonBar ========================= //
   //                                                                         //
   //                                                                         //
   public static final String AF_SINGLE_STEP_BUTTON_BAR =
    "af|singleStepButtonBar";
   public static final String AF_SINGLE_STEP_BUTTON_BAR_LABEL =
    "af|singleStepButtonBar::label";

   //                                                                         //
   //                                                                         //
   // ======================== tr:showManyAccordion ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // FIXME: Name inconsistency, should be SHOW_MANY_ACCORDION
   //        Could also be renamed AF_PANEL_ACCORDION since I think those two
   //        were merged and they'Re relatively new.
   public static final String AF_SHOWMANYACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
     "af|showManyAccordion::title-disabled-link";
   public static final String AF_SHOWMANYACCORDION_TITLE_LINK_STYLE_CLASS =
     "af|showManyAccordion::title-link";

   
   //                                                                         //
   //                                                                         //
   // ========================= tr:showOneAccordion ========================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_SHOWONEACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
     "af|showOneAccordion::title-disabled-link";
   public static final String AF_SHOWONEACCORDION_TITLE_LINK_STYLE_CLASS =
     "af|showOneAccordion::title-link";

   
   //                                                                         //
   //                                                                         //
   // ============== ===== tr:statusIndicator =============================== //
   //                                                                         //
   //                                                                         //
   public static final String AF_STATUS_INDICATOR_STYLE =
     "af|statusIndicator";
   public static final String AF_STATUS_INDICATOR_BUSY_STYLE =
     "af|statusIndicator::busy";
   public static final String AF_STATUS_INDICATOR_READY_STYLE =
     "af|statusIndicator::ready";

   // ================================ Icons ================================ //
   public static final String AF_STATUS_INDICATOR_BUSY_ICON =
     "af|statusIndicator::busy-icon";
   public static final String AF_STATUS_INDICATOR_READY_ICON =
     "af|statusIndicator::ready-icon";


   //                                                                         //
   //                                                                         //
   // ============================== tr:table =============================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_TABLE_STYLE =
     "af|table";
   public static final String AF_TABLE_COLUMN_FOOTER_STYLE =
     "af|table::column-footer";
   public static final String AF_TABLE_CONTENT_STYLE =
     "af|table::content";
   public static final String AF_TABLE_CONTROL_BAR_BOTTOM_STYLE =
     "af|table::control-bar-bottom";
   public static final String AF_TABLE_CONTROL_BAR_TOP_STYLE =
     "af|table::control-bar-top";
   public static final String AF_TABLE_DETAIL_STYLE =
     "af|table::detail";
   public static final String AF_TABLE_SUB_CONTROL_BAR_STYLE =
     "af|table::sub-control-bar";
   
   // ================================ Icons ================================ //
   public static final String AF_TABLE_NB_NEXT_DISABLED_ICON_NAME =
     "af|table::next-disabled-icon";
   public static final String AF_TABLE_NB_NEXT_ICON_NAME =
     "af|table::next-icon";
   public static final String AF_TABLE_NB_PREV_DISABLED_ICON_NAME =
     "af|table::prev-disabled-icon";
   public static final String AF_TABLE_NB_PREV_ICON_NAME =
     "af|table::prev-icon";
   public static final String AF_TABLE_SD_DISCLOSED_ICON_NAME =
     "af|table::disclosed-icon";
   public static final String AF_TABLE_SD_UNDISCLOSED_ICON_NAME =
     "af|table::undisclosed-icon";
   public static final String AF_TABLE_SELECT_ALL_ICON_NAME =
     "af|table::select-all-icon";
   public static final String AF_TABLE_SELECT_NONE_ICON_NAME =
     "af|table::select-none-icon";

   //                                                                         //
   //                                                                         //
   // ========================= tr:tableSelectMany ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_TABLE_SELECT_MANY_CELL_ICON_BAND_STYLE =
     "af|tableSelectMany::cell-icon-format-band";
   public static final String AF_TABLE_SELECT_MANY_CELL_ICON_FORMAT_STYLE =
     "af|tableSelectMany::cell-icon-format";


   //                                                                         //
   //                                                                         //
   // ========================== tr:tableSelectOne ========================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   public static final String AF_TABLE_SELECT_ONE_CELL_ICON_BAND_STYLE =
     "af|tableSelectOne::cell-icon-format-band";
   public static final String AF_TABLE_SELECT_ONE_CELL_ICON_FORMAT_STYLE =
     "af|tableSelectOne::cell-icon-format";


   //                                                                         //
   //                                                                         //
   // ============================== tr:train =============================== //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // Root class
   public static final String AF_TRAIN_ROOT_STYLE_CLASS =
     DEFAULT_NAMESPACE + "|train";
   
   // Joins
   public static final String AF_TRAIN_JOIN_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::join";
   public static final String AF_TRAIN_OVERFLOW_JOIN_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::join-overflow";
   public static final String AF_TRAIN_PARENT_JOIN_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::join-parent";
   
   // Links
   public static final String AF_TRAIN_LINK_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::link";
   
   // Overflow start
   public static final String AF_TRAIN_OVERFLOW_START_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::overflow-start";
   public static final String AF_TRAIN_OVERFLOW_START_CONTENT_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + "-content";
   public static final String AF_TRAIN_OVERFLOW_START_ICON_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + "-icon-cell";
   
   // Overflow end
   public static final String AF_TRAIN_OVERFLOW_END_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::overflow-end";
   public static final String AF_TRAIN_OVERFLOW_END_CONTENT_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + "-content";
   public static final String AF_TRAIN_OVERFLOW_END_ICON_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + "-icon-cell";
   
   // Parent start
   public static final String AF_TRAIN_PARENT_START_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::parent-start";
   public static final String AF_TRAIN_PARENT_START_CONTENT_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + "-content";
   public static final String AF_TRAIN_PARENT_START_ICON_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + "-icon-cell";
   
   // Parent end
   public static final String AF_TRAIN_PARENT_END_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::parent-end";
   public static final String AF_TRAIN_PARENT_END_CONTENT_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + "-content";
   public static final String AF_TRAIN_PARENT_END_ICON_STYLE_CLASS =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + "-icon-cell";
   
   // Stops
   public static final String AF_TRAIN_STOP_STYLE_CLASS =
     AF_TRAIN_ROOT_STYLE_CLASS + "::stop";
   public static final String AF_TRAIN_STOP_CONTENT_STYLE_CLASS =
     AF_TRAIN_STOP_STYLE_CLASS + "-content";
   public static final String AF_TRAIN_STOP_ICON_STYLE_CLASS =
     AF_TRAIN_STOP_STYLE_CLASS + "-icon-cell";

   // ================================ Icons ================================ //
   // Overflow start icons
   public static final String AF_TRAIN_OVERFLOW_START_DISABLED_ICON_NAME =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + ":disabled" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_START_READ_ONLY_ICON_NAME =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + ":read-only" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_START_UNVISITED_ICON_NAME =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + ":unvisited" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_START_VISITED_ICON_NAME =
     AF_TRAIN_OVERFLOW_START_STYLE_CLASS + ":visited" + ICON_SUFFIX;
   
   // Overflow end icons
   public static final String AF_TRAIN_OVERFLOW_END_DISABLED_ICON_NAME =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + ":disabled" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_END_READ_ONLY_ICON_NAME =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + ":read-only" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_END_UNVISITED_ICON_NAME =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + ":unvisited" + ICON_SUFFIX;
   public static final String AF_TRAIN_OVERFLOW_END_VISITED_ICON_NAME =
     AF_TRAIN_OVERFLOW_END_STYLE_CLASS + ":visited" + ICON_SUFFIX;
   
   // Parent train icons
   public static final String AF_TRAIN_PARENT_START_ICON_NAME =
     AF_TRAIN_PARENT_START_STYLE_CLASS + ICON_SUFFIX;
   public static final String AF_TRAIN_PARENT_END_ICON_NAME =
     AF_TRAIN_PARENT_END_STYLE_CLASS + ICON_SUFFIX;
   
   // Stop icons
   public static final String AF_TRAIN_STOP_ACTIVE_ICON_NAME =
     AF_TRAIN_STOP_STYLE_CLASS + ":selected" + ICON_SUFFIX;
   public static final String AF_TRAIN_STOP_DISABLED_ICON_NAME =
     AF_TRAIN_STOP_STYLE_CLASS + ":disabled" + ICON_SUFFIX;
   public static final String AF_TRAIN_STOP_READ_ONLY_ICON_NAME =
     AF_TRAIN_STOP_STYLE_CLASS + ":read-only" + ICON_SUFFIX;
   public static final String AF_TRAIN_STOP_UNVISITED_ICON_NAME =
     AF_TRAIN_STOP_STYLE_CLASS + ":unvisited" + ICON_SUFFIX;
   public static final String AF_TRAIN_STOP_VISITED_ICON_NAME =
     AF_TRAIN_STOP_STYLE_CLASS + ":visited" + ICON_SUFFIX;

   
   //                                                                         //
   //                                                                         //
   // ============================ tr:treeTable ============================= //
   //                                                                         //
   //                                                                         //
   
   // ============================ Style classes ============================ //
   // FIXME: Inconsistent names _STYLE vs. _STYLE_CLASS
   public static final String AF_TREE_TABLE_CONTENT_STYLE =
     "af|treeTable::content";
   public static final String AF_TREE_TABLE_CONTROL_BAR_BOTTOM_STYLE =
     "af|treeTable::control-bar-bottom";
   public static final String AF_TREE_TABLE_CONTROL_BAR_TOP_STYLE =
     "af|treeTable::control-bar-top";
   public static final String AF_TREE_TABLE_EXPANSION_ICON_STYLE_CLASS =
     "af|treeTable::expansion";
   public static final String AF_TREE_TABLE_FOCUS_ICON_STYLE_CLASS =
     "af|treeTable::focus";
   public static final String AF_TREE_TABLE_LOCATOR_ICON_STYLE_CLASS =
     "af|treeTable::locator";
   public static final String AF_TREE_TABLE_MP_SELECTED_STYLE_CLASS =
     "af|treeTable::path-selected-step";
   public static final String AF_TREE_TABLE_MP_STEP_STYLE_CLASS =
     "af|treeTable::path-step";
   public static final String AF_TREE_TABLE_MP_STYLE_CLASS =
     "af|treeTable::path";
   public static final String AF_TREE_TABLE_SUB_CONTROL_BAR_STYLE =
     "af|treeTable::sub-control-bar";

   // ================================ Icons ================================ //
   public static final String AF_TREE_TABLE_COLLAPSED_ICON_NAME =
     "af|treeTable::collapsed-icon";
   public static final String AF_TREE_TABLE_DISABLED_NAV_DOWN_ICON_NAME =
     "af|treeTable::disabled-nav-down-icon";
   public static final String AF_TREE_TABLE_DISABLED_NAV_UP_ICON_NAME =
     "af|treeTable::disabled-nav-up-icon";
   public static final String AF_TREE_TABLE_EXPANDED_ICON_NAME =
     "af|treeTable::expanded-icon";
   public static final String AF_TREE_TABLE_FOCUS_ICON_NAME =
     "af|treeTable::focus-icon";
   public static final String AF_TREE_TABLE_LOCATOR_ICON_NAME =
     "af|treeTable::locator-icon";
   public static final String AF_TREE_TABLE_MP_SEPARATOR_ICON_NAME =
     "af|treeTable::separator-icon";
   public static final String AF_TREE_TABLE_NAV_DOWN_ICON_NAME =
     "af|treeTable::nav-down-icon";
   public static final String AF_TREE_TABLE_NAV_UP_ICON_NAME =
     "af|treeTable::nav-up-icon";
   public static final String AF_TREE_TABLE_NB_PREV_ICON_NAME =
     "af|treeTable::prev-icon";
   public static final String AF_TREE_TABLE_NB_NEXT_ICON_NAME =
     "af|treeTable::next-icon";
   public static final String AF_TREE_TABLE_NB_PREV_DISABLED_ICON_NAME =
     "af|treeTable::prev-disabled-icon";
   public static final String AF_TREE_TABLE_NB_NEXT_DISABLED_ICON_NAME =
     "af|treeTable::next-disabled-icon";
   public static final String AF_TREE_TABLE_EXPAND_ALL_ICON_NAME =
     "af|treeTable::expand-all-icon";
   public static final String AF_TREE_TABLE_COLLAPSE_ALL_ICON_NAME =
     "af|treeTable::collapse-all-icon";
   public static final String AF_TREE_TABLE_SELECT_ALL_ICON_NAME =
     "af|treeTable::select-all-icon";
   public static final String AF_TREE_TABLE_SELECT_NONE_ICON_NAME =
     "af|treeTable::select-none-icon";    
   public static final String AF_TREE_TABLE_NODE_ICON =
     "af|treeTable::node-icon";


  //                                                                         //
  //                                                                         //
  // ============================== tr:tree =============================== //
  //                                                                         //
  //

  // ============================ Style classes ============================ //
  public static final String TREE_NODE_ADJUST_STYLE_CLASS =
    "p_OraTreeNodeAdjust";
  public static final String TREE_ROW_STYLE_CLASS = "p_OraTreeRow";
  public static final String TREE_ROW_SELECTED_STYLE_CLASS =
    "p_OraTreeRowSelected";
  public static final String TREE_ICON_STYLE_CLASS = "p_OraTreeIcon";
  public static final String TREE_DISCLOSED_SYMBOL_STYLE_CLASS =
    "p_OraTreeDisclosedSymbol";

  // ================================ Icons ================================ //
  public static final String AF_TREE_EXPANDED_ICON =
    "af|tree::expanded-icon";
  public static final String AF_TREE_COLLAPSED_ICON =
    "af|tree::collapsed-icon";
  public static final String AF_TREE_NO_CHILDREN_ICON =
    "af|tree::no-children-icon";

  public static final String AF_TREE_NODE_ICON =
    "af|tree::node-icon";

  public static final String AF_TREE_LINE_ICON =
    "af|tree::line-icon";
  public static final String AF_TREE_LINE_MIDDLE_ICON =
    "af|tree::line-middle-icon";
  public static final String AF_TREE_LINE_LAST_ICON =
    "af|tree::line-last-icon";


  //                                                                         //
  //                                                                         //
  // ============================== tr:chart =============================== //
  //                                                                         //
  //
  public static final String AF_CHART_STYLE_CLASS =
    "af|chart";
   
}
