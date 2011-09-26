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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafConstants;

/**
 * Constants used throughout the HTML Laf rendering.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlLafConstants.java#0 $) $Date: 10-nov-2005.18:54:18 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface XhtmlLafConstants extends BaseLafConstants
{

  // renderKitIds needed for Skins
  public static final String ORACLE_ADF_DESKTOP = "org.apache.myfaces.trinidad.desktop";
  public static final String ORACLE_ADF_PDA     = "org.apache.myfaces.trinidad.pda";

  // mobile dateField and lovField params
  public static final String TOK_PARAM  = UIConstants.TOK_PARAM;

  // context property to indicate that form elements are repeated and the
  // data needs to be kept in sync
  public static final Object REPEAT_PROPERTY = new Object();

  // This property is used by SubTabBarLayoutRenderer to pass the
  // partial targets to use to LinkRenderer.  Currently we pass
  // the partial targets in encoded (String) form, to avoid
  // re-encoding the partial targets for each link.
  public static final Object LINK_CONTAINER_PARTIAL_TARGETS_PROPERTY =
                               new Object();

  // This property is used to stash a script away on the rendering context so
  // we can support submitPrepare on Netscape 4.7
  public static final Object DEFERED_LOV_SCRIPT_PROPERTY = new Object();


  public static final String COLOR_PALETTE_TRANSPARENT_ICON_NAME = "cpt.gif";
  //
  // Roles
  //
  public static final NodeRole INLINE_MESSAGE_ROLE =
    new NodeRole("inlineMessage");

  public static final NodeRole FORM_SUBMIT_DATA_ROLE =
    new NodeRole("formSubmitData");

  // =-=AEW Note that we don't add COMPOSITE_ROLE;  that's up to
  // the renderer.
//  public static final NodeRole TABLE_HEADER_ROLE =
//    new NodeRole("tableHeader");

  public static final NodeRole TABLE_FOOTER_ROLE =
    new NodeRole("tableFooter");

//  public static final NodeRole TABLE_COLUMN_ROLE =
//    new NodeRole("tableColumn");
//
//  public static final NodeRole TABLE_COLUMN_GROUP_ROLE =
//    new NodeRole("tableColumnGroup");

  public static final NodeRole BODY_ROLE =
    new NodeRole("body",
                 new NodeRole[]{UIConstants.STRUCTURAL_ROLE});

  public static final NodeRole DOCUMENT_ROLE =
    new NodeRole("document",
                 new NodeRole[]{UIConstants.STRUCTURAL_ROLE});

  //
  // HTML ELEMENTS
  //
  public static final String BREAK_ELEMENT        = "br";
  public static final String DIV_ELEMENT          = "div";
  public static final String BUTTON_ELEMENT       = "button";
  public static final String FORM_ELEMENT         = "form";
  public static final String IMAGE_ELEMENT        = "img";
  public static final String INPUT_ELEMENT        = "input";
  public static final String LINK_ELEMENT         = "a";
  public static final String OPTION_ELEMENT       = "option";
  public static final String SELECT_ELEMENT       = "select";
  public static final String SPAN_ELEMENT         = "span";
  public static final String TABLE_ELEMENT        = "table";
  public static final String TABLE_DATA_ELEMENT   = "td";
  public static final String TABLE_HEADER_ELEMENT = "th";
  public static final String TABLE_ROW_ELEMENT    = "tr";
  public static final String TEXT_AREA_ELEMENT    = "textarea";
  public static final String SCRIPT_ELEMENT       = "script";
  public static final String LIST_ITEM_ELEMENT    = "li";
  public static final String UNORDERED_LIST_ELEMENT = "ul";
  public static final String ORDERED_LIST_ELEMENT = "ol";
  public static final String HORIZONTAL_RULE_ELEMENT = "hr";
  public static final String[] HEADER_ELEMENT     = {"h1", "h2", "h3",
                                                     "h4", "h5", "h6"};
  public static final String FIELDSET_ELEMENT     = "fieldset";
  public static final String LEGEND_ELEMENT       = "legend";
  //
  // non-basic HTML ELEMENTS
  //
  public static final String NO_BREAK_ELEMENT = "nobr";

  //
  // HTML ATTRIBUTES
  //
  public static final String ALIGN_ATTRIBUTE     = "align";
  public static final String ALT_ATTRIBUTE       = "alt";
  public static final String CHECKED_ATTRIBUTE   = "checked";
  // DO NOT uncomment the following line.  CLASS_ATTRIBUTE should never
  // be rendered directly, but always passed through renderStyleClassAttribute()
  // public static final String CLASS_ATTRIBUTE     = "class";
  public static final String COLS_ATTRIBUTE      = "cols";
  public static final String COLSPAN_ATTRIBUTE   = "colspan";
  public static final String HEIGHT_ATTRIBUTE    = "height";
  public static final String HREF_ATTRIBUTE      = "href";
  public static final String ID_ATTRIBUTE        = "id";
  public static final String IS_MAP_ATTRIBUTE    = "ismap";
  public static final String MAXLENGTH_ATTRIBUTE = "maxlength";
  public static final String MULTIPLE_ATTRIBUTE  = "multiple";
  public static final String NAME_ATTRIBUTE      = "name";
  public static final String ONCLICK_ATTRIBUTE   = "onclick";
  public static final String ROWS_ATTRIBUTE      = "rows";
  public static final String ROWSPAN_ATTRIBUTE   = "rowspan";
  public static final String SELECTED_ATTRIBUTE  = "selected";
  public static final String SIZE_ATTRIBUTE      = "size";
  public static final String SOURCE_ATTRIBUTE    = "src";
  public static final String TYPE_ATTRIBUTE      = "type";
  public static final String VALIGN_ATTRIBUTE    = "valign";
  public static final String VALUE_ATTRIBUTE     = "value";
  public static final String WIDTH_ATTRIBUTE     = "width";

  //
  // non-basic HTML ATTRIBUTES
  //
  public static final String BACKGROUND_ATTRIBUTE   = "background";
  public static final String BORDER_ATTRIBUTE       = "border";
  public static final String CELLPADDING_ATTRIBUTE  = "cellpadding";
  public static final String CELLSPACING_ATTRIBUTE  = "cellspacing";
  public static final String NOWRAP_ATTRIBUTE       = "nowrap";
  public static final String STYLE_ATTRIBUTE        = "style";
  public static final String TARGET_FRAME_ATTRIBUTE = "target";
  public static final String WRAP_ATTRIBUTE         = "wrap";
  public static final String SUMMARY_ATTRIBUTE      = "summary";
  public static final String ABBREVIATION_ATTRIBUTE = "abbr";

  //
  // HTML ATTRIBUTE VALUES
  //
  public static final String MIDDLE_ATTRIBUTE_VALUE = "middle";
  public static final String LEFT_ATTRIBUTE_VALUE   = "left";
  public static final String RIGHT_ATTRIBUTE_VALUE  = "right";
  public static final String CENTER_ATTRIBUTE_VALUE = "center";
  public static final String TOP_ATTRIBUTE_VALUE    = "top";
  public static final String BOTTOM_ATTRIBUTE_VALUE = "bottom";
  public static final String DIR_ATTRIBUTE_VALUE = "dir";
  public static final String SOFT_ATTRIBUTE_VALUE = "soft";
  public static final String HARD_ATTRIBUTE_VALUE = "hard";

  public static final String EMPTY_STRING_ATTRIBUTE_VALUE = "";
  public static final String ZERO_ATTRIBUTE_VALUE = "0";
  public static final String ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE = "100%";

  //
  // STYLE CLASSES
  //
  // BACKGROUND COLOR STYLES (used in oracle renderers)
  public static final String BGCOLOR_VERY_DARK_STYLE_CLASS="OraBGColorVeryDark";
  public static final String BGCOLOR_DARK_STYLE_CLASS = "OraBGColorDark";
  public static final String BGCOLOR_MEDIUM_STYLE_CLASS = "OraBGColorMedium";
  public static final String BGCOLOR_LIGHT_STYLE_CLASS = "OraBGColorLight";
  public static final String BGGRAY_VERY_DARK_STYLE_CLASS ="OraBGGrayVeryDark";
  public static final String BGGRAY_DARK_STYLE_CLASS = "OraBGGrayDark";
  public static final String BGGRAY_MEDIUM_STYLE_CLASS = "OraBGGrayMedium";
  public static final String BGGRAY_LIGHT_STYLE_CLASS = "OraBGGrayLight";
  public static final String BGACCENT_VERY_DARK_STYLE_CLASS="OraBGAccentVeryDark";
  public static final String BGACCENT_DARK_STYLE_CLASS = "OraBGAccentDark";
  public static final String BGACCENT_MEDIUM_STYLE_CLASS = "OraBGAccentMedium";
  public static final String BGACCENT_LIGHT_STYLE_CLASS = "OraBGAccentLight";


  // global icon styles
  // e.g, used to style the icons in XhtmlSkin
  public static final String ERROR_ICON_STYLE_CLASS    = "AFErrorIconStyle";
  public static final String INFO_ICON_STYLE_CLASS     = "AFInfoIconStyle";
  public static final String WARNING_ICON_STYLE_CLASS  = "AFWarningIconStyle";
  public static final String REQUIRED_ICON_STYLE_CLASS = "AFRequiredIconStyle";
  public static final String QUICK_SELECT_ICON_STYLE_CLASS
    = "AFQuickSelectIconStyle";
  public static final String QUICK_SELECT_DISABLED_ICON_STYLE_CLASS
    = "AFQuickSelectDisabledIconStyle";



  // button TEXT STYLES for pda for resetButton and submitButton.
  // These styles do not render. Instead the OraLink class renders
  // and these styles render as duplicate_class (this must be a bug).
  // For now, leave alone. If they want to customize browser-based
  // buttons, then they can use the html component button. button {font-size:xx}
  // @todo It would be nice I think to not render OraLink on the
  // browser-based buttons (this includes desktop's browser based buttons),
  // but a class like AFBrowserButton. (first make sure there is no
  // real reason why OraLink is the style on browser-based buttons)
  // private for now, since it doesn't work!
  public static final String BUTTON_TEXT_STYLE_CLASS =
    "OraButtonText";
  public static final String BUTTON_TEXT_STYLE_DISABLED_CLASS =
    "OraButtonTextDisabled";

  public static final String AF_MENU_CHOICE_LABEL_STYLE_CLASS =
    "af|menuChoice::label";

  public static final String AF_MENU_BUTTONS_IMAGE_STYLE_CLASS =
    "af|menuButtons::icon-style";
  public static final String AF_MENU_BUTTONS_TEXT_STYLE_CLASS =
    "af|menuButtons::text";
  public static final String AF_MENU_BUTTONS_TEXT_SELECTED_STYLE_CLASS =
    "af|menuButtons::text-selected";
  public static final String AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS =
    "af|menuButtons::text-disabled";

  // outputLabel's styles
  public static final String AF_OUTPUT_LABEL_STYLE_CLASS =
    "af|outputLabel";

  public static final String AF_PANEL_PAGE_BRANDING_STYLE_CLASS =
    "af|panelPage::branding";
  public static final String AF_PANEL_PAGE_MB_TEXT_STYLE_CLASS =
    "af|panelPage::menu-buttons-text";
  public static final String AF_PANEL_PAGE_MB_TEXT_SELECTED_STYLE_CLASS =
    "af|panelPage::menu-buttons-text-selected";
  public static final String AF_PANEL_PAGE_MB_TEXT_DISABLED_STYLE_CLASS =
    "af|panelPage::menu-buttons-text-disabled";

  // panelForm's styles
  public static final String AF_PANEL_FORM_STYLE_CLASS =
     "af|panelFormLayout";
  public static final String AF_PANEL_FORM_COLUMN_STYLE_CLASS =
    "af|panelFormLayout::column";
  public static final String AF_PANEL_FORM_SEPARATOR_STYLE_CLASS =
    "af|panelFormLayout::separator";
  public static final String AF_PANEL_FORM_LABEL_CELL_STYLE_CLASS =
    "af|panelFormLayout::label-cell";
  public static final String AF_PANEL_FORM_LABEL_STACKED_CELL_STYLE_CLASS =
    "af|panelFormLayout::label-stacked-cell";
  public static final String AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS =
    "af|panelFormLayout::content-cell";
  public static final String AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS =
    "af|panelFormLayout::message-cell";

  // Link style classes
  public static final String LINK_STYLE_CLASS =
    "OraLink";
  public static final String LINK_DISABLED_STYLE_CLASS =
    "OraLinkDisabled";

  public static final String LINK_TEXT_STYLE_CLASS =
    "OraLinkText";

  // used in our select components, and composites that use these, like calendar
  // and shuttle.
  // selectBooleanCheckbox, selectBooleanRadio, selectInputDate, selectInputColor,
  // etc. Best to make this global.
  public static final String AF_FIELD_TEXT_STYLE_CLASS =
    "AFFieldText";
  public static final String AF_FIELD_TEXT_DISABLED_STYLE_CLASS =
    "AFFieldTextDisabled";

  // used in FormattedText when styleUsage is instruction
  // also used in (oracle)styleList, (oracle)menuList, and
  // (base)shuttle's description.
  // keep private for now. No need to customize.
  public static final String INSTRUCTION_TEXT_STYLE_CLASS =
    "AFInstructionText";

  public static final String PAGE_STAMP_TEXT_STYLE_CLASS =
    "OraPageStampText";
  public static final String IN_CONTEXT_TEXT_STYLE_CLASS =
    "p_InContextBrandingText";
  public static final String DATA_TEXT_STYLE_CLASS =
    "AFDataText";

  // global styles to be used in styleClass attribute for inputText.
  // Also, used in TextInputRenderer and SimpleInputTextRenderer.
  public static final String AF_FIELD_NUMBER_STYLE_CLASS =
    "AFFieldNumber";
  public static final String AF_FIELD_NUMBER_DISABLED_STYLE_CLASS =
    "AFFieldNumberDisabled";

  public static final String SHUTTLE_HEADER_STYLE_CLASS = "OraShuttleHeader";

  // private
  public static final String PROCESS_STATUS_STYLE_CLASS =
    "p_OraProcessingStatus";

  // styles used in BLAF. I didn't make these public. @todo We can once
  // we fix the blaf renderers to extend simple.
  public static final String MESSAGE_BOX_PARAGRAPH_STYLE_CLASS =
    "OraMessageBoxParagraph";
  public static final String MESSAGE_BOX_ERROR_PARAGRAPH_STYLE_CLASS =
    "OraMessageBoxErrorParagraph";
  public static final String MESSAGE_BOX_LIST_STYLE_CLASS =
    "OraMessageBoxList";
  public static final String MESSAGE_BOX_ERROR_LIST_STYLE_CLASS =
    "OraMessageBoxErrorList";
  public static final String MESSAGE_BOX_ERROR_LINK_STYLE_CLASS =
    "OraMessageBoxErrorLink";
  public static final String MESSAGE_BOX_LINK_STYLE_CLASS =
    "OraMessageBoxLink";
  public static final String MESSAGE_BOX_TEXT_STYLE_CLASS =
    "OraMessageBoxText";
  public static final String ERROR_TEXT_STYLE_CLASS =
    "OraMessageBoxErrorText";

  // NAVIGATION TEXT STYLES @todo investigate. keep private for now since it
  // isn't used by simple.desktop (but maybe by simple.pda?)
  public static final String GLOBAL_PAGE_TITLE_STYLE_CLASS =
    "OraGlobalPageTitle";

  // @todo Investigate. Any of these that should be customizable? Make sure
  // they are per-component.
  public static final String LINK_SELECTED_STYLE_CLASS =
    "OraLinkSelected";

  public static final String NAV_2_STYLE_CLASS =
    "p_OraNav2";
  public static final String NAV_3_SELECTED_STYLE_CLASS =
    "OraNav3Selected";
  public static final String NAV_3_STYLE_CLASS =
    "OraNav3";

  // menuList
  public static final String AF_MENU_LIST_STYLE_CLASS =
    "af|menuList";
  public static final String AF_MENU_LIST_SELECTED_STYLE_CLASS =
    "af|menuList::selected";

  // PANEL BOX
  public static final String AF_PANEL_BOX_BG_TRANSPARENT_STYLE_CLASS =
    "af|panelBox::background-transparent";
  public static final String AF_PANEL_BOX_BG_LIGHT_STYLE_CLASS =
    "af|panelBox::background-light";
  public static final String AF_PANEL_BOX_BG_MEDIUM_STYLE_CLASS =
    "af|panelBox::background-medium";
  public static final String AF_PANEL_BOX_BG_DARK_STYLE_CLASS =
    "af|panelBox::background-dark";

  // Shuttle
  public static final String AF_SELECT_MANY_SHUTTLE_PB_BG_LIGHT_STYLE_CLASS  =
    "af|selectManyShuttle::panel-box-background-light";
  public static final String AF_SELECT_ORDER_SHUTTLE_PB_BG_LIGHT_STYLE_CLASS =
    "af|selectOrderShuttle::panel-box-background-light";

  // FOOTER STYLES for panelPage
  public static final String AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS =
    "af|panelPage::copyright";
  public static final String AF_PANEL_PAGE_PRIVACY_STYLE_CLASS   =
    "af|panelPage::privacy";
  public static final String AF_PANEL_PAGE_ABOUT_STYLE_CLASS   =
    "af|panelPage::about";

  // TABLE STYLES
  // jmw used only in Netscape 4
  public static final String TABLE_STYLE =
    "OraTable";

  public static final String AF_TABLE_CONTENT_STYLE =
    "af|table::content";
  public static final String AF_TREE_TABLE_CONTENT_STYLE =
    "af|treeTable::content";
  // deprecated style, also private
  public static final String TABLE_TITLE_STYLE =
    "OraTableTitle";

  public static final String AF_TABLE_SUB_CONTROL_BAR_STYLE =
    "af|table::sub-control-bar";
  public static final String AF_TREE_TABLE_SUB_CONTROL_BAR_STYLE =
    "af|treeTable::sub-control-bar";
  public static final String AF_TABLE_CONTROL_BAR_TOP_STYLE =
    "af|table::control-bar-top";
  public static final String AF_TREE_TABLE_CONTROL_BAR_TOP_STYLE =
    "af|treeTable::control-bar-top";
  public static final String AF_TABLE_CONTROL_BAR_BOTTOM_STYLE =
    "af|table::control-bar-bottom";
  public static final String AF_TREE_TABLE_CONTROL_BAR_BOTTOM_STYLE =
    "af|treeTable::control-bar-bottom";
  // private style
  public static final String TABLE_CONTROL_BAR_TEXT_STYLE =
    "OraTableControlBarText";

  // column's CELL_ICON_FORMAT and CELL_ICON_BAND styles
  public static final String AF_TABLE_SELECT_ONE_CELL_ICON_FORMAT_STYLE =
    "af|tableSelectOne::cell-icon-format";
  public static final String AF_TABLE_SELECT_MANY_CELL_ICON_FORMAT_STYLE =
    "af|tableSelectMany::cell-icon-format";
  public static final String AF_COLUMN_CELL_ICON_FORMAT_STYLE =
    "af|column::cell-icon-format";
  public static final String AF_COLUMN_CELL_ICON_BAND_STYLE =
    "af|column::cell-icon-format-band";
  public static final String AF_TABLE_SELECT_ONE_CELL_ICON_BAND_STYLE =
    "af|tableSelectOne::cell-icon-format-band";
  public static final String AF_TABLE_SELECT_MANY_CELL_ICON_BAND_STYLE =
    "af|tableSelectMany::cell-icon-format-band";


  public static final String AF_COLUMN_CELL_NUMBER_STYLE =
    "af|column::cell-number";
  public static final String AF_COLUMN_CELL_NUMBER_BAND_STYLE =
    "af|column::cell-number-band";

  public static final String AF_COLUMN_CELL_TEXT_STYLE =
    "af|column::cell-text";
  public static final String AF_COLUMN_CELL_TEXT_BAND_STYLE =
    "af|column::cell-text-band";


  // column headers
  public static final String AF_COLUMN_HEADER_TEXT_STYLE =
    "af|column::header-text";
  public static final String AF_COLUMN_HEADER_NUMBER_STYLE =
    "af|column::header-number";
  public static final String AF_COLUMN_HEADER_ICON_STYLE =
    "af|column::header-icon-format";
  public static final String AF_COLUMN_GROUP_HEADER_STYLE =
    "af|column::group-header";
  // column component's row header
  public static final String AF_COLUMN_ROW_HEADER_TEXT_STYLE =
    "af|column::row-header-text";

  // jmw @todo Figure out who/when/where uses these styles.
  // found in base.pda, when isSelect is true..
  public static final String TABLE_SELECT_CELL_STYLE =
    "OraTableCellSelect";
  public static final String TABLE_BAND_SELECT_CELL_STYLE =
    "OraTableCellSelectBand";

  public static final String AF_TABLE_COLUMN_FOOTER_STYLE =
    "af|table::column-footer";
  public static final String AF_COLUMN_TOTAL_TEXT_STYLE =
    "af|column::total-text";
  public static final String AF_COLUMN_TOTAL_NUMBER_STYLE =
    "af|column::total-number";

  public static final String AF_TABLE_DETAIL_STYLE =
    "af|table::detail";

  // jmw used in TotalRow renderer, but when is this called? @todo
  public static final String TABLE_TOTAL_STYLE =
    "OraTableTotal";

  // TreeTable style classes
  public static final String AF_TREE_TABLE_EXPANSION_ICON_STYLE_CLASS =
    "af|treeTable::expansion";
  public static final String AF_TREE_TABLE_FOCUS_ICON_STYLE_CLASS =
    "af|treeTable::focus";
  public static final String AF_TREE_TABLE_LOCATOR_ICON_STYLE_CLASS =
    "af|treeTable::locator";

  // HGRID private style. Need to review to see if we want to make this
  // customizable.
  public static final String HGRID_LOCATOR_HEADER_STYLE =
    "OraHGridLocatorHeader";
  // These styles are for large record sets, which we do not support in Trinidad
  // yet. When we do, should these be customizable styles? If so, the names
  // will have to change to be like af|treeTable::nav-row-ilink
  public static final String HGRID_NAV_ROW_ILINK_STYLE_CLASS =
    "OraHGridNavRowInactiveLink";
  public static final String HGRID_NAV_ROW_ALINK_STYLE_CLASS =
    "OraHGridNavRowActiveLink";
  public static final String HGRID_NAV_CELL_STYLE_CLASS =
    "OraHGridNavCell";

  // private styles
  public static final String TABLE_BORDER_0001_STYLE =  "OraTableBorder0001";
  public static final String TABLE_BORDER_0010_STYLE =  "OraTableBorder0010";
  public static final String TABLE_BORDER_0011_STYLE =  "OraTableBorder0011";
  public static final String TABLE_BORDER_0100_STYLE =  "OraTableBorder0100";
  public static final String TABLE_BORDER_0101_STYLE =  "OraTableBorder0101";
  public static final String TABLE_BORDER_0110_STYLE =  "OraTableBorder0110";
  public static final String TABLE_BORDER_0111_STYLE =  "OraTableBorder0111";
  public static final String TABLE_BORDER_1000_STYLE =  "OraTableBorder1000";
  public static final String TABLE_BORDER_1001_STYLE =  "OraTableBorder1001";
  public static final String TABLE_BORDER_1010_STYLE =  "OraTableBorder1010";
  public static final String TABLE_BORDER_1011_STYLE =  "OraTableBorder1011";
  public static final String TABLE_BORDER_1100_STYLE =  "OraTableBorder1100";
  public static final String TABLE_BORDER_1101_STYLE =  "OraTableBorder1101";
  public static final String TABLE_BORDER_1110_STYLE =  "OraTableBorder1110";
  public static final String TABLE_BORDER_1111_STYLE =  "OraTableBorder1111";

  // HIDE SHOW STYLES
  public static final String HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS =
                                                "p_OraHideShowDisclosedSymbol";

  // SORTABLE HEADER STYLES
  // private
  public static final String SORTABLE_HEADER_BORDER_STYLE_CLASS =
    "OraSortableHeaderBorder";
 // public
  public static final String AF_COLUMN_SORTABLE_HEADER_STYLE_CLASS =
    "af|column::sortable-header-text";
  public static final String AF_COLUMN_SORTABLE_HEADER_NUMBER_STYLE_CLASS =
    "af|column::sortable-header-number";
  public static final String AF_COLUMN_SORTABLE_HEADER_ICON_STYLE_CLASS =
    "af|column::sortable-header-icon-format";

  // processTrain styles
   public static final String AF_PROCESS_TRAIN_STYLE_CLASS =
     "af|train";
  public static final String AF_PROCESS_TRAIN_ACTIVE_STYLE_CLASS =
    "af|train::step-active";
  public static final String AF_PROCESS_TRAIN_VISITED_STYLE_CLASS  =
    "af|train::step-visited";
  public static final String AF_PROCESS_TRAIN_UNVISITED_STYLE_CLASS =
    "af|train::step-unvisited";
  public static final String AF_PROCESS_TRAIN_LINK_STYLE_CLASS =
    "af|train::link";
  public static final String AF_PROCESS_TRAIN_DISABLED_STYLE_CLASS =
    "af|train::step-disabled";
  public static final String AF_PROCESS_TRAIN_SUB_START_STYLE_CLASS =
    "af|train::sub-start";
  public static final String TRAIN_SUB_RIGHT_STYLE_CLASS =
    "af|train::sub-end";

  // HEADER STYLES (panelHeader and messages header)
  public static final String AF_PANEL_HEADER_STYLE_CLASS =
    "af|panelHeader";
  public static final String AF_MESSAGES_HEADER_STYLE_CLASS =
    "af|messages::header";
  public static final String AF_SHOW_DETAIL_HEADER_STYLE_CLASS =
    "af|showDetailHeader";

  // for errors
  public static final String AF_PANEL_HEADER_ERROR_STYLE_CLASS =
    "af|panelHeader::error";
  public static final String AF_MESSAGES_ERROR_STYLE_CLASS =
    "af|messages::error";

  // NAVIGATION LEVEL STYLES
  public static final String AF_NAVIGATION_LEVEL_STYLE_CLASS =
    "af|navigationPane";
  public static final String AF_NAVIGATION_LEVEL_BAR_STYLE_CLASS =
    "af|navigationPane::bar";
  public static final String AF_NAVIGATION_LEVEL_BAR_ACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::bar::bar-active-disabled";
  public static final String AF_NAVIGATION_LEVEL_BAR_ACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::bar::bar-active-enabled";
  public static final String AF_NAVIGATION_LEVEL_BAR_INACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::bar::bar-inactive-disabled";
  public static final String AF_NAVIGATION_LEVEL_BAR_INACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::bar::bar-inactive-enabled";
  public static final String AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS =
    "af|navigationPane::bar::bar-content";
  public static final String AF_NAVIGATION_LEVEL_BAR_SEPARATOR_STYLE_CLASS =
    "af|navigationPane::bar::bar-separator";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::buttons-active-disabled";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::buttons-active-enabled";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::buttons-inactive-disabled";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::buttons-inactive-enabled";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS =
    "af|navigationPane::buttons-content";
  public static final String AF_NAVIGATION_LEVEL_BUTTONS_SEPARATOR_STYLE_CLASS =
    "af|navigationPane::buttons-separator";
  public static final String AF_NAVIGATION_LEVEL_CHOICE_LABEL_STYLE_CLASS =
    "af|navigationPane::choice-label";
  public static final String AF_NAVIGATION_LEVEL_CHOICE_OPTIONS_STYLE_CLASS =
    "af|navigationPane::choice-options";
  public static final String AF_NAVIGATION_LEVEL_CHOICE_BUTTON_STYLE_CLASS =
    "af|navigationPane::choice-button";
  public static final String AF_NAVIGATION_LEVEL_LIST_ACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::list-active-disabled";
  public static final String AF_NAVIGATION_LEVEL_LIST_ACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::list-active-enabled";
  public static final String AF_NAVIGATION_LEVEL_LIST_INACTIVE_DISABLED_STYLE_CLASS =
    "af|navigationPane::list-inactive-disabled";
  public static final String AF_NAVIGATION_LEVEL_LIST_INACTIVE_ENABLED_STYLE_CLASS =
    "af|navigationPane::list-inactive-enabled";
  public static final String AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS =
    "af|navigationPane::list-content";
  public static final String AF_NAVIGATION_LEVEL_LIST_BULLET_STYLE_CLASS =
    "af|navigationPane::list-bullet";
  public static final String AF_NAVIGATION_LEVEL_TABS_STYLE_CLASS =
    "af|navigationPaneTabs";
  public static final String AF_NAVIGATION_LEVEL_TABS_ACTIVE_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-active";
  public static final String AF_NAVIGATION_LEVEL_TABS_INACTIVE_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-inactive";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-start";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_START_CONTENT_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-start-content";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-end";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_CONTENT_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-end-content";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_END_JOIN_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-end-join";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-mid";
  public static final String AF_NAVIGATION_LEVEL_TABS_BOTTOM_MID_CONTENT_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-bottom-mid-content";
  public static final String AF_NAVIGATION_LEVEL_TABS_END_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-end";
  public static final String AF_NAVIGATION_LEVEL_TABS_END_JOIN_TO_INACTIVE_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-end-join-to-inactive";
  public static final String AF_NAVIGATION_LEVEL_TABS_MID_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-mid";
  public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_ACTIVE_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-start-join-from-active";
  public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_FROM_INACTIVE_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-start-join-from-inactive";
  public static final String AF_NAVIGATION_LEVEL_TABS_START_JOIN_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-start-join";
  public static final String AF_NAVIGATION_LEVEL_TABS_START_STYLE_CLASS =
    "af|navigationPaneTabs::tabs-start";
  public static final String P_AF_DISABLED = "p_AFDisabled";

  // BREADCRUMBS STYLES
  // used in tr:breadCrumbs, tr:treeTable
  public static final String AF_NAVIGATION_PATH_STYLE_CLASS =
    "af|breadCrumbs";
  public static final String AF_NAVIGATION_PATH_STEP_STYLE_CLASS =
    "af|breadCrumbs::step";
  public static final String AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS =
    "af|breadCrumbs::selected-step";
  public static final String AF_TREE_TABLE_MP_STYLE_CLASS =
    "af|treeTable::path";
  public static final String AF_TREE_TABLE_MP_SELECTED_STYLE_CLASS =
    "af|treeTable::path-selected-step";
  public static final String AF_TREE_TABLE_MP_STEP_STYLE_CLASS =
    "af|treeTable::path-step";

  // ERROR/INFO STYLES
  public static final String TIP_TEXT_STYLE_CLASS = "OraTipText";
  public static final String TIP_LABEL_STYLE_CLASS  = "OraTipLabel";

  // NAVIGATION BAR STYLES
  public static final String NAV_BAR_ALINK_STYLE_CLASS =
    "OraNavBarActiveLink";
  public static final String NAV_BAR_ILINK_STYLE_CLASS =
    "OraNavBarInactiveLink";
  public static final String NAV_BAR_VIEW_STYLE_CLASS =
    "OraNavBarViewOnly";

  // the label in on select* fields
  public static final String AF_LABEL_TEXT_STYLE_CLASS =
    "AFLabelText";
  // the style that is on the td of the label of select* fields.
  public static final String AF_LABEL_CELL_STYLE_CLASS =
    "AFLabelCell";
  // the style that is on the td of the content of select* fields.
   public static final String AF_CONTENT_CELL_STYLE_CLASS =
     "AFContentCell";
  public static final String AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS =
    "AFComponentMessageCell";

  // private for now (was private in 2.2).
  // used to show the tips on select* fields
  public static final String INLINE_ERROR_TEXT_STYLE_CLASS =
    "OraInlineErrorText";
  public static final String INLINE_INFO_TEXT_STYLE_CLASS =
    "OraInlineInfoText";

  // STYLED LIST STYLES
  public static final String STYLED_LIST_STYLE_CLASS =
    "OraStyledList";

  // panelTabbed STYLES
  public static final String AF_SHOW_ONE_TAB_STYLE_CLASS =
    "af|panelTabbed::tab";

  public static final String AF_SHOW_ONE_TAB_SELECTED_STYLE_CLASS =
         "af|panelTabbed::tab-selected";

  public static final String AF_SHOW_ONE_TAB_BODY_STYLE_CLASS =
    "af|panelTabbed::body";

  // TREE STYLES
  public static final String TREE_NODE_ADJUST_STYLE_CLASS =
    "p_OraTreeNodeAdjust";
  public static final String TREE_ROW_STYLE_CLASS =
    "p_OraTreeRow";
  public static final String TREE_ROW_SELECTED_STYLE_CLASS =
    "p_OraTreeRowSelected";
  public static final String TREE_ICON_STYLE_CLASS =
    "p_OraTreeIcon";
  public static final String TREE_DISCLOSED_SYMBOL_STYLE_CLASS =
    "p_OraTreeDisclosedSymbol";

  public static final String QUICK_SEARCH_BOX_STYLE_CLASS =
    "p_OraQuickSearchBox";

  public static final String SELECTED_STYLE_CLASS =
    "p_OraSelected";
  // used in oracle.desktop's menuList renderer only.
  public static final String MENU_LIST_TEXT_STYLE_CLASS =
    "p_OraMenuListText";

  public static final String DISABLED_STYLE_CLASS =
    "p_OraDisabled";

  public static final String AF_PANEL_SIDE_BAR_STYLE_CLASS =
    "af|panelSideBar";

  // Calendar styles
  public static final String AF_SELECT_INPUT_DATE_NAV_STYLE_CLASS =
    "af|inputDate::nav";
  public static final String AF_SELECT_INPUT_DATE_TITLE_STYLE_CLASS =
    "af|inputDate::title";
  public static final String AF_SELECT_INPUT_DATE_HEADER_STYLE_CLASS =
    "af|inputDate::header";
  public static final String AF_SELECT_INPUT_DATE_DISABLED_STYLE_CLASS =
    "af|inputDate::disabled";
  public static final String AF_SELECT_INPUT_DATE_ENABLED_STYLE_CLASS =
    "af|inputDate::enabled";
  public static final String AF_SELECT_INPUT_DATE_SELECTED_STYLE_CLASS =
    "af|inputDate::selected";
  public static final String AF_SELECT_INPUT_DATE_CONTENT_STYLE_CLASS =
    "af|inputDate::content";


  // Inline calendar styles
  public static final String AF_CHOOSE_DATE_NAV_STYLE_CLASS =
    "af|chooseDate::nav";
  public static final String AF_CHOOSE_DATE_TITLE_STYLE_CLASS =
    "af|chooseDate::title";
  public static final String AF_CHOOSE_DATE_HEADER_STYLE_CLASS =
    "af|chooseDate::header";
  public static final String AF_CHOOSE_DATE_DISABLED_STYLE_CLASS =
    "af|chooseDate::disabled";
  public static final String AF_CHOOSE_DATE_ENABLED_STYLE_CLASS =
    "af|chooseDate::enabled";
  public static final String AF_CHOOSE_DATE_SELECTED_STYLE_CLASS =
    "af|chooseDate::selected";
  public static final String AF_CHOOSE_DATE_CONTENT_STYLE_CLASS =
    "af|chooseDate::content";




  // ColorPalette styles
  public static final String COLOR_PALETTE_STYLE_CLASS =
    "p_OraColorPalette";
  public static final String COLOR_PALETTE_EMPTY_CELL_STYLE_CLASS =
    "p_OraColorPaletteEmptyCell";

  // ColorField styles
  public static final String COLOR_FIELD_SWATCH_STYLE_CLASS =
    "p_OraColorFieldSwatch";


  public static final String TABLE_FOOTER_TOTAL_STYLE_CLASS =
    "p_OraTableFooterTotal";

  public static final String HIDDEN_LABEL_STYLE_CLASS =
    "p_OraHiddenLabel";

  // tr:panelList style
  public static final String AF_PANEL_LIST_STYLE_CLASS =
    "af|panelList";

  // tr:showManyAccordion styles
  public static final String AF_SHOWMANYACCORDION_CONTAINER_STYLE_CLASS =
    "af|showManyAccordion::container";

  public static final String AF_SHOWMANYACCORDION_HEADER_EXPANDED_STYLE_CLASS =
    "af|showManyAccordion::header-expanded";

  public static final String AF_SHOWMANYACCORDION_HEADER_COLLAPSED_STYLE_CLASS =
    "af|showManyAccordion::header-collapsed";

  public static final String AF_SHOWMANYACCORDION_HEADER_DISABLED_STYLE_CLASS =
    "af|showManyAccordion::header-disabled";

  public static final String AF_SHOWMANYACCORDION_CONTENT_STYLE_CLASS =
    "af|showManyAccordion::content";

  public static final String AF_SHOWMANYACCORDION_TITLE_LINK_STYLE_CLASS =
    "af|showManyAccordion::title-link";

  public static final String AF_SHOWMANYACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
    "af|showManyAccordion::title-disabled-link";

  // tr:showOneAccordion styles
  public static final String AF_SHOWONEACCORDION_CONTAINER_STYLE_CLASS =
    "af|showOneAccordion::container";

  public static final String AF_SHOWONEACCORDION_HEADER_EXPANDED_STYLE_CLASS =
    "af|showOneAccordion::header-expanded";

  public static final String AF_SHOWONEACCORDION_HEADER_COLLAPSED_STYLE_CLASS =
    "af|showOneAccordion::header-collapsed";

  public static final String AF_SHOWONEACCORDION_HEADER_DISABLED_STYLE_CLASS =
    "af|showOneAccordion::header-disabled";

  public static final String AF_SHOWONEACCORDION_CONTENT_STYLE_CLASS =
    "af|showOneAccordion::content";

  public static final String AF_SHOWONEACCORDION_TITLE_LINK_STYLE_CLASS =
    "af|showOneAccordion::title-link";

  public static final String AF_SHOWONEACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
    "af|showOneAccordion::title-disabled-link";

  // tr:panelAccordion styles
  public static final String AF_PANELACCORDION_CONTAINER_STYLE_CLASS =
    "af|panelAccordion::container";

  public static final String AF_PANELACCORDION_HEADER_EXPANDED_STYLE_CLASS =
    "af|panelAccordion::header-expanded";

  public static final String AF_PANELACCORDION_HEADER_COLLAPSED_STYLE_CLASS =
    "af|panelAccordion::header-collapsed";

  public static final String AF_PANELACCORDION_HEADER_DISABLED_STYLE_CLASS =
    "af|panelAccordion::header-disabled";

  public static final String AF_PANELACCORDION_CONTENT_STYLE_CLASS =
    "af|panelAccordion::content";

  public static final String AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS =
    "af|panelAccordion::title-link";

  public static final String AF_PANELACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS =
    "af|panelAccordion::title-disabled-link";


  // =============ICON NAMES================

  // used by MessagePromptRenderer for pda and desktop.
  // (i guess most of the select components use this)
  // for now we can have all components use the same key,
  // but in the future we can have component-level customization
  // can set these to a small icon
  public static final String ERROR_ICON_ALIAS_NAME           =
    "AFErrorIcon";
  public static final String ERROR_ANCHOR_ICON_ALIAS_NAME    =
    "AFErrorAnchorIcon";
  public static final String INFO_ICON_ALIAS_NAME            =
    "AFInfoIcon";
  public static final String INFO_ANCHOR_ICON_ALIAS_NAME     =
    "AFInfoAnchorIcon";
  public static final String WARNING_ICON_ALIAS_NAME         =
    "AFWarningIcon";
  public static final String WARNING_ANCHOR_ICON_ALIAS_NAME  =
    "AFWarningAnchorIcon";
  public static final String CONFIRMATION_ICON_ALIAS_NAME    =
    "AFConfirmationIcon";
  public static final String REQUIRED_ICON_ALIAS_NAME        =
    "AFRequiredIcon";

  // Used by clients to render a quick select in their LOV tables
  public static final String QUICK_SELECT_ICON_NAME          =
    "AFQuickSelectIcon";
  public static final String QUICK_SELECT_DISABLED_ICON_NAME =
    "AFQuickSelectDisabledIcon";

  // Header icons
  // used by MessageBox and Header (messages and panelHeader)
  // =-=jmw (pda header ignores these icons currently, and only renders
  // what is in the icon attribute)

  // =-=jmw not used currently for pda, but keep here instead of moving
  // to base.desktop because I might refactor base.pda's HeaderRenderer
  // to use these icons instead of just ignoring the icons and looking at
  // the icon attribute, which is what it does currently.


  // panelHeader icons
  public static final String AF_PANEL_HEADER_ERROR_ICON_NAME =
    "af|panelHeader::error-icon";
  public static final String AF_PANEL_HEADER_WARNING_ICON_NAME =
    "af|panelHeader::warning-icon";
  public static final String AF_PANEL_HEADER_INFO_ICON_NAME =
    "af|panelHeader::info-icon";
  public static final String AF_PANEL_HEADER_CONFIRMATION_ICON_NAME =
    "af|panelHeader::confirmation-icon";
  public static final String AF_PANEL_HEADER_PROCESSING_ICON_NAME =
    "af|panelHeader::processing-icon";

  // messages header icons
  public static final String AF_MESSAGES_ERROR_ICON_NAME =
   "af|messages::error-icon";
  public static final String AF_MESSAGES_WARNING_ICON_NAME =
   "af|messages::warning-icon";
  public static final String AF_MESSAGES_INFO_ICON_NAME =
   "af|messages::info-icon";
  public static final String AF_MESSAGES_CONFIRMATION_ICON_NAME =
   "af|messages::confirmation-icon";

  // the alias icons for the header icons for panelHeader and messages
  public static final String HEADER_ERROR_ICON_ALIAS_NAME =
    "AFHeaderErrorIcon";
  public static final String HEADER_WARNING_ICON_ALIAS_NAME =
    "AFHeaderWarningIcon";
  public static final String HEADER_INFO_ICON_ALIAS_NAME =
    "AFHeaderInfoIcon";
  public static final String HEADER_CONFIRMATION_ICON_ALIAS_NAME =
    "AFHeaderConfirmationIcon";

  // the alias icon for navigationPath separator icon that is shared
  // by tr:breadCrumbs and tr:treeTable.
  public static final String PATH_SEPARATOR_ICON_ALIAS_NAME =
    "AFPathSeparatorIcon";

  public static final String AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME =
    "af|breadCrumbs::separator-icon";

  public static final String AF_TREE_TABLE_MP_SEPARATOR_ICON_NAME =
    "af|treeTable::separator-icon";

  public static final String AF_PANEL_PAGE_MP_SEPARATOR_ICON_NAME =
    "af|panelPage::separator-icon";
  // this renders a button that launches the modal date picker.
  public static final String AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME =
    "af|inputDate::launch-icon";

  // ProcessIndicator icons
  public static final String AF_PROGRESS_INDICATOR_INDETERMINATE_ICON_NAME =
    "af|progressIndicator::indeterminate-icon";
  public static final String AF_PROGRESS_INDICATOR_ZERO_ICON_NAME =
    "af|progressIndicator::zero-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FIVE_ICON_NAME =
    "af|progressIndicator::five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_TEN_ICON_NAME =
    "af|progressIndicator::ten-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FIFTEEN_ICON_NAME =
    "af|progressIndicator::fifteen-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_TWENTY_ICON_NAME =
    "af|progressIndicator::twenty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_TWENTY_FIVE_ICON_NAME =
    "af|progressIndicator::twenty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_THIRTY_ICON_NAME =
    "af|progressIndicator::thirty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_THIRTY_FIVE_ICON_NAME =
    "af|progressIndicator::thirty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FORTY_ICON_NAME =
    "af|progressIndicator::forty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FORTY_FIVE_ICON_NAME =
    "af|progressIndicator::forty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FIFTY_ICON_NAME =
    "af|progressIndicator::fifty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_FIFTY_FIVE_ICON_NAME =
    "af|progressIndicator::fifty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_SIXTY_ICON_NAME =
    "af|progressIndicator::sixty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_SIXTY_FIVE_ICON_NAME =
    "af|progressIndicator::sixty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_SEVENTY_ICON_NAME =
    "af|progressIndicator::seventy-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_SEVENTY_FIVE_ICON_NAME =
    "af|progressIndicator::seventy-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_EIGHTY_ICON_NAME =
    "af|progressIndicator::eighty-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_EIGHTY_FIVE_ICON_NAME =
    "af|progressIndicator::eighty-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_NINETY_ICON_NAME =
    "af|progressIndicator::ninety-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_NINETY_FIVE_ICON_NAME =
    "af|progressIndicator::ninety-five-percent-icon";
  public static final String AF_PROGRESS_INDICATOR_ONE_HUNDRED_ICON_NAME =
    "af|progressIndicator::one-hundred-percent-icon";


  // ColorField icons
  // (selectInput)
  // Doesn't render in pda. Not supported in pda, that's why.
  // Defined in BaseDesktopSkin and OracleDesktopSkinExtension
  // not sure under what circumstances the code runs that renders this.
  public static final String AF_SELECT_INPUT_COLOR_LAUNCH_ICON_NAME =
    "af|inputColor::launch-icon";

  // (selectInput)
  // Doesn't render in pda. Not supported in pda, that's why.
  // Defined in BaseDesktopSkin and OracleDesktopSkinExtension
  public static final String AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_NAME =
    "af|inputColor::swatch-overlay-icon";


  // checkbox icons
  public static final String AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_CHECKED_ICON_NAME =
    "af|selectBooleanCheckbox::disabled-checked-icon";
  public static final String AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_UNCHECKED_ICON_NAME =
    "af|selectBooleanCheckbox::disabled-unchecked-icon";
  public static final String AF_SELECT_BOOLEAN_CHECKBOX_READONLY_CHECKED_ICON_NAME =
    "af|selectBooleanCheckbox::read-only-checked-icon";
  public static final String AF_SELECT_BOOLEAN_CHECKBOX_READONLY_UNCHECKED_ICON_NAME =
    "af|selectBooleanCheckbox::read-only-unchecked-icon";

  public static final String AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME =
    "af|selectBooleanRadio::disabled-selected-icon";
  public static final String AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME =
    "af|selectBooleanRadio::disabled-unselected-icon";
  public static final String AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME =
    "af|selectBooleanRadio::read-only-selected-icon";
  public static final String AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME =
    "af|selectBooleanRadio::read-only-unselected-icon";


  // ShowDetail icons
  // We have base.desktop and oracle.desktop renderers. pda uses base.desktop
  // =-=jmw @todo Shouldn't we move base.desktop up to base.xhtml package
  // and then we can get rid of pda's renderer?
  public static final String AF_SHOW_DETAIL_DISCLOSED_ICON_NAME =
    "af|showDetail::disclosed-icon";
  public static final String AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME =
    "af|showDetail::undisclosed-icon";

   // showDetailHeader icons
  public static final String AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME =
    "af|showDetailHeader::disclosed-icon";
  public static final String AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME =
    "af|showDetailHeader::undisclosed-icon";

   // navigationTree icons
  public static final String AF_NAVIGATION_TREE_DISCLOSED_ICON_NAME =
    "af|navigationTree::disclosed-icon";
  public static final String AF_NAVIGATION_TREE_UNDISCLOSED_ICON_NAME =
    "af|navigationTree::undisclosed-icon";

  // showDetail for table icons
  public static final String AF_TABLE_SD_DISCLOSED_ICON_NAME =
  "af|table::disclosed-icon";
  public static final String AF_TABLE_SD_UNDISCLOSED_ICON_NAME =
  "af|table::undisclosed-icon";
  // alias icon names that are referenced by table and showDetail.
  public static final String DETAIL_DISCLOSED_ICON_ALIAS_NAME =
    "AFDetailDisclosedIcon";
  public static final String DETAIL_UNDISCLOSED_ICON_ALIAS_NAME =
    "AFDetailUndisclosedIcon";


  // treeTable icons
  // defined in BaseDesktopSkin and OracleDesktopSkinExtension
  // used in base.desktop.table.TreeNodeTableCell
  // pda uses these icons
  public static final String AF_TREE_TABLE_EXPANDED_ICON_NAME =
    "af|treeTable::expanded-icon";
  public static final String AF_TREE_TABLE_COLLAPSED_ICON_NAME =
    "af|treeTable::collapsed-icon";
  public static final String AF_TREE_TABLE_FOCUS_ICON_NAME =
    "af|treeTable::focus-icon";
  public static final String AF_TREE_TABLE_LOCATOR_ICON_NAME =
    "af|treeTable::locator-icon";
  public static final String AF_TREE_TABLE_NAV_DOWN_ICON_NAME =
    "af|treeTable::nav-down-icon";
  public static final String AF_TREE_TABLE_DISABLED_NAV_DOWN_ICON_NAME =
    "af|treeTable::disabled-nav-down-icon";
  public static final String AF_TREE_TABLE_NAV_UP_ICON_NAME =
    "af|treeTable::nav-up-icon";
  public static final String AF_TREE_TABLE_DISABLED_NAV_UP_ICON_NAME =
    "af|treeTable::disabled-nav-up-icon";



  // selectInputText button icon
  // used by base.desktop and base.pda LovButtonRenderer
  // I don't see a button renderer for pda. It uses LovFieldRenderer,
  // and there is no LovFieldRenderer for pda (a comment says it is in dev)
  // , so it picks up base.xhtml's
  // and that renders null for the icon.
  public static final String AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME =
    "af|inputListOfValues::button-icon";

  // SortableHeader Icons (column, sortable=true)
  // used by base.desktop and base.pda SortableHeaderRenderer.
  // icons are defined in OracleDesktopSkinExtension, BaseDesktopSkin,
  // and PdaHtmlSkin
  public static final String AF_COLUMN_SORTED_ASCEND_ICON_NAME =
   "af|column::sort-ascend-icon";
  public static final String AF_COLUMN_SORTED_DESCEND_ICON_NAME =
   "af|column::sort-descend-icon";
  public static final String AF_COLUMN_UNSORTED_ICON_NAME =
   "af|column::unsorted-icon";

  // SelectRangeChoiceBar arrow icons (selectRangeChoiceBar)
  // Defined in OracleDesktopSkinExtension and XhtmlSkin.
  // same icons in OracleDesktopSkinExtension, but null in XhtmlSkin (and
  // thus pda, cuz it really isn't needed)
  // used in base.xhtml.SelectRangeChoiceBar renderer.
  public static final String AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME =
   "af|selectRangeChoiceBar::prev-icon";
  public static final String AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME =
   "af|selectRangeChoiceBar::next-icon";
  public static final String AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME =
   "af|selectRangeChoiceBar::prev-disabled-icon";
  public static final String AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME =
   "af|selectRangeChoiceBar::next-disabled-icon";

  public static final String AF_TABLE_NB_PREV_ICON_NAME =
   "af|table::prev-icon";
  public static final String AF_TABLE_NB_NEXT_ICON_NAME =
   "af|table::next-icon";
  public static final String AF_TABLE_NB_PREV_DISABLED_ICON_NAME =
   "af|table::prev-disabled-icon";
  public static final String AF_TABLE_NB_NEXT_DISABLED_ICON_NAME =
   "af|table::next-disabled-icon";

  public static final String AF_TREE_TABLE_NB_PREV_ICON_NAME =
   "af|treeTable::prev-icon";
  public static final String AF_TREE_TABLE_NB_NEXT_ICON_NAME =
   "af|treeTable::next-icon";
  public static final String AF_TREE_TABLE_NB_PREV_DISABLED_ICON_NAME =
   "af|treeTable::prev-disabled-icon";
  public static final String AF_TREE_TABLE_NB_NEXT_DISABLED_ICON_NAME =
   "af|treeTable::next-disabled-icon";

  // ChooseDate icons (chooseDate)
  // Defined in OracleDesktopSkinExtension (same as SelectRange's) and XhtmlSkin
  // (not the same as SelectRange's which is null)
  public static final String AF_CHOOSE_DATE_PREV_ICON_NAME =
   "af|chooseDate::prev-icon";
  public static final String AF_CHOOSE_DATE_NEXT_ICON_NAME =
   "af|chooseDate::next-icon";
  public static final String AF_CHOOSE_DATE_PREV_DISABLED_ICON_NAME =
   "af|chooseDate::prev-disabled-icon";
  public static final String AF_CHOOSE_DATE_NEXT_DISABLED_ICON_NAME =
   "af|chooseDate::next-disabled-icon";

  public static final String AF_SELECT_INPUT_DATE_PREV_ICON_NAME =
   "af|inputDate::prev-icon";
  public static final String AF_SELECT_INPUT_DATE_NEXT_ICON_NAME =
   "af|inputDate::next-icon";
  public static final String AF_SELECT_INPUT_DATE_PREV_DISABLED_ICON_NAME =
   "af|inputDate::prev-disabled-icon";
  public static final String AF_SELECT_INPUT_DATE_NEXT_DISABLED_ICON_NAME =
   "af|inputDate::next-disabled-icon";

  // tr:inputNumberSpinbox constants: increment/decrement icons&cells
  public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_ICON_NAME =
    "af|inputNumberSpinbox::increment-icon";
  public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_ICON_NAME =
    "af|inputNumberSpinbox::decrement-icon";
  public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_DISABLED_ICON_NAME =
    "af|inputNumberSpinbox::increment-disabled-icon";
  public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_DISABLED_ICON_NAME =
    "af|inputNumberSpinbox::decrement-disabled-icon";
  public static final String AF_INPUT_NUMBER_SPINBOX_INCREMENT_CELL =
    "af|inputNumberSpinbox::increment-cell";
  public static final String AF_INPUT_NUMBER_SPINBOX_DECREMENT_CELL =
    "af|inputNumberSpinbox::decrement-cell";

  /**
   * Constant string appended to various IDs to make another related
   * but unique (he typed hopefully) ID.
   *
   * For composite element, we need to be able to address the "most important"
   * piece, but also the element as a whole. For example, on the lovInput, we
   * need to address and update the textInput field, but also the lovInput (if
   * the disabled field is databound and updated in response to a PPR event, we
   * need to update both the textInput, and the search Icon).
   *
   * Adding this suffix to the client supplied ID is supposed to give us a
   * still-unique, but related ID.
   */

  // 'xc' stands for uiX Composite
  public static final String COMPOSITE_ID_EXTENSION = "__xc_";
}
