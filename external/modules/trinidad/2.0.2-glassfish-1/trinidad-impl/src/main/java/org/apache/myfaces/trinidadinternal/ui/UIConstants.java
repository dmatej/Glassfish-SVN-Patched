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
package org.apache.myfaces.trinidadinternal.ui;

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

/**
 * Attribute Constants common to many renderers of UINodes.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/UIConstants.java#0 $) $Date: 10-nov-2005.18:50:23 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface UIConstants
{
  /**
   * Namespace used by the marlin implementation.
   */
  public static final String MARLIN_NAMESPACE =
    "http://myfaces.apache.org/uix/ui";


  //
  // Attributes
  //
  // NB: These name-value pairs MUST follow the following relationship:
  //   1) Start with the string constant.
  //   2) Before every capital letter, add an underscore
  //   3) Capitalize every letter.
  //   4) Append _ATTR.
  // Any failure to follow these rules will break UIX.
  //

  /** TYPE: String */
  public static final AttributeKey ID_ATTR           = new AttributeKey("id", 0);
  public static final AttributeKey NAME_ATTR         = new AttributeKey("name", 1);
  public static final AttributeKey INLINE_STYLE_ATTR = new AttributeKey("inlineStyle", 2);
  public static final AttributeKey STYLE_CLASS_ATTR  = new AttributeKey("styleClass", 3);
  public static final AttributeKey DIRECTION_ATTR    = new AttributeKey("direction", 4);
  public static final AttributeKey LANGUAGE_ATTR     = new AttributeKey("language", 5);
  public static final AttributeKey H_ALIGN_ATTR      = new AttributeKey("hAlign");
  public static final AttributeKey V_ALIGN_ATTR      = new AttributeKey("vAlign");
  public static final AttributeKey ORIENTATION_ATTR  = new AttributeKey("orientation");
  public static final AttributeKey POSITION_ATTR     = new AttributeKey("position");
  public static final AttributeKey TITLE_ATTR        = new AttributeKey("title");
  public static final AttributeKey CATEGORY_TITLE_ATTR = new AttributeKey("categoryTitle");
  public static final AttributeKey ITEM_TITLE_ATTR   = new AttributeKey("itemTitle");
  public static final AttributeKey HEADERS_ATTR      = new AttributeKey("headers");
  public static final AttributeKey PATTERN_ATTR      = new AttributeKey("pattern");
  public static final AttributeKey PATTERNS_ATTR      = new AttributeKey("patterns");
  public static final AttributeKey EVENT_ATTR       = new AttributeKey("event");
  public static final AttributeKey STATE_CHECK_ATTR       = new AttributeKey("stateCheck");


  //actually TYPE: TreeProxy
  public static final AttributeKey PROXY_ATTR        = new AttributeKey("proxy");

  /** TYPE: either Integer or String */
  public static final AttributeKey VALUE_ATTR         = new AttributeKey("value");
  public static final AttributeKey HEIGHT_ATTR        = new AttributeKey("height");
  public static final AttributeKey WIDTH_ATTR         = new AttributeKey("width");
  public static final AttributeKey MINIMUM_WIDTH_ATTR = new AttributeKey("minimumWidth");
  public static final AttributeKey LABEL_WIDTH_ATTR   = new AttributeKey("labelWidth");
  public static final AttributeKey FIELD_WIDTH_ATTR   = new AttributeKey("fieldWidth");
  public static final AttributeKey MARGIN_WIDTH_ATTR  = new AttributeKey("marginWidth");
  public static final AttributeKey MARGIN_HEIGHT_ATTR = new AttributeKey("marginHeight");

  /** TYPE: URL or String */
  public static final AttributeKey IMAGE_ATTR          = new AttributeKey("image");
  public static final AttributeKey SOURCE_ATTR         = new AttributeKey("source", 6);
  public static final AttributeKey NAMED_SOURCE_ATTR   = new AttributeKey("namedSource");
  public static final AttributeKey DESTINATION_ATTR    = new AttributeKey("destination", 7);
  public static final AttributeKey TARGET_FRAME_ATTR   = new AttributeKey("targetFrame", 8);
  public static final AttributeKey ICON_ATTR           = new AttributeKey("icon");
  public static final AttributeKey LONG_DESC_URL_ATTR  = new AttributeKey("longDescURL");

  /** TYPE: Boolean */
  public static final AttributeKey EXPAND_ALL_ENABLED_ATTR = new AttributeKey("expandAllEnabled");
  public static final AttributeKey SIMPLE_ATTR = new AttributeKey("simple", 43);
  public static final AttributeKey ESCAPE_ATTR = new AttributeKey("escape", 44);
  public static final AttributeKey REORDER_ONLY_ATTR = new AttributeKey("reorderOnly");
  public static final AttributeKey BORDER_ATTR = new AttributeKey("border");

  public static final AttributeKey ALLOWS_TRANSPARENT_ATTR = new AttributeKey("allowsTransparent");
  public static final AttributeKey DISABLED_ATTR  = new AttributeKey("disabled", 9);
  public static final AttributeKey SELECTED_ATTR = new AttributeKey("selected", 10);
  public static final AttributeKey CHECKED_ATTR = new AttributeKey("checked", 11);
  public static final AttributeKey MULTIPLE_ATTR = new AttributeKey("multiple");
  public static final AttributeKey SECRET_ATTR = new AttributeKey("secret");
  public static final AttributeKey WRAPPING_DISABLED_ATTR = new AttributeKey("wrappingDisabled");
  public static final AttributeKey READ_ONLY_ATTR  = new AttributeKey("readOnly", 12);
  public static final AttributeKey RENDERED_ATTR  = new AttributeKey("rendered", 13);
  public static final AttributeKey DISCLOSE_NONE_ATTR  = new AttributeKey("discloseNone");
  public static final AttributeKey DISCLOSE_MANY_ATTR  = new AttributeKey("discloseMany");
  public static final AttributeKey DISCLOSED_ATTR  = new AttributeKey("disclosed");
  public static final AttributeKey USES_UPLOAD_ATTR  = new AttributeKey("usesUpload");
  public static final AttributeKey DEFAULT_COMMAND_ATTR  = new AttributeKey("defaultCommand");
  public static final AttributeKey FORM_SUBMITTED_ATTR = new AttributeKey("formSubmitted");
  public static final AttributeKey NAME_TRANSFORMED_ATTR = new AttributeKey("nameTransformed");
  public static final AttributeKey PROXIED_ATTR = new AttributeKey("proxied");
  public static final AttributeKey REORDERABLE_ATTR = new AttributeKey("reorderable");
  public static final AttributeKey AUTOMATIC_ATTR = new AttributeKey("automatic");
  public static final AttributeKey UNVALIDATED_ATTR = new AttributeKey("unvalidated");
  public static final AttributeKey THREADED_ATTR = new AttributeKey("threaded");
  public static final AttributeKey DEFAULT_CONTENTS_ATTR = new AttributeKey("defaultContents");
  public static final AttributeKey AUTOSTART_ATTR =
                                      new AttributeKey("autostart");
  public static final AttributeKey AUTOFLIP_ATTR =
                                      new AttributeKey("autoflip");
  public static final AttributeKey GENERATES_CONTENT_ATTR =
                                      new AttributeKey("generatesContent");
  public static final AttributeKey ALL_DETAILS_ENABLED_ATTR  = new AttributeKey("allDetailsEnabled");
  public static final AttributeKey DIRTY_ATTR =
                                      new AttributeKey("dirty");
  public static final AttributeKey COMPACT_ATTR = new AttributeKey("compact");
  public static final AttributeKey USE_SEPARATE_ROWS_ATTR = new AttributeKey("useSeparateRows");
  public static final AttributeKey HEADER_ATTR    = new AttributeKey("header");
  public static final AttributeKey HEADER_TEXT_ATTR    = new AttributeKey("headerText");
  public static final AttributeKey NO_AUTO_COMPLETE_ATTR = new AttributeKey("noAutoComplete");
  public static final AttributeKey GLOBAL_ONLY_ATTR = new AttributeKey("globalOnly");

  /** TYPE: Character */
  public static final AttributeKey ACCESS_KEY_ATTR = new AttributeKey("accessKey", 14);

  /** TYPE: Integer */
  public static final AttributeKey SHORT_DESC_ATTR = new AttributeKey("shortDesc", 15);
  public static final AttributeKey LONG_DESC_ATTR  = new AttributeKey("longDesc");
  public static final AttributeKey SIZE_ATTR = new AttributeKey("size");
  public static final AttributeKey ROWS_ATTR = new AttributeKey("rows");
  public static final AttributeKey COLUMNS_ATTR = new AttributeKey("columns");
  public static final AttributeKey MAX_COLUMNS_ATTR = new AttributeKey("maxColumns");
  public static final AttributeKey CELL_SPACING_ATTR = new AttributeKey("cellSpacing");
  public static final AttributeKey CELL_PADDING_ATTR = new AttributeKey("cellPadding");
  public static final AttributeKey COLUMN_SPAN_ATTR = new AttributeKey("columnSpan");
  public static final AttributeKey ROW_SPAN_ATTR = new AttributeKey("rowSpan");
  public static final AttributeKey MAXIMUM_LENGTH_ATTR = new AttributeKey("maximumLength");
  public static final AttributeKey SELECTED_INDEX_ATTR = new AttributeKey("selectedIndex");
  public static final AttributeKey MIN_VALUE_ATTR = new AttributeKey("minValue");
  public static final AttributeKey MAX_VALUE_ATTR = new AttributeKey("maxValue");
  public static final AttributeKey MAX_VISITED_ATTR = new AttributeKey("maxVisited");
  public static final AttributeKey BLOCK_SIZE_ATTR = new AttributeKey("blockSize");
  public static final AttributeKey SCROLLED_VALUE_ATTR = new AttributeKey("scrolledValue");
  public static final AttributeKey BORDER_WIDTH_ATTR = new AttributeKey("borderWidth");
  public static final AttributeKey PLAY_COUNT_ATTR = new AttributeKey("playCount");
  public static final AttributeKey INNER_HEIGHT_ATTR = new AttributeKey("innerHeight");
  public static final AttributeKey INNER_WIDTH_ATTR = new AttributeKey("innerWidth");
  public static final AttributeKey CHILD_BLOCK_SIZE_ATTR = new AttributeKey("childBlockSize");
  public static final AttributeKey VALID_ATTR = new AttributeKey("valid");
  public static final AttributeKey SHOW_WINDOW_ATTR = new AttributeKey("showWindow");
  public static final AttributeKey INTERVAL_ATTR = new AttributeKey("interval");
  public static final AttributeKey PERCENT_COMPLETE_ATTR = new AttributeKey("percentComplete");
  public static final AttributeKey STEPS_COMPLETE_ATTR = new AttributeKey("stepsComplete");
  public static final AttributeKey TRUNCATE_AT_ATTR = new AttributeKey("truncateAt");
  public static final AttributeKey CURRENT_INDEX_ATTR = new AttributeKey("currentIndex");
  public static final AttributeKey START_DEPTH_ATTR = new AttributeKey("startDepth");
  public static final AttributeKey START_LEVEL_ATTR = new AttributeKey("startLevel");
  public static final AttributeKey LEVEL_ATTR = new AttributeKey("level");


  /** @deprecated use CHILD_TYPE_TEXT string instead */
  @Deprecated
  public static final AttributeKey CHILD_TYPE_TEXT_ATTR = new AttributeKey("childTypeText");

  /** Type: String */
  public static final AttributeKey WRAP_ATTR  = new AttributeKey("wrap");
  public static final AttributeKey METHOD_ATTR  = new AttributeKey("method");
  public static final AttributeKey LABELED_NODE_ID_ATTR  = new AttributeKey("labeledNodeId", 16);
  public static final AttributeKey SELECTED_VALUE_ATTR  = new AttributeKey("selectedValue");
  public static final AttributeKey ANCHOR_ATTR = new AttributeKey("anchor", 17);
  public static final AttributeKey IMAGE_MAP_TYPE_ATTR = new AttributeKey("imageMapType");
  public static final AttributeKey PALETTE_ID_ATTR  = new AttributeKey("paletteId");
  public static final AttributeKey PICKER_ID_ATTR  = new AttributeKey("pickerId");


  /** TYPE: String */
  public static final AttributeKey ON_CLICK_ATTR        = new AttributeKey("onClick", 18);
  public static final AttributeKey ON_LOAD_ATTR         = new AttributeKey("onLoad");
  public static final AttributeKey ON_DOUBLE_CLICK_ATTR = new AttributeKey("onDoubleClick", 19);
  public static final AttributeKey ON_MOUSE_DOWN_ATTR   = new AttributeKey("onMouseDown", 20);
  public static final AttributeKey ON_MOUSE_UP_ATTR     = new AttributeKey("onMouseUp", 21);
  public static final AttributeKey ON_MOUSE_OVER_ATTR   = new AttributeKey("onMouseOver", 22);
  public static final AttributeKey ON_MOUSE_MOVE_ATTR   = new AttributeKey("onMouseMove", 23);
  public static final AttributeKey ON_MOUSE_OUT_ATTR    = new AttributeKey("onMouseOut", 24);
  public static final AttributeKey ON_KEY_PRESS_ATTR    = new AttributeKey("onKeyPress", 25);
  public static final AttributeKey ON_KEY_DOWN_ATTR     = new AttributeKey("onKeyDown", 26);
  public static final AttributeKey ON_KEY_UP_ATTR       = new AttributeKey("onKeyUp", 27);
  public static final AttributeKey ON_FOCUS_ATTR        = new AttributeKey("onFocus", 28);
  public static final AttributeKey ON_BLUR_ATTR         = new AttributeKey("onBlur", 29);
  public static final AttributeKey ON_CHANGE_ATTR       = new AttributeKey("onChange", 30);
  public static final AttributeKey ON_SELECT_ATTR       = new AttributeKey("onSelect");
  public static final AttributeKey ON_UNLOAD_ATTR       = new AttributeKey("onUnload");
  public static final AttributeKey ON_SUBMIT_ATTR       = new AttributeKey("onSubmit");
  public static final AttributeKey ON_SWITCH_APP_ATTR   = new AttributeKey("onSwitchApp");
  public static final AttributeKey ON_LOV_INIT_ATTR     = new AttributeKey("onLovInit");
  public static final AttributeKey ON_LOV_SELECT_ATTR   = new AttributeKey("onLovSelect");
  public static final AttributeKey ON_LOV_VALIDATE_ATTR = new AttributeKey("onLovValidate");
  public static final AttributeKey SUBMIT_PREPARE_ATTR = new AttributeKey("submitPrepare");
  public static final AttributeKey ON_NAVIGATE_ATTR = new AttributeKey("onNavigate");
  public static final AttributeKey ON_COLOR_SELECT_ATTR = new AttributeKey("onColorSelect");

  public static final AttributeKey VALIDATE_BLANKS_ATTR = new AttributeKey("validateBlanks");
  public static final AttributeKey DEFAULT_CASE_ATTR   = new AttributeKey("defaultCase");



  /** TYPE: DataSet */
  public static final AttributeKey COLOR_DATA_ATTR = new AttributeKey("colorData");
  public static final AttributeKey CUSTOM_COLOR_DATA_ATTR = new AttributeKey("customColorData");
  public static final AttributeKey COLUMN_HEADER_DATA_ATTR
                                                 = new AttributeKey("columnHeaderData");
  public static final AttributeKey ROW_HEADER_DATA_ATTR
                                                 = new AttributeKey("rowHeaderData");
  public static final AttributeKey COLUMN_FORMAT_ATTR = new AttributeKey("columnFormat");
  public static final AttributeKey ROW_FORMATS_ATTR    = new AttributeKey("rowFormats");
  public static final AttributeKey COLUMN_HEADER_FORMAT_ATTR
                                                 = new AttributeKey("columnHeaderFormat");
  public static final AttributeKey ROW_HEADER_FORMATS_ATTR
                                                 = new AttributeKey("rowHeaderFormats");
  public static final AttributeKey SELECTION_ATTR      = new AttributeKey("selection");
  public static final AttributeKey CHILD_DATA_ATTR     = new AttributeKey("childData", 31);
  public static final AttributeKey NODES_ATTR          = new AttributeKey("nodes");

  /** TYPE: DataObject */
  public static final AttributeKey CURRENT_DATA_ATTR   = new AttributeKey("currentData");
  public static final AttributeKey FORM_DATA_ATTR      = new AttributeKey("formData");

  /** TYPE: String */
  public static final AttributeKey TEXT_ATTR           = new AttributeKey("text", 32);
  public static final AttributeKey ABBREVIATION_ATTR   = new AttributeKey("abbreviation");
  public static final AttributeKey PRE_TEXT_ATTR       = new AttributeKey("preText");
  public static final AttributeKey POST_TEXT_ATTR      = new AttributeKey("postText");
  public static final AttributeKey BETWEEN_TEXT_ATTR   = new AttributeKey("betweenText");
  public static final AttributeKey DISCLOSED_TEXT_ATTR     = new AttributeKey("disclosedText");
  public static final AttributeKey UNDISCLOSED_TEXT_ATTR   = new AttributeKey("undisclosedText");
  public static final AttributeKey FORM_NAME_ATTR      = new AttributeKey("formName");
  public static final AttributeKey LABEL_ATTR         = new AttributeKey("label", 33);
  public static final AttributeKey MESSAGE_ATTR        = new AttributeKey("message", 34);
  public static final AttributeKey MESSAGE_TYPE_ATTR   = new AttributeKey("messageType", 35);
  public static final AttributeKey TIP_ATTR            = new AttributeKey("tip", 36);
  public static final AttributeKey CHILD_NAME_ATTR     = new AttributeKey("childName");
  public static final AttributeKey SORTABLE_ATTR       = new AttributeKey("sortable");
  public static final AttributeKey TYPE_ATTR           = new AttributeKey("type");
  public static final AttributeKey TYPE_TEXT_ATTR      = new AttributeKey("typeText");
  public static final AttributeKey BACKGROUND_ATTR      = new AttributeKey("background");
  public static final AttributeKey LEADING_HEADER_ATTR = new AttributeKey("leadingHeader");
  public static final AttributeKey TRAILING_HEADER_ATTR = new AttributeKey("trailingHeader");
  public static final AttributeKey SHORT_TEXT_ATTR      = new AttributeKey("shortText");
  public static final AttributeKey DATA_NAMESPACE_ATTR  = new AttributeKey("dataNamespace");
  public static final AttributeKey DATA_NAME_ATTR       = new AttributeKey("dataName");
  public static final AttributeKey EMPTY_TEXT_ATTR  = new AttributeKey("emptyText");
  public static final AttributeKey SUMMARY_ATTR  = new AttributeKey("summary");
  public static final AttributeKey CONTENT_STYLE_CLASS_ATTR = new AttributeKey("contentStyleClass");
  public static final AttributeKey CONTENT_STYLE_ATTR = new AttributeKey("contentStyle");
  public static final AttributeKey LEADING_DESC_SHOWN_ATTR = new AttributeKey("leadingDescShown");
  public static final AttributeKey TRAILING_DESC_SHOWN_ATTR = new AttributeKey("trailingDescShown");
  public static final AttributeKey LIST_STYLE_ATTR      = new AttributeKey( "listStyle");
  public static final AttributeKey SEARCH_AREA_MODE_ATTR     = new AttributeKey( "searchAreaMode");
  public static final AttributeKey SEARCH_TEXT_ATTR     = new AttributeKey( "searchText");
  public static final AttributeKey TABLE_NAME_ATTR      = new AttributeKey( "tableName");
  public static final AttributeKey VALUE_COLUMNS_ATTR   = new AttributeKey( "valueColumns");
  public static final AttributeKey STANDBY_TEXT_ATTR    = new AttributeKey( "standbyText");
  public static final AttributeKey CONTENT_TYPE_ATTR    = new AttributeKey( "contentType");
  public static final AttributeKey NAVIGATION_FORM_NAME_ATTR      = new AttributeKey("navigationFormName");
  public static final AttributeKey NAVIGATION_EXCLUDE_NAMES_ATTR      = new AttributeKey("navigationExcludeNames");
  public static final AttributeKey SHOW_ALL_ATTR        = new AttributeKey("showAll");
  public static final AttributeKey BREAD_CRUMB_TEXT_ATTR = new AttributeKey("breadCrumbText");
  public static final AttributeKey INITIAL_FOCUS_ID_ATTR  = new AttributeKey("initialFocusId");
  public static final AttributeKey CHROME_TYPE_ATTR     = new AttributeKey("chromeType");
  public static final AttributeKey SEARCH_DESC_ATTR     = new AttributeKey("searchDesc");
  public static final AttributeKey LAYOUT_ATTR = new AttributeKey("layout");

  /** TYPE: Path */
  public static final AttributeKey ANCESTOR_PATH_ATTR   = new AttributeKey("ancestorPath");
  public static final AttributeKey FOCUS_PATH_ATTR      = new AttributeKey("focusPath");

  /** TYPE: Enumerated */
  public static final AttributeKey REQUIRED_ATTR = new AttributeKey("required", 37);
  public static final AttributeKey SHOW_REQUIRED_ATTR = new AttributeKey("showRequired");
  public static final AttributeKey SCROLLING_ATTR = new AttributeKey("scrolling");
  public static final AttributeKey PLAYER_ATTR = new AttributeKey("player");
  public static final AttributeKey CONTROLS_ATTR = new AttributeKey("controls");
  public static final AttributeKey PARTIAL_RENDER_MODE_ATTR =
                                      new AttributeKey("partialRenderMode");
  public static final AttributeKey STYLE_USAGE_ATTR  = new AttributeKey("styleUsage");

  /** TYPE: ServerValidater */
  public static final AttributeKey SERVER_VALIDATER_ATTR  = new AttributeKey("serverValidater");

  /** TYPE: ClientValidater */
  public static final AttributeKey CONVERTER_ATTR    = new AttributeKey("converter");
  public static final AttributeKey VALIDATORS_ATTR    = new AttributeKey("validators");
  public static final AttributeKey ON_BLUR_VALIDATER_ATTR    = new AttributeKey("onBlurValidater", 38);
  public static final AttributeKey ON_SUBMIT_VALIDATER_ATTR  = new AttributeKey("onSubmitValidater", 39);

  /** TYPE: org.apache.myfaces.trinidadinternal.ui.data.DataProvider */
  public static final AttributeKey PROVIDER_ATTR = new AttributeKey("provider");

  /** TYPE: UINode */
  public static final AttributeKey NODE_ATTR     = new AttributeKey("node");

  /** TYPE: All sorts of dictionary-like objects */
  public static final AttributeKey NAME_VALUES_ATTR  = new AttributeKey("nameValues");

  /** TYPE: String[] */
  public static final AttributeKey NAMES_ATTR  =
                                          new AttributeKey("names");
  public static final AttributeKey PARTIAL_TARGETS_ATTR  =
                                          new AttributeKey("partialTargets");


  public static final AttributeKey ANNOTATION_ATTR =
                                          new AttributeKey("annotation");
  public static final AttributeKey MODEL_ATTR =
                                          new AttributeKey("model");

  /** TYPE: ClientAction */
  public static final AttributeKey PRIMARY_CLIENT_ACTION_ATTR =
                                     new AttributeKey("primaryClientAction", 40);
  public static final AttributeKey BUTTON_CLIENT_ACTION_ATTR =
                                     new AttributeKey("buttonClientAction");
  public static final AttributeKey ENTER_CLIENT_ACTION_ATTR =
                                     new AttributeKey("enterClientAction", 41);

  // This AttributeKey is not actually used by an UINode.
  // Instead, it is used in conjunction with Icon.renderIcon().
  // The EMBEDDED_ATTR is used to let the Icon instance know that
  // the Icon is embedded within its own wrapper element - and
  // that it is safe for the Icon to render its style class,
  // inline style and short description on the outer element.
  // This optimization serves two purposes.  First, it reduces
  // content size by collapsing two HTML elements (typically
  // an anchor containing a link) into a single element.
  //
  // The second optimization is that it means that Icons can
  // render their style classes on a containing anchor - and
  // thus the Icon's style class can be used to enable/disable
  // the anchor's text decoration.  This is actually an optmization
  // because if we didn't support this, the Renderer which generates
  // the anchor would itself always have to render the style class
  // on the anchor, even though the style class often isn't
  // necessary when the Icon is image-based.  Now, we can specify
  // the style class on the Icon itself, rather than specify
  // the style class in the Renderer, so we can selectively
  // include a style class for text-based Icons, and leave the
  // style class out when registering image-based Icons.
  public static final AttributeKey EMBEDDED_ATTR =
                                     new AttributeKey("embedded");

  // Allow the first click to go through in certain instances? When a PPR event
  // occurs, we block all user input until it completes. However, there may be
  // instances where the client wants to receive the very first click. For
  // example, If the user has entered text in a textInput with a ClientAction
  // attached, then immediately clicked a submit button, the click would get
  // consumed by the blocking code. This attribute (on the BodyBean), allows
  // the client to force the delivery of the first click.
  public static final AttributeKey FIRST_CLICK_PASSED_ATTR =
                                     new AttributeKey("firstClickPassed");

  /** @deprecated Do not use this attribute */
  @Deprecated
  public static final AttributeKey ANCESTOR_ID_ATTR     = new AttributeKey("ancestorID");


  //
  // Local Names for UINodes
  //
  // NB: These name-value pairs MUST follow the following relationship:
  //   1) Start with the string constant.
  //   2) Before every capital letter, add an underscore
  //   3) Capitalize every letter.
  //   4) Append _NAME
  // Any failure to follow these rules will break UIX.
  //

  public static final String APPLICATION_SWITCHER_NAME = "applicationSwitcher";
  public static final String BODY_NAME = "body";
  public static final String BORDER_LAYOUT_NAME = "borderLayout";
  public static final String BREAD_CRUMBS_NAME = "breadCrumbs";
  public static final String BUTTON_NAME = "button";
  public static final String CELL_FORMAT_NAME = "cellFormat";
  public static final String CHOICE_NAME = "choice";
  public static final String COLOR_BUTTON_NAME = "colorButton";
  public static final String COLOR_FIELD_NAME = "colorField";
  public static final String COLOR_PALETTE_NAME = "colorPalette";
  public static final String COLOR_SWATCH_NAME = "colorSwatch";
  public static final String COLUMN_NAME = "column";
  public static final String COLUMN_GROUP_NAME = "columnGroup";
  public static final String COMMAND_ITEM_NAME = "commandItem";
  public static final String COMMAND_NAVIGATION_ITEM_NAME = "commandNavigationItem";
  public static final String CONTENT_FOOTER_NAME = "contentFooter";
  public static final String CONTEXT_POPPING_NAME = "contextPopping";
  public static final String DATE_BUTTON_NAME = "dateButton";
  public static final String DATE_FIELD_NAME = "dateField";
  public static final String DOCUMENT_NAME = "document";
  public static final String FILE_UPLOAD_NAME = "fileUpload";
  public static final String FLOW_LAYOUT_NAME = "flowLayout";
  public static final String FOOTER_NAME = "footer";
  public static final String FORM_VALUE_NAME = "formValue";
  public static final String FRAME_NAME = "frame";
  public static final String FRAME_BORDER_LAYOUT_NAME = "frameBorderLayout";
  public static final String GLOBAL_BUTTON_NAME = "globalButton";
  public static final String GLOBAL_BUTTON_BAR_NAME = "globalButtonBar";
  public static final String GLOBAL_HEADER_NAME = "globalHeader";
  public static final String HEAD_NAME = "head";
  public static final String HEADER_NAME = "header";
  public static final String HTML_NAME = "html";
  public static final String ICON_NAME  = "icon";
  public static final String ICON_KEY_NAME  = "iconKey";
  public static final String IMAGE_NAME  = "image";
  public static final String INCLUDE_NAME = "include";
  public static final String INSERTED_NODE_LIST_NAME = "insertedNodeList";
  public static final String INLINE_DATE_PICKER_NAME = "inlineDatePicker";
  public static final String LINK_NAME   = "link";
  public static final String MENU_LIST_NAME = "menuList";
  public static final String NAVIGATION_PATH_NAME = "navigationPath";
  public static final String NAVIGATION_TREE_NAME = "navigationTree";
  public static final String MESSAGE_BOX_NAME = "messageBox";
  public static final String NAVIGATION_BAR_NAME = "navigationBar";
  public static final String OPTION_NAME   = "option";
  public static final String PAGE_NAME   = "page";
  public static final String PAGE_HEADER_LAYOUT_NAME = "pageHeaderLayout";
  public static final String PAGE_LAYOUT_NAME = "pageLayout";
  public static final String PAGE_MENU_BAR_NAME = "pageMenuBar";
  public static final String PAGE_MENU_BUTTONS_NAME = "pageMenuButtons";
  public static final String PAGE_MENU_LIST_NAME = "pageMenuList";
  public static final String PAGE_NAVIGATION_PATH_NAME = "pageNavigationPath";
  public static final String PAGE_MENU_TABS_NAME = "pageMenuTabs";
  public static final String PAGE_NAVIGATION_TREE_NAME = "pageNavigationTree";
  public static final String PROCESS_CHOICE_BAR_NAME = "processChoiceBar";
  public static final String PROCESS_TRAIN_NAME = "processTrain";
  public static final String PROCESSING_NAME = "processing";
  public static final String PROCESS_STEPS_NAME = "processSteps";
  public static final String RAW_TEXT_NAME = "rawText";
  public static final String ROW_LAYOUT_NAME = "rowLayout";
  public static final String SCRIPT_NAME = "script";
  public static final String SELECT_OPTION_NAME = "selectOption";
  public static final String SEPARATOR_NAME = "separator";
  public static final String SERVLET_INCLUDE_NAME = "servletInclude";
  public static final String SHOW_ITEM_NAME = "showItem";
  public static final String SHOW_ONE_TAB_NAME = "panelTabbed";
  public static final String SIDE_BAR_NAME = "sideBar";
  public static final String SIDE_NAV_NAME = "sideNav";
  public static final String SINGLE_SELECTION_NAME = "singleSelection";
  public static final String SORTABLE_HEADER_NAME = "sortableHeader";
  public static final String SPACER_NAME = "spacer";
  public static final String STACK_LAYOUT_NAME = "stackLayout";
  public static final String STYLED_TEXT_NAME = "styledText";
  public static final String STYLE_SHEET_NAME = "styleSheet";
  public static final String SUBMIT_BUTTON_NAME = "submitButton";
  public static final String SUB_TAB_BAR_NAME = "subTabBar";
  public static final String SUB_TAB_LAYOUT_NAME = "subTabLayout";
  public static final String SWITCHER_NAME = "switcher";
  public static final String TAB_BAR_NAME = "tabBar";
  public static final String TABLE_FOOTER_NAME = "tableFooter";
  public static final String TABLE_NAME = "table";
  public static final String TABLE_LAYOUT_NAME = "tableLayout";
  public static final String TEXT_NAME = "text";
  public static final String TREE_NAME = "tree";


  //
  //
  // Element names - for elements that don't have matching
  // beans.  It's important that no named children of UINodes be defined
  // with these names.
  //
  public static final String BOUND_ATTRIBUTE_NAME = "boundAttribute";
  public static final String BOUND_MESSAGE_NAME   = "boundMessage";
  public static final String BOUND_TEXT_NAME      = "boundText";
  public static final String CASE_NAME       = "case";
  public static final String CONTENTS_NAME   = "contents";
  public static final String CHILD_LIST_NAME  = "childList";
  public static final String CHILD_MAP_NAME   = "childMap";
  public static final String DEFAULT_NAME         = "default";
  public static final String ATTRIBUTE_MAP_NAME   = "attributeMap";

  //
  // More element names.  These are never added directly to
  // UINodes, so it's safe to have named children and UINode names
  // with these names
  //
  public static final String ADD_NAME         = "add";
  public static final String AGENT_NAME       = "agent";
  public static final String ANCESTOR_ATTRIBUTE_NAME = "ancestorAttribute";
  public static final String AND_NAME         = "and";
  public static final String BEAN_NAME        = "bean";
  public static final String EXPRESSION_LANGUAGE_ATTRIBUTE_NAME =
    "expressionLanguage";
  public static final String BOUND_VALUE_PROVIDER_NAME = "boundValueProvider";
  public static final String BUNDLE_NAME      = "bundle";
  public static final String BYTE_LENGTH_NAME = "byteLength";
  public static final String COLOR_NAME       = "color";
  public static final String COMPARISON_NAME  = "comparison";
  public static final String CONTEXT_PROPERTY_NAME = "contextProperty";
  public static final String CONCAT_NAME      = "concat";
  public static final String CONVERT_NAME     = "convert";
  public static final String DATA_NAME        = "data";
  public static final String DATA_OBJECT_NAME = "dataObject";
  public static final String DATE_NAME        = "date";
  public static final String DECIMAL_NAME     = "decimal";
  public static final String DEFAULTING_NAME  = "defaulting";
  public static final String ENCODED_PARAMETER_NAME = "encodedParameter";
  public static final String FIXED_NAME       = "fixed";
  public static final String FORMAT_NAME      = "format";
  public static final String IF_NAME          = "if";
  public static final String INLINE_NAME      = "inline";
  public static final String INSTANCE_NAME    = "instance";
  public static final String MAILTO_NAME      = "mailto";
  public static final String METHOD_NAME      = "method";
  public static final String MESSAGE_FORMAT_NAME = "messageFormat";
  public static final String NOT_NAME         = "not";
  public static final String NULL_NAME        = "null";
  public static final String OR_NAME          = "or";
  public static final String PALETTE_NAME     = "palette";
  public static final String PARSE_NAME       = "parse";
  public static final String REG_EXP_NAME     = "regExp";
  public static final String SAMPLE_NAME      = "sample";
  public static final String UTF8_LENGTH_NAME = "utf8Length";
  public static final String WML_NAME         = "wml";
  public static final String TEMPLATES_NAME   = "templates";
  public static final String TEMPLATE_DEFINITION_NAME = "templateDefinition";
  public static final String TEMPLATE_IMPORT_NAME     = "templateImport";
  public static final String TEMPLATE_LIBRARY_NAME    = "templateLibrary";
  public static final String TRUNCATE_NAME            = "truncate";
  public static final String ROOT_ATTRIBUTE_MAP_NAME = "rootAttributeMap";

  //
  // Named Children
  //
  public static final String SEPARATOR_CHILD     = "separator";
  public static final String COLUMN_HEADER_STAMP_CHILD = "columnHeaderStamp";
  public static final String HEADER_CHILD = "header";
  public static final String META_CONTAINER_CHILD  = "metaContainer";
  public static final String ROW_HEADER_STAMP_CHILD = "rowHeaderStamp";
  public static final String COLUMN_FOOTER_CHILD = "columnFooter";
  public static final String SELECTION_CHILD = "selection";
  public static final String TABLE_FILTER_CHILD  = "tableFilter";
  public static final String TOTAL_CHILD          = "total";
  public static final String NODE_STAMP_CHILD    = "nodeStamp";
  public static final String PATH_STAMP_CHILD    = "pathStamp";
  public static final String FILTER_CHILD         = "filter";


  // pageLayout children
  public static final String ACTIONS_CHILD        = "actions";
  public static final String APP_PRIVACY_CHILD    = "appPrivacy";
  public static final String APP_COPYRIGHT_CHILD  = "appCopyright";
  public static final String APP_ABOUT_CHILD      = "appAbout";
  public static final String BRANDING_CHILD       = "branding";
  public static final String BRANDING_APP_CHILD   = "brandingApp";
  public static final String BRANDING_APP_CONTEXTUAL_CHILD = "brandingAppContextual";
  public static final String CONTEXT_SWITCHER_CHILD  = "contextSwitcher";
  public static final String INFO_STATUS_CHILD    = "infoStatus";
  public static final String INFO_USER_CHILD      = "infoUser";
  public static final String INFO_FOOTNOTE_CHILD  = "infoFootnote";
  public static final String INFO_RETURN_CHILD    = "infoReturn";
  public static final String INFO_SUPPLEMENTAL_CHILD = "infoSupplemental";
  public static final String LOCATION_CHILD       = "location";
  public static final String NAVIGATION_GLOBAL_CHILD    = "navigationGlobal";
  public static final String MENU_SWITCH_CHILD    = "menuSwitch";
  public static final String NAVIGATION1_CHILD    = "navigation1";
  public static final String NAVIGATION2_CHILD    = "navigation2";
  public static final String NAVIGATION3_CHILD    = "navigation3";
  public static final String MESSAGES_CHILD       = "messages";
  public static final String SEARCH_CHILD         = "search";


  // these are not available in 3.0
  public static final String ADVERTISEMENT_LARGE_CHILD = "advertisementLarge";
  public static final String ADVERTISEMENT_MEDIUM_CHILD     = "advertisementMedium";
  public static final String BRANDING_COOPERATIVE_CHILD     = "brandingCooperative";

  public static final String CENTER_CHILD         = "center";
  public static final String TOP_CHILD            = "top";
  public static final String INNER_TOP_CHILD      = "innerTop";
  public static final String BOTTOM_CHILD         = "bottom";
  public static final String INNER_BOTTOM_CHILD   = "innerBottom";
  public static final String LEFT_CHILD           = "left";
  public static final String INNER_LEFT_CHILD     = "innerLeft";
  public static final String RIGHT_CHILD          = "right";
  public static final String INNER_RIGHT_CHILD    = "innerRight";
  public static final String START_CHILD          = "start";
  public static final String INNER_START_CHILD    = "innerStart";
  public static final String END_CHILD            = "end";
  public static final String INNER_END_CHILD      = "innerEnd";
  public static final String LEADING_CHILD        = "leading";
  public static final String TRAILING_CHILD       = "trailing";
  public static final String LEADING_FOOTER_CHILD = "leadingFooter";
  public static final String TRAILING_FOOTER_CHILD = "trailingFooter";
  public static final String CONTENT_FOOTER_CHILD = "contentFooter";
  public static final String CONTENT_FORM_CHILD   = "contentForm";
  public static final String ITEMS_CHILD          = "items";
  public static final String CATEGORIES_CHILD     = "categories";
  public static final String CONTENT_LINK_CHILD   = "contentLink";
  public static final String DETAIL_STAMP_CHILD   = "detailStamp";
  public static final String ALTERNATE_CONTENT_CHILD = "alternateContent";
  public static final String CATCH_CHILD             = "catch";
  public static final String SUB_TABS_CHILD          = "subTabs";
  public static final String PROMPT_CHILD            = "prompt";
  public static final String LABEL_CHILD             = "label";
  public static final String FOOTER_CHILD          = "footer";

  // from the listOfValues Template
  public static final String FILTER_CHOICE_CHILD       = "filterChoice";
  public static final String ADVANCED_CHOICES_CHILD    = "advancedChoices";
  public static final String HEADER_INSTRUCTIONS_CHILD = "headerInstructions";
  public static final String SEARCH_INSTRUCTIONS_CHILD = "searchInstructions";

  // from the processingLayout Template
  public static final String CONCISE_MESSAGE_CHILD   = "conciseMessage";
  public static final String DETAILED_MESSAGE_CHILD  = "detailedMessage";

  //
  // Facets
  //

  /**
   * The default facet;  if no facet is set, or the requested
   * facet is not available, this facet will be used.
   */
  public static final String FACET_DEFAULT   = CoreRenderKit.OUTPUT_MODE_DEFAULT;

  /**
   * A printable facet;  when supported, this facet should
   * result in a version of a page designed for printouts.
   */
  public static final String FACET_PRINTABLE = CoreRenderKit.OUTPUT_MODE_PRINTABLE;

  /**
   * A portlet facet;  when supported, this facet should
   * result in a version of page content optimized for use in portlets.
   */
  public static final String FACET_PORTLET   = CoreRenderKit.OUTPUT_MODE_PORTLET;


  /**
   * An e-mail facet;  when supported, this facet should
   * result in a version of page content optimized for use in e-mail.
   */
  public static final String FACET_EMAIL   = CoreRenderKit.OUTPUT_MODE_EMAIL;


  //
  // Enumerated Values
  //

  // Horizontal alignment
  /**
   * Horizontal alignment constant for centering
   */
  public static final String H_ALIGN_CENTER = "center";

  /**
   * Horizontal alignment constant for left alignment.
   */
  public static final String H_ALIGN_LEFT = "left";

  /**
   * Horizontal alignment constant for right alignment.
   */
  public static final String H_ALIGN_RIGHT = "right";

  /**
   * Horizontal alignment constant for start alignment;  left
   * alignment will be used in left-to-right languages, right
   * alignment in right-to-left languages.
   */
  public static final String H_ALIGN_START = "start";

  /**
   * Horizontal alignment constant for end alignment;  right
   * alignment will be used in left-to-right languages, left
   * alignment in right-to-left languages.
   */
  public static final String H_ALIGN_END = "end";

  // Vertical alignment
  /**
   * Vertical alignment constant for centering.
   */
  public static final String V_ALIGN_MIDDLE = "middle";

  /**
   * Vertical alignment constant for centering in I.E.
   * (works better than "middle").
   */
  public static final String V_ALIGN_ABSMIDDLE = "absmiddle";

  /**
   * Vertical alignment constant for top alignment.
   */
  public static final String V_ALIGN_TOP = "top";

  /**
   * Vertical alignment constant for bottom alignment.
   */
  public static final String V_ALIGN_BOTTOM = "bottom";

  // TABLE_BANDING_ATTR
  public static final String ROW_BANDING    = "row";
  public static final String COLUMN_BANDING = "column";
  public static final String NO_BANDING     = "none";

  // WRAP_ATTR
  public static final String SOFT_WRAP = "soft";
  public static final String HARD_WRAP = "hard";

  // BLOCK_SIZE_ATTR
  public static final int SINGLE_STEP = -1;

  // MAX_VALUE_ATTR
  public static final int MAX_VALUE_UNKNOWN = -1;

  // TABLE DATA TYPE FORMATTING
  public static final String OBJECT_NAME_FORMAT = "objectName";
  public static final String TEXT_FORMAT = "text";
  public static final String NUMBER_FORMAT = "number";
  public static final String ICON_BUTTON_FORMAT = "icon";

  // REQUIRED_ATTR
  /**
   * Validation succeeds if the field is empty, OR the Validater succeeds.
   * This is the default value.
   */
  public static final String REQUIRED_NO = "no";

  /**
   * Validation succeeds if the field is non-empty AND the Validater succeeds.
   */
  public static final String REQUIRED_YES = "yes";

  /**
   * Validation always succeeds.
   */
  public static final String REQUIRED_UI_ONLY = "uiOnly";

  // SCROLLING_ATTR
  /**
   * Frame Scroll Bars are always rendered.
   */
  public static final String SCROLLING_YES  = "yes";

  /**
   * Frame Scroll Bars are never rendered.
   */
  public static final String SCROLLING_NO   = "no";

  /**
   * Frame Scroll Bars are rendered at browser discretion.
   */
  public static final String SCROLLING_AUTO = "auto";

  // PLAYER_ATTR
  /**
   * Use a link to play the audi or video clip
   */
  public static final String PLAYER_LINK = "link";

  /**
   * Use the Quicktime Player to play the audi or video clip
   */
  public static final String PLAYER_QUICKTIME = "quicktime";

  /**
   * Use the Windows Media Player to play the audi or video clip
   */
  public static final String PLAYER_WINDOWS = "windows";

  /**
   * Use the Real Player to play the audi or video clip
   */
  public static final String PLAYER_REAL = "real";

  // CONTROLS_ATTR
  /**
   * Don't show any controls for the media player and don't allow control
   * access through alternate means, such as context menus
   */
  public static final String CONTROLS_NONE ="none";

  /**
   * Don't show any controls for the media player but allow control access
   * through alternate means, such as context menus
   */
  public static final String CONTROLS_NONE_VISIBLE ="noneVisible";

  /**
   * Show a minimal set of controls for playing media on the media player
   */
  public static final String CONTROLS_MINIMAL ="minimal";

  /**
   * Show the typical set of controls for playing media on the media player
   */
  public static final String CONTROLS_TYPICAL ="typical";

  /**
   * Show all available controls for playing media on the media player
   */
  public static final String CONTROLS_ALL ="all";


  // MESSAGE_TYPE_ATTR
  /**
   * Message type if the associated message is informational.
   */
  public static final String MESSAGE_TYPE_INFO = "info";

  /**
   * Message type if the associated message is a warning.
   */
  public static final String MESSAGE_TYPE_WARNING = "warning";

  /**
   * Message type if the associated message is an error.
   */
  public static final String MESSAGE_TYPE_ERROR = "error";

  /**
   * Message type if the associated message is a confirmation.
   */
  public static final String MESSAGE_TYPE_CONFIRMATION = "confirmation";

  /**
   * Message type if the page layout messageType is processing.
   */
  public static final String MESSAGE_TYPE_PROCESSING = "processing";
  /**
   * Message type if there is no type of message.
   */
  public static final String MESSAGE_TYPE_NONE = "none";

  // SORTABLE_ATTR
  /**
   * Sortable value if the table column should not be sortable.
   */
  public static final String SORTABLE_NO = "no";

  /**
   * Sortable type if the table column should be sortable, but is
   * not currently sorted.
   */
  public static final String SORTABLE_YES = "yes";

  /**
   * Sortable value if the table column is sortable and is currently
   * sorted with ascending values down the column.
   */
  public static final String SORTABLE_ASCENDING = "ascending";

  /**
   * Sortable value if the table column is sortable and is currently
   * sorted with descending values down the column.
   */
  public static final String SORTABLE_DESCENDING = "descending";

  /**
   * The default text used for a secret field.
   * When working with password fields, compare the value submitted
   * with this value. If they are the same, then the user did not modify
   * the field.
   */
  public static final String SECRET_FIELD_DEFAULT_VALUE = "******";

  // IMAGE_MAP_TYPE_ATTR
  /**
  * Image map type if the desired image map is server-side.
  */
  public static final String IMAGE_MAP_TYPE_SERVER = "server";

  /**
  * Image map type if no image map is desired.
  */
  public static final String IMAGE_MAP_TYPE_NONE = "none";

  // ICON_ATTR
  /**
   * Constant for the "required" icon
   */
  public static final String ICON_REQUIRED = "required";

  // TYPE_ATTR
  /**
   * Comparison type value to compare for equals.
   */
  public static final String COMPARISON_TYPE_EQUALS = "equals";

  /**
   * Comparison type value to compare for not equals.
   */
  public static final String COMPARISON_TYPE_NOT_EQUALS = "notEquals";

  /**
   * Comparison type value to compare for greater.
   */
  public static final String COMPARISON_TYPE_GREATER_THAN = "greaterThan";

  /**
   * Comparison type value to compare for greater or equals.
   */
  public static final String COMPARISON_TYPE_GREATER_THAN_OR_EQUALS = "greaterThanOrEquals";

  /**
   * Comparison type value to compare for less.
   */
  public static final String COMPARISON_TYPE_LESS_THAN = "lessThan";

  /**
   * Comparison type value to compare for less or equals.
   */
  public static final String COMPARISON_TYPE_LESS_THAN_OR_EQUALS = "lessThanOrEquals";

  // BACKGROUND_ATTR

  /**
   * Value to display a transparent content container background
   */
  public final static String BACKGROUND_TRANSPARENT="transparent";

  /**
   * Light value to display the content container with a light scheme:
   */
  public final static String BACKGROUND_LIGHT="light";

  /**
   * Medium value to display the content container with a medium scheme:
   */
  public final static String BACKGROUND_MEDIUM="medium";

  /**
   * Dark value to display the content container with a dark scheme:
   */
  public final static String BACKGROUND_DARK="dark";

  // EXPANDABLE_KEY
  /**
   * Value to indicate that a tree node is not expandable, i.e. is a leaf.
   */
  public final static String EXPANDABLE_NO="no";

  /**
   * Value to indicate that a tree node is expandable and currently open.
   */
  public final static String EXPANDABLE_EXPANDED="expanded";

  /**
   * Value to indicate that a tree node is expandable and currently closed.
   */
  public final static String EXPANDABLE_COLLAPSED="collapsed";

  /**
   * Value to indicate that a tree node is expanded and cannot be collapsed:
   */
  public final static String EXPANDABLE_ALWAYS="always";

  // BANDING_SHADE_KEY

  /**
   * Value to indicate that the table should use a light banding shade.
   */
  public static final String BANDING_SHADE_LIGHT = "light";

  /**
   * Value to indicate that the table should use a dark banding shade.
   */
  public static final String BANDING_SHADE_DARK  = "dark";

  // ORIENTATION_ATTR
  /**
   * Value to indicate orientation is horizontal (currently used by BreadCrumbs)
   */
  public final static String ORIENTATION_HORIZONTAL="horizontal";

  /**
   * Value to indicate orientation is vertical (currently used by BreadCrumbs)
   */
  public final static String ORIENTATION_VERTICAL="vertical";

  /**
   * Value to indicate orientation is top (currently used by SubTabBar)
   */
  public final static String ORIENTATION_TOP="top";

  /**
   * Value to indicate orientation is bottom (currently used by SubTabBar)
   */
  public final static String ORIENTATION_BOTTOM="bottom";

  /**
   * Value to indicate orientation is default (currently used by SubTabBar)
   */
  public final static String ORIENTATION_DEFAULT="default";

  /**
   * Value of POSITION_ATTR
   * Indicates that the top tabbar is to be rendered
   */
  public final static String POSITION_ABOVE="above";

  /**
   * Value of POSITION_ATTR
   * Indicates that the bottom tabbar is to be rendered
   */
  public final static String POSITION_BELOW="below";

  /**
   * Value of POSITION_ATTR
   * Indicates that both bottom and top tabbars are to be rendered
   */
  public final static String POSITION_BOTH="both";

  /**
   * Value to indicate list style is disc (currently used by StyledList)
   */
  public final static String LIST_STYLE_DISC="disc";

  /**
   * Value to indicate list style is circle (currently used by StyledList)
   */
  public final static String LIST_STYLE_CIRCLE="circle";

  /**
   * Value to indicate list style is square (currently used by StyledList)
   */
  public final static String LIST_STYLE_SQUARE="square";

  /**
   * Value to indicate list style is decimal (currently used by StyledList)
   */
  public final static String LIST_STYLE_DECIMAL="decimal";

  /**
   * Value to indicate list style is upper-alpha(currently used by StyledList)
   */
  public final static String LIST_STYLE_UPPER_ALPHA="upperAlpha";

  /**
   * Value to indicate list style is lower-alpha (currently used by StyledList)
   */
  public final static String LIST_STYLE_LOWER_ALPHA="lowerAlpha";

  /**
   * Value to indicate list style is none (currently used by StyledList)
   */
  public final static String LIST_STYLE_NONE ="none";

  // STYLE_USAGE_ATTR of formattedText
  /**
   * This sets the style needed for the pageStamp region
   */
  public static final String PAGE_STAMP_STYLE = "pageStamp";
  /**
   * This sets the style needed for the instruction text
   */
  public static final String INSTRUCTION_STYLE   = "instruction";
  /**
   * This sets the style needed for the inContextBranding
   */
  public static final String IN_CONTEXT_BRANDING_STYLE = "inContextBranding";

  /**
   * PARTIAL_RENDER_MODE value used to indicate that no partial page
   * rendering should be performed.
   */
  public final static String PARTIAL_RENDER_MODE_NONE = "none";

  /**
   * PARTIAL_RENDER_MODE value used to indicate that partial page
   * rendering should be used to refresh this node.
   */
  public final static String PARTIAL_RENDER_MODE_SELF = "self";

  /**
   * Processing attribute percentComplete's UNKNOWN constant
   */
  public final static int PERCENT_UNKNOWN = -1;
  //
  // Data Select Keys
  //

  /** TYPE: Boolean */
  public static final String DISPLAY_GRID_KEY          = "displayGrid"; // rowformat
  public static final String COLUMN_DATA_FORMAT_KEY    = "columnDataFormat";
  public static final String CELL_NO_WRAP_FORMAT_KEY   = "cellNoWrapFormat";
  public static final String SELECTED_KEY              = "selected";
  public static final String DISCLOSED_KEY             = "disclosed";

  /** TYPE: String */
  public static final String TEXT_KEY                  = "text";
  public static final String ICON_KEY                  = "icon";
  public static final String ICON_SHORT_DESC_KEY       = "iconShortDesc";
  public static final String DESTINATION_KEY           = "destination";
  public static final String TARGET_FRAME_KEY          = "targetFrame";
  public static final String ON_CLICK_KEY              = "onClick";
  public static final String EXPAND_DESTINATION_KEY    = "expandDestination";
  public static final String COLLAPSE_DESTINATION_KEY  = "collapseDestination";
  public static final String WIDTH_KEY                 = "width";
  public static final String DESCRIPTION_KEY           = "description";
  public static final String DESTINATION_TEXT_KEY      = "destinationText";

  public static final String EXPANDABLE_KEY            = "expandable";
  public static final String SELECT_MODE_KEY           = "selectMode";


  /** TYPE: DataSet */
  public static final String NODES_KEY                 = "nodes";

  /** TYPE: Object */
  public static final String DATA_KEY                  = "data";


  //
  // Context Property Names
  //

  /**
   * String rendering property for the name of the current form bean.
   */
  public static final String FORM_NAME_PROPERTY   = "formName";


  /**
   * Key used to store the initialFocus on the RenderingContext
   * under the UIConstants.MARLIN_NAMESPACE. initialFocus is an attribute
   * of body and is an id indicating the component
   * which should have the initial focus.
   */
  public static final Object INITIAL_FOCUS_CONTEXT_PROPERTY = "initialFocus";


  //
  // parameter names
  //
  public static final String EVENT_PARAM     = "event";
  public static final String SOURCE_PARAM    = "source";
  public static final String TYPE_PARAM      = "type";
  public static final String LOCATION_PARAM  = "location";
  public static final String NODE_PARAM      = "node";
  public static final String STATE_PARAM     = "state";
  public static final String SELECTION_PARAM = "selection";
  public static final String ROOT_PARAM      = "root";
  public static final String SIZE_PARAM      = "size";
  public static final String VALUE_PARAM     = "value";
  public static final String PARTIAL_TARGETS_PARAM = "partialTargets";
  public static final String PARTIAL_PARAM   = "partial";
  public static final String URI_PARAM      = "uri";

  //
  // parameter values
  //
  // calendar, mobile dateField params
  public static final String TOK_PARAM            = "tok";
  public static final String CONVERT_PARAM        = "convert";
  public static final String JSP_PARAM            = "jsp";
  public static final String ENC_PARAM            = "enc";
  public static final String CONFIG_NAME_PARAM    = "configName";
  public static final String CONTEXT_URI_PARAM    = "contextURI";
  public static final String MIN_VALUE_PARAM      = "minValue";
  public static final String MAX_VALUE_PARAM      = "maxValue";
  public static final String LOC_PARAM            = "loc";
  public static final String SCROLLED_VALUE_PARAM = "scrolledValue";
  public static final String MONTH_PARAM          = "month";
  public static final String YEAR_PARAM           = "year";
  public static final String PATTERN_PARAM        = "ptn";
  public static final String DATE_STYLE_PARAM     = "dStl";


  // Mobile dateField and LovField
  // "picker" events and types
  public static final String LOV_EVENT           = "lov";
  public static final String DATE_EVENT          = "date";
  public static final String TYPE_PRE            = "pre";
  public static final String TYPE_POST           = "post";
  public static final String CANCEL_EVENT        = "cancel";

  // AppSwitcher
  public static final String SWITCH_APP_EVENT       = "switchApp";
  public static final String SWITCH_APP_TYPE_GOTO   = "goto";

  /** @deprecated The application switcher no longer has a back button */
  @Deprecated
  public static final String SWITCH_APP_TYPE_BACK   = "back";

  // HideShow
  public static final String HIDE_EVENT             = "hide";
  public static final String SHOW_EVENT             = "show";

  // HGrid/Tree
  public static final String EXPAND_EVENT           = "expand";

  // HGrid
  public static final String FOCUS_EVENT            = "focus";
  public static final String EXPAND_ALL_EVENT       = "expandAll";
  public static final String COLLAPSE_ALL_EVENT     = "collapseAll";
  public static final String NEXT_EVENT             = "next";
  public static final String PREVIOUS_EVENT         = "previous";
  public static final String CHILD_TYPE_TEXT        = "childTypeText";
  public static final int    INCOMPLETE_DATA_SET    = -1;

  // TotalRow
  public static final String UPDATE_EVENT            = "update";

  // AddTableRow
  public static final String ADD_ROWS_EVENT         = "addRows";

  // NavigationBar
  public static final String GOTO_EVENT             = "goto";

  // SortableHeader
  public static final String SORT_EVENT             = "sort";
  public static final String SORT_STATE_ASCENDING   = SORTABLE_ASCENDING;
  public static final String SORT_STATE_DESCENDING  = SORTABLE_DESCENDING;

  // LOV Template
  public static final String LOV_FILTER_EVENT       = "lovFilter";
  public static final String LOV_SEARCH_TEXT        = "searchText";


  // Navigate event
  public static final String NAVIGATE_EVENT         = "navigate";


  /**
   * Constant for the value of the "value" event parameter when
   * the user is asking to see all rows.
   */
  public static final String VALUE_SHOW_ALL         = "all";

  //
  // Constants for SHOW_ALL_ATTR
  //
  public static final String SHOW_ALL_YES        = "yes";
  public static final String SHOW_ALL_NO         = "no";
  public static final String SHOW_ALL_ACTIVE     = "active";

  //
  // Node roles
  //

  /**
   * Role for nodes that play a structural role - that is, layouts.
   */
  public static final NodeRole STRUCTURAL_ROLE = new NodeRole("structural", 0);

  /**
   * Role for nodes that are invisible to the end user.
   */
  public static final NodeRole USER_INVISIBLE_ROLE = new NodeRole("userInvisible", 1);

  /**
   * Role for nodes that change rendering state.
   */
  public static final NodeRole STATE_ROLE     = new NodeRole("state", 2);

  /**
   * Role for nodes that are composite widgets.
   */
  public static final NodeRole COMPOSITE_ROLE = new NodeRole("composite", 3);

  /**
   * Role for nodes that can't be identified.
   */
  public static final NodeRole UNKNOWN_ROLE =
    new NodeRole("unknown", new NodeRole[]{STRUCTURAL_ROLE});


  /**
   * Constants for the LAYOUT_ATTR of panelGroup
   */
  public static final String DEFAULT_LAYOUT = "default";
  public static final String HORIZONTAL_LAYOUT = "horizontal";
  public static final String VERTICAL_LAYOUT= "vertical";

  /**
   * @deprecated This constant is for dev purposes only
   */
  @Deprecated
  public static final AttributeKey DESCRIPTION_ATTR =
    new AttributeKey("description", 42);


  /**
   * @deprecated This constant is for dev purposes only
   */
  @Deprecated
  public static final AttributeKey SELECT_ATTR = new AttributeKey("select");

  /**
   * @deprecated This constant is for dev purposes only
   */
  @Deprecated
  public static final String PARTIAL_ROOT_NAME = "partialRoot";

}
