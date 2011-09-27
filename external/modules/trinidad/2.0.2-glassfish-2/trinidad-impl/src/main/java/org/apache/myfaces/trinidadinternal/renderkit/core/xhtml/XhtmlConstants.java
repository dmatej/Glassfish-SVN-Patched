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

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

import java.util.Arrays;
import java.util.List;

public final class XhtmlConstants
{
  private XhtmlConstants(){}
  
  /**
   * A portlet facet;  when supported, this facet should
   * result in a version of page content optimized for use in portlets.
   */
  public static final String FACET_PORTLET = CoreRenderKit.OUTPUT_MODE_PORTLET;
  
  public static final String SCRIPT_NAME = "script";

  // Input components
  public static final String AUTOSUBMIT_EVENT = "autosub";

  // Mobile dateField and LovField
  // "picker" events and types
  public static final String DATE_EVENT   = "date";
  public static final String CANCEL_EVENT = "cancel";
  public static final String TYPE_POST    = "post";

  // NavigationBar
  public static final String GOTO_EVENT = "goto";

  // HideShow
  public static final String HIDE_EVENT = "hide";
  public static final String SHOW_EVENT = "show";

  // HGrid
  public static final int INCOMPLETE_DATA_SET = -1;
  
  // SortableHeader
  public static final String SORT_EVENT = "sort";

  // Poll
  public static final String POLL_EVENT = "poll";

  // Chart Drill down
  public static final String CHART_DRILL_DOWN_EVENT = "chartDrillDown";

  /**
   * Constant for the value of the "value" event parameter when
   * the user is asking to see all rows.
   */
  public static final String VALUE_SHOW_ALL = "all";

  
  
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
  public static final String PARTIAL_PARAM         = "partial";
  public static final String PARTIAL_TARGETS_PARAM = "partialTargets";
  public static final String SIZE_PARAM            = "size";
  public static final String SOURCE_PARAM          = "source";
  public static final String STATE_PARAM           = "state";
  public static final String TYPE_PARAM            = "type";
  public static final String VALUE_PARAM           = "value";
  public static final String TARGETITEM_PARAM      = "targetItem";


  //
  // parameter values
  //
  // calendar, mobile dateField params
  public static final String EVENT_PARAM          = "event";
  public static final String LOC_PARAM            = "loc";
  public static final String MAX_VALUE_PARAM      = "maxValue";
  public static final String MIN_VALUE_PARAM      = "minValue";
  public static final String MONTH_PARAM          = "month";
  public static final String SCROLLED_VALUE_PARAM = "scrolledValue";
  public static final String YEAR_PARAM           = "year";
  
  
  //
  // Named Children
  //
  public static final String PATH_STAMP_CHILD = "pathStamp";

  
  //
  // Enumerated Values
  //

  // Horizontal alignment
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
   * Vertical alignment constant for top alignment.
   */
  public static final String V_ALIGN_TOP = "top";
  


  // MAX_VALUE_ATTR
  public static final int MAX_VALUE_UNKNOWN = -1;


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
  
  
  // ORIENTATION_ATTR
  /**
   * Value to indicate orientation is vertical (currently used by BreadCrumbs)
   */
  public final static String ORIENTATION_VERTICAL="vertical";
  
  
  public static final String SELECTED_KEY = "selected";
  




  // ===================== End of copy from UIConstants =======================

  public static final String STYLES_CACHE_DIRECTORY = "/adf/styles/cache/";
  public static final String OUTPUT_MODE_PORTLET = FACET_PORTLET;
  
  // ============= Html elements ================
  public static final String DIV_ELEMENT          = "div";
  public static final List<String> HEADER_ELEMENTS =
    Arrays.asList(new String[]{"h1", "h2", "h3",
                               "h4", "h5", "h6"});
  public static final String LINK_ELEMENT         = "a";
  public static final String PARAGRAPH_ELEMENT = "p";
  public static final String SCRIPT_ELEMENT       = "script";
  public static final String SPAN_ELEMENT         = "span";
  public static final String TABLE_DATA_ELEMENT   = "td";
  public static final String TABLE_BODY_ELEMENT   = "tbody";
  public static final String TABLE_ELEMENT        = "table";
  public static final String TABLE_HEADER_ELEMENT = "th";
  public static final String TABLE_ROW_ELEMENT    = "tr";
  
  public static final String FIELDSET_ELEMENT     = "fieldset";
  public static final String LEGEND_ELEMENT       = "legend";
  
  //
  // Copied from BaseDesktopConstants
  //
  public static final String BASE_DESKTOP_ID = "base.desktop";

  //
  // Copied from XhtmlLafConstants
  //
  public static final String APACHE_TRINIDAD_DESKTOP = 
    "org.apache.myfaces.trinidad.desktop";
  public static final String APACHE_TRINIDAD_PDA = 
    "org.apache.myfaces.trinidad.pda";
  public static final String APACHE_TRINIDAD_PORTLET =
    CoreRenderKit.OUTPUT_MODE_PORTLET;
  
  /** Unicode character for non-breaking space */
  public static final char NBSP_CHAR = 0xA0;

  /** String containing Unicode character for non-breaking space */
  public static final String NBSP_STRING = String.valueOf(NBSP_CHAR);
  
  public static final String ALIGN_ATTRIBUTE      = "align";
  public static final String COLS_ATTRIBUTE       = "cols";
  public static final String COLSPAN_ATTRIBUTE    = "colspan";
  public static final String HEIGHT_ATTRIBUTE     = "height";
  public static final String HREF_ATTRIBUTE       = "href";
  public static final String ID_ATTRIBUTE         = "id";
  public static final String NOWRAP_ATTRIBUTE     = "nowrap";
  public static final String ONCLICK_ATTRIBUTE    = "onclick";
  public static final String ROWS_ATTRIBUTE      = "rows";
  public static final String ROWSPAN_ATTRIBUTE    = "rowspan";
  public static final String SIZE_ATTRIBUTE       = "size";
  public static final String STYLE_ATTRIBUTE      = "style";
  public static final String VALIGN_ATTRIBUTE     = "valign";
  public static final String WIDTH_ATTRIBUTE      = "width";
  
  public static final String DIR_ATTRIBUTE_VALUE                 = "dir";
  public static final String EMPTY_STRING_ATTRIBUTE_VALUE        = "";
  public static final String LEFT_ATTRIBUTE_VALUE                = "left";
  public static final String MIDDLE_ATTRIBUTE_VALUE              = "middle";
  public static final String ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE = "100%";
  public static final String RIGHT_ATTRIBUTE_VALUE               = "right";

  
  public static final String COLOR_PALETTE_TRANSPARENT_ICON_NAME = "cpt.gif";
  
  // 'xc' stands for uiX Composite
  // -= Simon =-
  // FIXME: Should it be renamed to remove UIX reference?
  public static final String COMPOSITE_ID_EXTENSION = "__xc_";
  
  // context property to indicate that form elements are repeated and the
  // data needs to be kept in sync
  public static final Object REPEAT_PROPERTY = new Object();


  //Constants for Non JavaScript browser support
  public static final String NO_JS_PARAMETER_KEY = "_parameterkey";
  public static final String NO_JS_INPUT_IMAGE_KEY = "_inputImagekey";
  public static final String MULTIPLE_VALUE_PARAM = "multipleValueParam";
  public static final String NO_JS_PARAMETER_KEY_BUTTON = "Go";
  public static final String NO_JS_PARAMETER_BACK_BUTTON = "Back";
  public static final String NO_JS_PARAMETER_NEXT_BUTTON = "Next";
  
  public static final String NON_JS_BROWSER = "_noJavaScript";
  public static final String NON_JS_BROWSER_TRUE = "true";
  public static final String NON_JS_DETAIL_DISCLOSED_ICON = "-";
  public static final String NON_JS_DETAIL_UNDISCLOSED_ICON = "+";
  public static final String NON_JS_ASC_ICON = "v";
  public static final String NON_JS_DESC_ICON = "^";
  
  // Maximum width of a narrow-screen PDA device in pixels
  public static final int NARROW_SCREEN_PDA_MAX_WIDTH = 240; 
  
  // The name of the hidden parameter that stores the value of the 
  // request-header, UA-pixels. This hidden paramter is rendered only 
  // for windows mobile.
  public static final String WINDOWS_MOBILE_UAPIXELS = "uapixels";
     
}
