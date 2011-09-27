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

import java.util.HashMap;
import java.util.Map;


import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.BaseDesktopSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;

/**
 * Skin implementation for simple portlet
 */
public class SimplePortletSkin  extends BaseDesktopSkin
{
  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  @Override
  public String getStyleSheetName()
  {
    return "META-INF/adf/styles/simple-portlet.css";
  }

  /**
   * Returns the id for the portlet implementation of the Simple
   * Skin: "simple.portlet".
   */
  @Override
  public String getId()
  {
    return "simple.portlet";
  }

  /**
   * Returns the family for the portlet implementation of the Simple
   * Skin: "simple.portlet".
   */
  @Override
  public String getFamily()
  {
    return "simple";
  }

  /**
   * Returns the renderKitId for the portlet implementation of the Simple
   * Skin.
   */
  @Override
  public String getRenderKitId()
  {
    return CoreRenderKit.OUTPUT_MODE_PORTLET;
  }


 /**
  * Returns the style class map, or null if there is no map.
  * The style class map for Portlet skins is a full style class name TO portlet style class name
  * map. It does not compress style classes.
  * @param arc RenderingContext
  * @return Map&lt;String, String&gt; It should be a map that contains the full style class name as
  * the key, and the value is a portlet style class.
  */
  @Override
  public Map<String, String> getStyleClassMap(
    RenderingContext arc
    )
  {
    return _STYLES_MAP;
  }


  private static final Map<String, String> _STYLES_MAP = new HashMap<String, String>(350);
  private static final String _NULL_STYLE = "";
  private static final String _PORTLET_FONT = "portlet-font";
  private static final String _PORTLET_FONT_DIM = "portlet-font-dim";

  // all these map to portlet-form-label
  private static final String[] _FORM_LABEL_SELECTORS = new String[]
  {
    SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS,
    "af|outputLabel",
    "af|inputChoice::label",
    "af|inputColor::label",
    "af|inputDate::label",
    "af|inputFile::label",
    "af|inputListOfValues::label",
    "af|inputNumberSpinbox::label",
    "af|inputText::label",
    "af|panelLabelAndMessage::label",
    "af|selectBooleanCheckbox::label",
    "af|selectBooleanRadio::label",
    "af|selectManyCheckbox::label",
    "af|selectManyListbox::label",
    "af|selectOneChoice::label",
    "af|selectOneListbox::label",
    "af|selectOneRadio::label",
    SkinSelectors.AF_MENU_CHOICE_LABEL_STYLE_CLASS,
    SkinSelectors.SHUTTLE_HEADER_STYLE_CLASS,
  };

  // all these map to portlet-form-input-field
  private static final String[] _FORM_INPUT_SELECTORS = new String[]
  {
    "af|inputChoice::content",
    "af|inputFile::content",
    "af|inputNumberSpinbox::content",
    "af|inputText::content",
    "af|inputColor::content",
    SkinSelectors.AF_SELECT_INPUT_DATE_CONTENT_STYLE_CLASS,
    "af|inputListOfValues::content",
    "af|panelLabelAndMessage::content",
  };

  // all these map to portlet-form-field - Text for a field (not input field. e.g., checkboxes)
  private static final String[] _FORM_FIELD_SELECTORS = new String[]
  {
    "af|selectBooleanCheckbox::content",
    "af|selectBooleanRadio::content",
    "af|selectManyCheckbox::content",
    "af|selectManyChoice::content",
    "af|selectManyListbox::content",
    "af|selectManyShuttle::content",
    "af|selectOneChoice::content",
    "af|selectOneListbox::content",
    "af|selectOneRadio::content",
    "af|selectOrderShuttle::content"
  };

  // portlet-form-button
  private static final String[] _PORTLET_FORM_BUTTON = new String[]
  {
    SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS,
    SkinSelectors.AF_GO_BUTTON_STYLE_CLASS,
    SkinSelectors.AF_RESET_BUTTON_STYLE_CLASS,
    SkinSelectors.AF_NAVIGATION_LEVEL_CHOICE_BUTTON_STYLE_CLASS
  };


  // all these map to portlet-table-text
  // portlet-table-text: Any text in a table cell
  private static final String[] _TABLE_TEXT_SELECTORS = new String[]
  {
    SkinSelectors.AF_TABLE_CONTROL_BAR_TOP_STYLE,
    SkinSelectors.AF_TREE_TABLE_CONTROL_BAR_TOP_STYLE,
    SkinSelectors.AF_TABLE_CONTROL_BAR_BOTTOM_STYLE,
    SkinSelectors.AF_TREE_TABLE_CONTROL_BAR_BOTTOM_STYLE,
    SkinSelectors.AF_TABLE_SUB_CONTROL_BAR_STYLE,
    SkinSelectors.AF_TREE_TABLE_SUB_CONTROL_BAR_STYLE,
    "OraHGridLocatorHeader",
    SkinSelectors.AF_MENU_TABS_STYLE_CLASS,
    SkinSelectors.AF_MENU_BAR_STYLE_CLASS,
    SkinSelectors.AF_PANEL_SIDE_BAR_STYLE_CLASS,
  };

  // These map to 'portlet-table-body' + itself (add portlet-table-body to the style class )
  private static final String[] _TABLE_BODY_PLUS_ITSELF = new String[]
  {
    SkinSelectors.AF_COLUMN_CELL_NUMBER_STYLE,
    SkinSelectors.AF_COLUMN_CELL_ICON_FORMAT_STYLE,
    SkinSelectors.AF_TABLE_SELECT_ONE_CELL_ICON_FORMAT_STYLE,
    SkinSelectors.AF_TABLE_SELECT_MANY_CELL_ICON_FORMAT_STYLE,
    SkinSelectors.TABLE_SELECT_CELL_STYLE,
  };

  // These map to 'portlet-table-alternate' + itself (add portlet-table-alternate to the style class )
  private static final String[] _TABLE_ALT_PLUS_ITSELF = new String[]
  {
    SkinSelectors.AF_COLUMN_CELL_ICON_BAND_STYLE,
    SkinSelectors.AF_TABLE_SELECT_ONE_CELL_ICON_BAND_STYLE,
    SkinSelectors.AF_TABLE_SELECT_MANY_CELL_ICON_BAND_STYLE,
    SkinSelectors.AF_COLUMN_CELL_NUMBER_BAND_STYLE,
    SkinSelectors.TABLE_BAND_SELECT_CELL_STYLE,
  };

  // These map to 'portlet-table-header' + itself (add portlet-table-header to the style class )
  private static final String[] _TABLE_HDR_PLUS_ITSELF = new String[]
  {
      SkinSelectors.AF_COLUMN_HEADER_NUMBER_STYLE,
      SkinSelectors.AF_COLUMN_HEADER_ICON_STYLE,
      SkinSelectors.AF_COLUMN_ROW_HEADER_TEXT_STYLE,
  };



  static
  {
    // some styles I map to portlet styles + the original style class
    // The reason I do that is because most of the original style
    // class properties are nulled out (like font properties), but they
    // have padding and borders and text-alignment that is needed to
    // make the component look decent.
    // I blank out the parts I don't want in porlet-desktop.xss


    // Begin form control mappings
    // ---------------------------
    // portlet-form-label -   Text used for the descriptive label
    //                        of the whole form not the label for fields
    // portlet-form-input-field - Text of the user-input in an input field.
    // portlet-form-field-label - Text for a separator of fields (e.g., checkboxes)
    // portlet-form-field       - Text for a field (not input field. e.g., checkboxes)
    // Currently UNUSED
    // portlet-form-button (text on a button)
    // portlet-icon-label (Text that appears beside a context depended action icon)
    // portlet-dlg-icon-label (Text that appears beside a 'standard' icon (OK, Cancel))

    // TODO: put some style class on the <input> for checkboxes so I can
    // map it to portlet-form-field????
    // TODO: I use portlet-font for the text in a choice. Is this right?


    for (int i=0; i < _FORM_LABEL_SELECTORS.length; i++)
      _STYLES_MAP.put(_FORM_LABEL_SELECTORS[i], "portlet-form-label");

    for (int i=0; i < _FORM_INPUT_SELECTORS.length; i++)
      _STYLES_MAP.put(_FORM_INPUT_SELECTORS[i], "portlet-form-input-field");

    for (int i=0; i < _FORM_FIELD_SELECTORS.length; i++)
      _STYLES_MAP.put(_FORM_FIELD_SELECTORS[i], "portlet-form-field");


    for (int i=0; i < _PORTLET_FORM_BUTTON.length; i++)
      _STYLES_MAP.put(_PORTLET_FORM_BUTTON[i], "portlet-form-button");

    _STYLES_MAP.put(SkinSelectors.AF_PANEL_FORM_LABEL_CELL_STYLE_CLASS, _NULL_STYLE);

    // End form control mappings
    // -------------------------

    // Begin message mappings:
    // portlet-msg-error, portlet-msg-alert, portlet-msg-info, portlet-msg-status
    // ------------------------- //
    _STYLES_MAP.put(SkinSelectors.INLINE_ERROR_TEXT_STYLE_CLASS, "portlet-msg-error");
    _STYLES_MAP.put(SkinSelectors.INLINE_INFO_TEXT_STYLE_CLASS, "portlet-msg-info");
    _STYLES_MAP.put(SkinSelectors.PROCESS_STATUS_STYLE_CLASS, "portlet-msg-status");
    _STYLES_MAP.put(SkinSelectors.TIP_TEXT_STYLE_CLASS, "portlet-msg-info");
    _STYLES_MAP.put(SkinSelectors.TIP_LABEL_STYLE_CLASS, "portlet-msg-info");
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS,"portlet-msg-error");
    _STYLES_MAP.put(SkinSelectors.ERROR_ICON_STYLE_CLASS, "portlet-msg-error");
    _STYLES_MAP.put(SkinSelectors.INFO_ICON_STYLE_CLASS, "portlet-msg-info");
    _STYLES_MAP.put(SkinSelectors.WARNING_ICON_STYLE_CLASS, "portlet-msg-alert");
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_HEADER_STYLE_CLASS, "portlet-msg-error" );
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS,"portlet-msg-error");
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS, "portlet-msg-error");
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_BODY_STYLE_CLASS, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_ERROR_STYLE_CLASS,"portlet-msg-error");
    // just to give it the same background color as table's header.
    _STYLES_MAP.put(SkinSelectors.AF_MESSAGES_STYLE_CLASS, "portlet-table-header");
    // End message mappings      //
    // ------------------------- //

    // Begin table mappings
    // --------------------
     for (int i=0; i < _TABLE_TEXT_SELECTORS.length; i++)
       _STYLES_MAP.put(_TABLE_TEXT_SELECTORS[i], "portlet-table-text");

    _STYLES_MAP.put(SkinSelectors.AF_TABLE_COLUMN_FOOTER_STYLE, "portlet-table-footer");
    _STYLES_MAP.put(SkinSelectors.AF_TABLE_DETAIL_STYLE, "portlet-table-body");
    _STYLES_MAP.put(SkinSelectors.AF_COLUMN_CELL_TEXT_STYLE, "portlet-table-body");
    _STYLES_MAP.put(SkinSelectors.AF_COLUMN_TOTAL_NUMBER_STYLE, "portlet-table-body");
    _STYLES_MAP.put(SkinSelectors.AF_COLUMN_TOTAL_TEXT_STYLE, "portlet-table-body");
    _STYLES_MAP.put(SkinSelectors.AF_COLUMN_CELL_TEXT_BAND_STYLE, "portlet-table-alternate");
    _STYLES_MAP.put(SkinSelectors.AF_COLUMN_HEADER_TEXT_STYLE, "portlet-table-header");
    // selectInputDate and chooseDate have tables
    _STYLES_MAP.put(SkinSelectors.AF_SELECT_INPUT_DATE_HEADER_STYLE_CLASS, "portlet-table-header");
    _STYLES_MAP.put(SkinSelectors.AF_CHOOSE_DATE_HEADER_STYLE_CLASS, "portlet-table-header");
    _STYLES_MAP.put(SkinSelectors.AF_CHOOSE_DATE_CONTENT_STYLE_CLASS, "portlet-table-body");

    // add portlet-table-body + itself
    for (int i=0; i < _TABLE_BODY_PLUS_ITSELF.length; i++)
    {
      _STYLES_MAP.put(_TABLE_BODY_PLUS_ITSELF[i], "portlet-table-text " +
        StyleUtils.convertToValidSelector(_TABLE_BODY_PLUS_ITSELF[i]));
    }

    // add portlet-table-alternate + itself
    for (int i=0; i < _TABLE_ALT_PLUS_ITSELF.length; i++)
    {
      _STYLES_MAP.put(_TABLE_ALT_PLUS_ITSELF[i], "portlet-table-text " +
        StyleUtils.convertToValidSelector(_TABLE_ALT_PLUS_ITSELF[i]));
    }

    // add portlet-table-header + itself
    for (int i=0; i < _TABLE_HDR_PLUS_ITSELF.length; i++)
    {
      _STYLES_MAP.put(_TABLE_HDR_PLUS_ITSELF[i], "portlet-table-text " +
        StyleUtils.convertToValidSelector(_TABLE_HDR_PLUS_ITSELF[i]));
    }

    // Since we null'd out the properties, there is no reason to output this
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0001_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0010_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0011_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0100_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0101_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0110_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_0111_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1000_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1001_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1010_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1011_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1100_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1101_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1110_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.TABLE_BORDER_1111_STYLE, _NULL_STYLE);
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_SIDE_BAR_BODY_STYLE_CLASS, _NULL_STYLE);
    // End table mappings
    // -------------------

    // portlet-font-dim mappings
    // -------------------
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_PATH_STEP_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.NAV_BAR_ILINK_STYLE_CLASS, _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFInstructionTextDisabled", _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFFieldTextDisabled", _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFFieldTextLTRDisabled", _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFPhoneFieldTextDisabled", _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFPostalCodeFieldTextDisabled", _PORTLET_FONT_DIM);
    _STYLES_MAP.put("AFAddressFieldTextDisabled", _PORTLET_FONT_DIM);


    _STYLES_MAP.put(SkinSelectors.IN_CONTEXT_TEXT_STYLE_CLASS, _PORTLET_FONT_DIM);
    _STYLES_MAP.put(SkinSelectors.LINK_DISABLED_STYLE_CLASS, _PORTLET_FONT_DIM);
    _STYLES_MAP.put(SkinSelectors.DISABLED_STYLE_CLASS, _PORTLET_FONT_DIM);
    _STYLES_MAP.put(SkinSelectors.AF_SELECT_INPUT_DATE_DISABLED_STYLE_CLASS, _PORTLET_FONT_DIM);
    _STYLES_MAP.put(SkinSelectors.AF_CHOOSE_DATE_DISABLED_STYLE_CLASS, _PORTLET_FONT_DIM);

    // We want to keep certain aspects of these styles, so we blank
    // out the aspects we don't want in portlet-desktop.xss:
    // keep boldness
    _STYLES_MAP.put("AFDataTextDisabled", "portlet-font-dim AFDataTextDisabled");
    // keep boldness and alignment
    _STYLES_MAP.put("AFDataNumberDisabled", "portlet-font-dim AFDataNumberDisabled");
    _STYLES_MAP.put("AFFieldNumberDisabled", "portlet-font-dim AFFieldNumberDisabled");
    _STYLES_MAP.put(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS,"portlet-font-dim " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS));


    // end portlet-font-dim mappings
    // -------------------

     // begin portlet-font mappings
    // -------------------

    _STYLES_MAP.put(SkinSelectors.INSTRUCTION_TEXT_STYLE_CLASS,_PORTLET_FONT);
    _STYLES_MAP.put("AFFieldText", _PORTLET_FONT);
    _STYLES_MAP.put("AFFieldTextLTR", _PORTLET_FONT);
    _STYLES_MAP.put("AFPhoneFieldText",_PORTLET_FONT);
    _STYLES_MAP.put("AFPostalCodeFieldText",_PORTLET_FONT);
    _STYLES_MAP.put("AFAddressFieldText", _PORTLET_FONT);
    // We want to keep certain aspects of these styles:
    // keep boldness
    _STYLES_MAP.put(SkinSelectors.AF_DATA_TEXT_STYLE_CLASS,
        "portlet-font " + SkinSelectors.AF_DATA_TEXT_STYLE_CLASS);
    // keep boldness and alignment
    _STYLES_MAP.put("AFDataNumber", "portlet-font AFDataNumber");
    _STYLES_MAP.put("AFFieldNumber", "portlet-font AFFieldNumber");
    // af|document has a portlet-font style on it. This is important because all elements in the
    // dom tree that do not have text info on it will pick up the portlet-font styling. This
    // makes it so that I don't have to map every styleclass to portlet-font.
    // NOTE: right now we don't render a af|document, but we could, so just in case that changes,
    // I'll leave this in.
    _STYLES_MAP.put("af|document", "portlet-font " +
      StyleUtils.convertToValidSelector("af|document"));

    _STYLES_MAP.put(SkinSelectors.AF_SELECT_INPUT_DATE_ENABLED_STYLE_CLASS,_PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.NAV_BAR_ALINK_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.NAV_BAR_VIEW_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.PAGE_STAMP_TEXT_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_TREE_TABLE_MP_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_PATH_STYLE_CLASS,_PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_LIST_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_TRAIN_LINK_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.LINK_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.LINK_TEXT_STYLE_CLASS, _PORTLET_FONT);
    _STYLES_MAP.put(SkinSelectors.AF_CHOOSE_DATE_ENABLED_STYLE_CLASS, _PORTLET_FONT);

    // keep boldness
    _STYLES_MAP.put(SkinSelectors.AF_CHOOSE_DATE_SELECTED_STYLE_CLASS,
      "portlet-font " +
      StyleUtils.convertToValidSelector(SkinSelectors.AF_CHOOSE_DATE_SELECTED_STYLE_CLASS));

    // keep boldness
    _STYLES_MAP.put(SkinSelectors.AF_SELECT_INPUT_DATE_SELECTED_STYLE_CLASS,
      "portlet-font " +
      StyleUtils.convertToValidSelector(SkinSelectors.AF_SELECT_INPUT_DATE_SELECTED_STYLE_CLASS));

    // navigationPane components.
    // these contains contextual selectors which will break unless we keep the class in tact
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_LEVEL_TABS_MID_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_NAVIGATION_LEVEL_TABS_MID_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS));
    // may need padding + nowrap, so we keep the style
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_PAGE_PRIVACY_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_PANEL_PAGE_PRIVACY_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_PAGE_ABOUT_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_PANEL_PAGE_ABOUT_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS, "portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS));
    // keep padding; delete color
    _STYLES_MAP.put(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS,"portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS,"portlet-font " +
    StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BUTTONS_TEXT_DISABLED_STYLE_CLASS));

    // end portlet-font
    // -------------------

    // begin portlet-section--header
    // TODO?? Is this worth it?? we render our headers by using the same style class on headers
    // and subheaders, and we style them in the css file using the
    // H1/H2 selectors, like H1.af_panelHeader, H2.af_panelHeader.
    // for portlet we need to have distinctive style classes for headers and
    // subheaders, so the way we will get this to work is to
    // rework the renderer to also render out AFHeaderAlias
    // and AFSubheaderAlias. For portlet skin, we will map the
    // header classes (e.g., AF_PANEL_HEADER_STYLE_CLAS) to null and
    // map AFHeaderAlias to portlet-section-header and map
    // AFSubheaderAlias to portlet-section-subheader.

    // For now, we just render portlet-section-header for the header
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_HEADER_STYLE_CLASS, "portlet-section-header");
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_HEADER_ERROR_STYLE_CLASS, "portlet-section-header");
    _STYLES_MAP.put(SkinSelectors.AF_SHOW_DETAIL_HEADER_STYLE_CLASS, "portlet-section-header");
    _STYLES_MAP.put(SkinSelectors.AF_PANEL_BOX_HEADER_STYLE_CLASS,
      "portlet-section-subheader " +
      StyleUtils.convertToValidSelector(SkinSelectors.AF_PANEL_BOX_HEADER_STYLE_CLASS));
    _STYLES_MAP.put(SkinSelectors.AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS,
      "portlet-section-header");

    // end portlet-section-header



      // af:menuTabs and af:menuBar is still used within the page component.
     //
     // use portlet-table styles instead of portlet-menu styles. This
     // was requested by the portal team
     //
     // tabs might be styled, so leave them alone for padding and such.
     _STYLES_MAP.put(SkinSelectors.AF_MENU_TABS_SELECTED_STYLE_CLASS,
      "portlet-table-selected " +
         StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_TABS_SELECTED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_TABS_ENABLED_STYLE_CLASS,
       "portlet-table-alternate "+
       StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_TABS_ENABLED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_TABS_DISABLED_STYLE_CLASS,
       "portlet-table-alternate " +
       StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_TABS_DISABLED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_TABS_SEPARATOR_STYLE_CLASS,
       "portlet-table-alternate");


     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_BODY_STYLE_CLASS,
       _NULL_STYLE);
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_TITLE_STYLE_CLASS,
       _NULL_STYLE);
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_EMPTY_STYLE_CLASS,
       _NULL_STYLE);
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_SELECTED_STYLE_CLASS,
       "portlet-table-selected " +
       StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BAR_SELECTED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_ENABLED_STYLE_CLASS,
       "portlet-table-alternate "+
       StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BAR_ENABLED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_DISABLED_STYLE_CLASS,
       "portlet-table-alternate " +
       StyleUtils.convertToValidSelector(SkinSelectors.AF_MENU_BAR_DISABLED_STYLE_CLASS));
     _STYLES_MAP.put(SkinSelectors.AF_MENU_BAR_SEPARATOR_STYLE_CLASS,
       "portlet-table-alternate");


     // end menu

  }


}