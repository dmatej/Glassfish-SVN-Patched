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

import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;
import org.apache.myfaces.trinidadinternal.style.UnmodifiableStyle;


/**
 * Skin implementation for XHTML
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/XhtmlSkin.java#0 $) $Date: 22-nov-2005.14:42:24 $
 */
public class XhtmlSkin extends BaseSkin
{
  public XhtmlSkin()
  {
    super();

    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
    _registerSkinProperties();
  }

  /**
   * @todo Move the "BLAF" bundle to a more generic location.
   */
  @Override
  protected String getBundleName()
  {
    return _BUNDLE_CLASS;
  }

  private void _registerSkinProperties()
  {
    setProperty(SkinProperties.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY,
                Boolean.TRUE);
    setProperty(SkinProperties.AF_PANEL_LIST_DEFAULT_COLUMNS, 3);
  }

  // fully qualified class name of our resource bundle
  // Note: this is the same as BLAF!
  private static final String _BUNDLE_CLASS =
    "org.apache.myfaces.trinidadinternal.renderkit.core.resource.CoreBundle";

  // Customizable LAF Icons
  private static final TextIcon _ERROR_ICON =
    new TextIcon("X",
                 null,
                 SkinSelectors.ERROR_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _INFO_ICON =
    new TextIcon("i",
                 null,
                 SkinSelectors.INFO_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _REQUIRED_ICON =
    new TextIcon("*",
                 null,
                 SkinSelectors.REQUIRED_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _WARNING_ICON =
    new TextIcon("!",
                 null,
                 SkinSelectors.WARNING_ICON_STYLE_CLASS,
                 null);


  private static final TextIcon _QUICK_SELECT_ICON =
    new TextIcon("Q",
                 null,
                 SkinSelectors.QUICK_SELECT_ICON_STYLE_CLASS,
                 null);


  private static final TextIcon _QUICK_SELECT_DISABLED_ICON =
    new TextIcon("Q",
                 null,
                 SkinSelectors.QUICK_SELECT_DISABLED_ICON_STYLE_CLASS,
                 null);
  
  private static final Map<String, String> _spinboxTopStyleMap;
  private static final Map<String, String> _spinboxBottomStyleMap;
  
  static
  {
    // todo Use ArrayMap instead of ConcurrentHashMap
    // We were using CSSStyle instead of UnmodifiableStyle and that class copied the properties 
    // into a ConcurrentHashMap. Changing this to another map will change the spinbox golden files.
    _spinboxTopStyleMap = new ConcurrentHashMap<String, String>();
    _spinboxTopStyleMap.put("display", "block");
    // this is needed for the image
    _spinboxBottomStyleMap = new ConcurrentHashMap<String, String>();
    _spinboxBottomStyleMap.put("display", "block");
    _spinboxBottomStyleMap.put("padding-top", "2px");

  }
  
  private static final Style spinboxTopStyle = new UnmodifiableStyle(_spinboxTopStyleMap);
  private static final Style spinboxBottomStyle = new UnmodifiableStyle(_spinboxBottomStyleMap);



  // Icons array
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // Global Icons. These are alias icons (they are referenced )
    // Someday we'll reference them from the select* components.
    SkinSelectors.ERROR_ICON_ALIAS_NAME,
    _ERROR_ICON,

    SkinSelectors.ERROR_ANCHOR_ICON_ALIAS_NAME,
    _ERROR_ICON,

    SkinSelectors.INFO_ICON_ALIAS_NAME,
    _INFO_ICON,

    SkinSelectors.INFO_ANCHOR_ICON_ALIAS_NAME,
    _INFO_ICON,

    SkinSelectors.REQUIRED_ICON_ALIAS_NAME,
    _REQUIRED_ICON,

    SkinSelectors.WARNING_ICON_ALIAS_NAME,
    _WARNING_ICON,

    SkinSelectors.WARNING_ANCHOR_ICON_ALIAS_NAME,
    _WARNING_ICON,

    SkinSelectors.QUICK_SELECT_ICON_NAME,
    _QUICK_SELECT_ICON,

    SkinSelectors.QUICK_SELECT_DISABLED_ICON_NAME,
    _QUICK_SELECT_DISABLED_ICON,

    SkinSelectors.PATH_SEPARATOR_ICON_ALIAS_NAME,
    new TextIcon("\u00a0"),

    SkinSelectors.AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME,
    new ReferenceIcon(SkinSelectors.PATH_SEPARATOR_ICON_ALIAS_NAME),

    // navigationPath in treeTable
    SkinSelectors.AF_TREE_TABLE_MP_SEPARATOR_ICON_NAME,
    new ReferenceIcon(SkinSelectors.PATH_SEPARATOR_ICON_ALIAS_NAME),

    // alias icon for messages's header/panelHeader
    SkinSelectors.HEADER_ERROR_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.HEADER_WARNING_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.HEADER_INFO_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.HEADER_CONFIRMATION_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    // tr:messages header icons point to reference icon so they can
    // be easily shared with tr:panelHeader's icons
    SkinSelectors.AF_MESSAGES_ERROR_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_ERROR_ICON_ALIAS_NAME),

    SkinSelectors.AF_MESSAGES_WARNING_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_WARNING_ICON_ALIAS_NAME),

    SkinSelectors.AF_MESSAGES_INFO_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_INFO_ICON_ALIAS_NAME),

    SkinSelectors.AF_MESSAGES_CONFIRMATION_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_CONFIRMATION_ICON_ALIAS_NAME),

    // tr:panelHeader header icons point to reference icon so they can
    // be easily shared with tr:messages's icons
    SkinSelectors.AF_PANEL_HEADER_ERROR_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_ERROR_ICON_ALIAS_NAME),

    SkinSelectors.AF_PANEL_HEADER_WARNING_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_WARNING_ICON_ALIAS_NAME),

    SkinSelectors.AF_PANEL_HEADER_INFO_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_INFO_ICON_ALIAS_NAME),

    SkinSelectors.AF_PANEL_HEADER_CONFIRMATION_ICON_NAME,
    new ReferenceIcon(SkinSelectors.HEADER_CONFIRMATION_ICON_ALIAS_NAME),

    // inputNumberSpinbox increment/decrement icons

    SkinSelectors.AF_INPUT_NUMBER_SPINBOX_INCREMENT_ICON_NAME,
      new ContextImageIcon("adf/images/spbxup.png",
                           null,
                           5,
                           5,
                           null,
                           spinboxTopStyle),

     SkinSelectors.AF_INPUT_NUMBER_SPINBOX_DECREMENT_ICON_NAME,
     new ContextImageIcon("adf/images/spbxdn.png",
                          null,
                          5,
                          5,
                          null,
                          spinboxBottomStyle),

     SkinSelectors.AF_INPUT_NUMBER_SPINBOX_INCREMENT_DISABLED_ICON_NAME,
     new ContextImageIcon("adf/images/spbxupd.png",
                          null,
                          5,
                          5,
                          null,
                          spinboxTopStyle),

     SkinSelectors.AF_INPUT_NUMBER_SPINBOX_DECREMENT_DISABLED_ICON_NAME,
     new ContextImageIcon("adf/images/spbxdnd.png",
                          null,
                          5,
                          5,
                          null,
                          spinboxBottomStyle),
     // checkbox icons
    SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_CHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkdc.gif",
                        null,
                        12,
                        12),
    SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_UNCHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkdn.gif",
                        null,
                        12,
                        12),

    SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_CHECKED_ICON_NAME,
    new ContextImageIcon( "adf/images/checkrc.gif",
                        null,
                        12,
                        12),

    SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_UNCHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkrn.gif",
                        null,
                        12,
                        12),

     // radio icons
    SkinSelectors.AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiods.gif",
                        null,
                        11,
                        11),
    SkinSelectors.AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiodn.gif",
                        null,
                        11,
                        11),

    SkinSelectors.AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME,
    new ContextImageIcon( "adf/images/radiors.gif",
                        null,
                        10,
                        10),

    SkinSelectors.AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiorn.gif",
                        null,
                        10,
                        10),

    // progressIndicator icons
    SkinSelectors.AF_PROGRESS_INDICATOR_INDETERMINATE_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the selectRangeChoiceBar
    SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the navigationBar
    SkinSelectors.AF_TABLE_NB_PREV_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TABLE_NB_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TABLE_NB_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TABLE_NB_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the navigationBar
    SkinSelectors.AF_TREE_TABLE_NB_PREV_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TREE_TABLE_NB_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TREE_TABLE_NB_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_TREE_TABLE_NB_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the chooseDate
    // @todo think about how chooseDate and selectRangeChoiceBar have different
    // icons for the XhtmlSkin, but the same for oracleDesktopSkin.
    // This is weird, although I can see how they are needed to move months
    // for chooseDate, but you have the Next 5/ Prev 5 links for selectRangeCB.
    // OracleDesktopSkin
    // will need to create a global icon that it shares between its SelectRangeChoiceBar
    // and ChooseDate components.

    // buttons are the true hierarchical example. Work on those next.
    SkinSelectors.AF_CHOOSE_DATE_PREV_ICON_NAME,
    new TextIcon("<"),

    SkinSelectors.AF_CHOOSE_DATE_NEXT_ICON_NAME,
    new TextIcon(">"),

    SkinSelectors.AF_CHOOSE_DATE_PREV_DISABLED_ICON_NAME,
    new TextIcon("<"),

    SkinSelectors.AF_CHOOSE_DATE_NEXT_DISABLED_ICON_NAME,
    new TextIcon(">"),

    SkinSelectors.AF_SELECT_INPUT_DATE_PREV_ICON_NAME,
    new TextIcon("<"),

    SkinSelectors.AF_SELECT_INPUT_DATE_NEXT_ICON_NAME,
    new TextIcon(">"),

    SkinSelectors.AF_SELECT_INPUT_DATE_PREV_DISABLED_ICON_NAME,
    new TextIcon("<"),

    SkinSelectors.AF_SELECT_INPUT_DATE_NEXT_DISABLED_ICON_NAME,
    new TextIcon(">"),

    // StatusIndicator Icons

    // Icon aliases (for tr:icon access)
    SkinSelectors.BUSY_ICON_ALIAS_NAME, 
    new ContextImageIcon("adf/images/sibusy.gif",
                         "adf/images/sibusy.gif",
                         16,
                         16),
    
    SkinSelectors.READY_ICON_ALIAS_NAME, 
    new ContextImageIcon("adf/images/siready.gif",
                         "adf/images/siready.gif",
                         16,
                         16),
    
    SkinSelectors.AF_STATUS_INDICATOR_READY_ICON, 
    new ReferenceIcon(SkinSelectors.READY_ICON_ALIAS_NAME),
    
    SkinSelectors.AF_STATUS_INDICATOR_BUSY_ICON, 
    new ReferenceIcon(SkinSelectors.BUSY_ICON_ALIAS_NAME),

  };
}
