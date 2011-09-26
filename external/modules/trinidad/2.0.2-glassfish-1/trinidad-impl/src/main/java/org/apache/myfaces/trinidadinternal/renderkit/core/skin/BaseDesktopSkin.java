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

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;

/**
 * Skin implementation for HTML browsers
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/BaseDesktopSkin.java#0 $) $Date: 10-nov-2005.19:02:49 $
 */
public class BaseDesktopSkin extends XhtmlSkin
{
  /**
   * Constructs a BaseDesktopSkin instance
   */
  public BaseDesktopSkin()
  {

    // Register our icons
    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
    _registerSkinProperties();
  }

  private void _registerSkinProperties()
  {
    // Not sure where this comes from!
    setProperty(SkinProperties.AF_PANEL_BORDER_LAYOUT_SPACER_WIDTH, "2");
    setProperty(SkinProperties.AF_TREE_SHOW_LINES, true);
  }

  /**
   * Returns the id for the desktop implementation of the Base
   * Look And Feel: "base.desktop".
   */
  @Override
  public String getId()
  {
    return XhtmlConstants.BASE_DESKTOP_ID;
  }

  /**
   * Returns the family for the Base
   * Look And Feel: "base".
   */
  @Override
  public String getFamily()
  {
    return "base";
  }


  /**
   * Returns the renderKitId for the BaseDesktopSkin: "org.apache.myfaces.trinidad.desktop".
   */
  @Override
  public String getRenderKitId()
  {
    return XhtmlConstants.APACHE_TRINIDAD_DESKTOP;
  }

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  @Override
  public String getStyleSheetName()
  {
    return "base-desktop.css";
  }

  // Customizable LAF Icons
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // navigationPath and treeTable path separator icon.
    SkinSelectors.PATH_SEPARATOR_ICON_ALIAS_NAME,
      new TextIcon("\u00a0\u00a0>\u00a0\u00a0"),

    // ColorField Icons
    SkinSelectors.AF_SELECT_INPUT_COLOR_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/cfb.gif",
                          "adf/images/cfbr.gif",
                          24,
                          24),

                         
    SkinSelectors.AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_NAME,
    new ContextImageIcon("adf/images/cfso.gif", 
                         "adf/images/cfsor.gif", 
                         12,
                          12, 
                         SkinSelectors.AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_STYLE_CLASS, 
                         null),  
    // DateField Icons
    SkinSelectors.AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/dfb.gif",
                          "adf/images/dfbr.gif",
                          19,
                          24),

    // GlobalHeader Icons
    SkinSelectors.AF_MENU_BAR_SEPARATOR_ICON_NAME,
    new TextIcon("\u00a0| "),

    // HideShow Icons
    // (showDetail) defined in OracleDesktopSkinExtension and BaseDesktopSkin
    // used in base.desktop.HideShowRenderer by simple, and pda
    // and oracle.desktop extends it.

    SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BC", 
                                        null, 
                                        SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                        null), 
                            new TextIcon("\u2193", 
                                         null, 
                                         SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null)),

    SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BA", 
                                         "\u25C4", 
                                         SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null), 
                            new TextIcon("\u2192", 
                                         "\u2190", 
                                         SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null)),

    SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),
   
    // The Webkit browser that runs in Nokia platform doesn't support Unicode 
    // characters that display icons, so we need to render text-icons for
    // Nokia.
    // Ideally, we should be able to use different disclosed/undisclosed icons 
    // based on different platforms using the same disclosed/undisclosed 
    // skinning-key, but we don't have such mechanism in Trinidad right now.  
    // Hence, I have created new skinning-keys for Nokia.
    SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME_FOR_NOKIA_S60,
      new TextIcon("[-]", 
                   null, 
                   SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS,
                   null), 
                   
    SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME_FOR_NOKIA_S60,
       new TextIcon("[+]", 
                    null, 
                    SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS,
                    null),  

    // tr:table icons
    SkinSelectors.AF_TABLE_SD_DISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    SkinSelectors.AF_TABLE_SD_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),

    // tr:showDetailHeader icons
    SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),

    // tr:navigationTree icons
    SkinSelectors.AF_NAVIGATION_TREE_DISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    SkinSelectors.AF_NAVIGATION_TREE_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),
    // HGrid icons
    // (treeTable)
    SkinSelectors.AF_TREE_TABLE_EXPANDED_ICON_NAME,
      new ContextImageIcon("adf/images/nav-minus.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_TABLE_COLLAPSED_ICON_NAME,
      new ContextImageIcon("adf/images/nav-plus.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_TABLE_FOCUS_ICON_NAME,
      new TextIcon("X",
                   null,
                   SkinSelectors.AF_TREE_TABLE_FOCUS_ICON_STYLE_CLASS,
                   null),

    SkinSelectors.AF_TREE_TABLE_LOCATOR_ICON_NAME,
      new TextIcon("X",
                   null,
                   SkinSelectors.AF_TREE_TABLE_LOCATOR_ICON_STYLE_CLASS,
                   null),

    SkinSelectors.AF_TREE_EXPANDED_ICON,
      new ContextImageIcon("adf/images/nav-minus.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_COLLAPSED_ICON,
      new ContextImageIcon("adf/images/nav-plus.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_LINE_ICON,
      new ContextImageIcon("adf/images/tree-line-trunk.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_LINE_MIDDLE_ICON,
      new ContextImageIcon("adf/images/tree-line-middle.gif",
                          19,
                          18),

    SkinSelectors.AF_TREE_LINE_LAST_ICON,
      new ContextImageIcon("adf/images/tree-line-last.gif",
                          19,
                          18),

    // SelectInputText Icons
    SkinSelectors.AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME,
      new ContextImageIcon("adf/images/lvib.gif",
                          "adf/images/lvibr.gif",
                          24,
                          24),

    // shuttle's 'Move' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move icon keys reference.
    // shuttle's icons are in BaseDesktopSkin instead of XhtmlSkin because
    // pda renderers don't support shuttle.
    // establish the icon hierarchy here, so that skins that extend this
    // skin can customize both selectOrder/selectManyShuttle very easily
    // by just customizing the alias.
                          SkinSelectors.SHUTTLE_MOVE_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_MOVE_ICON_ALIAS_NAME),

    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_MOVE_ICON_ALIAS_NAME),

    // shuttle's 'Move All' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SkinSelectors.SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME),

    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME),

    // shuttle's 'Remove' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SkinSelectors.SHUTTLE_REMOVE_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_REMOVE_ICON_ALIAS_NAME),

    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_REMOVE_ICON_ALIAS_NAME),


    // shuttle's 'Remove All' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SkinSelectors.SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME),

    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME,
    new ReferenceIcon(SkinSelectors.SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME),
    
    // SelectOrderShuttle's reorder icons
    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_UP_ALL"),
    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_UP"),
    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_DOWN"),
    SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_DOWN_ALL"),    

    // SortableHeader Icons
    SkinSelectors.AF_COLUMN_SORTED_ASCEND_ICON_NAME,
      new TextIcon("\u25B2",
                   null,
                   SkinSelectors.SORTABLE_HEADER_SORT_ICON_STYLE_CLASS,
                   null),

    SkinSelectors.AF_COLUMN_SORTED_DESCEND_ICON_NAME,
      new TextIcon("\u25BC",
                   null,
                   SkinSelectors.SORTABLE_HEADER_SORT_ICON_STYLE_CLASS,
                   null),

    // for now, pda does not have a separator icon, so we put this definition
    // in BaseDesktopSkin instead of XhtmlSkin.
    SkinSelectors.AF_SELECT_ONE_TAB_SEPARATOR_ICON_NAME,
    NullIcon.sharedInstance(),    
  };
}
