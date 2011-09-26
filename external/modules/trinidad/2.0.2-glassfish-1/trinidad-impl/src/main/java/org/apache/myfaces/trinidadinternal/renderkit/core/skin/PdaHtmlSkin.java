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
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;

/**
 * Skin implementation for HTML browsers
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/PdaHtmlSkin.java#0 $) $Date: 10-nov-2005.19:02:55 $
 */
public class PdaHtmlSkin extends XhtmlSkin
{

  public PdaHtmlSkin()
  {
    // Register customizable Icons
    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
    _registerSkinProperties();
  }



  private void _registerSkinProperties()
  {    
    setProperty(SkinProperties.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY, Boolean.FALSE);
    setProperty(SkinProperties.AF_PANELHEADER_INDENT_CONTENT, Boolean.FALSE);
    setProperty(SkinProperties.AF_PANEL_LIST_DEFAULT_COLUMNS, 2);
  }

  // This array contains entries for all of the customizable
  // org.apache.myfaces.trinidadinternal.skin.icon.Icons for PdaHtmlSkin
  // and subclasses of PdaHtmlSkin.
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // Path separator: &nbsp;>&nbsp;
    SkinSelectors.PATH_SEPARATOR_ICON_ALIAS_NAME,
      new TextIcon("\u00a0>\u00a0"),

    // define icons
    //PH: HideShow Icons. showDetail prompt facet should add a link so that
    //it can be expanded; on desktop there is an arrow the user can click to
    //expand regardless of what the prompt is, but on PDA we normally cause the
    //default Hide/Show text to be a link; since the prompt facet can be
    //anything we need to add an ICON like the desktop. However, unicode
    //characters do not work for Pocket IE and IE Mobile. Therefore, create
    //icons from text [+] [-]

    SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME,
      new TextIcon("[-]",
                    null,
                    SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS,
                    null),

    SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME,
      new TextIcon("[+]",
                    null,
                    SkinSelectors.HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS,
                    null),

    SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(SkinSelectors.DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),
      
    SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME,
      new TranslatedTextIcon("af_showDetailHeader.DISCLOSED"),
    SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME,
      new TranslatedTextIcon("af_showDetailHeader.UNDISCLOSED"),
      
    // define icons
    SkinSelectors.AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/dp.gif", 
                           "adf/images/dprtl.gif",
                           17,
                           18),
                            
    SkinSelectors.AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME,
      new ContextImageIcon("adf/images/lovi.gif", 
                           "adf/images/lovirtl.gif",
                           18,
                           18), 
    // @todo these need to be green and in adf/images/lovi (these are for
    // OraclePdaSkin, but we are sharing for now.
    SkinSelectors.AF_COLUMN_UNSORTED_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrt.gif", 
                           null,
                           16,
                           7), 
    SkinSelectors.AF_COLUMN_SORTED_ASCEND_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrta.gif", 
                           null, 
                           9,
                           9),    
    SkinSelectors.AF_COLUMN_SORTED_DESCEND_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrtd.gif", 
                           null, 
                           9,
                           9),    

  };

}
