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

import java.util.HashMap;
import java.util.Map;

/**
 * This class contains all valid predefined skin properties used by Trinidad.
 * The properties are sorted alphabetically.
 * 
 */
public final class SkinProperties
{
  private SkinProperties(){}
  
  //
  // Copied from XhtmlLafConstants
  //
  // FIXME: Name inconsistency, should be AF_NAVIGATION_PATH
  public static final String AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY =
    "af|breadCrumbs-tr-show-last-item";
  public static final String AF_BREAD_CRUMBS_SEPARATOR_ON_NEW_LINE =
    "af|breadCrumbs-tr-separator-on-new-line";
  public static final String AF_BREAD_CRUMBS_INDENT_SPACES = 
    "af|breadCrumbs-tr-indent-spaces";
  // FIXME: Name inconsistency, should be AF_PANEL_HEADER
  public static final String AF_PANELHEADER_INDENT_CONTENT = 
    "af|panelHeader-tr-indent-content";
  public static final String AF_PANEL_BORDER_LAYOUT_SPACER_WIDTH =
    "af|panelBorderLayout-tr-spacer-width";
  public static final String AF_PANEL_LIST_DEFAULT_COLUMNS =
    "af|panelList-tr-default-columns";    
  public static final String AF_TABLE_REPEAT_CONTROL_BAR =
    "af|table-tr-repeat-control-bar";
  public static final String AF_TABLE_SELECTION_BAR_IN_TABLE =
    "af|table-tr-selection-bar-in-table";
  public static final String AF_TRAIN_RENDER_PARENT_TRAIN =
    "af|train-tr-render-parent-train";
  public static final String AF_TRAIN_VISIBLE_STOP_COUNT =
    "af|train-tr-visible-stop-count";
  public static final String AF_TREE_TABLE_SPACER_WIDTH =
    "af|treeTable-tr-spacer-width";
  public static final String AF_TREE_SHOW_LINES =
    "af|tree-tr-show-lines";
  
  // Map of property to class type
  public static final Map<String, Class<?>> PROPERTY_CLASS_TYPE_MAP;
  static
  {
    PROPERTY_CLASS_TYPE_MAP = new HashMap<String, Class<?>>();

    PROPERTY_CLASS_TYPE_MAP.put(
      AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY, Boolean.class);
    PROPERTY_CLASS_TYPE_MAP.put(
      AF_TABLE_SELECTION_BAR_IN_TABLE, Boolean.class);
    PROPERTY_CLASS_TYPE_MAP.put(
      AF_TABLE_REPEAT_CONTROL_BAR, Boolean.class);
    PROPERTY_CLASS_TYPE_MAP.put(
      AF_TREE_SHOW_LINES, Boolean.class);
    PROPERTY_CLASS_TYPE_MAP.put(
      AF_BREAD_CRUMBS_SEPARATOR_ON_NEW_LINE, Boolean.class);
    PROPERTY_CLASS_TYPE_MAP.put(
      AF_BREAD_CRUMBS_INDENT_SPACES, Integer.class);
  }


}
