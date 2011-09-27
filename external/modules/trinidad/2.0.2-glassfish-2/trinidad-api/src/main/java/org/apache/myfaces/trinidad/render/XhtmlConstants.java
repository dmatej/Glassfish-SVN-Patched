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
package org.apache.myfaces.trinidad.render;

import java.util.Arrays;
import java.util.List;

/**
 * This class contains some useful constants for (X)HTML rendering.
 */
public final class XhtmlConstants
{
  private XhtmlConstants(){}

  /**
   * A portlet facet;  when supported, this facet should
   * result in a version of page content optimized for use in portlets.
   */
  public static final String FACET_PORTLET = "portlet";
  
  public static final String SCRIPT_NAME = "script";

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

}