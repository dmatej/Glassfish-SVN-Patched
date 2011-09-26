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
package org.apache.myfaces.trinidadinternal.image.xml;



/**
 * Constants for org.apache.myfaces.trinidadinternal.image.xml.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/XMLConstants.java#0 $) $Date: 10-nov-2005.19:03:59 $
 */
public interface XMLConstants
{
  /* Image types */
  public static final String BUTTON_NAME          = "button";
  public static final String COLORIZED_ICON_NAME  = "colorizedIcon";
  public static final String FLIPPED_ICON_NAME    = "flippedIcon";
  public static final String COMPOSITE_BUTTON_NAME = "compositeButton";

  /* Element Names */
  public static final String IMAGE_GENERATOR_NAME = "ImageGenerator";
  public static final String IMAGE_METADATA_NAME  = "ImageMetadata";
  public static final String DEFAULTS_NAME        = "defaults";
  public static final String FONT_NAME            = "font";
  public static final String FOREGROUND_NAME      = "foreground";
  public static final String BACKGROUND_NAME      = "background";
  public static final String SURROUNDING_COLOR_NAME = "surroundingColor";
  public static final String TEXT_NAME            = "text";
  public static final String TRANSLATED_TEXT_NAME = "translatedText";
  public static final String ICON_NAME            = "icon";
  public static final String TAB_NAME             = "tab";
  public static final String LINK_NAME            = "link";
  public static final String DISABLED_FOREGROUND_NAME = "disabledForeground";
  public static final String DISABLED_BACKGROUND_NAME = "disabledBackground";
  public static final String DISABLED_FONT_NAME       = "disabledFont";
  public static final String SELECTED_FOREGROUND_NAME = "selectedForeground";
  public static final String SELECTED_BACKGROUND_NAME = "selectedBackground";
  public static final String SELECTED_FONT_NAME       = "selectedFont";
  public static final String IMAGE_MAP_AREA_NAME  = "area";
  public static final String IMAGE_MAP_NAME       = "imageMap";
  public static final String BORDER_COLOR_NAME    = "borderColor";
  public static final String FONT_NAME_NAME       = "name";
  public static final String FONT_SIZE_NAME       = "size";
  public static final String FONT_STYLE_NAME      = "style";
  public static final String DARK_COLOR_NAME      = "darkColor";
  public static final String DARK_ACCENT_COLOR_NAME = "darkAccentColor";

  /* Attribute Names */
  public static final String LAF_ATTR             = "laf";
  public static final String NAME_ATTR            = "name";
  public static final String NAMESPACE_ATTR       = "namespace";
  public static final String TEXT_ANTIALIAS_ATTR  = "textAntialias";
  public static final String DISABLED_ATTR        = "disabled";
  public static final String KEY_ATTR             = "key";
  public static final String BUNDLE_ATTR          = "bundle";
  public static final String START_ROUNDED_ATTR   = "startRounded";
  public static final String END_ROUNDED_ATTR     = "endRounded";
  public static final String SOURCE_ATTR          = "source";
  public static final String SELECTED_ATTR        = "selected";
  public static final String SELECTED_INDEX_ATTR  = "selectedIndex";
  public static final String DESTINATION_ATTR     = "destination";
  public static final String SHAPE_ATTR           = "shape";
  public static final String COORDINATES_ATTR     = "coords";
  public static final String WIDTH_ATTR           = "width";
  public static final String HEIGHT_ATTR          = "height";
  public static final String DIRECTION_ATTR       = "direction";
  public static final String ENCODING_TYPE_ATTR   = "type";
  public static final String ACCESS_KEY_ATTR      = "accessKey";
  public static final String TEXT_ATTR            = "text";
  public static final String RED_ATTR             = "red";
  public static final String GREEN_ATTR           = "green";
  public static final String BLUE_ATTR            = "blue";
  public static final String RGB_ATTR             = "rgb";
  public static final String VERSION_ATTR         = "version";
  public static final String LOOK_AND_FEEL_ID_ATTR = "lookAndFeelId";

  // Shape attribute values
  public static final String RECTANGLE_SHAPE      = "rect";
  public static final String POLYGON_SHAPE        = "poly";

  // Direction attribute values
  public static final String LEFT_TO_RIGHT_DIRECTION = "ltr";
  public static final String RIGHT_TO_LEFT_DIRECTION = "rtl";

  // Font style value
  public static final String PLAIN_FONT_STYLE     = "plain";
  public static final String ITALIC_FONT_STYLE    = "italic";
  public static final String BOLD_FONT_STYLE      = "bold";

  /**
   * Property name constant for specifying a ResourceBundle to use when
   * parsing ImageGenerator XML documents.  This property should be placed
   * on the ImageContext using the ImageConstants.TECATE_NAMESPACE.  The value
   * is a java.util.ResourceBundle.
   */
  public static final Object RESOURCE_BUNDLE_PROPERTY = "decodingResourceBundle";

  /**
   * Property name constant for specifying a LocaleContext to use when
   * parsing ImageGenerator XML documents.  This property is placed on
   * the ParseContext by
   * ImageProviderRequestUtils.createImageProviderRequests() in order to
   * using the ImageConstants.TECATE_NAMESPACE.  The value is a LocaleContext
   * instance.
   */
  public static final Object LOCALE_CONTEXT_PROPERTY = "localeContext";
}
