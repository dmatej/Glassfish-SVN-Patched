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
package org.apache.myfaces.trinidadinternal.image;

/**
 * Common constants for the org.apache.myfaces.trinidadinternal.image package.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageConstants.java#0 $) $Date: 10-nov-2005.19:03:52 $
 */
public interface ImageConstants
{
  /**
   * Namespace for UIX Dynamic Images
   */
  // =-=ags Seems strange to use a codename in code, but
  //        that's what UIConstants.MARLIN_NAMESPACE does,
  //        so we'll follow that example.
  public static final String TECATE_NAMESPACE =
    "http://myfaces.apache.org/uix/image";


  /**
   * Property key for specifying an ImageProvider
   * This property key is used by Renderer implementations
   * to retrieve an ImageProvider from a RenderingContext.  Clients
   * are responsible for installing an ImageProvider on the
   * RenderingContext via the RenderingContext.setProperty()
   * method.
   */
  public static final Object IMAGE_PROVIDER_PROPERTY = "org.apache.myfaces.trinidadinternal.image.imageProvider";

  /**
   * Name for the colorized icon type
   */
  public static final String COLORIZED_ICON_NAME = "colorizedIcon";

  /**
   * Name for the flipped icon type
   */
  public static final String FLIPPED_ICON_NAME = "flippedIcon";

  /**
   * Name for the composite button type
   */
  public static final String COMPOSITE_BUTTON_NAME = "compositeButton";

  /**
   * Key used to specify the foreground color.
   * The value of this key is a Color object specifying the
   * foreground to use when rendering an image.
   */
  public static final Object FOREGROUND_KEY = "foreground";

  /**
   * Key used to specify the background color.
   * The value of this key is a Color object specifying the
   * background to use when rendering an image.
   */
  public static final Object BACKGROUND_KEY = "background";

  /**
   * Key used to specify the font.
   * The value of this key is either a java.awt.Font or a
   * org.apache.myfaces.trinidadinternal.style.util.FontProxy object specifying the
   * font to use when rendering an image.
   * @see org.apache.myfaces.trinidadinternal.style.util.FontProxy
   */
  public static final Object FONT_KEY = "font";

  /**
   * Key used to specify the disabled state of the image.
   * The value of this key is a Boolean object specifying
   * whether image should appear disabled.
   */
  public static final Object DISABLED_KEY = "disabled";

  /**
   * Key used to specify the String mime type of the image encoding.
   */
  public static final Object ENCODING_TYPE_KEY = "encoding";

  /**
   * Key used to specify the selected state of the image.
   * The value of this key is a Boolean object specifying
   * whether image should appear selected
   */
  public static final Object SELECTED_KEY = "selected";

  /**
   * Key used to specify whether text should be antialiased.
   * The value of this key is a Boolean object specifying
   * whether text should be antialiased
   */
  public static final Object TEXT_ANTIALIAS_KEY = "textAntialias";

  /**
   * Key used to specify the text value for an image.
   * The value of this key is a String object specifying
   * the text that should be displayed in the generated
   * image.
   */
  public static final Object TEXT_KEY = "text";

  /**
   * Key used to specify the version value for an image.
   * The version is changed when the visual spec is changed
   * so that cached images not in line with the spec are ignored
   */
  public static final Object VERSION_KEY = "version";

  /**
   * Key used to specify the mnemonic character for an image.
   * The value of this key is a Character object specifying
   * the character that should be rendered as the access key.
   */
  public static final Object ACCESS_KEY_KEY = "accessKey";

  /**
   * Key used to specify the name for an image.
   * The value of this key is a String object specifying
   * the name that should be use for the generated image.
   */
  public static final Object NAME_KEY = "name";

  /**
   * Key used to specify whether an action button should be
   * rounded on its "start" side.
   * The value of this key is a Boolean object specifying
   * whether the button should be rounded on the start side -
   * ie. the left side in left to right languages, the right
   * side in right to left languages.
   */
  public static final Object START_ROUNDED_KEY = "startRounded";

  /**
   * Key used to specify whether an action buton should should be
   * rounded on its "end" side.
   * The value of this key is a Boolean object specifying
   * whether the button should be rounded on the end side -
   * ie. the right side in left to right languages, the left
   * side in right to left languages.
   */
  public static final Object END_ROUNDED_KEY = "endRounded";

  /**
   * Key used to specify the border color for an action button.
   * The value of this key is a Color object specifying the
   * color to use when rendering the border of a button.
   */
  public static final Object BORDER_COLOR_KEY = "borderColor";

  /**
   * Key used to specify the image source for a generated image.
   * The value of this key is a String object specifying
   * the name of an image that should be use as a source image
   * for the generated image
   */
  public static final Object SOURCE_KEY = "source";

  /**
   * Key used to specify the tabs for a tab bar image.
   * The value of this key is an array of Tab objects
   * to display in the tab bar.
   */
  public static final Object TABS_KEY = "tabs";

  /**
   * Key used to specify the foreground color for selected items.
   * The value of this key is a Color object specifying the
   * selected foreground to use when rendering an image.
   */
  public static final Object SELECTED_FOREGROUND_KEY = "selectedForeground";

  /**
   * Key used to specify the background color for selected items.
   * The value of this key is a Color object specifying the
   * selected background to use when rendering an image.
   */
  public static final Object SELECTED_BACKGROUND_KEY = "selectedBackground";

  /**
   * Key used to specify the font for selected items.
   * The value of this key is a Font object specifying the
   * selected font to use when rendering an image.
   */
  public static final Object SELECTED_FONT_KEY = "selectedFont";

  /**
   * Key used to specify the foreground color for disabled items.
   * The value of this key is a Color object specifying the
   * disabled foreground to use when rendering an image.
   */
  public static final Object DISABLED_FOREGROUND_KEY = "disabledForeground";

  /**
   * Key used to specify the background color for disabled items.
   * The value of this key is a Color object specifying the
   * disabled background to use when rendering an image.
   */
  public static final Object DISABLED_BACKGROUND_KEY = "disabledBackground";

  /**
   * Key used to specify the font for disabled items.
   * The value of this key is a Font object specifying the
   * disabled font to use when rendering an image.
   */
  public static final Object DISABLED_FONT_KEY = "disabledFont";

  /**
   * Key used to specifiy an InputStreamProvider for a source icon.
   * The InputStreamProvider is used to obtain image data for the
   * source images for global buttons and colorizeds.
   *
   * @see org.apache.myfaces.trinidad.share.io.InputStreamProvider
   */
  public static final Object SOURCE_INPUT_STREAM_PROVIDER_KEY = "inputStreamProvider";

  /**
   * Key used to specify the dark core color of a colorized icon.
   * The value of this key is a Color object specifying the dark
   * color to use when rendering a colorized icon.
   */
  public static final Object DARK_COLOR_KEY = "dark";

  /**
   * Key used to specify the dark accent color of a colorized icon.
   * The value of this key is a Color object specifying the dark accent
   * color to use when rendering a colorized icon.
   */
  public static final Object DARK_ACCENT_COLOR_KEY = "darkAccent";

  /**
   * Key used to specify the surrounding background color for an
   * image.  If null, the surround color is transparent.  Otherwise,
   * the image is filled with the surrounding color before any other
   * contents are rendered.
   */
  public static final Object SURROUNDING_COLOR_KEY = "surroundingColor";

  /**
   * Key used to specify the look and feel for an
   * image.
   */
  public static final Object LAF_KEY = "laf";

  /**
   * Key used to specify the direction for an image.
   * The value of this key is a Integer object specifying
   * the LocaleUtils.DIRECTION constant value for the image direction.
   */
  public static final Object DIRECTION_KEY = "direction";

  /**
   * Key used to retrieve the image map areas.
   * The value of this key is an array of MapArea objects
   * (MapArea[]).
   *
   * @see org.apache.myfaces.trinidadinternal.image.util.MapArea
   */
  public static final Object IMAGE_MAP_AREAS_RESPONSE_KEY = "areas";

  /**
   * Key used to retrieve the image width.
   * The value of this key is an Integer object specifying the
   * width of the generated image.
   */
  public static final Object WIDTH_RESPONSE_KEY = "width";

  /**
   * Key used to retrieve the image height.
   * The value of this key is an Integer object specifying the
   * height of the generated image.
   */
  public static final Object HEIGHT_RESPONSE_KEY = "height";


  /**
   * Key used to retrieve an InputStreamProvider which provides
   * access to the button start icon data.
   */
  public static final Object BUTTON_START_ICON_KEY = "buttonStartIcon";

  /**
   * Key used to retrieve an InputStreamProvider which provides
   * access to the button end icon data.
   */
  public static final Object BUTTON_END_ICON_KEY = "buttonEndIcon";

  /**
   * Key used to retrieve an InputStreamProvider which provides
   * access to the button's top background icon data.
   */
  public static final Object BUTTON_TOP_BACKGROUND_ICON_KEY = "buttonTopBackgroundIcon";

  /**
   * Key used to retrieve an InputStreamProvider which provides
   * access to the button's bottom background icon data.
   */
  public static final Object BUTTON_BOTTOM_BACKGROUND_ICON_KEY = "buttonBottomBackgroundIcon";

  /**
   * Key used to specify the id of the LookAndFeel for
   * which the image is being generated.
   */
  public static final Object LOOK_AND_FEEL_ID_KEY = "lookAndFeelId";
}
