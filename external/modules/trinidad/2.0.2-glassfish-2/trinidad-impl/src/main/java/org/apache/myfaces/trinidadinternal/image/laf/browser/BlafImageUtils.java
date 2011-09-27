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
package org.apache.myfaces.trinidadinternal.image.laf.browser;


import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageType;
import org.apache.myfaces.trinidadinternal.image.ImageTypeManager;
import org.apache.myfaces.trinidadinternal.image.PropertyInstantiator;

import org.apache.myfaces.trinidadinternal.image.cache.CacheKeyFactory;
import org.apache.myfaces.trinidadinternal.image.cache.NameProvider;
import org.apache.myfaces.trinidadinternal.image.cache.PropertiesFilter;

import org.apache.myfaces.trinidadinternal.image.painter.ImmInsets;

import org.apache.myfaces.trinidadinternal.style.ParsedPropertyKey;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;


/**
 * Utilities for Browser Look And Feel support.
 * This class and everything in the image package needs to be deleted.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/BlafImageUtils.java#0 $) $Date: 10-nov-2005.19:05:04 $
 */
public class BlafImageUtils implements ImageConstants
{
  /**
   * Registers the BLAF image types.
   */
  public static void registerImageTypes(ImageTypeManager manager)
  {
    _registerDefaultType(manager, COLORIZED_ICON_NAME, false, false);
    _registerDefaultType(manager, FLIPPED_ICON_NAME, false, false);

    _registerDefaultType(manager, COMPOSITE_BUTTON_NAME, true, true);
  }

  static private void _registerDefaultType(
    ImageTypeManager manager,
    String name,
    boolean isLocalized,
    boolean hasPropertiesFilter
    )
  {
    _registerDefaultType(manager,
                         name,
                         isLocalized,
                         hasPropertiesFilter,
                         false);
  }

  static private void _registerDefaultType(
    ImageTypeManager manager,
    String name,
    boolean isLocalized,
    boolean hasPropertiesFilter,
    boolean checkSource
    )
  {
    // Register the type
    ImageType type =
      manager.registerImageType(
        ImageConstants.TECATE_NAMESPACE,
        name,
        null
        );

    char initialCap = Character.toUpperCase(name.charAt(0));
    String baseName = initialCap + name.substring(1);

    String rendererName =
      "org.apache.myfaces.trinidadinternal.image.laf.browser." + baseName + "ImageRenderer";

    // Initialize renderer name
    type.setProperty(ImageType.IMAGE_RENDERER_NAME_PROPERTY, rendererName);

    // Initialize the renderer
    type.setProperty(
      ImageType.IMAGE_RENDERER_PROPERTY,
      new PropertyInstantiator(rendererName));

    // Initialize encoder
    type.setProperty(
      ImageType.XML_ENCODER_PROPERTY,
      new PropertyInstantiator(
        "org.apache.myfaces.trinidadinternal.image.xml.encode." + baseName + "Encoder"
      ));

    // FileSystemImageCache properties
    type.setProperty(
      CacheKeyFactory.CACHE_KEY_FACTORY_PROPERTY,
      new PropertyInstantiator(
        "org.apache.myfaces.trinidadinternal.image.cache." + baseName + "KeyFactory"
      ));

    type.setProperty(
      NameProvider.NAME_PROVIDER_PROPERTY,
      new PropertyInstantiator(
        "org.apache.myfaces.trinidadinternal.image.cache." + baseName + "NameProvider"
      ));


    if (hasPropertiesFilter)
    {
      type.setProperty(
        PropertiesFilter.PROPERTIES_FILTER_PROPERTY,
        new PropertyInstantiator(
          "org.apache.myfaces.trinidadinternal.image.cache." + baseName + "PropertiesFilter"
        ));
    }

    // Initialize localized property
    type.setProperty(
      ImageType.LOCALIZED_PROPERTY,
      isLocalized ? Boolean.TRUE : Boolean.FALSE
      );

    // Initialize check source property
    type.setProperty(
      ImageType.CHECK_SOURCE_PROPERTY,
      checkSource ? Boolean.TRUE : Boolean.FALSE
      );
  }

  // Utility methods for org.apache.myfaces.trinidadinternal.image.laf.browser

  // Returns the foreground Color from the specified style
  static Color __getForeground(
    ImageContext context,
    String       styleClass
    )
  {
    return _getColor(context, styleClass, CoreStyle.FOREGROUND_KEY, false);
  }

  // Returns the foreground Color from the specified style
  static Color __getBackground(
    ImageContext context,
    String       styleClass
    )
  {
    return _getColor(context, styleClass, CoreStyle.BACKGROUND_KEY, false);
  }

  // Returns the foreground Color from the specified style
  static Color __getNamedForeground(
    ImageContext context,
    String       styleName
    )
  {
    return _getColor(context, styleName, CoreStyle.FOREGROUND_KEY, true);
  }

  // Returns the foreground Color from the specified style
  static Color __getNamedBackground(
    ImageContext context,
    String       styleName
    )
  {
    return _getColor(context, styleName, CoreStyle.BACKGROUND_KEY, true);
  }

  // Get a mnemonic index given a String and access char.
  // Returns -1 if the mnemonic does not appear in the string
  static int __getMnemonicIndex(String text, char accessChar)
  {
    if (text == null)
      return -1;

    char flippedChar = Character.isLowerCase(accessChar) ?
                         Character.toUpperCase(accessChar) :
                         Character.toLowerCase(accessChar);

    for (int i = 0; i < text.length(); i++)
    {
      char c = text.charAt(i);
      if ((c == accessChar) || (c == flippedChar))
        return i;
    }

    return -1;
  }

  // Returns a darker shade of the specified color
  static Color __getDarkerColor(Color color)
  {
    // For now, we just use a brain-dead algorithm.  Kick each component
    // up a notch in the web safe color ramp.
    return new Color(_getDarkerColorComponent(color.getRed()),
                     _getDarkerColorComponent(color.getGreen()),
                     _getDarkerColorComponent(color.getBlue()));
  }

  // Returns a lighter shade of the specified color
  static Color __getLighterColor(Color color)
  {
    // For now, we just use a brain-dead algorithm.  Kick each component
    // up a notch in the web safe color ramp.
    return new Color(_getLighterColorComponent(color.getRed()),
                     _getLighterColorComponent(color.getGreen()),
                     _getLighterColorComponent(color.getBlue()));
  }

  // Returns a derived shade of the specified color
  static Color __getDerivedColor(
    Color color,
    int offsetRed,
    int offsetGreen,
    int offsetBlue
    )
  {
    return new Color(_getDerivedColorComponent(color.getRed(), offsetRed),
                     _getDerivedColorComponent(color.getGreen(), offsetGreen),
                     _getDerivedColorComponent(color.getBlue(), offsetBlue));
  }

  // Computes the padding above/below the English glyphs
  // in the Dialog font.  We need to compute this information
  // because the information provided by FontMetrics is not
  // always quite right.  Our ImageRenderers need to ensure
  // hat there is always a specific margin above the tallest letter
  // and below the lowest letter.  It seems that the only way to
  // do this is to actually examine the glyphs rendered by the
  // font to find the precise padding.
  static ImmInsets __getDialogPadding()
  {
    if (_sDialogPadding != null)
      return _sDialogPadding;

    // Create a BufferedImage that we can use to rasterize some glyphs.
    int width = 40;
    int height = 40;
    BufferedImage image = new BufferedImage(40,
                                            40,
                                            BufferedImage.TYPE_INT_ARGB);

    // Get the Graphics object to use to draw into the image
    Graphics g = image.getGraphics();

    // Clear out the image
    g.setColor(Color.white);
    g.fillRect(0, 0, width, height);

    // Render our glyphs
    g.setColor(Color.black);
    g.setFont(new Font("Dialog", Font.PLAIN, 12));

    FontMetrics metrics = g.getFontMetrics();
    int baseline = metrics.getAscent();

    g.drawString("X", 0, baseline);

    // Now that we have rendered the glyphs, we examine the
    // image to see how many lines of padding we've got.
    int top = 0;
    for (int y = 0; y < height; y++)
    {
      if (!_isWhiteScanline(image, y, width))
      {
        top = y;
        break;
      }
    }

    // Just use the descent as the bottom padding
    int bottom = metrics.getDescent();

    _sDialogPadding = new ImmInsets(top, 0, bottom, 0);


    // Clean up
    g.dispose();
    image.flush();

    return _sDialogPadding;
  }

  // Tests whether the scanline at y contains all white pixels.
  private static boolean _isWhiteScanline(
    BufferedImage image,
    int           y,
    int           width
    )
  {
    for (int x = 0; x < width; x++)
    {
      if ((image.getRGB(x, y) & 0x00ffffff) != 0x00ffffff)
        return false;
    }

    return true;
  }

  // Gets a darker shape for the specified color component
  private static int _getDarkerColorComponent(int component)
  {
    int darker = component - 51;
    if (darker < 0)
      darker = 0;

    return darker;
  }

  // Gets a darker shape for the specified color component
  private static int _getLighterColorComponent(int component)
  {
    int lighter = component + 51;
    if (lighter > 255)
      lighter = 255;

    return lighter;
  }

  // Gets a derived color component for the specified color component
  private static int _getDerivedColorComponent(int component, int offset)
  {
    int derivedComponent = component + offset;
    if (derivedComponent > 255)
      derivedComponent = 255;
    else if ( derivedComponent < 0 )
      derivedComponent = 0;

    return derivedComponent;
  }


  // Looks up a color in the style map
  private static Color _getColor(
    ImageContext context,
    String       styleID,
    ParsedPropertyKey key,
    boolean      isNamed
    )
  {
    return null;
  }

  // Default Color values used by BLAF ImageRenderers
  static final Color __VERY_DARK_COLOR = new Color(0, 51, 102);
  static final Color __DARK_COLOR = new Color(51, 102, 153);
  static final Color __MEDIUM_COLOR = new Color(102, 153, 204);
  static final Color __LIGHT_COLOR = new Color(153, 204, 255);
  static final Color __VERY_DARK_SHADOW_ACCENT_COLOR = new Color(51, 51, 0);
  static final Color __DARK_SHADOW_ACCENT_COLOR = new Color(102, 102, 51);
  static final Color __VERY_DARK_ACCENT_COLOR = new Color(153, 153, 102);
  static final Color __DARK_ACCENT_COLOR = new Color(204, 204, 153);
  static final Color __MEDIUM_ACCENT_COLOR = new Color(255, 255, 204);
  static final Color __LIGHT_ACCENT_COLOR = new Color(247, 247, 231);
  static final Color __TEXT_FOREGROUND_COLOR = new Color(0, 0, 0);
  static final Color __TEXT_BACKGROUND_COLOR = new Color(255, 255, 255);

  //  private static final String _MAP_ERROR =
  //    "BlafImageUtils: Could not get StyleMap from ImageContext";
  private static final String _STYLE_ERROR =
    "BlafImageUtils: Could not get Style for style class: ";

  // Top/bottom padding for Dialog font
  private static ImmInsets _sDialogPadding;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BlafImageUtils.class);
}
