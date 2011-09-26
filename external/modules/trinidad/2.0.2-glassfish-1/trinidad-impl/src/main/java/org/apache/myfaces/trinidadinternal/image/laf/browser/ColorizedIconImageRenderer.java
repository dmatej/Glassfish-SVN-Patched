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
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.awt.image.RGBImageFilter;
import java.util.Map;


import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageRenderer;
import org.apache.myfaces.trinidadinternal.image.painter.ImageLoader;
import org.apache.myfaces.trinidadinternal.image.painter.ImageUtils;

import org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ImageRenderer for colorized icons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/ColorizedIconImageRenderer.java#0 $) $Date: 10-nov-2005.19:05:06 $
 */
public class ColorizedIconImageRenderer implements ImageRenderer
{
  /**
   * Implementation of ImageRenderer.renderImage().
   */
  public Image renderImage(
    ImageContext context,
    Map<Object, Object> requestedProperties,
    Map<Object, Object> responseProperties
    )
  {
    // Make sure we've got a graphical environment before we try rendering.
    if (!GraphicsUtils.isGraphicalEnvironment())
      return null;

    Image icon = SourceUtils.getSourceIcon(context, requestedProperties);

    if (icon == null)
    {
      assert false;
      return null;
    }

    // Get the colorizing filter
    int[] targetColors = _getTargetColors(requestedProperties);
    ImageFilter filter = new ColorizingFilter(_SOURCE_COLORS, targetColors);

    Toolkit toolkit = Toolkit.getDefaultToolkit();
    ImageProducer producer = icon.getSource();

    // If direction is RTL, flip the source image
    if (_isRightToLeft(context))
      producer = new FilteredImageSource(producer, new MirrorImageFilter());

    Image colorizedIcon =
      toolkit.createImage(new FilteredImageSource(producer, filter));

    // Be sure that the image is fully loaded
    ImageUtils.loadImage(colorizedIcon);

    // Get the width/height.  We use an ImageLoader object the ImageObserver
    // just for the heck of it.
    ImageLoader loader = new ImageLoader(icon);
    int width = icon.getWidth(loader);
    int height = icon.getHeight(loader);

    // Store width/height for client
    if (width != -1)
    {
      responseProperties.put(ImageConstants.WIDTH_RESPONSE_KEY,
                             width);
    }

    if (height != -1)
    {
      responseProperties.put(ImageConstants.HEIGHT_RESPONSE_KEY,
                             height);
    }

    // This very strange code is here to work around problems
    // with colorized icon generation in which garbage pixels are
    // randomly generated for no apparent reason.  (See bug 1398379.)
    // The idea here is that by copying the colorized data into a
    // BufferedImage, we remove the colorization filter from the
    // image encoding pipeline.  That is, when we go to encode the
    // image using an ImageEncoder, the colorized image data will be
    // available in the BufferedImage's data buffer.  Otherwise, it
    // seems as if color filtering gets re-run during image encoding.
    // Admittedly, we aren't really fixing the underlying problem here -
    // the image encoding pipeline needs to be examined.  However, this
    // small change seems to reduce the likelihood of garbage pixels
    // showing up in our colorized images, so what the heck.
    BufferedImage bufferedIcon = new BufferedImage(width,
                                                   height,
                                                BufferedImage.TYPE_4BYTE_ABGR);
    Graphics g = bufferedIcon.getGraphics();

    // Fill in the surrounding color first
    Color surroundingColor = (Color)requestedProperties.get(
                                      ImageConstants.SURROUNDING_COLOR_KEY);
    if (surroundingColor != null)
    {
      g.setColor(surroundingColor);
      g.fillRect(0, 0, width, height);
    }

    g.drawImage(colorizedIcon, 0, 0, loader);

    // Free up resources used by any images we created
    icon.flush();
    colorizedIcon.flush();

    return bufferedIcon;
  }

  // Tests whether the requested direction is RTL
  private boolean _isRightToLeft(
    ImageContext context
    )
  {
    return context.getLocaleContext().isRightToLeft();
  }

  // Returns the target colors for a colorization based on the
  // specified context
  private static int[] _getTargetColors(
    Map<Object, Object> properties
    )
  {
    // We derive the colors for the two color ramps based on the
    // DARK_COLOR_KEY and DARK_ACCENT_COLOR_KEY values.  Get these
    // values from the request...
    Color darkColor = BlafImageUtils.__DARK_COLOR;
    Color darkAccentColor = BlafImageUtils.__DARK_ACCENT_COLOR;

    Color color = (Color)properties.get(ImageConstants.DARK_COLOR_KEY);
    if (color != null)
      darkColor = color;

    color = (Color)properties.get(ImageConstants.DARK_ACCENT_COLOR_KEY);
    if (color != null)
      darkAccentColor = color;

    // Now, derive the rest of the colors in both ramps
    Color veryDarkColor = BlafImageUtils.__getDarkerColor(darkColor);
    Color mediumColor = BlafImageUtils.__getLighterColor(darkColor);
    Color lightColor = BlafImageUtils.__getLighterColor(mediumColor);
    Color veryDarkAccentColor = BlafImageUtils.__getDarkerColor(
                                                   darkAccentColor);
    Color mediumAccentColor = BlafImageUtils.__getLighterColor(
                                                   darkAccentColor);

    // The light accent color is derived but with wacky hardcoded offsets
    //The new offset for AFLightAccentBackground in
    //oracle-desktop.xss is +#2b2b4e (43, 43, 78) to +#251f37 (37, 31, 55)
    // @todo. Derive these values from the stylesheet instead of hardcoding
    Color lightAccentColor =
            BlafImageUtils.__getDerivedColor(darkAccentColor, 37, 31, 55);

    int[] targetColors = new int[_SOURCE_COLORS.length];
    targetColors[0] = (veryDarkColor.getRGB() & 0x00ffffff);
    targetColors[1] = (darkColor.getRGB() & 0x00ffffff);
    targetColors[2] = (mediumColor.getRGB() & 0x00ffffff);
    targetColors[3] = (lightColor.getRGB() & 0x00ffffff);
    targetColors[4] = (veryDarkAccentColor.getRGB() & 0x00ffffff);
    targetColors[5] = (darkAccentColor.getRGB() & 0x00ffffff);
    targetColors[6] = (mediumAccentColor.getRGB() & 0x00ffffff);
    targetColors[7] = (lightAccentColor.getRGB() & 0x00ffffff);

    return targetColors;
  }

  // An ImageFilter which colorizes a set of source colors to a
  // set of target colors
  private static class ColorizingFilter extends RGBImageFilter
  {
    public ColorizingFilter(int[] sourceColors, int[] targetColors)
    {
      if (sourceColors.length != targetColors.length)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "DIFFERENT_LENGTHS_SOURCECOLORS_TARGETCOLORS"));
      }

      _sourceColors = new int[sourceColors.length];
      System.arraycopy(sourceColors, 0, _sourceColors, 0, sourceColors.length);

      _targetColors = new int[targetColors.length];
      System.arraycopy(targetColors, 0, _targetColors, 0, targetColors.length);

      // We can filter the color table
      canFilterIndexColorModel = true;
    }

    @Override
    public int filterRGB(int x, int y, int rgb)
    {
      // First, see if we can short circuit
      synchronized (this)
      {
        if (_last == rgb)
          return _lastValue;
      }

      int source = (rgb & 0x00ffffff);
      int alpha =  (rgb & 0xff000000);

      for (int i = 0; i < _sourceColors.length; i++)
      {
        if (source == _sourceColors[i])
        {
          int target = _targetColors[i] | alpha;

          if (_last != rgb)
          {
             synchronized (this)
             {
               _last = rgb;
               _lastValue = target;
             }
          }

          return target;
        }
      }

      return rgb;
    }

    private final int[] _sourceColors;
    private final int[] _targetColors;

    // We track the last key/value for performance
    private int _last;
    private int _lastValue;
  }

  // The colors in the source images which we colorize.
  private static final int[] _SOURCE_COLORS = new int[]
  {
    0x003366, // VeryDark
    0x336699, // Dark
    0x6699cc, // Medium
    0x99ccff, // Light
    0x999966, // VeryDarkAccent
    0xcccc99, // DarkAccent
    0xffffcc, // MediumAccent
    0xf7f7e7, // LightAccent
  };
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ColorizedIconImageRenderer.class);
}
