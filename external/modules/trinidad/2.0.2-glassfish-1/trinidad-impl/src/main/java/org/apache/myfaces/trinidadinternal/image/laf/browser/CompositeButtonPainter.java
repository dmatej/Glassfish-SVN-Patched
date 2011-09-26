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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.image.ImageObserver;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.painter.AbstractBorderPainter;
import org.apache.myfaces.trinidadinternal.image.painter.AdjustableBorderPainter;
import org.apache.myfaces.trinidadinternal.image.painter.ColorChange;
import org.apache.myfaces.trinidadinternal.image.painter.ImmInsets;
import org.apache.myfaces.trinidadinternal.image.painter.OffscreenWrappingPainter;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContext;
import org.apache.myfaces.trinidadinternal.image.painter.Painter;
import org.apache.myfaces.trinidadinternal.image.painter.TextPainter;
import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

/**
 * Painter used to paint composite button objects with the correct Oracle Look
 * and Feel.
 * The values of the following keys from
 * the ImageConstants interface affect how the button is rendered:
 * <ul>
 * <li>TEXT_KEY
 * <li>FOREGROUND_KEY
 * <li>BACKGROUND_KEY
 * <li>FONT_KEY
 * <li>TEXT_ANTIALIAS_KEY
 * <li>DISABLED_KEY
 * <li>START_ROUNDED_KEY
 * <li>END_ROUNDED_KEY
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/CompositeButtonPainter.java#0 $) $Date: 10-nov-2005.19:05:08 $
 */
public class CompositeButtonPainter extends AbstractBorderPainter
  implements ImageConstants
{
  /**
   * Creates an CompositeButtonPainter.
   */
  public CompositeButtonPainter()
  {
    super(
      new AdjustableBorderPainter(
        new DisabledColorChange(
          new OffscreenWrappingPainter(
            new TextPainter(TEXT_KEY)
            )),
        0,
        _TEXT_START_MARGIN,
        0,
        _TEXT_END_MARGIN,
        "ButtonServerPadding"
        ));
  }

  @Override
  protected ImmInsets getOwnInsets(
    PaintContext context
    )
  {
    boolean isRTL = _isRightToLeft(context);

    Object leftKey = (isRTL) ? _END_IMAGE_KEY : _START_IMAGE_KEY;
    Object rightKey = (isRTL) ? _START_IMAGE_KEY : _END_IMAGE_KEY;

    Dimension leftSize = _getImageSize(context, leftKey);
    Dimension rightSize = _getImageSize(context, rightKey);
    Dimension topSize = _getImageSize(context, _TOP_BACKGROUND_IMAGE_KEY);
    Dimension bottomSize = _getImageSize(context,
                                         _BOTTOM_BACKGROUND_IMAGE_KEY);

    // Need to get height of wrapped contents in order to figure out
    // top/bottom insets.
    Painter wrappedPainter = getWrappedPainter(context);
    Dimension innerSize = wrappedPainter.getPreferredSize(context);

    int topInset = topSize.height;
    int bottomInset = bottomSize.height;

    int innerHeight = innerSize.height + topInset + bottomInset;
    int maxHeight = _getMaxHeight(leftSize,
                                  rightSize,
                                  innerHeight);

    // If our start/end/background images are
    // taller than the inner contents, then
    // we need to adjust our insets to make
    // room for the taller images.
    if (maxHeight > innerHeight)
    {
      int vertInsets = (maxHeight - innerHeight);
      int halfInsets = vertInsets / 2;

      // Divy up the vertical insets between top and bottom
      topInset += halfInsets;
      bottomInset += halfInsets;

      // If we've got an extra pixel, tack it on the top
      if ((vertInsets % 2) != 0)
        topInset++;
    }

    return new ImmInsets(topInset,
                         leftSize.width,
                         bottomInset,
                         rightSize.width);
  }

  @Override
  protected void paintBorder(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height)
  {
    boolean isRTL = _isRightToLeft(context);

    Object leftKey = (isRTL) ? _END_IMAGE_KEY : _START_IMAGE_KEY;
    Object rightKey = (isRTL) ? _START_IMAGE_KEY : _END_IMAGE_KEY;

    Image leftImage = _getImage(context, leftKey);
    Image rightImage = _getImage(context, rightKey);
    Image topBackgroundImage = _getImage(context, _TOP_BACKGROUND_IMAGE_KEY);
    Image bottomBackgroundImage = _getImage(context,
                                            _BOTTOM_BACKGROUND_IMAGE_KEY);

    int leftWidth = 0;
    int rightWidth = 0;
    int topHeight = 0;
    int bottomHeight = 0;

    if (leftImage != null)
    {
      Dimension leftSize = _getImageSize(context, leftImage);
      leftWidth = leftSize.width;

      g.drawImage(leftImage,
                  x,
                  y,
                  leftWidth,
                  height,
                  context.getImageObserver());
    }

    if (rightImage != null)
    {
      // Position the right image
      Dimension rightSize = _getImageSize(context, rightImage);
      rightWidth = rightSize.width;
      int rightX = (x + width - rightWidth);

      g.drawImage(rightImage,
                  rightX,
                  y,
                  rightWidth,
                  height,
                  context.getImageObserver());
    }

    // Render top/bottom background images
    int x1 = x + leftWidth;
    int x2 = x + width - rightWidth;

    if (topBackgroundImage != null)
    {
      topHeight = _getImageSize(context, topBackgroundImage).height;

      // Tile the background image
      for (int x0 = x1; x0 < x2; x0++)
      {
        g.drawImage(topBackgroundImage,
                    x0,
                    y,
                    context.getImageObserver());
      }
    }


    if (bottomBackgroundImage != null)
    {
      bottomHeight = _getImageSize(context, bottomBackgroundImage).height;

      // Tile the background image
      for (int x0 = x1; x0 < x2; x0++)
      {
        Dimension bottomSize = _getImageSize(context, bottomBackgroundImage);

        g.drawImage(bottomBackgroundImage,
                    x0,
                    y + height - bottomSize.height,
                    context.getImageObserver());
      }
    }

    _fillBackground(context,
                    g,
                    x, y,
                    width, height,
                    leftWidth, rightWidth,
                    topHeight, bottomHeight);
  }

  public static Color getDefaultForeground(
    ImageContext context,
    boolean      disabled
    )
  {
    // First, try to get the color from the StyleMap
    String styleName = (disabled) ? _DISABLED_STYLE_NAME : _STYLE_NAME;
    Color color = BlafImageUtils.__getNamedForeground(context, styleName);

    // If we didn't get a color from the style, just use defaults
    if (color == null)
      color = (disabled) ? _DEFAULT_DISABLED_FOREGROUND : _DEFAULT_FOREGROUND;

    return color;
  }

  public static Color getDefaultBackground(
    ImageContext context,
    boolean     disabled
    )
  {
    // First, try to get the color from the StyleMap
    String styleName = (disabled) ? _DISABLED_STYLE_NAME : _STYLE_NAME;
    Color color = BlafImageUtils.__getNamedBackground(context, styleName);

    // If we didn't get a color from the style, just use defaults
    if (color == null)
      color = _DEFAULT_BACKGROUND;

    return color;
  }

  public static FontProxy getDefaultFont()
  {
    return _DEFAULT_FONT_PROXY;
  }

  // Tests whether we're painting a disabled button
  static boolean _isDisabled(PaintContext context)
  {
    return ((context.getPaintState() & PaintContext.STATE_DISABLED) != 0);
  }

  private boolean _isRightToLeft(PaintContext context)
  {
    return (context.getReadingDirection() ==
             LocaleUtils.DIRECTION_RIGHTTOLEFT);
  }

  // Retrieves the Image object using the specified key
  private static Image _getImage(
    PaintContext context,
    Object       key
    )
  {
    return (Image)context.getPaintData(key);
  }

  private Dimension _getImageSize(
    PaintContext context,
    Object       key
    )
  {
    Object data = context.getPaintData(key);

    if (!(data instanceof Image))
      return new Dimension(0,0);

    return _getImageSize(context, (Image)data);
  }

  private Dimension _getImageSize(
    PaintContext context,
    Image        image
    )
  {
    if (image == null)
      return new Dimension(0,0);

    ImageObserver observer = context.getImageObserver();

    return new Dimension(image.getWidth(observer),
                         image.getHeight(observer));
  }

  // Figure out the maximum height for the button
  private int _getMaxHeight(
    Dimension startSize,
    Dimension endSize,
    int       innerHeight
    )
  {
    int maxHeight = innerHeight;

    if (startSize.height > maxHeight)
      maxHeight = startSize.height;
    if (endSize.height > maxHeight)
      maxHeight = endSize.height;

    return maxHeight;
  }

  private void _fillBackground(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height,
    int          startWidth,
    int          endWidth,
    int          topHeight,
    int          bottomHeight
    )
  {
    Color oldBackground = g.getColor();
    Color background = context.getPaintBackground();
    g.setColor(background);

    g.fillRect(x + startWidth,
               y + topHeight,
               width - (startWidth + endWidth),
               height - (topHeight + bottomHeight));

    g.setColor(oldBackground);
  }

  private static class DisabledColorChange extends ColorChange
  {
    public DisabledColorChange(Painter wrappedPainter)
    {
      super(wrappedPainter);
    }

    @Override
    protected Color getColor(PaintContext context)
    {
      // If the foreground is explicitly set, just use it
      Object foreground = context.getPaintData(ImageConstants.FOREGROUND_KEY);
      if (foreground != null)
        return (Color)foreground;

      // Otherwise, use the default foreground
      return getDefaultForeground(context.getImageContext(),
                                  _isDisabled(context));
    }
  }

  // Style class names for button styles
  private static final String _STYLE_NAME =
    "AFButtonServerText";
  private static final String _DISABLED_STYLE_NAME =
    "AFButtonServerTextDisabled";

  private static final FontProxy _DEFAULT_FONT_PROXY =
    new FontProxy("Dialog", Font.PLAIN, 12);

  // Default colors
  private static final Color _DEFAULT_FOREGROUND =
    BlafImageUtils.__TEXT_FOREGROUND_COLOR;
  private static final Color _DEFAULT_DISABLED_FOREGROUND =
    BlafImageUtils.__VERY_DARK_ACCENT_COLOR;
  private static final Color _DEFAULT_BACKGROUND =
    BlafImageUtils.__LIGHT_ACCENT_COLOR;

  private static final int _TEXT_START_MARGIN  = 2;
  private static final int _TEXT_END_MARGIN    = 3;

  // Some aliases for the image key names
  private Object _START_IMAGE_KEY =
    PaintContext.BUTTON_START_IMAGE_KEY;
  private Object _END_IMAGE_KEY =
    PaintContext.BUTTON_END_IMAGE_KEY;
  private Object _TOP_BACKGROUND_IMAGE_KEY =
    PaintContext.BUTTON_TOP_BACKGROUND_IMAGE_KEY;
  private Object _BOTTOM_BACKGROUND_IMAGE_KEY =
    PaintContext.BUTTON_BOTTOM_BACKGROUND_IMAGE_KEY;

}

