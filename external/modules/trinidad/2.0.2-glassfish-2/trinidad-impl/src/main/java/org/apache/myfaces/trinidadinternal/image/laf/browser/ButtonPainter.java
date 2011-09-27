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
 * Painter used to paint action button objects with the correct Oracle Look
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
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/ButtonPainter.java#0 $) $Date: 10-nov-2005.19:05:05 $
 */
public class ButtonPainter extends AbstractBorderPainter
  implements ImageConstants
{
  /**
   * Creates an ButtonPainter.
   */
  public ButtonPainter()
  {
    this(_TEXT_TOP_MARGIN, _TEXT_BOTTOM_MARGIN);
  }

  // This package private constructor is used by ButtonImageRenderer
  // which provides specialized margins for buttons which use the
  // Dialog virtual font
  ButtonPainter(int topMargin, int bottomMargin)
  {
    super(
      new AdjustableBorderPainter(
        new DisabledColorChange(
          new OffscreenWrappingPainter(
            new TextPainter(TEXT_KEY)
            )),
        topMargin,
        _TEXT_START_MARGIN,
        bottomMargin,
        _TEXT_END_MARGIN,
        "BLAFServerButtonPadding"
        ));
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

  public static Color getDefaultBorderColor(
    ImageContext context,
    boolean      disabled
    )
  {
    // First, try to get the color from the StyleMap
    Color color = BlafImageUtils.__getNamedBackground(context, _BORDER_STYLE_NAME);

    // If we didn't get a color from the style, just use defaults
    if (color == null)
      color = _DEFAULT_BORDER_COLOR;

    return color;
  }

  /**
   * Returns the insets of just the ButtonPainter.
   */
  @Override
  protected ImmInsets getOwnInsets(PaintContext context)
  {
    int start  = _START;
    int top    = _TOP;
    int end    = _END;
    int bottom = _BOTTOM;

    int height = _getHeight(context);
    int halfCurveWidth = _getCurveWidth(height)/2;

    // Compensate for left rounding
    if (_hasStartCurve(context))
      start = halfCurveWidth;

    // Compensate for right rounding
    if (_hasEndCurve(context))
      end = halfCurveWidth;


    if (context.getReadingDirection() == LocaleUtils.DIRECTION_RIGHTTOLEFT)
      return new ImmInsets(top, end, bottom, start);

    return new ImmInsets(top, start, bottom, end);
  }


  /**
   * Paint just the border of the ButtonPainter.
   */
  @Override
  protected void paintBorder(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height)
  {
    // =-=AEW This isn't used: is this a latent bug?
    //    Color savedColor = g.getColor();

    Color background = context.getPaintBackground();

    Color borderColor = _getBorderColor(context);

    Color darker = null;
    Color dark = null;
    Color light = null;

    if (_isDisabled(context))
    {
      darker = dark = light = BlafImageUtils.__getDarkerColor(borderColor);
    }
    else
    {
      light = BlafImageUtils.__getDarkerColor(borderColor);
      dark = BlafImageUtils.__getDarkerColor(light);
      darker = BlafImageUtils.__getDarkerColor(dark);
    }


    int startXOffset = 1;
    int endXOffset = 1;

    // Draw start
    if (_hasStartCurve(context))
    {
      startXOffset = _drawStartCurve(context,
                      g,
                      x, y,
                      width, height,
                      background,
                      light,
                      dark);
    }
    else
    {
      _drawStartLine(context, g, x, y, width, height, light);
    }

    // Draw end
    if (_hasEndCurve(context))
    {
      endXOffset = _drawEndCurve(context,
                    g,
                    x, y,
                    width , height,
                    background,
                    dark,
                    darker);
    }
    else
    {
      _drawEndLine(context, g, x, y, width, height, dark, darker);
    }



    // Fill in the background and draw top/bottom
    _fillBackground(context,
                    g,
                    x, y,
                    width, height,
                    startXOffset, endXOffset,
                    background);

    _drawTopLine(context,
                 g,
                 x, y,
                 width,
                 startXOffset, endXOffset,
                 light);

    _drawBottomLine(context,
                    g,
                    x, y,
                    width, height,
                    startXOffset, endXOffset,
                    borderColor,
                    dark, darker);

  }


  private int _drawEndCurve(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height,
    Color background,
    Color dark,
    Color darker
    )
  {


    int boxWidth = height/2 ;

    if (_isRightToLeft(context))
    {

      int tempX = x;

      g.setColor(darker);
      // draw right 1/2 of a diameter = heightcircle
      // filled with the shade color
      g.fillArc(tempX, y, boxWidth + 1, height, 195, 75);

      g.setColor(dark);
      g.fillArc(tempX, y, boxWidth + 1, height, 197, -112);

      g.setColor(background);
      // draw right 1/2 of a diameter = height - 2  circle
      // with center offset -1, -1 of previous circle
      // and filled with the background
      // no need to adjust origin, since offset and shrink
      // cancel each other
      g.fillArc(tempX + 2, y + 1, boxWidth, height-3, 90, 180);

    }
    else
    {
      // draw this circle at the end of the button.
      int tempX = (x + width - boxWidth - 1 );//- boxWidth - 4);

      g.setColor(darker);
      // draw right 1/2 of a diameter = heightcircle
      // filled with the shade color
      g.fillArc(tempX, y, boxWidth, height, -15, -75);


      g.setColor(dark);
      // draw right 1/2 of a diameter = heightcircle
      // filled with the shade color
      g.fillArc(tempX, y, boxWidth, height, -17, 112);

      g.setColor(background);
      // draw right 1/2 of a diameter = height - 2  circle
      // with center offset -1, -1 of previous circle
      // and filled with the background
      // no need to adjust origin, since offset and shrink
      // cancel each other
      g.fillArc(tempX, y + 1, boxWidth-1, height-3, 90, -180);

    }

    return boxWidth/2;
  }

  private void _drawEndLine(
   PaintContext context,
   Graphics g,
   int x,
   int y,
   int width,
   int height,
   Color dark,
   Color darker
   )
  {
    int innerX = x + 1;
    int outerX = x;

    if (!_isRightToLeft(context))
    {
      innerX = x + width - 2;
      outerX = x + width - 1;
    }

    // draw inner right line
    g.setColor(dark);
    g.drawLine(innerX, y+1, innerX, y + height-1);

    // draw outer right line
    g.setColor(darker);
    g.drawLine(outerX, y+1, outerX, y + height);
  }

  private int _drawStartCurve(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height,
    Color background,
    Color light,
    Color dark
    )
  {
    int boxWidth = height/2 ;

    if (_isRightToLeft(context))
    {
      int tempX = x + width - boxWidth - 1;

      // left bottom
      g.setColor(dark);
      g.fillArc(tempX ,
                y - 1,
                boxWidth ,
                height + 1,
                354, -84);

      // top left: centered on a circle of diameter = height, a wedge of
      // height 58.3%
      g.setColor(light);
      g.fillArc(tempX  , y, boxWidth, height + 1, 90, -102);

      // Draw the fill: the left half of a circle of diameter=height
      g.setColor(background);
      g.fillArc(tempX  , y + 1, boxWidth -1, height - 2, 90, -180);
    }
    else
    {
      int tempX = x;;

      // left bottom
      g.setColor(dark);
      g.fillArc(tempX, y -1, boxWidth + 1, height + 1, 186, 84);

      // top left: centered on a circle of diameter = height, a wedge of
      // height 58.3%
      g.setColor(light);
      g.fillArc(tempX, y, boxWidth + 1, height + 1, 90, 102);

      // Draw the fill: the left half of a circle of diameter=height
      g.setColor(background);
      g.fillArc(tempX + 1, y + 1, boxWidth, height - 2, 90, 180);


    }

    return boxWidth / 2;
  }

  private void _drawStartLine(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height,
    Color color
    )
  {
    g.setColor(color);

    if (_isRightToLeft(context))
      x = x + width - 1;

    // a simple vertical line.
    g.drawLine(x, y, x, y + height - 1);
  }

  private void _drawBottomLine(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height,
    int startXOffset,
    int endXOffset,
    Color light,
    Color dark,
    Color darker
    )
  {
    int innerX1;
    int innerX2;
    int outerX1;
    int outerX2;

    // We need one more point for shading purposes - the pixel
    // just inside the start of the bottom line.  We call this outerX3.
    int outerX3;

    if (_isRightToLeft(context))
    {
      innerX1 = x + endXOffset + 1;
      innerX2 = x + width - startXOffset + 1;
      outerX1 = x + width - startXOffset;
      outerX2 = x + endXOffset + 1;
      outerX3 = outerX1 - 1;
    }
    else
    {
      innerX1 = x + startXOffset - 2;
      innerX2 = x + width - endXOffset - 2;
      outerX1 = x + startXOffset - 1;
      outerX2 = x + width - endXOffset - 2;
      outerX3 = outerX1 + 1;
    }

    g.setColor(dark);
    g.drawLine(innerX1, y + height - 2, innerX2, y + height - 2);

    int y1 = y + height - 1;
    g.setColor(darker);
    g.drawLine(outerX1, y1, outerX2, y1);

    // Add some shading to the start of the bottom line
    g.setColor(light);
    g.drawLine(outerX1, y1, outerX1, y1);

    g.setColor(dark);
    g.drawLine(outerX3, y1, outerX3, y1);
  }

  private void _drawTopLine(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int startXOffset,
    int endXOffset,
    Color light
    )
  {
    g.setColor(light);

    if (_isRightToLeft(context))
    {
      g.drawLine(x + endXOffset, y, x + width - startXOffset - 1, y);
    }
    else
    {
      g.drawLine(x + startXOffset, y, x + width - endXOffset - 1, y);
    }
  }

  private void _fillBackground(
    PaintContext context,
    Graphics g,
    int x,
    int y,
    int width,
    int height,
    int startXOffset,
    int endXOffset,
    Color background
    )
  {
    g.setColor(background);

    if (_isRightToLeft(context))
    {
      g.fillRect(x + endXOffset + 1,
                 y + 1,
                 x + width - startXOffset - endXOffset - 1,
                 height - 2);

    }
    else
    {
      g.fillRect(x + startXOffset,
                 y + 1,
                 x + width - endXOffset - startXOffset - 1,
                 height - 2);
    }
  }

  private int _getHeight(PaintContext context)
  {
    // The curve width is proportional to the height of the button.
    // So, we first compute the height of the button.
    FontMetrics metrics = context.getFontMetrics(context.getPaintFont());
    int fontHeight = metrics.getHeight() - metrics.getLeading();
    int height =
      fontHeight +
      _TOP + _BOTTOM +
      _TEXT_TOP_MARGIN + _TEXT_BOTTOM_MARGIN;

    return height;
  }

  // Get the width of the curve for this button
  private int _getCurveWidth(int height)
  {
    // The curve width is about 3/4 of the height

    return (int)(height * 0.75);

  }

  // Returns the base border color
  private Color _getBorderColor(PaintContext context)
  {
    Color color = (Color)context.getPaintData(BORDER_COLOR_KEY);
    if (color != null)
      return color;

    return getDefaultBorderColor(context.getImageContext(),
                                 _isDisabled(context));

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

  private boolean _hasStartCurve(PaintContext context)
  {
    return !(Boolean.FALSE.equals(context.getPaintData(START_ROUNDED_KEY)));
  }

  private boolean _hasEndCurve(PaintContext context)
  {
    return !(Boolean.FALSE.equals(context.getPaintData(END_ROUNDED_KEY)));
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
    "BLAFServerButtonText";
  private static final String _DISABLED_STYLE_NAME =
    "BLAFServerButtonTextDisabled";
  private static final String _BORDER_STYLE_NAME =
    "AFDarkAccentBackground";

  private static final FontProxy _DEFAULT_FONT_PROXY =
    new FontProxy("Dialog", Font.PLAIN, 12);

  // Default colors
  private static final Color _DEFAULT_FOREGROUND =
    BlafImageUtils.__TEXT_FOREGROUND_COLOR;
  private static final Color _DEFAULT_DISABLED_FOREGROUND =
    BlafImageUtils.__VERY_DARK_ACCENT_COLOR;
  private static final Color _DEFAULT_BACKGROUND =
    BlafImageUtils.__LIGHT_ACCENT_COLOR;
  private static final Color _DEFAULT_BORDER_COLOR =
    BlafImageUtils.__DARK_ACCENT_COLOR;

  private static final int _TOP    = 1;
  private static final int _BOTTOM = 2;
  private static final int _START  = 2;
  private static final int _END    = 3;

  private static final int _TEXT_TOP_MARGIN    = 0;
  private static final int _TEXT_BOTTOM_MARGIN = 0;
  private static final int _TEXT_START_MARGIN  = 2;
  private static final int _TEXT_END_MARGIN    = 3;

}

