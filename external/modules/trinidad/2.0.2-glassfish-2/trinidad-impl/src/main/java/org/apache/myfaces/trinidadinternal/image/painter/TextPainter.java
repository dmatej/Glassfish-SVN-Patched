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
package org.apache.myfaces.trinidadinternal.image.painter;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.text.AttributedString;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * A painter capable of painting a string using the alignment of its
 * PaintContext.  If the string contains newlines, multiple lines
 * of text will be painted.  <p>
 *
 * This class uses new APIs in Java2 and propertly works with
 * BufferedImages.<p>
 *
 * This TextPainter should be used for all String objects used in the
 * ImageGenerator.<p>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/TextPainter.java#1 $) $Date: 11-nov-2005.14:59:38 $
 */
public class TextPainter extends AbstractPainter
{
  /**
   * Creates a TextPainter object, using the default label data key.
   * and supporting mnemonics.
   */
  public TextPainter()
  {
    _init(PaintContext.LABEL_KEY, true);
  }


  /**
   * Creates a TextPainter object using the <code>dataKey</code> to get its
   * data and supporting mnemonics.
   * <p>
   * @param dataKey Key used by this TextPainter to retrieve the String
   *                this TextPainter paints.
   * <p>
   * @see PaintContext#getPaintData
   */
  public TextPainter(
    Object dataKey
    )
  {
    _init(dataKey, true);
  }


  /**
   * Creates a TextPainter object.
   * <p>
   * @param dataKey          Key used by this TextPainter to retrieve the
   *                         String this TextPainter paints.
   * @param supportMnemonics True if this TextPainter should ask its
   *                         Paint context for a mnemonic to paint.
   * <p>
   * @see PaintContext#getPaintData
   */
  public TextPainter(
    Object  dataKey,
    boolean supportMnemonics
    )
  {
    _init(dataKey, supportMnemonics);
  }

  /**
   * Returns the preferred size of the painter.
   * <p>
   * @param context Context for determining the preferred size.
   * @return The preferred size of the Painter.
   */
  @Override
  public Dimension getPreferredSize(
    PaintContext context
    )
  {
    return _getSize(context, true);
  }


  /**
   * Returns the minimum size of the painter.
   * <p>
   * @param context Context for determining the minimum size.
   * @return The minimum size of the Painter.
   */
  public Dimension getMinimumSize(
    PaintContext context
    )
  {
    return _getSize(context, false);
  }


  /**
   * Paints the TextPainter at the given location.
   * <p>
   * @param context Context for painting.
   * @param g Graphics object to draw into.
   * @param x X position to draw at.
   * @param y Y position to draw at.
   * @param width Width to draw into.
   * @param height Height to draw into.
   */
  public void paint(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height
    )
  {
    // We synchronize on the _FONT_LOCK object so that we only
    // render/measure one text String at a time.
    // This oddness is due to bug 1852105 (ANTIALIASED TEXT MAY
    // SOMETIMES APPEAR BOLD).  The problem seems to be due to
    // buggy behavior in how AWT maintains internal font state.
    // After rendering/measuring with a bold antialiased font,
    // subsequent rendering/measuring with the same font results
    // in bold appearance/metrics, even if the style is set to
    // Font.PLAIN.  To avoid this, we serialize all text
    // rendering/measuring, and explicitly force the font
    // to be reset before doing any rendering/measuring.
    // Note: This behavior has not yet been observed in JDK 1.4,
    // so hopefully this hack can be removed before long.
    synchronized (_FONT_LOCK)
    {
      // Reset the font state before we do anything.
      _resetFont(g);

      // Now it should be safe to get/use the FontMetrics,
    // as well as to paint the text.
    FontMetrics metrics = g.getFontMetrics();

    if (metrics != null)
    {
      String text = getStringData(context);

      if (text != null)
      {
        int textLength = text.length();

          if (textLength != 0)
          {
            float alignmentX = context.getInteriorAlignmentX();

            int numLines = _getNumberOfTextLines(text);

            //
            // adjust Y for vertical alignment
            //
            float alignmentY = context.getInteriorAlignmentY();

            if (alignmentY != Component.TOP_ALIGNMENT)
            {
              int stringHeight = numLines * metrics.getHeight();

              if (stringHeight < height)
              {
                y += (height - stringHeight) * alignmentY;
              }
            }

            // adjust y for baseline of text
            y += metrics.getAscent();


            //
            // get the mnemonic, if any
            //
            int mnemonicIndex = -1;

            if (_supportMnemonics)
            {
              Object mnemonic =
                            context.getPaintData(PaintContext.MNEMONIC_INDEX_KEY);

              if ((mnemonic != null) && (mnemonic instanceof Integer))
              {
                mnemonicIndex = ((Integer)mnemonic).intValue();
              }
            }


            //
            // Draw each line of text.  We treat single line of text that
            // end in a carraige return as multiple lines of text to make
            // sure that we don't pass a carraige return at the end of
            // a single line of text to paintText()
            //
            if ((numLines == 1) && (text.charAt(textLength - 1) != '\n'))
            {
              // all text on the same line
              paintText(context,
                        g,
                        metrics,
                        text,
                        x,
                        y,
                        width,
                        alignmentX,
                        mnemonicIndex);
            }
            else
            {
              int lastIndex;
              int currIndex = -1;
              int lineHeight = metrics.getHeight();

              do
              {
                lastIndex = currIndex + 1;

                if (lastIndex >= textLength)
                  break;

                currIndex = text.indexOf('\n', lastIndex);

                if (currIndex == -1)
                {
                  currIndex = textLength;
                }

                paintText(context,
                          g,
                          metrics,
                          text.substring(lastIndex, currIndex),
                          x,
                          y,
                          width,
                          alignmentX,
                          mnemonicIndex - lastIndex);

                // move down to next line
                y += lineHeight;

              } while (true);
            }
          }
        }
      }
    }
  }


  /**
   * This method is called for each line of text painted.  Given a line of text,
   * return the line of text to actually draw. Subclasses, that wish to modify
   * the text displayed should override this method.
   * <p>
   * @param context        Context for determining the text to paint.
   * @param text           Text data for this line.
   * @param metrics        FontMetrics to use to paint the text.
   * @param availableWidth Number of pixels available to paint the text.
   * <p>
   * @return The String to actually paint.
   */
  protected String getPaintText(
    PaintContext context,
    String       text,
    FontMetrics  metrics,
    int          availableWidth
    )
  {
    return text;
  }


  /**
   * Typesafe method to return the String to Paint.
   * Subclasses that do not use data keys to retrieve
   * their data should override this method, rather than
   * <code>getData()</code>, as overriding getStringData()
   * directly is more efficient.
   * <p>
   * @param context PaintContext to use to retrieve the TextPainter's
   *                String data.
   * <p>
   * @return The String to paint.
   * <p>
   * @see #getData
   */
  protected String getStringData(
    PaintContext context
    )
  {
    return (String)getData(context);
  }

  /**
   * Returns the String to use for the minimum size calculation. This allows
   * subclasses to create TextPainters that use different strings for
   * calculating their minimum sizes than they do for actually displaying
   * the current value.  The default implementation is to use the current
   * value.
   */
  protected String getMinimumStringData(
    PaintContext context
    )
  {
    return getStringData(context);
  }


  @Override
  protected Object getData(
    PaintContext context
    )
  {
    Object data = super.getData(context);

    if (data != null)
    {
      return data.toString();
    }
    else
    {
      return null;
    }
  }


  @Override
  protected Object getDataKey()
  {
    return _dataKey;
  }


  private Dimension _getSize(
    PaintContext context,
    boolean      usePreferredSize
    )
  {
    int width  = 0;
    int height = 0;

    Font font = context.getPaintFont();
    Graphics g = context.getPaintGraphics();

    if (font != null)
    {
      String      text    = (usePreferredSize
                              ? getStringData(context)
                              : getMinimumStringData(context));

      // Due to bug 1852105 (ANTIALIASED TEXT MAY SOMETIMES APPEAR BOLD),
      // we synchronize on the _FONT_LOCK object to serialize all text
      // rendering/measuring.  See comment in paint() method above for
      // more details.
      synchronized (_FONT_LOCK)
      {
        // Reset the font state before we do anything.
        _resetFont(g);

        // Now it should be safe to get/use the FontMetrics
        FontMetrics metrics = context.getFontMetrics(font);
        if ((metrics != null) && (text != null))
        {
          int numLines = _getNumberOfTextLines(text);

          if (numLines == 1)
          {
            text = StringUtils.getDisplayString(text, context);

            // get the string width
            width = _getStringWidth(text, metrics, g);
          }
          else
          {
            width = _getMaxLineWidth(context, text, metrics, g);
          }

          height = (numLines * metrics.getHeight()) - metrics.getLeading();
        }
      }
    }

    return new Dimension(width, height);
  }


  /**
   * Computer the number of lines of text that the passed in text contains
   */
  private static final int _getNumberOfTextLines(
    String text
    )
  {
    int numLines = 0;
    int currIndex;
    int foundIndex = -1;
    int textLength = text.length();

    do
    {
      currIndex = foundIndex + 1;

      if (currIndex == textLength)
        break;

      foundIndex = text.indexOf('\n', currIndex);
      numLines++;

    } while (foundIndex != -1);

    return numLines;
  }

  private static final int _getStringWidth(
    String       text,
    FontMetrics  metrics,
    Graphics g
    )
  {
    // =-=AEW Techically, this isn't correct.  We really should
    // be using AttributedString and TextLayout after setting up
    // the reading direction.  However, as long as we're only
    // ever getting the width of the _entire_ string, we should be OK.
    return (int)metrics.getStringBounds(text, g).getWidth();
  }

  private static final int _getMaxLineWidth(
    PaintContext context,
    String       text,
    FontMetrics  metrics,
    Graphics g
    )
  {
    int maxWidth = 0;
    int lastIndex;
    int currIndex = -1;
    int textLength = text.length();

    do
    {
      lastIndex = currIndex + 1;

      if (lastIndex >= textLength)
        break;

      currIndex = text.indexOf('\n', lastIndex);

      if (currIndex == -1)
        currIndex = textLength;

      String subtext = StringUtils.getDisplayString(
                                 text.substring(lastIndex, currIndex),
                                 context);

      // get the width of the string
      int currWidth = _getStringWidth(subtext, metrics, g);

      if (maxWidth < currWidth)
        maxWidth = currWidth;

    } while (true);

    return maxWidth;
  }


  protected int paintText(
    PaintContext context,
    Graphics     g,
    FontMetrics  metrics,
    String       text,
    int          x,
    int          y,
    int          availableWidth,
    float        alignmentX,
    int          mnemonicIndex
    )
  {
    // Convert the text to bidi
    text = StringUtils.getDisplayString(text, context);

    // get the text to paint
    String paintText = getPaintText(context, text, metrics, availableWidth);
    if (alignmentX != Component.LEFT_ALIGNMENT)
    {
      int stringWidth = _getStringWidth(paintText, metrics, g);

      if (stringWidth < availableWidth)
      {
        x += (availableWidth - stringWidth) * alignmentX;
      }
    }

    // paint the text.  Before drawing the text, make sure
    // we clue the system in on the default direction of the run -
    // we do _not_ want to use the direction of the first character
    // in this text.
    Graphics2D g2 = (Graphics2D) g;

    AttributedString as  = new AttributedString(paintText);
    as.addAttribute(TextAttribute.FONT, g2.getFont());

    Object direction;
    if (context.getReadingDirection() == LocaleUtils.DIRECTION_LEFTTORIGHT)
      direction = TextAttribute.RUN_DIRECTION_LTR;
    else
      direction = TextAttribute.RUN_DIRECTION_RTL;

    as.addAttribute(TextAttribute.RUN_DIRECTION, direction);

    g2.drawString(as.getIterator(), x, y);

    //
    // paint the mnemonic if necessary
    //
    if ((mnemonicIndex >= 0) && (mnemonicIndex < paintText.length()))
    {
      TextLayout layout = new TextLayout(as.getIterator(),
                                         g2.getFontRenderContext());

      TextHitInfo leading = TextHitInfo.leading(mnemonicIndex);
      TextHitInfo trailing = TextHitInfo.trailing(mnemonicIndex);

      Rectangle r =
        layout.getVisualHighlightShape(leading, trailing).getBounds();

      // draw a horizontal line under the specified character to indicate
      // that it is the mnemonic.
      int left = r.x + x;
      g.drawLine(left + 1,
                 y + 1,
                 left + r.width - 1,
                 y + 1);
    }

    return x;
  }


  private void _init(
    Object  dataKey,
    boolean supportMnemonics
    )
  {
    _dataKey          = dataKey;
    _supportMnemonics = supportMnemonics;
  }

  // This method is needed to work around
  // bug 1852105 (ANTIALIASED TEXT MAY SOMETIMES APPEAR BOLD).
  // To get AWT to pick up the right Font/FontMetrics, we
  // first reset the font on the Graphics object to a 1pt
  // version of the current font, and then get the font
  // metrics.  Then, we reset the Graphics object to use the
  // actual font that we want, at which point subsequent calls
  // to Graphics.getFontMetrics()/paintText() work correctly.
  private static void _resetFont(Graphics g)
  {
    Font font = g.getFont();

    // We set the font to be the same font family/style as
    // the current font, but a different size (1pt).
    g.setFont(new Font(font.getName(), font.getStyle(), 1));

    // Then, get the font metrics, this seems to reset the
    // graphics state.
    g.getFontMetrics();

    // Finally, reset the original font.  Now, if we get the
    // font metrics again or paint some text, the right font/
    // font metrics will be used.
    g.setFont(font);
  }

  private boolean _supportMnemonics;
  private Object _dataKey;

  // This lock is used to serialize all text rendering/measuring,
  // which is done to work around bug 1852105 (ANTIALIASED TEXT
  // MAY SOMETIMES APPEAR BOLD).
  private static final Object _FONT_LOCK = new Object();
}
