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

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import java.awt.image.ImageObserver;

import java.util.Locale;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * PaintContext that delegates all PaintContext calls to the PaintContext
 * returned by the abstract method getPaintContext().  This class is used
 * in situations where an Object already has a PaintCOntext but wishes
 * to override a few of that PaintContext's methods.
 * <p>
 * @see #getPaintContext
 * @see PaintContext
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/PaintContextProxy.java#0 $) $Date: 10-nov-2005.19:05:02 $
 */
public abstract class PaintContextProxy implements PaintContext
{
  /**
   * Create a PaintContextProxy.
   */
  public PaintContextProxy()
  {
  }


  /**
   * Sets a property on the response dictionary
   */
  public void setResponseProperty(Object key, Object value)
  {
    getPaintContext().setResponseProperty(key, value);
  }

  // =-=ags Added for TextPainter???
  public Graphics getPaintGraphics()
  {
    return getPaintContext().getPaintGraphics();
  }

  /**
   * Returns a mask for the current state.
   * <p>
   * @return A bitmask indicating the current state of the PaintContext.
   */
  public int getPaintState()
  {
    return getPaintContext().getPaintState();
  }


  /**
   * Returns the color scheme to used when painting in this PaintContext.
   * <p>
   * @return the color scheme to use.
   */
  public ImageContext getImageContext()
  {
    return getPaintContext().getImageContext();
  }


  /**
   * Returns the foreground color of the PaintContext.
   * <p>
   * @return The foreground color of the PaintContext.
   */
  public Color getPaintForeground()
  {
    return getPaintContext().getPaintForeground();
  }


  /**
   * Returns the background color of the PaintContext.
   * <p>
   * @return The background color of the PaintContext.
   */
  public Color getPaintBackground()
  {
    return getPaintContext().getPaintBackground();
  }

  /**
   * Returns the color surrounding the Object to be painted with the
   * PaintContext.  This color is typically used to determine the
   * UIDefaults object to associate with the Object painted with
   * <p>
   * @return The Color surrounding the Object to be painted.
   */
  public Color getSurroundingColor()
  {
    return getPaintContext().getSurroundingColor();
  }


  /**
   * Returns the font of the PaintContext.
   * <p>
   * @return The Font of the PaintContext.
   */
  public Font getPaintFont()
  {
    return getPaintContext().getPaintFont();
  }


  /**
   * Returns a font metrics object for the specified font.
   * <p>
   * @param font Font object to return FontMetrics of.
   * <p>
   * @return The FontMetrics of the specified Font.
   */
  public FontMetrics getFontMetrics(
    Font font
    )
  {
    return getPaintContext().getFontMetrics(font);
  }


  /**
   * Returns an image observer object that can be used to
   * monitor progress in loading images.
   * <p>
   * @return The ImageObserver of the PaintContext.
   */
  public ImageObserver getImageObserver()
  {
    return getPaintContext().getImageObserver();
  }


  /**
   * Returns the Locale of the PaintContext.
   * <p>
   * @return The Locale of the PaintContext.
   */
  public Locale getPaintLocale()
  {
    return getPaintContext().getPaintLocale();
  }


  /**
   * Returns the data Object for the specified key.
   * <p>
   * @param key The key specifying which data Object the PaintContext should
   *            return.
   * <p>
   * @return The data Object specified by <code>key</code> or <code>null</code>
   *         if no Object for that key is supported.
   */
  public Object getPaintData(
    Object key
    )
  {
    return getPaintContext().getPaintData(key);
  }


  /**
   * Returns the horizontal interior alignment of the PaintContext.
   * <p>
   * @return The horizontal interior alignment of the PaintContext as a
   *         floating point number where 0.0 should be interpreted as
   *         left aligned, 0.5 as center aligned, and 1.0 as right aligned.
   */
  public float getInteriorAlignmentX()
  {
    return getPaintContext().getInteriorAlignmentX();
  }


  /**
   * Returns the vertical interior alignment of the PaintContext.
   * <p>
   * @return The vertical interior alignment of the PaintContext as a
   *         floating point number where 0.0 should be interpreted as
   *         top aligned, 0.5 as center aligned, and 1.0 as bottom aligned.
   */
  public float getInteriorAlignmentY()
  {
    return getPaintContext().getInteriorAlignmentY();
  }

  /*
   * Returns the reading direction of the PaintContext.
   * <p>
   * @return The reading direction of the PaintContext.
   * <p>
   * @see org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils
   */
  public int getReadingDirection()
  {
    return getPaintContext().getReadingDirection();
  }

  /**
   * Subclasses should override this method to return the PaintContext
   * that the PaintContextProxy will delegate to.
   * <p>
   * @return The PaintContext that the PaintContextProxy will delegate to.
   */
  protected abstract PaintContext getPaintContext();
}

