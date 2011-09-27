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
 * This interface provides painters and fills with enough context
 * to draw themselves.
 * <p>
 * @see Painter
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/PaintContext.java#0 $) $Date: 10-nov-2005.19:05:00 $
 */
public interface PaintContext
{
  /**
   * Key used with getPaintData() to retrieve the main Image Object, if any,
   * from the PaintContext.
   * <p>
   * @see #getPaintData
   */
  public final static Object IMAGE_KEY = "Image";

  /**
   * Key used with getPaintData() to retrieve the main ImageSet Object, if any,
   * from the PaintContext.
   * <p>
   * @see #getPaintData
   */
  public final static Object IMAGESET_KEY = "ImageSet";


  /**
   * Key used with getPaintData() to retrieve the main Icon Object, if any,
   * from the PaintContext.
   * <p>
   * <STRONG>Although this key is currently defined as a String, clients
   * should not depend on this.</STRONG>
   * <p>
   * @see #getPaintData
   */
  public final static Object ICON_KEY = "Icon";

  /**
   * Key used with getPaintData() to retrieve the label String Object, if any,
   * from the PaintContext.
   * <p>
   * <STRONG>Although this key is currently defined as a String, clients
   * should not depend on this.</STRONG>
   * <p>
   * @see #getPaintData
   */
  public final static Object LABEL_KEY = "Label";


  /**
   * Key used with getPaintData() to retrieve the mnemonic index, if any,
   * from the PaintContext.
   * <p>
   * <STRONG>Although this key is currently defined as a String, clients
   * should not depend on this.</STRONG>
   * <p>
   * @see #getPaintData
   */
  public final static Object MNEMONIC_INDEX_KEY = "Mnemonic";


  /**
   * Key used with getPaintData() to retrieve the buttonStart Image.
   * <p>
   * @see #getPaintData
   */
  public final static Object BUTTON_START_IMAGE_KEY = "buttonStartImage";

  /**
   * Key used with getPaintData() to retrieve the buttonEnd Image.
   * <p>
   * @see #getPaintData
   */
  public final static Object BUTTON_END_IMAGE_KEY = "buttonEndImage";

  /**
   * Key used with getPaintData() to retrieve the buttonTopBackground Image.
   * <p>
   * @see #getPaintData
   */
  public final static Object BUTTON_TOP_BACKGROUND_IMAGE_KEY = "buttonTopBackgroundImage";

  /**
   * Key used with getPaintData() to retrieve the buttonBottomBackground Image.
   * <p>
   * @see #getPaintData
   */
  public final static Object BUTTON_BOTTOM_BACKGROUND_IMAGE_KEY = "buttonBottomBackgroundImage";


  /**
   * State constant for a disabled object.
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_DISABLED = 0x00000001;

  /**
   * State constant for an object that is currently
   * pressed.
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_ARMED = 0x00000002;

  /**
   * State constant for an inactive object.
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_INACTIVE = 0x00000004;

  /**
   * State constant for an object that is in the set state
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_SET = 0x00000008;

  /**
   * State constant for an object that has the mouse over it.
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_MOUSE_OVER = 0x00000010;

  /**
   * State constant for the context having the selection
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_SELECTED = 0x00000020;

  /**
   * State constant for the context having the keyboard focus
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_FOCUSED = 0x00000040;

  /**
   * State constant for the context being the input default
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_ISDEFAULT = 0x00000080;


  /**
   * State constant for the context not wanting the background filled
   * =-=AEW It might be better to just report getPaintBackground() == null,
   * but that forces code to understand this issue.
   * <p>
   * @see #getPaintState
   */
  public final static int STATE_BACKGROUND_NOT_FILLED = 0x00000100;

  /**
   * Sets a property on the response dictionary
   */
  public void setResponseProperty(Object key, Object value);

  // =-=ags Added for TextPainter???
  public Graphics getPaintGraphics();

  /**
   * Returns a mask for the current state.
   * <p>
   * @return A bitmask indicating the current state of the PaintContext.
   */
  public int getPaintState();


  /**
   * Returns the ImageContext for this render.
   */
  public ImageContext getImageContext();

  /**
   * Returns the foreground color of the PaintContext.
   * <p>
   * @return The foreground color of the PaintContext.
   */
  public Color getPaintForeground();


  /**
   * Returns the background color of the PaintContext.
   * <p>
   * @return The background color of the PaintContext.
   */
  public Color getPaintBackground();


  /**
   * Returns the color surrounding the Object to be painted with the
   * PaintContext.
   * <p>
   * @return The Color surrounding the Object to be painted.
   */
  public Color getSurroundingColor();


  /**
   * Returns the font of the PaintContext.
   * <p>
   * @return The Font of the PaintContext.
   */
  public Font getPaintFont();


  /**
   * Returns a font metrics object for the specified font.
   * <p>
   * @param font Font object to return FontMetrics of.
   * <p>
   * @return The FontMetrics of the specified Font.
   */
  public FontMetrics getFontMetrics(Font font);


  /**
   * Returns an image observer object that can be used to
   * monitor progress in loading images.
   * <p>
   * @return The ImageObserver of the PaintContext.
   */
  public ImageObserver getImageObserver();


  /**
   * Returns the Locale of the PaintContext.
   * <p>
   * @return The Locale of the PaintContext.
   */
  public Locale getPaintLocale();



  /**
   * Returns the data Object for the specified key.  This is the method
   * that Painters use to retrieve their data.  In addition to the
   * two constants defined in this Interface for retrieving data, several
   * abstract UI's specify the constants supported by their implementations
   * for use by Painters.  Further, implementors should interpret the passing
   * of the key value <code>null</code> to indicate that their default data
   * Object be returned by this method.
   * <p>
   * @param key The key specifying which data Object the PaintContext should
   *            return.
   * <p>
   * @return The data Object specified by <code>key</code> or <code>null</code>
   *         if no Object for that key is supported.
   */
  public Object getPaintData(Object key);


  /**
   * Returns the horizontal interior alignment of the PaintContext.
   * <p>
   * @return The horizontal interior alignment of the PaintContext as a
   *         floating point number where 0.0 should be interpreted as
   *         left aligned, 0.5 as center aligned, and 1.0 as right aligned.
   */
  public float getInteriorAlignmentX();


  /**
   * Returns the vertical interior alignment of the PaintContext.
   * <p>
   * @return The vertical interior alignment of the PaintContext as a
   *         floating point number where 0.0 should be interpreted as
   *         top aligned, 0.5 as center aligned, and 1.0 as bottom aligned.
   */
  public float getInteriorAlignmentY();


  /**
   * Returns the reading direction of the PaintContext.
   * <p>
   * @return The reading direction of the PaintContext.
   * <p>
   * @see org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils
   */
  public int getReadingDirection();
}
