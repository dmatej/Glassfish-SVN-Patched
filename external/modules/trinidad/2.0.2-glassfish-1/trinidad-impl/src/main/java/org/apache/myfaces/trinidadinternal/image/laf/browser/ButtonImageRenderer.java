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
import java.awt.image.BufferedImage;

import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.PainterImageRenderer;
import org.apache.myfaces.trinidadinternal.image.painter.ImmInsets;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContext;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContextProxy;
import org.apache.myfaces.trinidadinternal.image.painter.Painter;
import org.apache.myfaces.trinidadinternal.style.util.FontProxy;
import org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils;

/**
 * ButtonImageRenderer is an ImageRenderer implementation which
 * renders button images.  The values of the following keys from
 * the ImageConstants interface affect how the button is rendered:
 * <ul>
 * <li>TEXT_KEY
 * <li>FOREGROUND_KEY
 * <li>BACKGROUND_KEY
 * <li>FONT_KEY
 * <li>ANTIALIAS_KEY
 * <li>DISABLED_KEY
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/ButtonImageRenderer.java#0 $) $Date: 10-nov-2005.19:05:05 $
 */
public class ButtonImageRenderer extends PainterImageRenderer
{
  // Creates a ButtonImageRenderer
  public ButtonImageRenderer()
  {
    super(new ButtonPainter());
  }

  @Override
  protected Painter getPainter(
    ImageContext imageContext,
    Map<Object, Object> requestedProperties
    )
  {
    // We use a special ButtonPainter for Dialog which dynamically
    // computes the margins based on the atual space used by the
    // Dialog font glyphs.
    if ("dialog".equalsIgnoreCase(_getFontName(requestedProperties)))
    {
      ImmInsets insets = BlafImageUtils.__getDialogPadding();

      // We want to have a total top margin of 3 pixels from the
      // top of the tallest glyph in the font and a bottom margin
      // of 3 pixels beneath the base line.
      // needs to be at least 3 from the bottom to see access
      // keys underlined
      int topMargin = 3 - insets.top;
      int bottomMargin = 3 - insets.bottom;

      if (_sDialogButtonPainter == null)
        _sDialogButtonPainter = new ButtonPainter(topMargin, bottomMargin);

      return _sDialogButtonPainter;
    }

    return super.getPainter(imageContext, requestedProperties);
  }

  @Override
  protected boolean isRenderable(
      ImageContext imageContext,
      Map<Object, Object> requestedProperties
      )
  {
    if (!super.isRenderable(imageContext, requestedProperties))
      return false;

    return GraphicsUtils.isFontName(_getFontName(requestedProperties));
  }

  @Override
  protected PaintContext createPaintContext(
    ImageContext imageContext,
    BufferedImage image,
    Map<Object, Object> requestedProperties,
    Map<Object, Object> responseProperties
    )
  {
    return new ProxyContext(super.createPaintContext(imageContext,
                                                     image,
                                                     requestedProperties,
                                                     responseProperties));
  }

  /**
   * Returns the foreground color to use when painting an image
   * with the specified dictionary.
   */
  @Override
  protected Color getPaintForeground(
      ImageContext context, 
      Map<Object, Object> d)
  {
    Color foreground = super.getPaintForeground(context, d);

    if (foreground != null)
      return foreground;

    return ButtonPainter.getDefaultForeground(context, _isDisabled(d));
  }

  /**
   * Returns the background color to use when painting an image
   * with the specified dictionary.
   */
  @Override
  protected Color getPaintBackground(
      ImageContext context, 
      Map<Object, Object> d)
  {
    Color background = super.getPaintBackground(context, d);

    if (background != null)
      return background;

    return ButtonPainter.getDefaultBackground(context, _isDisabled(d));
  }

  /**
   * Returns the font color to use when painting an image
   * with the specified dictionary.
   */
  @Override
  protected Font getPaintFont(Map<Object, Object> d)
  {
    Font font = super.getPaintFont(d);

    if (font != null)
      return font;

    return ButtonPainter.getDefaultFont().getFont();
  }

  private String _getFontName(Map<Object, Object> requestedProperties)
  {
    // Make sure we've got a valid font
    Object o = requestedProperties.get(FONT_KEY);
    String name = null;
    if (o instanceof FontProxy)
      name = ((FontProxy)o).getName();
    else if (o instanceof Font)
      name = ((Font)o).getName();

    return name;
  }

  private boolean _isDisabled(Map<Object, Object> properties)
  {
    Boolean disabled = (Boolean)properties.get(DISABLED_KEY);

    if (disabled == null)
      return false;

    return disabled.booleanValue();
  }

  private static class ProxyContext extends PaintContextProxy
  {
    public ProxyContext(PaintContext context)
    {
      _context = context;
    }

    @Override
    public Object getPaintData(Object key)
    {
      Object o = super.getPaintData(key);
      if (o != null)
      {
        // Make sure we don't have an empty text value -
        // this can result in an IllegalArgumentException
        // when we try to create the BufferedImage, since
        // the image height might be zero.
        if (key.equals(TEXT_KEY) && "".equals(o))
          o = " ";

        return o;
      }

      if (key.equals(MNEMONIC_INDEX_KEY))
        return _getMnemonicIndex();

      return null;
    }

    @Override
    protected PaintContext getPaintContext()
    {
      return _context;
    }

    private Integer _getMnemonicIndex()
    {
      Object o = super.getPaintData(ImageConstants.ACCESS_KEY_KEY);
      if (!(o instanceof Character))
        return null;

      char c = ((Character)o).charValue();

      String text = (String)super.getPaintData(ImageConstants.TEXT_KEY);
      if (text == null)
        return null;

      int index = BlafImageUtils.__getMnemonicIndex(text, c);
      if (index < 0)
        return null;

      return index;
    }

    private PaintContext _context;
  }

  // Special Painter for buttons rendered with the Dialog font
  private static Painter _sDialogButtonPainter;
}
