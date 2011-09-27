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
import java.awt.Image;
import java.awt.image.BufferedImage;

import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.PainterImageRenderer;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContext;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContextProxy;
import org.apache.myfaces.trinidadinternal.style.util.FontProxy;
import org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils;

/**
 * CompositeButtonImageRenderer is an ImageRenderer implementation which
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
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/CompositeButtonImageRenderer.java#0 $) $Date: 10-nov-2005.19:05:07 $
 */
public class CompositeButtonImageRenderer extends PainterImageRenderer
{
  // Creates a ButtonImageRenderer
  public CompositeButtonImageRenderer()
  {
    super(new CompositeButtonPainter());
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
    Image startImage = SourceUtils.getSourceIcon(imageContext,
                               requestedProperties,
                               ImageConstants.BUTTON_START_ICON_KEY);

    Image endImage = SourceUtils.getSourceIcon(imageContext,
                               requestedProperties,
                               ImageConstants.BUTTON_END_ICON_KEY);


    Image topBackgroundImage = SourceUtils.getSourceIcon(imageContext,
                               requestedProperties,
                               ImageConstants.BUTTON_TOP_BACKGROUND_ICON_KEY);

    Image bottomBackgroundImage = SourceUtils.getSourceIcon(imageContext,
                               requestedProperties,
                             ImageConstants.BUTTON_BOTTOM_BACKGROUND_ICON_KEY);

    return new ProxyContext(super.createPaintContext(imageContext,
                                                     image,
                                                     requestedProperties,
                                                     responseProperties),
                            startImage,
                            endImage,
                            topBackgroundImage,
                            bottomBackgroundImage);
  }

  @Override
  protected void disposePaintContext(PaintContext context)
  {
    if (context instanceof ProxyContext)
      ((ProxyContext)context).flush();
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
    public ProxyContext(
      PaintContext context,
      Image        startImage,
      Image        endImage,
      Image        topBackgroundImage,
      Image        bottomBackgroundImage
      )
    {
      _context = context;
      _startImage = startImage;
      _endImage = endImage;
      _topBackgroundImage = topBackgroundImage;
      _bottomBackgroundImage = bottomBackgroundImage;
    }

    @Override
    public Object getPaintData(Object key)
    {
      Object o = super.getPaintData(key);
      if (o != null)
        return o;

      if (key.equals(MNEMONIC_INDEX_KEY))
        return _getMnemonicIndex();

      if (PaintContext.BUTTON_START_IMAGE_KEY.equals(key))
        return _startImage;
      if (PaintContext.BUTTON_END_IMAGE_KEY.equals(key))
        return _endImage;
      if (PaintContext.BUTTON_TOP_BACKGROUND_IMAGE_KEY.equals(key))
        return _topBackgroundImage;
      if (PaintContext.BUTTON_BOTTOM_BACKGROUND_IMAGE_KEY.equals(key))
        return _bottomBackgroundImage;

      return null;
    }

    public void flush()
    {
      if (_startImage != null)
      {
        _startImage.flush();
        _startImage = null;
      }

      if (_endImage != null)
      {
        _endImage.flush();
        _endImage = null;
      }

      if (_topBackgroundImage != null)
      {
        _topBackgroundImage.flush();
        _topBackgroundImage = null;
      }

      if (_bottomBackgroundImage != null)
      {
        _bottomBackgroundImage.flush();
        _bottomBackgroundImage = null;
      }
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
    private Image        _startImage;
    private Image        _endImage;
    private Image        _topBackgroundImage;
    private Image        _bottomBackgroundImage;
  }
}
