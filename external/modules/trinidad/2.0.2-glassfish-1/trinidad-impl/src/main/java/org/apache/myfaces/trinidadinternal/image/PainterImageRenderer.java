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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;

import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import java.util.Locale;

import java.util.Map;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.image.painter.FontUtils;
import org.apache.myfaces.trinidadinternal.image.painter.Painter;
import org.apache.myfaces.trinidadinternal.image.painter.PaintContext;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;
import org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils;

/**
 * ImageRenderer implementation that uses Painter objects to
 * render an image.  Clients must provide a Painter object in the
 * constructor.<p>
 *
 * The PainterImageRenderer creates a PaintContext object in
 * response to a call to renderImage().  The PaintContext
 * object will obtain all its data about the state of the
 * BufferedImage by using the Map of properties.  Clients
 * should set the properties in the Map using the given
 * key constants described below.  <p>
 *
 * Clients can pass custom properties by using their own key objects.<p>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/PainterImageRenderer.java#0 $) $Date: 10-nov-2005.19:03:58 $
 */
public class PainterImageRenderer extends AbstractImageRenderer
  implements ImageConstants
{
    /**
      * Create a PainterImageRenderer that uses the given Painter
      * to create a BufferedImage.
      */
    protected PainterImageRenderer(Painter painter)
    {
        _painter = painter;
    }

    /**
      * Get the Painter used to create the image.
      */
    public Painter getPainter()
    {
        return _painter;
    }


    /**
      * Render the image given the Map of
      * properties that describe what to render.  A PaintContext
      * object is created using the given Map of properties.
      */
    @Override
    public Image renderImage(
      ImageContext imageContext,
      Map<Object, Object> requestedProperties,
      Map<Object, Object> responseProperties
      )
    {
        if (!isRenderable(imageContext, requestedProperties))
        {
          return null;
        }

        Painter painter = getPainter(imageContext, requestedProperties);

        // First we measure the preferred size using a dummy image of
        // size 1x1 - we need this image to get the Graphics obejct
        // for measuring.
        BufferedImage measureImage = createImage(1, 1);

        // Create a PaintContext to use for measuring
        PaintContext measureContext = createPaintContext(imageContext,
                                                         measureImage,
                                                         requestedProperties,
                                                         responseProperties);

        // Get a measurement for the requested image
        Dimension d = painter.getPreferredSize(measureContext);

        int width = d.width;
        int height = d.height;

        // We're done with the measure image and context - free them up
        measureImage.flush();
        disposePaintContext(measureContext);

        // Now that we know how big the image should be, create the image
        // that we'll use for painting
        BufferedImage paintImage = createImage(width, height);

        // Create a PaintContext to use for drawing
        PaintContext paintContext = createPaintContext(imageContext,
                                                       paintImage,
                                                       requestedProperties,
                                                       responseProperties);

        // Fill in the image with the surrounding color
        Graphics g = paintContext.getPaintGraphics();
        Color oldColor = g.getColor();
        g.setColor(paintContext.getSurroundingColor());
        g.fillRect(0, 0, width, height);
        g.setColor(oldColor);

        // Paint the image
        painter.paint(paintContext, g, 0, 0, width, height);

        // Now that we are done painting, dispose the PaintContext
        disposePaintContext(paintContext);

        // Store width/height for client
        responseProperties.put(WIDTH_RESPONSE_KEY,
                               width);
        responseProperties.put(HEIGHT_RESPONSE_KEY,
                               height);

        return paintImage;
    }


    /**
     * Returns the Painter to use for the specified request.
     */
    protected Painter getPainter(
      ImageContext imageContext,
      Map<Object, Object> requestedProperties
      )
    {
      return getPainter();
    }

    /**
     * Tests whether the requested image can be rendered.
     * The default implementation of isRenderable() returns false if
     * org.apache.myfaces.trinidadinternal.style.util.GraphicsUtils.isGraphicalEnvironment()
     * returns false.  Subclasses can override this to verify that all
     * required properties are present in the requested properties
     * Map.  All overrides must either call super.isRenderable()
     * or GraphicsUtils.isGraphicalEnvironment() to ensure that a
     * graphical environment is available.
     */
    protected boolean isRenderable(
      ImageContext imageContext,
      Map<Object, Object> requestedProperties
      )
    {
      return GraphicsUtils.isGraphicalEnvironment();
    }

    /**
     * Returns the foreground color to use when painting an image
     * with the specified Map.
     */
    protected Color getPaintForeground(
        ImageContext context, 
        Map<Object,Object> d)
    {
      return (Color)d.get(FOREGROUND_KEY);
    }

    /**
     * Returns the background color to use when painting an image
     * with the specified Map.
     */
    protected Color getPaintBackground(
        ImageContext context, 
        Map<Object,Object> d)
    {
      return (Color)d.get(BACKGROUND_KEY);
    }

    /**
     * Returns the font color to use when painting an image
     * with the specified Map.
     */
    protected Font getPaintFont(Map<Object,Object> d)
    {
      Object o = d.get(FONT_KEY);

      if (o instanceof FontProxy)
        return ((FontProxy)o).getFont();

      return (Font)d.get(FONT_KEY);
    }

    protected Object getPaintData(Object key, Map<Object,Object> d)
    {
      return d.get(key);
    }

    //////////////////////////////////////////////////////////////////////////
    // private classes
    //////////////////////////////////////////////////////////////////////////

    protected PaintContext createPaintContext(
      ImageContext imageContext,
      BufferedImage image,
      Map<Object,Object> requestedProperties,
      Map<Object,Object> responseProperties
      )
    {
      return
        new Context(
          imageContext,
          image,
          requestedProperties,
          responseProperties
         );
    }

    protected void disposePaintContext(PaintContext context)
    {
      context.getPaintGraphics().dispose();
    }

    /**
      * Class that implements PaintContext using the
      * Map of properties.
      */
    private class Context implements PaintContext
    {
        public Context(
          ImageContext imageContext,
          BufferedImage image,
          Map<Object,Object> requestedProperties,
          Map<Object,Object> responseProperties
          )

        {
            _imageContext = imageContext;
            _requested = requestedProperties;
            _response = responseProperties;

            // Initialize the reading direction
            _direction = LocaleUtils.getReadingDirection(imageContext.getLocaleContext());

            Object o = null;
            if ((o = requestedProperties.get(DIRECTION_KEY)) != null)
              _direction = ((Integer)o).intValue();


            // Set up the graphics object for painting
            Graphics2D g = image.createGraphics();
            g.setFont(getPaintFont());
            g.setColor(getPaintForeground());

            // Turn on antialiasing
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                               RenderingHints.VALUE_ANTIALIAS_ON);

            _graphics = g;
        }

        public int getPaintState()
        {
            int state = 0;

            if (Boolean.TRUE.equals(_requested.get(DISABLED_KEY)))
                state |= PaintContext.STATE_DISABLED;

            return state;
        }

        public ImageContext getImageContext()
        {
          return _imageContext;
        }

        public Color getPaintForeground()
        {
            // Try to get the paint foreground from the renderer
            PainterImageRenderer renderer = PainterImageRenderer.this;
            Color foreground = renderer.getPaintForeground(_imageContext,
                                                           _requested);

            return (foreground == null) ? Color.black : foreground;
        }

        public Color getPaintBackground()
        {
            // Try to get the paint background from the renderer
            PainterImageRenderer renderer = PainterImageRenderer.this;
            Color background = renderer.getPaintBackground(_imageContext,
                                                           _requested);

            return (background == null) ? Color.white : background;
        }

        public Color getSurroundingColor()
        {
          Color color = (Color)_requested.get(
                                 ImageConstants.SURROUNDING_COLOR_KEY);

          if (color != null)
            return color;

          return _TRANSPARENT_COLOR;
        }

        public Font getPaintFont()
        {
            // Try to get the paint font from the renderer
            PainterImageRenderer renderer = PainterImageRenderer.this;
            Font font = renderer.getPaintFont(_requested);

            if (font != null)
              return font;

            return FontUtils.getDefaultSansSerifFont();
        }

        public Locale getPaintLocale()
        {

            Locale l = _imageContext.getLocaleContext().getTranslationLocale();
            if (l != null)
                return l;

            return Locale.getDefault();
        }

        /**
         * Sets a property on the response Map
         */
        public void setResponseProperty(Object key, Object value)
        {
          _response.put(key, value);
        }


        public Graphics getPaintGraphics()
        {
          return _graphics;
        }

        public Object getPaintData(Object key)
        {
          // Try to get the paint data from the renderer
          PainterImageRenderer renderer = PainterImageRenderer.this;
          return renderer.getPaintData(key, _requested);
        }

        public float getInteriorAlignmentX()
        {
            return _CENTER_ALIGN;
        }

        public float getInteriorAlignmentY()
        {
            return _CENTER_ALIGN;
        }

        public FontMetrics getFontMetrics(Font font)
        {
            return _graphics.getFontMetrics(font);
        }

        public ImageObserver getImageObserver()
        {
            return PainterImageRenderer.this;
        }

        public int getReadingDirection()
        {
          return _direction;
        }

        private Graphics2D            _graphics;
        private ImageContext          _imageContext;
        private Map<Object,Object>    _requested;
        private Map<Object,Object>    _response;
        private int                   _direction;

    }

    //////////////////////////////////////////////////////////////////////////
    // private variables
    //////////////////////////////////////////////////////////////////////////
    static final private Color _TRANSPARENT_COLOR = new Color(255,255,255, 0);

    private Painter _painter;

  private static final float _CENTER_ALIGN = 0.5f;
}
