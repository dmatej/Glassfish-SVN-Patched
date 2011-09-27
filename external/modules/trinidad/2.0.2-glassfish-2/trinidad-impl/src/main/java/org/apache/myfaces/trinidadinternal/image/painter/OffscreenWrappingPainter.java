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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.RGBImageFilter;



import org.apache.myfaces.trinidadinternal.image.ImageConstants;

/**
 * This is used to render the contents of another painter (the wrapped
 * painter) into an offscreen buffer before rendering to the paint
 * Graphics object.  This is an unusual thing to do, but seems to be
 * the only way to workaround the funky Java2D/BufferedImage text
 * rendering problems.  (See bug 1288470).
 * <p>
 * Java2D mucks up non-antialiased text drawn into BufferedImages.
 * So, when drawing non-antialiased text, we first draw it into an
 * offscreen buffer and then into the BufferedImage.  This class
 * will certainly go away once the underlying Java2D bug is fixed.
 * <p>
 * Think twice about using this class - it adds overhead to the
 * render since any wrapped rendering is double buffered.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/OffscreenWrappingPainter.java#0 $) $Date: 10-nov-2005.19:04:59 $
 */
public class OffscreenWrappingPainter extends AbstractWrappingPainter
  implements ImageObserver
{
  public OffscreenWrappingPainter(Painter wrappedPainter)
  {
    super(wrappedPainter);
  }

  @Override
  public void paint(
    PaintContext context,
    Graphics     g,
    int          x,
    int          y,
    int          width,
    int          height
    )
  {
    // On X servers with limited color capacities, we have problems with
    // getting the background color of the offscreen image to match the
    // actual background color of the destination BufferedImage.  The
    // problem is that when the X server runs out of colors, it uses a
    // near match - which might not be the background color that we want.
    // The result is that the area behind the text may have a different
    // background color than the rest of the image.  (eg. in buttons, the
    // bounding box of the text has a white background, while the rest of
    // the button content is the light off-white specified by BLAF.)
    //
    // To work around this problem, we convert background pixels to
    // transparent before drawing the offscreen buffer.  However, we
    // can't simply filter on pixels where the rgb value is equal to
    // our desired background rgb value - as the actual rgb value is
    // picked by the X server.  So, we've got a real hack here...
    //
    // We make the offscreen buffer one pixel taller than needed.
    // We fill this entire area, including the extra pixel scan line
    // at the top, with the background color.  Then, we draw the content
    // starting at y=1.  So, when we get around to filtering the background,
    // we know that the pixels at y=0 are the background color - all pixel
    // values which match the value at 0, 0 are filtered to transparent.
    // Yeah, I know this is insane.  Feel free to rip this out if you've
    // got a better solution.

    // Create the offscreen buffer.  We make it one pixel taller than
    // necessary.  We want the top scan line to be filled with the background
    // color, but without any actual content, so that we can use it later
    // (during transparency filtering) to get the real background color.
    BufferedImage buffer = _createOffscreenBuffer(context, width, height + 1);

    if (buffer == null)
    {
      super.paint(context, g, x, y, width, height);
      return;
    }

    // If we've got a buffer, use it's graphics object for rendering
    // our wrapped painter
    Graphics offscreenG = _getInitializedGraphics(context, buffer);

    // Fill in the background - including the extra 1 pixel at the top
    offscreenG.setColor(context.getPaintBackground());
    offscreenG.fillRect(0, 0, width, height + 1);

    // Reset for text rendering
    offscreenG.setColor(g.getColor());
    offscreenG.translate(-x, -y);

    // Render the wrapped painter into the offscreen buffer.  We offset
    // the y coordinate by one so that no content will be rendered into
    // the top pixel.
    super.paint(context, offscreenG, x, y + 1, width, height);

    // Filter out the background
    Image transparentImage = ImageUtils.createFilteredImage(buffer,
                               new TransparencyFilter());
    ImageUtils.loadImage(transparentImage);

    // Now, render the transparent image into the original in Graphics object
    g.drawImage(transparentImage,
                x, y, x+width, y+height, 0, 1, width, height + 1, this);

    // Clean up
    offscreenG.dispose();
    transparentImage.flush();
    buffer.flush();
  }

  @Override
  public Dimension getPreferredSize(PaintContext context)
  {
    Dimension size = null;

    // Get a shared buffer to use for measuring
    BufferedImage buffer = _createOffscreenBuffer(context, 1, 1);

    if (buffer != null)
    {
      Graphics g = _getInitializedGraphics(context, buffer);
      size = super.getPreferredSize(new ProxyContext(context, g));

      // Clean up
      g.dispose();
      buffer.flush();
    }
    else
    {
      // If we didn't get a buffer, just paint the contents directly
      size = super.getPreferredSize(context);
    }

    return size;
  }

  /**
   * ImageObserver implementation
   */
  public boolean imageUpdate(
    Image img,
    int infoflags,
    int x,
    int y,
    int width,
    int height
    )
  {
    return (infoflags & (ALLBITS|ABORT)) == 0;
  }

  // Creates a new Image of the specified width/height
  private BufferedImage _createOffscreenBuffer(
    PaintContext context,
    int width,
    int height
    )
  {
    if ((width == 0) || (height == 0))
      return null;

    // Check to make sure requested buffer isn't too big
    if (width * height > _MAX_BUFFER_AREA)
    {
      assert false;
      return null;
    }

    // Create a new offscreen buffer.  We use TYPE_INT_ARGB as this seems
    // to produce higher quality text rasterization than the default
    // image type (TYPE_4BYTE_ABGR).  Perhaps we should just use
    // TYPE_INT_ARGB for all images, but that might have negative affects
    // on other parts of our rendering - like antialiased arcs.
    // Note: For bold antialiased text, TYPE_4BYTE_ABGR produces better
    // results, particularly when rendering on Solaris.
    int type = BufferedImage.TYPE_INT_ARGB;

    if (_isTextAntialiased(context) &&
        ((context.getPaintFont().getStyle() & Font.BOLD) != 0))
    {
      type = BufferedImage.TYPE_4BYTE_ABGR;
    }

    return new BufferedImage(width, height, type);
  }

  // Get the Graphics object to use for painting into the image,
  // with font and rendering hints intialized.
  private Graphics _getInitializedGraphics(
    PaintContext  context,
    BufferedImage image
    )
  {
    Graphics2D g = image.createGraphics();

    g.setFont(context.getPaintFont());

    if (_isTextAntialiased(context))
    {
      // If text antialiasing is enabled, turn on the text antialias
      // rendering hint.  Note, we use KEY_TEXT_ANTIALIASING instead
      // of KEY_ANTIALIASING, as this seems to produce cleaner looking
      // text in general.  Also, there is a problem on Windows JDK 1.3,
      // where text rendered with KEY_ANTIALIASING into a TYPE_INT_ARGB
      // BufferedImage does not appear!  However, when the same text
      // is rendered with KEY_TEXT_ANTIALIASING, everything looks fine.
      // (This problem can be reproduced with the oracle.uix.tools.uix22.image
      // CharacterMap tool - try looking at CJK Unified glyphs using
      // Albany WT J with antialiasing enabling.  Scroll down a couple of
      // pages and the text vanishes.  Then, switch to text antialiasing
      // and the text reappears!)
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                         RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

      // If we are using TYPE_4BYTE_ABGR (for bold, antialiased text),
      // then oddly enough we also need to turn on the KEY_ANTIALIASING
      // hint - otherwise text isn't antialiased!  Note: we don't want
      // to specify KEY_ANTIALIASING for the normal case (TYPE_INT_ARGB),
      // as it makes text disappear as described above!
      if (image.getType() == BufferedImage.TYPE_4BYTE_ABGR)
      {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);
      }
    }

    return g;
  }

  // Tests whether text should be antialiased
  private boolean _isTextAntialiased(PaintContext context)
  {
    return Boolean.TRUE.equals(context.getPaintData(
                               ImageConstants.TEXT_ANTIALIAS_KEY));
  }

  // PaintContext proxy for passing offscreen graphics into
  // wrapped Painter.  This is necessary so that the same
  // graphics is used for both measuring and drawing.  We
  // can't measure with the BufferedImage graphics and then
  // draw with the offscreen graphics, as the font metrics
  // are different.  This bites.
  private static class ProxyContext extends PaintContextProxy
  {
    public ProxyContext(PaintContext wrappedContext, Graphics offscreenG)
    {
      _context = wrappedContext;
      _g = offscreenG;
    }

    @Override
    protected PaintContext getPaintContext()
    {
      return _context;
    }

    @Override
    public Graphics getPaintGraphics()
    {
      return _g;
    }

    @Override
    public Font getPaintFont()
    {
      return _g.getFont();
    }

    @Override
    public FontMetrics getFontMetrics(Font font)
    {
       return _g.getFontMetrics(font);
    }

    private PaintContext _context;  // The wrapped paint context
    private Graphics     _g;        // The offscreen graphics object
  }

  // This is a strange transparency filter implementation.  Normally,
  // A transparency filter would take an rbg value to filter.  However,
  // we don't know what value to filter until we start filtering - it
  // isn't necessarily the paint background color, as the background color
  // might be adjusted (ie. by the X server) when offscreen image is
  // painted.
  //
  // We assume that the top scanline of the image contains the background
  // color.  (The OffscreenWrappingPainter.paint() method coordinates
  // this for us.)  The pixel value at 0, 0 is treated as the background
  // color - we filter all such values to be transparent.
  //
  // Note - this assumes that we will see the pixels at y=0 before we see
  // any other pixels.  If that is incorrect, we are in trouble.
  private static class TransparencyFilter extends RGBImageFilter
  {
    public TransparencyFilter()
    {
      // We don't want to filter the index - we need to examine individual
      // pixels so that we can get the rgb value of 0, 0.
      canFilterIndexColorModel = false;
    }

    @Override
    public int filterRGB(int x, int y, int rgb)
    {
      // Make sure we see pixels at y=0 before any other pixels.
      if (y!=0 && !_gotRGB)
      {
	   assert(_gotRGB);
       if(y!=0)
       {
         throw new IllegalArgumentException("Non zero y");
       }
      }


      if (!_gotRGB && (y == 0))
      {
        _rgb = rgb;
        _gotRGB = true;
      }

      if (_rgb == rgb)
      {
        return 0;
      }

      return rgb;
    }

    // The transparent pixel
    private int _rgb;

    // Have we found the rgb value to make transparent?
    private boolean _gotRGB;
  }

  // Set an upper size on how big we are willing to allow
  // our buffers to get.  Buffers should not be too large - we
  // should only be using this for button/tab labels.
  private static final int _MAX_BUFFER_AREA = 30 * 800;
}
