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
package org.apache.myfaces.trinidadinternal.image.encode;

import java.awt.Color;
import java.awt.Image;
import java.awt.image.ImageObserver;
import java.awt.image.PixelGrabber;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Iterator;
import java.util.Hashtable;
import java.util.zip.CRC32;
import java.util.zip.DeflaterOutputStream;

import org.apache.myfaces.trinidadinternal.image.painter.ImageLoader;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Generates a PNG graphics file given pixel data.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/PNGEncoder.java#0 $) $Date: 10-nov-2005.19:05:21 $
 * @since 0.1.4
 */
final class PNGEncoder
{
  // =-=ags Todo: Support 2-bit, 4-bit pixel depth for palette images.
  //              Also, crank up Deflater compression level

  /**
   * Encodes the Image to the specified OutputStream in PNG format
   */
  public static void encode(Image image, OutputStream out)
    throws IOException
  {
    // First make sure the image is loaded
    ImageLoader loader = new ImageLoader(image);
    loader.start();
    if(!loader.waitFor())
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "PROBLEM_LOADING"));
    }

    int width = image.getWidth(loader);
    int height = image.getHeight(loader);
    int[] pixels = new int[width*height]; // all the image's pixels

    // Use a PixelGrabber to get the pixel values
    PixelGrabber grabber = new PixelGrabber(image.getSource(),
                                            0, 0,
                                            width, height,
                                            pixels,
                                            0, width);
    try
    {
      grabber.grabPixels();
    }
    catch (InterruptedException e)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "GRABBING_PIXELS"));
    }

    if ((grabber.getStatus() & ImageObserver.ABORT) != 0)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ERROR_FETCHING_IMAGE", new Object[]{pixels.length, width, height}));
    }

    // -= Simon Lessard =-
    // FIXME: HashMap please if synchronization is not required...
    Hashtable<Color, Integer> colors = new Hashtable<Color, Integer>();
    int   count = 0;
    Color lastColor = null;
    // Use -2 instead of -1 for last pixel because -1 is tranparent white
    int lastPixel = -2;

    // We currently only support a single fully-transparent Color, due to
    // limitations with browser transparency support.  We place the first
    // fully transparent color that we find at index zero in the palette,
    // so that we can have a single entry in the tRNS chunk.
    Color firstColor = _createColor(pixels[0]);
    Color transparentColor = null;

    for (int i = 0; i < pixels.length; i++)
    {
      int pixel = pixels[i];

      if ((pixel != lastPixel) || (lastColor == null))
      {
        Color color = _createColor(pixel);
        if (!colors.containsKey(color))
        {
          // Check for first transparent color
          if ((color.getAlpha() == 0) && (transparentColor == null))
          {
            // Put the transparent color at index zero.
            // Put the old zero index color at the current index.
            colors.put(color, 0);
            colors.put(firstColor, count);
            transparentColor = color;
          }
          else
          {
            colors.put(color, count);
          }

          count++;
        }

        lastPixel = pixel;
        lastColor = color;
      }
    }


    boolean transparent = (transparentColor != null);

    // Write the PNG signature
    _writeSignature(out);

    // Write the IHDR and IDAT chunks.  If we have 256 colors or less, we
    // can use a color palette, otherwise we write out RGB triplets
    if (count <= 256)
      _writePaletteImage(width, height, pixels, colors, transparent, out);
    else
      _writeRGBImage(width, height, pixels, out);

    // Write the IEND chunk
    _writeEnd(out);
  }

  private PNGEncoder() {}

  // Write out header and image data as palette image
  private static void _writePaletteImage(
    int          width,
    int          height,
    int[]        pixels,
    Map<Color, Integer> colors,
    boolean      transparent,
    OutputStream out
    ) throws IOException
  {
    int count = colors.size();

    assert (count <= 256);

    // Before we write the header, we need to determine the optimal bit depth

    // =-=ags Just hardcode 8 for now
    // int depth = (count <= 4) ? 2 (count <= 16) ? 4 : 8;
    int depth = 8;

    // Write out the IHDR chunk
    _writeHeader(width, height, (byte)depth, (byte)3, out);

    // Write out the PLTE chunk
    _writePalette(colors, out);

    // Write out the tRNS chunk
    if (transparent)
    {
      _writeTransparency(out);
    }

    // Convert the pixels array into an array of palette indices.
    byte[] data = _getIndexedData(width, height, depth, pixels, colors);

    // Write out the IDAT chunk
    _writeData(data, out);
  }

  // Write out header and image data as RGB triplets
  private static void _writeRGBImage(
    int          width,
    int          height,
    int[]        pixels,
    OutputStream out
    ) throws IOException
  {
    _writeHeader(width, height, (byte)8, (byte)2, out);

    // Convert pixels to an array of RGB byte triplets.  Each scanline
    // starts with a filter byte of zero.
    byte[] data = new byte[(pixels.length * 3) + height];

    int sourceLine = 0;  // Offset of the source scan line
    int targetLine = 0;  // Offset of the target scan line

    for (int i = 0; i < height; i++)
    {
      // Write out the filter byte for this scan line
      data[targetLine] = (byte)0;

      for (int j = 0; j < width; j++)
      {
        int pixel = pixels[sourceLine + j];

        int target = targetLine + (j * 3) + 1;
        data[target]     = _getRed(pixel);
        data[target + 1] = _getGreen(pixel);
        data[target + 2] = _getBlue(pixel);
      }

      sourceLine += width;
      targetLine += ((width * 3) + 1);
    }

    _writeData(data, out);
  }

  // Write out the PNG IDAT chunk
  private static void _writeData(
    byte[]       data,
    OutputStream out
    ) throws IOException
  {
    // Compress the data
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    OutputStream deflater = new DeflaterOutputStream(baos);
    deflater.write(data);
    deflater.flush();
    deflater.close();

    // Write out the IDAT chunk
    _writeChunk(_IDAT, baos.toByteArray(), out);
  }

  // Write out the PNG IEND chunk
  private static void _writeEnd(OutputStream out) throws IOException
  {
    _writeChunk(_IEND, null, out);
  }

  // Write out the PNG IHDR chunk
  private static void _writeHeader(
    int          width,
    int          height,
    byte         depth,
    byte         type,
    OutputStream out
    ) throws IOException
  {
    byte[] data = new byte[13];

    // Write the width/height
    _writeInt(width, 0, data);
    _writeInt(height, 4, data);

    data[8]  = depth;    // bit depth
    data[9]  = type;     // color type
    data[10] = (byte)0;  // Compression method
    data[11] = (byte)0;  // Filter method
    data[12] = (byte)0;  // Interlace method

    _writeChunk(_IHDR, data, out);
  }

  // Write out the PLTE chunk
  private static void _writePalette(
    Map<Color, Integer> colors,
    OutputStream out
    ) throws IOException
  {
    // Convert the colors Map into the palette data, sorted by index
    int count = colors.size();
    byte[] data = new byte[count * 3];

    for (Map.Entry<Color, Integer> entry : colors.entrySet())
    {
      Color color = entry.getKey();
      int index = entry.getValue().intValue() * 3;

      int rgb = color.getRGB();
      data[index]     = _getRed(rgb);
      data[index + 1] = _getGreen(rgb);
      data[index + 2] = _getBlue(rgb);
    }

    _writeChunk(_PLTE, data, out);
  }

  // Write out the PNG signature
  private static void _writeSignature(OutputStream out) throws IOException
  {
    out.write(_SIGNATURE);
  }

  // Write out the tRNS chunk if neccessary
  private static void _writeTransparency(
    OutputStream out
    ) throws IOException
  {
    // We only support a single fully transparent pixel, which we force
    // into palette index zero so that we can have a single entry in our
    // tRNS chunk.
    _writeChunk(_tRNS, _TRANSPARENT_DATA, out);
  }

  private static void _writeChunk(
    int          type,
    byte[]       data,
    OutputStream out
    ) throws IOException
  {
    int length = (data == null) ? 0 : data.length;

    _writeInt(length, out);
    _writeInt(type, out);

    if (data != null)
      out.write(data);

    _write32(_getCRC(type, data), out);
  }

  // Write an MSB int
  private static void _writeInt(int i, OutputStream out) throws IOException
  {
    out.write((i >> 24) & 0x000000ff);
    out.write((i >> 16) & 0x000000ff);
    out.write((i >> 8) & 0x000000ff);
    out.write(i & 0x000000ff);
  }

  // Writes an MSB int into the data buffer at the specified offset
  private static void _writeInt(int i, int offset, byte[]buffer)
    throws IOException
  {
    buffer[offset]     = (byte)((i >> 24) & 0x000000ff);
    buffer[offset + 1] = (byte)((i >> 16) & 0x000000ff);
    buffer[offset + 2] = (byte)((i >> 8) & 0x000000ff);
    buffer[offset + 3] = (byte)(i & 0x000000ff);
  }

  // Write 32 bits from a long
  private static void _write32(long l, OutputStream out) throws IOException
  {
    out.write((int)(l >> 24) & 0x000000ff);
    out.write((int)(l >> 16) & 0x000000ff);
    out.write((int)(l >> 8) & 0x000000ff);
    out.write((int)l & 0x000000ff);
  }

  // Computes the CRC for the given type and data
  private static long _getCRC(int type, byte[] data)
  {
    CRC32 crc = new CRC32();

    // First add in the type bytes
    crc.update((type >> 24) & 0x000000ff);
    crc.update((type >> 16) & 0x000000ff);
    crc.update((type >> 8) & 0x000000ff);
    crc.update(type & 0x000000ff);

    // Now, add in the data
    if (data != null)
      crc.update(data);

    return crc.getValue();
  }

  // Converts the pixels array into palette indices.
  // Also, inserts the filter byte at the start of each scanline,
  // so the data is ready for compression
  private static byte[] _getIndexedData(
    int        width,
    int        height,
    int        depth,
    int[]      pixels,
    Map<Color, Integer> colors
    )
  {
// =-=ags At the moment we only support 8-bit pixel depths
//    if (depth == 2)
//      return _getIndexedData2(width, height, pixels, colors);
//    if (depth == 4)
//      return _getIndexedData4(width, height, pixels, colors);

    assert (depth == 8);

    return _getIndexedData8(width, height, pixels, colors);
  }

  // 2-bit version of _getIndexedData()  :-)
//  private static byte[] _getIndexedData2(
//    int        width,
//    int        height,
//    int[]      pixels,
//    Dictionary colors
//    )
//  {
//    if (Assert.DEBUG) Assert.assertion(false);
//    return null;
//  }

  // 4-bit version of _getIndexedData()  :-)
//  private static byte[] _getIndexedData4(
//    int        width,
//    int        height,
//    int[]      pixels,
//    Dictionary colors
//    )
//  {
//    if (Assert.DEBUG) Assert.assertion(false);
//    return null;
//  }

  // 8-bit version of _getIndexedData()
  private static byte[] _getIndexedData8(
    int        width,
    int        height,
    int[]      pixels,
    Map<Color, Integer> colors
    )
  {
    // Convert pixels to an array of palette indices.  Each scanline
    // starts with a filter byte of zero.
    byte[] data = new byte[pixels.length + height];

    int sourceLine = 0;  // Offset of the source scan line
    int targetLine = 0;  // Offset of the target scan line

    // Track last pixel/Color as an optimization
    Color lastColor = null;
    // Use -2 instead of -1 for last pixel because -1 is tranparent white
    int lastPixel = -2;

    for (int i = 0; i < height; i++)
    {
      // Write out the filter byte for this scan line
      data[targetLine] = (byte)0;

      for (int j = 0; j < width; j++)
      {
        int pixel = pixels[sourceLine + j];
        Color color;

        if ((pixel == lastPixel) && (lastColor != null))
          color = lastColor;
        else
          color = _createColor(pixel);

        int index = colors.get(color).intValue();

        data[targetLine + j + 1] = (byte)index;

        lastPixel = pixel;
        lastColor = color;
      }

      sourceLine += width;
      targetLine += (width + 1);
    }

    return data;
  }

  // Creates a Color, preserving fully transparent alpha values
  private static final Color _createColor(int argb)
  {
    if ((argb & 0xff000000)  != 0)
      return new Color(argb | 0xff000000);

    // If we're fully tranparent, make sure that we preserve the alpha value
    int a = ((argb >> 24) & 0xff);
    int r = ((argb >> 16) & 0xff);
    int g = ((argb >> 8) & 0xff);
    int b = (argb & 0xff);

    return new Color(r, g, b, a);
  }

  // convenience methods for getting red, green, blue elements from an int
  private static byte _getRed(int c)
  {
    return (byte)((c>>16)&255);
  }

  private static byte _getGreen(int c)
  {
    return (byte)((c>>8)&255);
  }

  private static byte _getBlue(int c)
  {
    return (byte)(c&255);
  }

  // The PNG signature
  private static final byte[] _SIGNATURE =
  {
    (byte)137, // non-ascii
    (byte)80,  // 'P'
    (byte)78,  // 'N'
    (byte)71,  // 'G'
    (byte)13,  // '\r'
    (byte)10,  // '\n'
    (byte)26,  // control-Z
    (byte)10   // '\n'
  };

  private static final int _IHDR = 0x49484452;
  private static final int _PLTE = 0x504c5445;
  private static final int _IDAT = 0x49444154;
  private static final int _IEND = 0x49454e44;
  private static final int _tRNS = 0x74524e53;

  private static final byte[] _TRANSPARENT_DATA = new byte[] { (byte)0 };
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    PNGEncoder.class);
}
