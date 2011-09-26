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

import java.awt.Image;
import java.awt.image.ImageObserver;
import java.awt.image.PixelGrabber;
import java.io.OutputStream;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.util.Hashtable;

import org.apache.myfaces.trinidadinternal.image.painter.ImageLoader;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Generates a Gif89a graphics file given pixel data.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/GifEncoder.java#0 $) $Date: 10-nov-2005.19:05:17 $
 * @since 0.1.4
 */
final class GifEncoder
{

  /**
   * Generate a gif to the stream. The file consists of an identification
   * block, a color table, a graphics control extension which enables
   * transparency, and the image data.
   * @param image The image, in pixel form.
   * @param stream The output stream that the gif is written to.
   * @return true if the encoding was a success.
   */
  public static void encode(Image image, OutputStream stream)
    throws IOException
  {

    // first retrieve the pixels
    ImageLoader il = new ImageLoader(image);
    il.start();
    if(!il.waitFor()){
      throw new IllegalArgumentException(_LOG.getMessage(
        "PROBLEM_LOADING"));
    }
    int width = image.getWidth(il);
    int height = image.getHeight(il);
    int[] pixels = new int[width*height]; // all the image's pixels

    PixelGrabber grabber = new PixelGrabber(image.getSource(), 0, 0,
                                            width, height,
                                            pixels, 0, width);
    try // get the pixels
    {
      grabber.grabPixels();
    }
    catch (InterruptedException e)
    {
      throw new InterruptedIOException(_LOG.getMessage(
        "GRABBING_PIXELS"));
    }
    if ((grabber.getStatus() & ImageObserver.ABORT) != 0)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ERROR_FETCHING_IMAGE", new Object[]{pixels.length,width,height}));
    }

    // Read the pixels to determine the color table
    byte[] globalColorTable = new byte[_MAXIMUM_COLOR_TABLE_SIZE*3];

    // A hashtable is used to keep track of colors already in the table
    // max. one-color entries
    // -= Simon Lessard =-
    // FIXME: JDK 1.2 was truly evil... 
    //        HashMap would be better
    Hashtable<Integer, Integer> hsh = 
      new Hashtable<Integer, Integer>(_MAXIMUM_COLOR_TABLE_SIZE);

    int colorIndex = 0;           // the code values of the colors
    int background = 0;           // the code of the background color
    int lastColor  =_NO_COLOR;    // will be the most recent color (for speed)
    int lastColorIndex = 0;       // The most recent color index
    boolean transparency = false; // Do we have any transparent pixels?

    // the color table is constructed by scanning the image and creating a
    // numerical value for each unique color. The hashtable keeps track of
    // colors that have already been seen, and the rgb pixel data is replaced
    // by the new numerical values of these colors.

    for (int i = 0; i < pixels.length; i++)
    {
      // Get the current color value
      int color = pixels[i];

      if (lastColor == color)
      {
        // If we've have just seen this color, we can just short-circuit
        // right here, since we already know the color index.
        pixels[i] = lastColorIndex;
      }
      else if (_isTransparent(color))
      {
        // Transparent colors don't get added to the color table yet.
        // Mark the pixel as transparent.
        pixels[i] = _TRANSPARENT_COLOR;

        // If the color is fully transparent, make it the background
        if (_isFullyTransparent(color))
          background = colorIndex;

        transparency = true;
      }
      else
      {
        // For non-transparent pixels, we first check to see if the
        // color has already been added to the color table.  We use an
        // the Integer RGB value as our hash key
        Integer colorKey = (color & 0x00ffffff);
        Integer colorIndexValue = hsh.get(colorKey);

        if (colorIndexValue != null)
        {
          // We've already got an index for this color.  Convert the pixel
          // value to the color index.
          lastColorIndex = colorIndexValue.intValue();
        }
        else
        {
          // We've got a new color! Make sure we've got room for it
          if (colorIndex >= _MAXIMUM_COLOR_TABLE_SIZE)
            throw new IllegalArgumentException(_LOG.getMessage(
              "EXCEEDED_GIF_COLOR_LIMIT"));

          // Store the new color in the color table
          int off = 3 * colorIndex; // offset for table
          globalColorTable[off] = _getRed(color);
          globalColorTable[off+1] = _getGreen(color);
          globalColorTable[off+2] = _getBlue(color);

          // Hash the color->index mapping for fast lookups
          hsh.put(colorKey, colorIndex);

          // and Update the index count
          lastColorIndex = colorIndex++;
        }

        // Update the last seen color
        lastColor = color;
        // Convert the pixel value to the color index
        pixels[i] = lastColorIndex;
      }
    }


    // add an alpha color
    // the transparent color must be a color not otherwise found in the
    // image. For history's sake we start at r=255, g=0, b=255, a particularly gaudy shade
    // of pink, and increase color vals by 1 until we find a color not
    // found in the color table. Usually we don't have to go beyond the first color.

    int transparentIndex=0; // the code of transparent
    if (transparency)
    {
      if (colorIndex >= _MAXIMUM_COLOR_TABLE_SIZE)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "NO_SPACE_LEFT_FOR_TRANSPARENCY"));
      }
      else
      { // start at the pink historically used for transparency
        for (int i = 0x00ff00ff; i < 0x00ffffff; i++)
        {
          int col = i;
          Integer icol = col;
          if (!hsh.containsKey(icol))
          {
            // add entry to table
            int off = 3*colorIndex; // offset for
            // table
            transparentIndex=(colorIndex)+2; // add id of
            // transparent color
            // +2 because of Float storage scheme - see explanation
            // under "The LZW algorithm"
            globalColorTable[off] = _getRed(col);
            globalColorTable[off+1] = _getGreen(col);
            globalColorTable[off+2] = _getBlue(col);
            break;
          }
        }
      }
    }
    int codeSize = getLog(colorIndex);

    // extra zeroes at end of globalColorTable if necessary
    int globalColorTableSize = 3*(1<<codeSize);

    // now write the preliminary gif info. This is mostly static information
    // but the fields are as follows:
    // [note: all multi-byte fields are little-endian, but bytes themselves are
    // big-endian]

    // _HEADER : "GIF89a" identifies the format

    // Logical Screen Descriptor (lsd): 7 bytes as follows:
    // [0-1]: image width
    // [2-3]: image height
    // 4: Packed byte (see below)
    // 5: color index of screen background
    // 6: 0 (signifies end of descriptor)
    // the packed byte is, by bit:
    // 7: =1 if there is a global color table (always for us)
    // 6-4: = the number of bits of color resolution - 1
    // 3: always 0
    // 2-0: the number of bits per pixel in the image-1
    // despite the description of bits 6-4, for our purposes (and, it seems, all other gifs,)
    // they are the same as bits 2-0.

    // Color Table: for each color, 3 bytes signifying, respectively, the r, g, and b values
    // of the color. The table is the size of a power of 2. If necessary, extra entries with
    // zero values are included.

    // Graphics control extension: allows for transparency. 8 bytes as follows:
    // [0]: 0x21 identifies block as an extension
    // [1]: 0xf9 identifies block as a graphics extension
    // [2]: 0x04 identifies # bytes remaining (not including terminator) always 4
    // [3]: packed byte (see below)
    // [4-5]: delay time (always 0)
    // [6]: tranparent color index - as determined above
    // [7]: block terminator (always 0)
    // the packed byte is, by bit:
    // 7-5: reserved (always 0)
    // 4-2: disposal method (always 0)
    // 1: 1 if user input is required (always 0)
    // 0: 1 if transparency index is included (always 1)

    // Local image descriptor: (lid) identifies the "first" (only) image in the gif
    // 10 bytes as follows:
    // [0]: 0x2c identifies new lid
    // [1-2]: start of image from the left (0)
    // [3-4]: start of image from the top (0)
    // [5-6]: width of image (=width from lsd)
    // [7-8]: height of image (=height from lsd)
    // [9]: packed bit. All 0 for us.


    stream.write(_HEADER);  // identifies this as a gif
    byte[] blocks = new byte[18]; // first serve as the 7-byte lsd
    blocks[0] = (byte)width;
    blocks[1] = (byte)(width >>> 8);
    blocks[2] = (byte)height;
    blocks[3] = (byte)(height >>> 8);
    blocks[4] = (byte)((byte)128 |
                       (byte)((codeSize-1)<<4) |
                       (byte)(codeSize-1));
    blocks[5] = (byte)background;
    // initial value is 6
    //   blocks[6] = 0;
    stream.write(blocks, 0, 7); // logical screen descriptor
    // combo graphics control extension & local image description
    blocks[13]=blocks[0]; // get width and height from lsd
    blocks[14]=blocks[1];
    blocks[15]=blocks[2];
    blocks[16]=blocks[3];
    blocks[0]= (byte)0x21; // identification stuff
    blocks[1]= (transparency ? (byte)0xf9 : (byte)0xfe);
    blocks[2]= (byte)0x04;
    blocks[3]= (byte)0x01; // packed byte - transparent flag
    blocks[4]= (transparency ? (byte)0 : (byte)0x4a);
    blocks[5]= (transparency ? (byte)0 : (byte)0x44);

    // -2 compensates for +2 applied to all color values, needed for
    // Float storage scenario. See "the LZW algorithm" below
    blocks[6]= (transparency ? (byte)(transparentIndex-2) : (byte)0x4c);
    blocks[8] = (byte)0x2c;
    // these values are already 0
    //  blocks[7]=blocks[9]=blocks[10]=blocks[11]=blocks[12]=blocks[17]=0;
    // xpos and ypos both 0

    // global color table
    stream.write(globalColorTable, 0, globalColorTableSize);

    stream.write(blocks); // graphic control extension

    // due to algorithmic constraints, codeSize must be one larger
    // for two-color algorithms.
    //jm
    if (codeSize < 2)
      codeSize = 2;
    stream.write((byte)codeSize);
    // initial compression size-1 = codeSize



    // the LZW algorithm

    // we can store any string of characters and next character as
    // a pair of codes, (i, j) since for every current
    // sqnc_newcol, sqnc has a valid entry in the table. The pair
    // structure I use is a Float. To ensure code 10 and code 100
    // store differently, the inverse of j is stored instead. And
    // to prevent div by 0 and an inverse >= 1, all numbers are
    // bumped up 2. There's no concatenation here, so things are
    // sped up considerably.

    // Actually, sequences are now stored in Integers instead of Floats,
    // to avoid loss of precision problems encountered with the decimal
    // portion of the Float.

    // Formerly, the hashtable held (Float, Integer) pairs, where the
    // Float is in the form a.b where a = sqnc, and b = 1/(newcol+2).
    // Now, we use a Integer where the top 16 bits store the sequence and
    // the bottom 16 bits store the newcol.
    // -= Simon Lessard =-
    // FIXME: Another line of code, another Hashtable,
    //        Yet again HashMap would be more efficient
    hsh = new Hashtable<Integer, Integer>(_LARGEST_CODE); // max. compression entries

    int code = (1<< codeSize)+2; // where code values start
    int clearCode = (code++)-2; // special codes
    int endOfInformation = (code++)-2;

    int sqnc = 0; // the prefix string of colors
    int newcol = 0; // the new color
    int sqnc_newcol = 0; // concatenation of the above
    Integer fsqnc_newcol = null;



    // these variables are mostly manipulated by _writeByte
    Info info = new Info();

    info.compressionSize = codeSize+1; // compressionSize
    info.theByte = (byte)0;  // theByte
    info.bitsLeft = 8;        // bitsLeft
    info.blockOffset = 1;        // blockOffset
    info.byteData = new byte[_MAXIMUM_COLOR_TABLE_SIZE];     // size of byteData
    info.byteData[0] = (byte)_BLOCK_SIZE; // size byte

    double infoCompSizeExp = 1<< info.compressionSize;

    // clear code starts the data stream
    _writeByte(stream, clearCode, info);
    for (int i = 0; i < pixels.length; i++)
    {
      final int pixel = pixels[i];
      newcol = (pixel >= 0)
        ? pixel+2
        : transparentIndex; // sub in transparent for -1

      assert (sqnc <= 0xffff);
      assert (newcol <= 0xffff);

      sqnc_newcol = (((0xffff0000) & (sqnc << 16)) |
                     ((0x0000ffff) & newcol));

      if (sqnc > 0)
      {
        fsqnc_newcol = sqnc_newcol;
        Integer sqnc_newcol_code = hsh.get(fsqnc_newcol);
        if (sqnc_newcol_code == null)
        {
          // string not in table.
          // write prefix and add string to table
          _writeByte(stream, sqnc-2, info);
          hsh.put(fsqnc_newcol, code++);

          if ((code-2) > infoCompSizeExp)
          { // increase code length
            infoCompSizeExp = 1 << ++info.compressionSize;
          }
          // check for rehash time. If it is, send the
          // clear_code signal,
          // clear hashtable of everything but initialization
          // stuff, reset code, and continue.
          if ((code-2) == _LARGEST_CODE)
          {
            _writeByte(stream, clearCode, info);
            //hsh = new Hashtable(_LARGEST_CODE);
            hsh.clear();

            // +2 for float offset scheme
            // +2 for clear code, eoi. = +4
            code = (1 << codeSize)+4;
            info.compressionSize=codeSize+1;
            infoCompSizeExp = 1<< info.compressionSize;
          }
          sqnc = newcol; // slide everything over
        }
        else
        { // if the float is there, find its integer equivalent
          sqnc = sqnc_newcol_code.intValue();
        }
      }
      else
        sqnc = newcol;
    }
    // last bit of sqnc must be written.
    _writeByte(stream, sqnc-2, info);
    // end the stream
    _writeByte(stream, endOfInformation, info);
    // clean up last block, byte, etc.
    if (info.bitsLeft < 8)
    {
      info.byteData[info.blockOffset++] = info.theByte;
      info.theByte = 0; info.bitsLeft = 8;
    }
    // change size of last block if we didn't just complete one.
    // if we did, this won't change the value
    info.byteData[0] = (byte)(info.blockOffset-1);
    // add 0 byte
    info.byteData[info.blockOffset++] = (byte)0;
    stream.write(info.byteData, 0, info.blockOffset); // image data
    stream.write(0x3b); // terminal byte
  }



  // to avoid unnecessary object creator
  private GifEncoder(){
  }

  // packs bits into bytes and bytes into blocks. Writes when
  // appropriate.
  private static void _writeByte(OutputStream stream, int data, Info info)
    throws IOException
  {
    int offset = info.blockOffset;
    byte b = info.theByte;
    int bitsLeft = info.bitsLeft;
    int size = info.compressionSize; // remaining size of data
    while(size >0)
    {
      // bits from data moving onto b
      int numBits = (bitsLeft < size) ? bitsLeft : size;

      // take new_bits off data.
      byte new_bits = (byte)(data & ((1<<numBits)-1));
      data >>>= numBits; // smash new_bits taken off of data
      new_bits <<= (8-bitsLeft); // move it into position
      b |= new_bits; // push them together
      bitsLeft -= numBits; // spots are now taken
      size -= numBits; // fewer remain
      // might be done with b.
      if (bitsLeft == 0)
      {
        info.byteData[offset++] = b;
        b = 0; bitsLeft = 8;
        // might be done with block
        if (offset > _BLOCK_SIZE)
        {
          stream.write(info.byteData, 0, offset); // image data
          //info.byteData = new byte[_MAXIMUM_COLOR_TABLE_SIZE];
          info.byteData[0] = (byte)_BLOCK_SIZE;
          offset = 1;
        }
      }
    }
    info.blockOffset = offset;
    info.bitsLeft = bitsLeft;
    info.theByte = b;
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

  // Tests whether the color is transparent
  private static boolean _isTransparent(int c)
  {
    // We consider a color to be transparent if the alpha
    // value is lower than a predetermined threshold.
    return (((c >> 24) & 0x000000ff) < _TRANSPARENCY_THRESHHOLD);
  }

  // Tests whether the color is 100% transparent (alpha is zero)
  private static boolean _isFullyTransparent(int c)
  {
    return ((c & 0xff000000) == 0);
  }

  // convenience method to avoid computing log
  private static int getLog(int n)
  {
    int i = 0;
    while (n != 0)
    {
      n >>=1;
      i++;
    }
    return i;
  }
  // largest code value, according to gif spec
  private static final int _LARGEST_CODE = 4096;
  private static final int _BLOCK_SIZE = 254;
  private static final int _MAXIMUM_COLOR_TABLE_SIZE = 256;
  private static final int _NO_COLOR = -2;
  private static final int _TRANSPARENT_COLOR = -1;
  private static final byte[] _HEADER = "GIF89a".getBytes();

  // under this alpha value, pixels are taken to be transparent
  private static final int _TRANSPARENCY_THRESHHOLD = 1;

  // fields used in writing bytes
  private static class Info
  {
    public int bitsLeft;
    public int compressionSize;
    public int blockOffset;
    public byte theByte;
    public byte[] byteData;
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    GifEncoder.class);
}
