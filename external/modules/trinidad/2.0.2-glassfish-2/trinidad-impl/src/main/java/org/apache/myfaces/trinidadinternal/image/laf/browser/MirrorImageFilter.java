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

import java.awt.image.ColorModel;
import java.awt.image.ImageFilter;

/**
 * ImageFilter which produces the mirror image.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/MirrorImageFilter.java#0 $) $Date: 10-nov-2005.19:05:11 $
 */
class MirrorImageFilter extends ImageFilter
{
  @Override
  public void setDimensions(int width, int height) 
  {
    if (!_done)
    {
      _width = width;
      _height = height;
      _pixels = new int[width * height];
    }

    super.setDimensions(width, height);
  }

  @Override
  public void setPixels(
    int x, 
    int y, 
    int width, 
    int height,
    ColorModel model, 
    byte pixels[], 
    int off,
    int scansize
    )
  {
    // If setDimensions() hasn't been called - and we haven't allocated
    // our pixels array, just blow off this call to setPixels().  Hopefully,
    // we'll see a subsequent call to setDimensions() and redelivery of
    // the pixels in question.
    if (_pixels == null)
      return;

    for (int n = y; n < (y + height); n++)
    {
      for (int m = x; m < (x + width); m++)
      {
        byte value = pixels[n * scansize + m + off];
        _pixels[n * _width + m] = model.getRGB((value & 0xff));
      }
    }
  }

  @Override
  public void setPixels(
    int x, 
    int y, 
    int width, 
    int height,
    ColorModel model, 
    int pixels[], 
    int off,
    int scansize
    )
  {
    // If setDimensions() hasn't been called - and we haven't allocated
    // our pixels array, just blow off this call to setPixels().  Hopefully,
    // we'll see a subsequent call to setDimensions() and redelivery of
    // the pixels in question.
    if (_pixels == null)
      return;

    for (int n = y; n < (y + height); n++)
    {
      for (int m = x; m < (x + width); m++)
      {
        int value = pixels[n * scansize + m + off];
        _pixels[n * _width + m] = model.getRGB(value);
      }
    }
  }

  @Override
  public void imageComplete(int status) 
  {
    int[] pixels = _pixels;

    if (!_done && (pixels != null) && (status == STATICIMAGEDONE))
    {
      
      // Send one scan line at a time, with the flipped results
      int[] scanline = new int[_width];

      for (int y = 0; y < _height; y++)
      {
        for (int x = 0; x < _width; x++)
        {
          scanline[_width - x - 1] = pixels[y * _width + x];
        }

        super.setPixels(
          0, y, 
          _width, 1, 
          ColorModel.getRGBdefault(), 
          scanline,
          0, _width);
      }

      _pixels = null;
      _done = true;
    }

    super.imageComplete(status);
  }

  private int[]   _pixels;
  private int     _width;
  private int     _height;
  private boolean _done;
}
