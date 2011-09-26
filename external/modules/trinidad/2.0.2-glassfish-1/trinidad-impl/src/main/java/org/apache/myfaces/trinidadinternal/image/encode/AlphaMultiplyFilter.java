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

import java.awt.image.RGBImageFilter;

/**
 * A filter which transforms an alpha based rgb value into an rgb value
 * which incorporates alpha values without a discrete alpha channel.
 * The background color must be presumed beforehand.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/AlphaMultiplyFilter.java#0 $) $Date: 10-nov-2005.19:05:15 $
 * @since 0.1.4
 */
class AlphaMultiplyFilter extends RGBImageFilter
{

  /** 
   * Constructor for the requires the background color.
   * @param rgb The background color, as an rgb int
   */
  public AlphaMultiplyFilter(int rgb)
  {
    canFilterIndexColorModel = true;
    _red = (rgb>>16)&_BYTE_MASK;
    _green = (rgb>>8)&_BYTE_MASK;
    _blue = rgb&_BYTE_MASK;
  
  }

  /**
   * Implementation of abstract method from RGBImageFilter
   */
  @Override
  public int filterRGB(int x, int y, int rgb)
  {

    // percentage of opacity of the color
    int alpha = (rgb>>24)&_BYTE_MASK;
    double opacity = (double)alpha / (double)_OPAQUE;
    double transparency = 1 - opacity;

    //initial color values
    int red = (rgb>>16)&_BYTE_MASK;
    int green = (rgb>>8)&_BYTE_MASK;
    int blue = rgb&_BYTE_MASK;

    red = (int)((red*opacity) + (_red*transparency)); 
    green = (int)((green*opacity) + (_green*transparency)); 
    blue = (int)((blue*opacity) + (_blue*transparency)); 
    
    // this code should be replaced when background is reliably determined. 
    // It kills stray alphas that come out really white.
    if (alpha < 100 && alpha > 0)
    {
      int value = Math.max(red, Math.max(green, blue));
      if (value > _VALUE_THRESHHOLD)
        alpha = 0;
    }
    
    // return color is opaque, but rgb has been appropriately modified
    int retVal = (alpha<<24)|(red<<16)|(green<<8)|blue;
    
    return retVal;
  }
  

  // ensure only a byte of data survives
  private static final int _BYTE_MASK = 0xff;
  private static final int _OPAQUE = 0xff;

  // point at which alpha-multiplied pixels are made transparent to avoid artifacts
  private static final int _VALUE_THRESHHOLD = 220;
  // the background color
  private int _red;
  private int _green;
  private int _blue;

}


