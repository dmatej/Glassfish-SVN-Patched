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

import java.awt.Toolkit;
import java.awt.Image;
import java.awt.image.FilteredImageSource;
import java.io.OutputStream;
import java.io.IOException;

/**
 * Implementation of ImageEncoder that runs an image through an AlphaMultiplyFilter
 * before passing it to an encoder.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/AlphaMultiplyEncoder.java#0 $) $Date: 10-nov-2005.19:05:14 $
 * @since 0.1.4
 */
public class AlphaMultiplyEncoder implements ImageEncoder
{
  public AlphaMultiplyEncoder(ImageEncoder wrappedEncoder)
  {
    _wrappedEncoder = wrappedEncoder;

  }

  /**
   * Implementation of ImageEncoder.encodeImage()
   */
  public void encodeImage(Image image, OutputStream out)
    throws IOException
  {
    Image alphaFilteredImage = Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(image.getSource(), 
                                                                                               new AlphaMultiplyFilter(_WHITE))); 
    Image filteredImage = Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(alphaFilteredImage.getSource(),
                                                                                          new OctreeFilter(alphaFilteredImage)));
    _wrappedEncoder.encodeImage(filteredImage, out);
    
  }
  
  private static final int _WHITE = 0x00ffffff;
  private ImageEncoder _wrappedEncoder;


}
