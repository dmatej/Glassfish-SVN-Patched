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

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import java.util.Map;

/**
 * Abstract base class that implements ImageRenderer.
 * <p>
 * Subclasses should always create their images
 * by calling createImage() so that the type of 
 * image 
 *
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/AbstractImageRenderer.java#0 $) $Date: 10-nov-2005.19:03:51 $
 */
abstract public class AbstractImageRenderer 
  implements ImageRenderer, ImageObserver
{
  /**
   * Render the BufferedImage given the Map of
   * properties that describe what to render.
   */
  abstract public Image renderImage(
    ImageContext context,
    Map<Object, Object> requestedProperties,
    Map<Object, Object> responseProperties
    );

  /**
   * Implementation of ImageObserver.ImageUpdate().
   * <p>
   * @see java.awt.image.ImageObserver
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

  /**
   * Create an AbstractImageRenderer.
   */
  protected AbstractImageRenderer() {}

  /**
   * Creates an Image with the given width and height.
   * Subclasses should use this method to create images so
   * that the correct memory representation is used.
   */
  protected BufferedImage createImage(int width, int height)
  {
    return new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
  }
}
