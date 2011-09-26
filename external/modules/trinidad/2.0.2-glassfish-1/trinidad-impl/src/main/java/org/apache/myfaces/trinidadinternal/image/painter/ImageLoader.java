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

import java.awt.Image;
import java.awt.Toolkit;

import java.awt.image.ImageObserver;

/**
 * ImageObserver implementation used for synchronously loading
 * images.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/ImageLoader.java#0 $) $Date: 10-nov-2005.19:04:56 $
 */
public class ImageLoader implements ImageObserver
{
  // Creates the ImageLoader
  public ImageLoader(Image image)
  {
    if (image == null)
      throw new IllegalArgumentException();

    _image = image;
    //    _status = 0;
  }


  // Starts the image loading
  public synchronized void start()
  {
    Toolkit.getDefaultToolkit().prepareImage(_image, -1, -1, this);
  }

  // Waits until the image is done loading;  start(image) must
  // have already been called (or any other function that starts
  // preparing the image)
  public synchronized boolean waitFor()
  {
    Toolkit tk = Toolkit.getDefaultToolkit();

    _status |= tk.checkImage(_image, -1, -1, this);

    while ((_status &
            (ImageObserver.ALLBITS   |
             ImageObserver.FRAMEBITS |
             ImageObserver.ERROR     |
             ImageObserver.ABORT)) == 0)
    {
      try
      {
        wait();
      }
      catch (InterruptedException e)
      {
        Thread.currentThread().interrupt();
        return false;
      }
    }

    return ((_status & (ImageObserver.ALLBITS |
                        ImageObserver.FRAMEBITS)) != 0);
  }

  // Implementation of the ImageObserver interface;  we don't
  // care about any of the values except for infoflags
  public synchronized boolean imageUpdate(
    Image img,
    int infoflags,
    int x,
    int y,
    int width,
    int height
    )
  {
    _status |= infoflags;

    if ((infoflags &
          (ImageObserver.ALLBITS |
           ImageObserver.FRAMEBITS |
           ImageObserver.ERROR   |
           ImageObserver.ABORT)) != 0)
    {
      notifyAll();
      return false;
    }

    return true;
  }

  private Image _image;
  private int   _status;
}
