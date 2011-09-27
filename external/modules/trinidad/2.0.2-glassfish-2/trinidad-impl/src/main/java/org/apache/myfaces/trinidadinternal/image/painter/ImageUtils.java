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

import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import java.net.URL;


/**
 * A utility class for working with images.
 * <p>
 * ImageUtils contains utility functions for:
 * <ul>
 * <li>Forcing images to load synchronously
 * <li>Loading image resources
 * <li>Creating a filtered version of an image
 * <li>Creating a disabled version of an image
 * <li>Creating a "not loaded" image that doesn't rely on any
 *     resources external to the class.
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/ImageUtils.java#0 $) $Date: 10-nov-2005.19:04:58 $
 */
public class ImageUtils
{
  /**
   * Given an image and a filter, creates the resulting image.
   * @param baseImage the base image
   * @param imageFilter the image filter
   */
  public static Image createFilteredImage(
    Image       baseImage,
    ImageFilter imageFilter
    )
  {
    // get the filtered image producer
    ImageProducer producer = new FilteredImageSource(baseImage.getSource(),
                                                     imageFilter);

    // return the filtered image
    return Toolkit.getDefaultToolkit().createImage(producer);
  }

  /**
   * Gets an image resource.  If the image cannot be loaded,
   * returns the special "not loaded" image.
   * @param cl the class used to load the resource
   * @param name the path to the resource, relative to the class
   */
  static public Image getImageResource(Class<?> cl, String name)
  {
    Image image = _getImageResource(cl, name);
    if (image == null)
    {
      image = getNotLoadedImage();
    }

    return image;
  }

  /**
   * Gets an image from a InputStream of image data.
   * If the image cannot be loaded, returns null.
   *
   * @param in The InputStream to use for loading image data.  The
   *           InputStream is automatically closed once the image data 
   *           is read.
   */
  static public Image getImageFromStream(InputStream in)
  {
    return _getImageFromStream(in);
  }

  /**
   * Returns the special "not loaded" image, indicating that
   * an image failed to be loaded properly.
   */
  static public Image getNotLoadedImage()
  {
    if (_sNotLoadedImage == null)
    {
      _sNotLoadedImage = _createImage(_NOLOAD_DATA);
    }

    return _sNotLoadedImage;
  }


  /**
   * Return true if the Image has been successfully loaded.
   * <p>
   * @param image Image to check for sucessful loading
   * <p>
   * @return True if the image has been sucessfully loaded.
   */
  static public boolean isImageLoaded(
    Image image
    )
  {
    Toolkit tk = Toolkit.getDefaultToolkit();
    int status = tk.checkImage(image, -1, -1, null);

    return ((status & (ImageObserver.ALLBITS |ImageObserver.FRAMEBITS)) != 0);
  }


  /**
   * Synchronously loads a single image.
   * @return true if the image loaded successfully, false if it
   * failed.
   */
  static public boolean loadImage(Image image)
  {
    if (!isImageLoaded(image))
    {
      ImageLoader load = new ImageLoader(image);

      load.start();
      return load.waitFor();
    }
    else
    {
      return true;
    }
  }


  /**
   * Synchronously loads multiple images.
   * @return true if all images loaded successfully, false if any failed
   */
  static public boolean loadImages(Image[] images)
  {
    boolean value = true;

    ImageLoader loaders[] = new ImageLoader[images.length];
    for (int i = 0; i < images.length; i++)
    {
      loaders[i] = new ImageLoader(images[i]);
      loaders[i].start();
    }

    for (int i = 0; i < images.length; i++)
      value = (value && loaders[i].waitFor());

    /*
    // We could use an array of loaders, but this is only
    // minimally less efficient, since we're only waiting on
    // one image at any one time - it'll just woken up a bit
    // more than necessary
    ImageLoader load = new ImageLoader();

    // Start each image loading, so each'll be loading in
    // its own thread.
    for (int i = 0; i < images.length; i++)
      load.start(images[i]);

    // Now wait for each image in turn - they'll be loading
    // simultaneously, so this is more multithreaded than it looks
    for (int i = 0; i < images.length; i++)
      value = (value && load.waitFor(images[i]));
      */

    return value;
  }


  /**
   * Gets an image resource - won't substitute the "not loaded"
   * image.
   * @param cl the class used to load the resource
   * @param name the path to the resource, relative to the class
   */
  static private Image _getImageResource(Class<?> cl, String name)
  {
     //
    // =-=  bts completely rewrite this method to use getResourceAsStream()
    //      rather than getResource().  There are two reasons for this.
    //      1) Netscape Navigator 4.0 always returns null from getResource(),
    //         while allowing getResourceAsStream() to work
    //
    //      2) The appletviewer's code for loading images from JARs
    //         doesn't work correctly with getResource() if the image
    //         has been purged from memory.
    //
    // Calling getResourceAsStream() instead of getResource() solves both
    // of these problems at the cost of:
    //
    // 1) Slower performance since we have to slurp the stream into a byte
    //    array first.
    //
    // 2) Not being able to purge images from memory.
    //
    // Hopefully, at some point in the future, we will be able to go back
    // to the original code.
    //

    // assume that we won;t have any image to return
    Image  outImage = null;

    // get a stream for the specified resource
    InputStream inStream = cl.getResourceAsStream(name);

    if (inStream != null)
    {
      outImage = _getImageFromStream(inStream);
      if (outImage != null)
        return outImage;
    }

    // =-= bwa restoring the original code in order to give the Appletviewer a
    // second chance to succeed when images are not in a JAR -- the previous
    // strategy fails in that case

    URL imageURL = cl.getResource(name);

    if (imageURL != null)
    {
      try
      {
        Object urlContent = imageURL.getContent();

        // Sun VM's return ImageProducers
        if (urlContent instanceof ImageProducer)
        {
          ImageProducer producer = (ImageProducer) urlContent;

          outImage = Toolkit.getDefaultToolkit().createImage(producer);
        }
        // Microsoft VM's return Images
        else if (urlContent instanceof Image)
        {
          outImage = (Image) urlContent;
        }

        if (outImage != null)
        {
          if (ImageUtils.loadImage(outImage))
            return outImage;
          else
            return null;
        }
      }
      catch (IOException e)
      {
        ;
      }
    }

    return null;
  }

  // Creates an Image using data from the InputStream
  static private Image _getImageFromStream(InputStream inStream)
  {
    // assume that we won;t have any image to return
    Image  outImage = null;

    // assume that we won't have any imageBuffer to turn into an image
    byte[] imageBuffer = null;

    //
    // load the entire stream into the imageBuffer byte array
    //
    try
    {
      BufferedInputStream inBuffer = new BufferedInputStream(inStream);
      ByteArrayOutputStream out =
        new ByteArrayOutputStream(1024);

      imageBuffer = new byte[1024];

      int n;
      while ((n = inBuffer.read(imageBuffer)) > 0)
        out.write(imageBuffer, 0, n);

      out.flush();
      imageBuffer = out.toByteArray();
    }
    catch (IOException e)
    {
      ;
    }
    finally
    {
      try
      {
        // make sure that we close the stream no matter what
        inStream.close();
      }
      catch (IOException e)
      {
        ;
      }
    }

    //
    // if we managed to load the byte array from the stream,
    // create an image from it and synchronously load the image
    //
    if (imageBuffer != null)
    {
      outImage = Toolkit.getDefaultToolkit().createImage(imageBuffer);

      if (!ImageUtils.loadImage(outImage))
      {
        // synchronous loading of image failded, so don't return any image
        //
        // =-= bts it might be better to still return the image in this
        //     case.  What are we hurting by doing so?
        outImage = null;
      }
    }

    // return the image
    return outImage;
  }

  // Private constructor to prevent clients from creating
  // instances of this utility class.
  private ImageUtils()
  {
  }


  // Creates an image from a string representation of the
  // byte data.  This implementation currently only uses
  // the bottom 8 bits of each character (i.e., one byte
  // per character) - this is efficient on disk, given its
  // UTF-8 representation, but twice as big as necessary
  // in memory.  OTOH, it means we don't need any special
  // case code for odd lengths!
  static private Image _createImage(String string)
  {
    int length = string.length();

    byte[] data = new byte[length];
    for (int i = 0;  i < length; i++)
      data[i] = (byte) string.charAt(i);

    return Toolkit.getDefaultToolkit().createImage(data);
  }

  // Inline version of the "This image couldn't be loaded" image
  private static final String _NOLOAD_DATA =
  "\u0047\u0049\u0046\u0038\u0039\u0061\u000e\u0000\u0010\u0000\u00b3" +
  "\u0000\u0000\u0000\u0000\u0000\u0080\u0000\u0000\u0000\u0080\u0000" +
  "\u0080\u0080\u0000\u0000\u0000\u0080\u0080\u0000\u0080\u0000\u0080" +
  "\u0080\u00c0\u00c0\u00c0\u0080\u0080\u0080\u00ff\u0000\u0000\u0000" +
  "\u00ff\u0000\u00ff\u00ff\u0000\u0000\u0000\u00ff\u00ff\u0000\u00ff" +
  "\u0000\u00ff\u00ff\u00ff\u00ff\u00ff\u0021\u00f9\u0004\u0001\u0000" +
  "\u0000\u000f\u0000\u002c\u0000\u0000\u0000\u0000\u000e\u0000\u0010" +
  "\u0000\u0000\u0004\u0049\u0010\u00c8\u0049\u00d1\u00bb\u00e0\u00ea" +
  "\u008d\u00ec\u00cb\u0047\u0028\u0086\u0096\u0005\u0086\u0082\u0020" +
  "\u007a\u00c8\u0029\u0028\u0002\u0010\u0052\u006e\u002a\u008b\u00df" +
  "\u0079\u0048\u00e3\u0091\u003f\u00bd\u0003\u0063\u0098\u0091\u0000" +
  "\u0013\u0023\u0086\u0083\u0051\u0004\u0035\u0090\u00a1\u0021\u00a1" +
  "\u0009\u006c\u003c\u0067\u0093\u00df\u00c1\u007a\u001d\u0069\u0069" +
  "\u005e\u0063\u0030\u009c\u00d9\u0098\u0031\u00b4\u00f4\u0024\u0002" +
  "\u0000\u003b";

  // Image cache for common images
  static private Image     _sNotLoadedImage;
}
