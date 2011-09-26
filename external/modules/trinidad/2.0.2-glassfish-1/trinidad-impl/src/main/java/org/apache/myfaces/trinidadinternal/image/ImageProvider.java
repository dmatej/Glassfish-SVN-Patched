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

/**
 * The ImageProvider interface provides access to a set of images.
 * Images are requested via the getImage() method.  getImage()
 * takes a number of arguments which describe the requested image,
 * including an ImageProviderRequest object.
 * It returns an ImageProviderResponse  which can be used to access the 
 * requested image's URI.
 * 
 * @see ImageProviderRequest
 * @see ImageProviderResponse
 * @see ImageType
 * @see ImageTypeManager
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageProvider.java#0 $) $Date: 10-nov-2005.19:03:53 $
 */
public interface ImageProvider
{
  /**
   * Looks up an image based on a set of requested properties.  If
   * the requested image exists (or can be created), the ImageProvider
   * returns an response object which specified information about the
   * requested image, such as its URI.
   * 
   * @param context The context object
   * @param request The request object is used to accesss the properties
   *   of the requested image.
   * @return Returns an ImageProviderResponse object which indicates 
   *  the location of the image, among other information.
   *
   * @see ImageContext
   * @see ImageProviderRequest
   * @see ImageProviderResponse
   */
  public ImageProviderResponse getImage(
    ImageContext         context,
    ImageProviderRequest request
    );
}
