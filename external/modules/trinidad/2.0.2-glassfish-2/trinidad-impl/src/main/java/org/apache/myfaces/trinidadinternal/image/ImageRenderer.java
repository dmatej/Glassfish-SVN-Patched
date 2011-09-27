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

import java.util.Map;

import java.awt.Image;

/**
 * ImageRenderer defines an interface for generated an AWT Image from
 * a set of properties describing the desired image.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageRenderer.java#0 $) $Date: 10-nov-2005.19:03:56 $
 */
public interface ImageRenderer
{
  /**
   * Render and return an image given the dictionary of
   * properties that describe what to render.  In addition
   * to returning the rendered image, the image renderer may
   * store additional properties describing the generated image
   * (such as its width and height) in the provided responseProperties
   * dictionary.
   *
   * @param context The rendering context
   * @param requestedProperties Map of requested properties.
   *   The keys for this dictionary are the KEY constants defined
   *   in ImageConstants.
   * @param responseProperties Map for response properties.
   *   The keys for this dictionary are the RESPONSE_KEY constants
   *   defined in ImageConstants.
   * @return an Image containing the rendered results
   * @see ImageContext
   * @see ImageConstants
   */
  public Image renderImage(
    ImageContext context,
    Map<Object, Object> requestedProperties,
    Map<Object, Object> responseProperties
    );
}
