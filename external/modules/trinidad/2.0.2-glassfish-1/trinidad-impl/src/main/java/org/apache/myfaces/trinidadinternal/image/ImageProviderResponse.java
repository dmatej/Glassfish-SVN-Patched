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

import java.util.Collection;

import org.apache.myfaces.trinidadinternal.image.util.MapArea;


/**
 * ImageProviderResponse objects are returned by the ImageProvider
 * in response to successful image requests.  The ImageProviderResponse
 * interface provide access to the properties of the returned image,
 * including its URI, width and height.
 *
 * @see ImageProvider
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageProviderResponse.java#0 $) $Date: 10-nov-2005.19:03:55 $
 */
public interface ImageProviderResponse
{
  /**
   * Value returned by getWidth() and getHeight() if the size
   * is not known.
   */
  public static final int UNKNOWN_SIZE = -1;

  /**
   * Returns the URI of the image.
   */
  public String getImageURI();

  /**
   * Returns the width of the requested image.
   * <p>
   * Returns UNKNOWN_SIZE if the width is unknown.
   */
  public int getWidth();

  /**
   * Returns the height of the requested image.
   * <p>
   * Returns UNKNOWN_SIZE if the height is unknown.
   */
  public int getHeight();

  /**
   * Returns an ImmutableArray of MapArea objects which describe the
   * regions of the image which should be included in the image's
   * map.
   *
   * @see org.apache.myfaces.trinidadinternal.image.util.MapArea
   */
  public Collection<MapArea> getMapAreas();
}
