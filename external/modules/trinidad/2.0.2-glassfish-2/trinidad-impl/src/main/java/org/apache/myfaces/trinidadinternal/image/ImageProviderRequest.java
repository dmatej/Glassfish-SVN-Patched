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

/**
 * The ImageProviderRequest interface is used by the ImageProvider
 * to access the properties of a requested image.
 *
 * @see ImageProvider
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageProviderRequest.java#0 $) $Date: 10-nov-2005.19:03:54 $
 */
public interface ImageProviderRequest
{
  /**
   * Returns the namespace URI for the image described by this request.
   */
  public String getNamespaceURI();

  /**
   * Returns the local name of the image described by this request.
   */
  public String getLocalName();

  /**
   * Returns the dictionary of properties to use when rendering
   * the requested image.
   * <P>
   * The properties returned from getRenderProperties() should
   * be suitable for consumption by an ImageRenderer.  As such,
   * properties should be defined by the *_KEY constants defined
   * by the ImageConstants interface.
   *
   * @see ImageRenderer
   * @see ImageConstants
   */
  public Map<Object, Object> getRenderProperties(ImageContext context);
}
