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
 * ImageType objects define type-specific properties of
 * the supported types of images (ie. buttons, tab bars, etc...).  Each
 * image type is identified by a namespace and a name and specifies
 * a set of type-specific properties.  ImageTypes can be registered
 * and retrieved via the ImageTypeManager.
 *
 * @see org.apache.myfaces.trinidadinternal.image.ImageTypeManager
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageType.java#0 $) $Date: 10-nov-2005.19:03:56 $
 */
public interface ImageType
{
  /**
   * Key used to access the ImageType's renderer.
   * The value of this property is an ImageRenderer instance which
   * can be used to renderer images of this type.
   * @see org.apache.myfaces.trinidadinternal.image.ImageRenderer
   */
  public static final Object IMAGE_RENDERER_PROPERTY = "renderer";

  /**
   * Key used to access the ImageType's XML encoder.
   * The value of this property is an XMLEncoder instance which can
   * be used to encode ImageGenerator XML data for this type.
   * @see org.apache.myfaces.trinidadinternal.image.xml.encode.XMLEncoder
   * @see org.apache.myfaces.trinidadinternal.image.tools.ImageGenerator
   */
  public static final Object XML_ENCODER_PROPERTY = "encoder";

  /**
   * Key used to access the Boolean "localized" property.
   * Some image types are not localized - eg. colorized icons and
   * global buttons are constant across locales.  If the value of
   * this property is Boolean.FALSE, a single image is created for
   * all locales.
   */
  public static final Object LOCALIZED_PROPERTY = "localized";

  /**
   * Key used to access the name of the ImageType's renderer.
   * The value of this property is a String which uniquely identifies
   * the ImageRenderer for this type, eg. the class name of the
   * ImageRenderer.
   * @see org.apache.myfaces.trinidadinternal.image.ImageRenderer
   */
  public static final Object IMAGE_RENDERER_NAME_PROPERTY = "rendererName";

  /**
   * Key used to specify whether or not the source icon
   * should be checked for changes.  Some image types - eg. global buttons,
   * depend on source icons which may be modified by the customer.
   * If this property is set to Boolean.TRUE, the timestamp of the
   * source icon is periodically checked to see if the file has been
   * changed.
   */
  public static final Object CHECK_SOURCE_PROPERTY = "checkSource";

  /**
   * Returns the namespace of this ImageType
   */
  public String getNamespaceURI();

  /**
   * Returns the name of this ImageType
   */
  public String getLocalName();

  /**
   * Returns a property associated with this image type.
   */
  public Object getProperty(Object key);

  /**
   * Sets a property on the ImageType.
   * @param key The key for the property
   * @param value The value of the property.  If the value is an
   *  instance of PropertyInstantiator, the PropertyInstantiator will
   *  be used to create the true value of this property when it is
   *  first requested.
   * @see org.apache.myfaces.trinidadinternal.image.PropertyInstantiator
   */
  public void setProperty(Object key, Object value);
}
