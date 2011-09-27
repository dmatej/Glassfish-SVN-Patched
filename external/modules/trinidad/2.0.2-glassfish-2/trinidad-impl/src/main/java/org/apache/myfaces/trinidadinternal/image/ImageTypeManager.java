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



import org.apache.myfaces.trinidadinternal.share.util.NamespaceMap;

import org.apache.myfaces.trinidadinternal.image.laf.browser.BlafImageUtils;

/**
 * ImageTypeManager maintains a set of ImageType objects, each of which
 * defines a type of image component (eg. button, tab bar, etc...) which
 * can be generated (and cached).  Each ImageType is identified by
 * a name and namespace.  ImageType instances provide the type-specific
 * information needed to generate (and cache) images through a set of
 * properties which can be accessed via the ImageType inteface.
 * <p>
 * Clients can use the default ImageTypeManager, which can be obtained
 * via the getDefaultImageTypeManager(), or create their own.
 *
 * @see org.apache.myfaces.trinidadinternal.image.ImageType
 * @see org.apache.myfaces.trinidadinternal.image.ImageConstants
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageTypeManager.java#0 $) $Date: 10-nov-2005.19:03:57 $
 */
public class ImageTypeManager
{
  /**
   * Creates a new, empty ImageTypeManager.
   */
  public ImageTypeManager()
  {
  }

  /**
   * Returns the default, shared instance of ImageTypeManager.
   * This instance is initialized with entries for the
   * BLAF image types.
   */
  static public ImageTypeManager getDefaultImageTypeManager()
  {
    if (_sDefaultInstance == null)
    {
      ImageTypeManager manager = new ImageTypeManager();
      BlafImageUtils.registerImageTypes(manager);
      _sDefaultInstance = manager;
    }

    return _sDefaultInstance;
  }

  /**
   * Returns an ImageType object for the specified type.
   * If no ImageType of the specified namespace and name has been
   * registered, returns null.
   * @param namespace The namespace for the type
   * @param name The name of the type
   */
  synchronized public ImageType getImageType(String namespace, String name)
  {
    return (ImageType)_types.get(namespace, name);
  }

  /**
   * Registers the specified image type.
   * If an image type of the specified name/namespace is already
   * registered, the specified properties are registered with the
   * image type, which is returned.
   *
   * @param namespace The namespace of the image type
   * @param name The name of the image type
   * @param properties The properties of the image type
   * @return The ImageType for the specified name and namespace
   * is returned.
   */
  synchronized public ImageType registerImageType(
    String namespace,
    String name,
    Map<Object, Object> properties
    )
  {
    ImageType type = getImageType(namespace, name);

    if (type != null)
    {
      // Just register the properties
      assert (type instanceof ImageTypeImpl);

      ((ImageTypeImpl)type).setProperties(properties);
    }
    else
    {
      // Create the new image type
      type = new ImageTypeImpl(namespace, name, properties);

      _types.put(namespace, name, type);
    }

    return type;
  }

  /**
   * Unregisters the specified image type.
   * If the specified type is not registered, no action is performed.
   * @param type The ImageType to unregister.
   */
  synchronized public void unregisterImageType(ImageType type)
  {
    _types.remove(type.getNamespaceURI(), type.getLocalName());
  }

  static private ImageTypeManager _sDefaultInstance;

  // Map of namespaces/names to ImageTypes
  private NamespaceMap _types = new NamespaceMap();
}
