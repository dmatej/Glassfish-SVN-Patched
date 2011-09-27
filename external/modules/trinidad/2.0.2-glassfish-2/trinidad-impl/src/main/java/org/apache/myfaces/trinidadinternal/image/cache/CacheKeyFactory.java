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
package org.apache.myfaces.trinidadinternal.image.cache;

import java.util.Map;



import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * The file system-based ImageProvider implementations convert the Map
 * of requested image properties into an object more optimized for hashtable
 * lookups via a CacheKeyFactory.  The image type-specific CacheKeyFactory
 * is stored on the ImageType instance using the
 * CACHE_KEY_FACTORY_PROPERTY key.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CacheKeyFactory.java#0 $) $Date: 10-nov-2005.19:06:01 $
 */
public interface CacheKeyFactory
{
  /**
   * Key used to retrieve the CacheKeyFactory property from an ImageType.
   */
  public static final Object CACHE_KEY_FACTORY_PROPERTY =
    "cacheKeyFactory";

  /**
   * Creates a key suitable for use as a hashtable key.
   * @param context The image context
   * @param property The dictionary of requested image properties
   */
  public Object getCacheKey(ImageContext context, Map<Object, Object> properties);
}

