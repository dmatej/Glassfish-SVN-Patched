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
 * Interface for obtaining a type-specific name for an image, based on the
 * set of requested properties.  Type-specific name providers are stored
 * on the ImageType using the NAME_PROVIDER_PROPERTY key.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/NameProvider.java#0 $) $Date: 10-nov-2005.19:06:11 $
 */
public interface NameProvider
{
  /**
   * Key used to retrieve the NameProvider property from an ImageType.
   */
  public static final Object NAME_PROVIDER_PROPERTY = "nameProvider";

  /**
   * Returns a name for the image which is generated using the specified
   * properties.
   *
   * @param context The image context
   * @param requestedProperties The client-provided properties which
   *   describe the requested image.
   */
  public String getName(ImageContext context, Map<Object, Object> requestedProperties);
}
