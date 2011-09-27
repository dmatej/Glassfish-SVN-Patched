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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;


/**
 * Abstracts out the retrieval of icons for icon indices.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/LafIconProvider.java#0 $) $Date: 10-nov-2005.18:53:03 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class LafIconProvider
{

  /**
   * Returns the URI to the icon indentified by the icon key
   */
  public abstract String getIconURI(
    UIXRenderingContext context,
    IconKey          iconKey
    );

  /**
   * Returns the URI to the image cache
   */
  public static String getCacheImageURI(
    UIXRenderingContext context
    )
  {
    // See if we've cached off the URI
    String cacheImageURI = (String)context.getProperty(
                                     UIConstants.MARLIN_NAMESPACE,
                                     _CACHE_IMAGE_URI_PROPERTY);

    // Nope, have to compute it
    if (cacheImageURI == null)
    {
      // We explicitly avoid using getSharedConfiguredURL(), since image
      // caches are currently never shared.
      cacheImageURI = BaseLafUtils.getConfiguredURL(
                                      context,
                                      Configuration.IMAGES_CACHE_DIRECTORY);
      context.setProperty(UIConstants.MARLIN_NAMESPACE,
                          _CACHE_IMAGE_URI_PROPERTY,
                          cacheImageURI);
    }

    // Configurations are always supposed to serve directories
    // ending with a delimiter
    assert (cacheImageURI.charAt(cacheImageURI.length() - 1) ==
                    BaseLafConstants.URI_DELIMITER);

    return cacheImageURI;
  }


  private static final String _CACHE_IMAGE_URI_PROPERTY = "cacheImageURI";
}
