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


import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Abstracts out the retrieval of ImageProviderResponses for
 * icon indices.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/ColorizedLafIconProvider.java#0 $) $Date: 10-nov-2005.18:52:59 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class ColorizedLafIconProvider extends LafIconProvider
                           implements ColorizedIconProvider
{
  
  /**
   * Retrieves the ImageProviderReponse for the image indentified
   * by the iconKey
   */
  public abstract ImageProviderResponse getColorizedIcon(
    UIXRenderingContext context,
    IconKey          iconKey);
  



  /**
   * Returns the URI to the icon indentified by the icon key
   */
  public String getColorizedIconURI(
    UIXRenderingContext context,
    IconKey          iconKey
    )
  {
    ImageProviderResponse response = getColorizedIcon(context, iconKey);
    
    if (response != null)
    {
      return getCacheImageURI(context) + response.getImageURI();
    }
    else
    {
      return null;
    }
  }

}
