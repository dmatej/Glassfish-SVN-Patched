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

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;

/**
 * Icon key for flipped icons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/FlippedIconKey.java#0 $) $Date: 10-nov-2005.19:06:06 $
 */
public class FlippedIconKey
  implements ImageProviderRequest, CacheKey, ImageConstants
{
  /**
   * Creates a FlippedIconKey for the specified ImageContext
   * and properties.
   */
  public FlippedIconKey(
    Map<Object, Object> properties
    )
  {
    String source = (String)properties.get(SOURCE_KEY);

    _init(source);
  }

  /**
   * Creates a key for the specified context, source and direction.
   */
  public FlippedIconKey(
    String source)
  {
    _init(source);
  }


  /**
   * Implementation of ImageProviderRequest.getNamespaceURI().
   */
  public String getNamespaceURI()
  {
    return TECATE_NAMESPACE;
  }

  /**
   * Implementation of ImageProviderRequest.getLocalName().
   */
  public String getLocalName()
  {
    return FLIPPED_ICON_NAME;
  }

  /**
   * Override of Object.equals().
   */
  @Override
  public boolean equals(Object o)
  {
    // This equals implementation assumes that the same source icon name
    // is not used as both a CoreColorizedIcon and an AccentColorizedIcon.
    if (this == o)
      return true;

    if (!(o instanceof FlippedIconKey))
      return false;

    FlippedIconKey key = (FlippedIconKey)o;

    return (_source.equals(key._source) );
  }

  /**
   * Override of Object.hashCode().
   */
  @Override
  public int hashCode()
  {
    return (_source.hashCode() );
  }

  /**
   * Implementation of ImageProviderRequest.getRenderProperties().
   */
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    ArrayMap<Object, Object> properties = new ArrayMap<Object, Object>(_MAP_SIZE);
    properties.put(ImageConstants.SOURCE_KEY, getSource());

    return properties;
  }

  /**
   * Returns the source property.
   */
  protected String getSource()
  {
    return _source;
  }

  /**
   * Sets the source property.
   */
  protected void setSource(String source)
  {
    _source = source;
  }

  private void _init(
    String source
    )
  {
    _source = source;
  }

  private String      _source;

  // The size for the render properties ArrayMap.  We leave room for
  // the SOURCE_INPUT_STREAM_PROVIDER_KEY property, typically provided
  // via a subclass.
  private static final int _MAP_SIZE = 3;

}
