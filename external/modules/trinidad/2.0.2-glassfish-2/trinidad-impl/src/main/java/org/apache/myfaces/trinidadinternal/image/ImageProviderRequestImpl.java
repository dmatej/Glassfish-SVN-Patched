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
import org.apache.myfaces.trinidad.logging.TrinidadLogger;



/**
 * Implementation of ImageProviderRequest.
 *
 * @see ImageProviderRequest
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageProviderRequestImpl.java#0 $) $Date: 10-nov-2005.19:03:54 $
 */
public class ImageProviderRequestImpl implements ImageProviderRequest
{
  private ImageProviderRequestImpl() {}

  /**
   * Creates an ImageProviderRequestImpl with the specified
   * namespace, name and render properties.
   */
  public ImageProviderRequestImpl(
    String     namespaceURI,
    String     name,
    Map<Object, Object> renderProperties)
  {
    if (namespaceURI == null) 
    {
      throw new NullPointerException("Null namespaceURI");
    }
    
    if (name == null) 
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_NAME"));
    }

    _namespace = namespaceURI;
    _name = name;
    _properties = renderProperties;
  }

  /**
   * Implementation of ImageProviderRequest.getNamespaceURI().
   */
  public String getNamespaceURI()
  {
    return _namespace;
  }

  /**
   * Implementation of ImageProviderRequest.getLocalName().
   */
  public String getLocalName()
  {
    return _name;
  }

  /**
   * Implementation of ImageProviderRequest.getRenderProperties().
   */
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    return _properties;
  }

  private String     _namespace;
  private String     _name;
  private Map<Object, Object> _properties;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ImageProviderRequestImpl.class);
}
