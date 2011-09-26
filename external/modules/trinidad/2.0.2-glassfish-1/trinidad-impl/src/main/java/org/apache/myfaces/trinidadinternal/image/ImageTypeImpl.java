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

import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ImageType implementation used by the ImageTypeManager.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageTypeImpl.java#0 $) $Date: 10-nov-2005.19:03:57 $
 */
class ImageTypeImpl implements ImageType
{
  public ImageTypeImpl(
    String namespaceURI,
    String name,
    Map<Object, Object> properties)
  {
    _namespace = namespaceURI;
    _name = name;
    _properties = new ArrayMap<Object, Object>();

    setProperties(properties);
  }

  /**
   * Implementation of ImageType.getNamespace().
   */
  public String getNamespaceURI()
  {
    return _namespace;
  }

  /**
   * Implementation of ImageType.getName().
   */
  public String getLocalName()
  {
    return _name;
  }

  /**
   * Implementation of ImageType.getProperty().
   */
  synchronized public Object getProperty(Object key)
  {
    Object value = _properties.get(key);

    if (value instanceof PropertyInstantiator)
    {
      value = ((PropertyInstantiator)value).instantiate();

      assert (value != null);

      _properties.put(key, value);
    }

    // We do some defaulting to be nice
    if (value == null)
    {
      if (IMAGE_RENDERER_NAME_PROPERTY.equals(key))
      {
        Object renderer = getProperty(IMAGE_RENDERER_PROPERTY);
        if (renderer != null)
          value = renderer.getClass().getName();
      }
    }

    return value;
  }

  /**
   * Implementation of MutableImageType.setProperty().
   */
  synchronized public void setProperty(Object key, Object value)
  {
    if (value == null)
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_VALUE"));
    }

    _properties.put(key, value);
  }

  /**
   * Sets the properties contained in the specified dictionary.
   * This method is not defined on ImageType or MutableImageType -
   * it is provided as a convenience for ImageTypeManager.
   */
  public void setProperties(Map<Object, Object> properties)
  {
    if (properties == null)
      return;
    
    for(Map.Entry<Object, Object> entry : properties.entrySet())
    {
      setProperty(entry.getKey(), entry.getValue());
    }
  }

  @Override
  public String toString()
  {
    return getClass().getName() + "[" + _namespace + ", " + _name + "]";
  }

  private String   _namespace;
  private String   _name;
  private ArrayMap<Object, Object> _properties;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ImageTypeImpl.class);
}
