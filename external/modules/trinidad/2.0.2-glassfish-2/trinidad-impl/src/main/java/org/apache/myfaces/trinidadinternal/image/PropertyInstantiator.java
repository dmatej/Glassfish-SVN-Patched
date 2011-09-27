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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * PropertyInstantiator is used by ImageType instances to implement deferred
 * loading of type-specific properties.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/PropertyInstantiator.java#0 $) $Date: 10-nov-2005.19:03:58 $
 */
public class PropertyInstantiator
{
  public PropertyInstantiator(String className)
  {
    if (className == null)
      throw new IllegalArgumentException();

    _className = className;
  }

  public String getClassName()
  {
    return _className;
  }

  public Object instantiate()
  {
    try
    {
      Class<?> classInstance = ClassLoaderUtils.loadClass(_className);
      return classInstance.newInstance();
    }
    catch (ClassNotFoundException cnfe)
    {
      _showInstantiationError(cnfe);
    }
    catch (IllegalAccessException iae)
    {
      _showInstantiationError(iae);
    }
    catch (InstantiationException ie)
    {
      _showInstantiationError(ie);
    }

    return null;
  }

  private void _showInstantiationError(
    Throwable e
    )
  {
    _LOG.severe("FAIL_INSTANTIATE_PROPERTY", _className);
    _LOG.severe(e);
  }

  private String _className;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(PropertyInstantiator.class);
}
