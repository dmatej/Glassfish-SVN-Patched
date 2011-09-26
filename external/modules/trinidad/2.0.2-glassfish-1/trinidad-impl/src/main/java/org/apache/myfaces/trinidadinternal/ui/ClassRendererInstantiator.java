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
package org.apache.myfaces.trinidadinternal.ui;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * Implements deferred loading of Renderer implementations.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/ClassRendererInstantiator.java#0 $) $Date: 10-nov-2005.18:50:12 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class ClassRendererInstantiator implements RendererInstantiator
{
  public ClassRendererInstantiator(String className)
  {
    if (className == null)
      throw new IllegalArgumentException();

    _className = className;
  }


  public Renderer instantiate()
  {
    try
    {
      Class<?> classInstance = ClassLoaderUtils.loadClass(_className);
      return (Renderer) classInstance.newInstance();
    }
    catch (ClassNotFoundException cnfe)
    {
      _showInstantiationError(cnfe);
      throw new UndeclaredThrowableException( cnfe,
         "Instantiation of UIX Components Renderer failed, class " +
         _className + " not found.");
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
    _LOG.severe("RENDERER_INSTANTIATION_FAILED", _className);
    _LOG.severe(e);
  }

  private String _className;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ClassRendererInstantiator.class);
}
