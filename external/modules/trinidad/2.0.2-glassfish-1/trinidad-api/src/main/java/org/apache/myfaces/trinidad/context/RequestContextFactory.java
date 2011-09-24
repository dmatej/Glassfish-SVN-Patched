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
package org.apache.myfaces.trinidad.context;

import java.util.WeakHashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Factory for creating RequestContext objects.
 * 
 */
abstract public class RequestContextFactory
{
  /**
   * Retrieve the current RequestContextFactory.
   */
  static public RequestContextFactory getFactory()
  {
    synchronized (_FACTORIES)
    {
      return _FACTORIES.get(_getClassLoader());
    }
  }

  /**
   * Store the current RequestContextFactory.
   */
  static public void setFactory(RequestContextFactory factory)
  {
    synchronized (_FACTORIES)
    {
      ClassLoader cl = _getClassLoader();
      if (_FACTORIES.get(cl) != null)
      {
        throw new IllegalStateException(_LOG.getMessage(
          "FACTORY_ALREADY_AVAILABLE_FOR_CLASS_LOADER"));
      }

      _FACTORIES.put(cl, factory);
    }
  }
  
  /**
   * Create a RequestContext from a ServletContext and ServletRequest.
   * 
   * @param context an object which must be a ServletContext
   * @param request an object which must be a ServletRequest
   * 
   * @deprecated This method does not work in a Portal environment.  It will
   *             only work with a ServletRequest.  Please use 
   *             {@link #createContext(ExternalContext)} which is container
   *             agnostic.
   */
  @Deprecated
  abstract public RequestContext createContext(Object context,
                                                Object request);
  
  /**
   * Creates a RequestContext.
   * 
   * @param ec The current ExternalContext.
   */
  abstract public RequestContext createContext(ExternalContext ec);
  
  static private ClassLoader _getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  static private final Map<ClassLoader, RequestContextFactory> _FACTORIES = 
    new WeakHashMap<ClassLoader, RequestContextFactory>();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RequestContextFactory.class);
}
