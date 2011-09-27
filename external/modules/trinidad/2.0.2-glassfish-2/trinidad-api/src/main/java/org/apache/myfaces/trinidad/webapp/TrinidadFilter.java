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
package org.apache.myfaces.trinidad.webapp;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Servlet filter that ensures that Trinidad is properly initialized
 * by establishing a RequestContext object;  this filter also processes file
 * uploads.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/webapp/AdfFacesFilter.java#0 $) $Date: 10-nov-2005.19:08:29 $
 */
public class TrinidadFilter implements Filter
{
  public void init(
    FilterConfig filterConfig) throws ServletException
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      _LOG.severe("CANNOT_FIND_CONTEXT_CLASS_LOADER");
    else
    {
      try
      {
        Class<?> proxiedClass = loader.loadClass(
                      "org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl");
        _proxied = (Filter) proxiedClass.newInstance();
        _proxied.init(filterConfig);
      }
      catch (ClassNotFoundException cnfe)
      {
        _LOG.severe(cnfe);
      }
      catch (IllegalAccessException iae)
      {
        _LOG.severe(iae);
      }
      catch (InstantiationException ie)
      {
        _LOG.severe(ie);
      }
      catch (RuntimeException e)
      {
        // OC4J was not reporting these errors properly:
        _LOG.severe(e);
        throw e;
      }
    }


  }

  public void destroy()
  {
    if (_proxied != null)
      _proxied.destroy();
    _proxied = null;
  }

  public void doFilter(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    if (_proxied != null)
      _proxied.doFilter(request, response, chain);
    else
      chain.doFilter(request, response);
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TrinidadFilter.class);

  private Filter _proxied;
}
