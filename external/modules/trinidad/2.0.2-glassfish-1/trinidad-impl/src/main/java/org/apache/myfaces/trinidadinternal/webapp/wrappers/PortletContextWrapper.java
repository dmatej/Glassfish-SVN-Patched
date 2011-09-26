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
package org.apache.myfaces.trinidadinternal.webapp.wrappers;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Set;

import javax.portlet.PortletContext;
import javax.portlet.PortletRequestDispatcher;

import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestType;

 /**
  * Wrapper for the native PortletContext object.  Unlike the other Portlet Wrapper classes in
  * this package, this class may be used for both Portlet 1.0 and 2.0 containers.
  *
  * @version $Revision$ $Date$
  */
public class PortletContextWrapper implements PortletContext
{
  public PortletContextWrapper(PortletContext context)
  {
    _context = context;
  }

  private PortletContext _context;

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getAttribute(java.lang.String)
   */
  public Object getAttribute(String arg0)
  {
    return _context.getAttribute(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getAttributeNames()
   */
  public Enumeration<String> getAttributeNames()
  {
    return _context.getAttributeNames();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getInitParameter(java.lang.String)
   */
  public String getInitParameter(String arg0)
  {
    return _context.getInitParameter(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getInitParameterNames()
   */
  public Enumeration<String> getInitParameterNames()
  {
    return _context.getInitParameterNames();
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getMajorVersion()
   */
  public int getMajorVersion()
  {
    return _context.getMajorVersion();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getMimeType(java.lang.String)
   */
  public String getMimeType(String arg0)
  {
    return _context.getMimeType(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getMinorVersion()
   */
  public int getMinorVersion()
  {
    return _context.getMinorVersion();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getNamedDispatcher(java.lang.String)
   */
  public PortletRequestDispatcher getNamedDispatcher(String arg0)
  {
    return _context.getNamedDispatcher(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getPortletContextName()
   */
  public String getPortletContextName()
  {
    return _context.getPortletContextName();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getRealPath(java.lang.String)
   */
  public String getRealPath(String arg0)
  {
    return _context.getRealPath(arg0);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getRequestDispatcher(java.lang.String)
   */
  public PortletRequestDispatcher getRequestDispatcher(String arg0)
  {
    return _context.getRequestDispatcher(arg0);
  }

  /**
   * @param arg0
   * @return
   * @throws MalformedURLException
   * @see javax.portlet.PortletContext#getResource(java.lang.String)
   */
  public URL getResource(String arg0) throws MalformedURLException
  {
    return _context.getResource(arg0);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getResourceAsStream(java.lang.String)
   */
  public InputStream getResourceAsStream(String arg0)
  {
    return _context.getResourceAsStream(arg0);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletContext#getResourcePaths(java.lang.String)
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Set getResourcePaths(String arg0)
  {
    return _context.getResourcePaths(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletContext#getServerInfo()
   */
  public String getServerInfo()
  {
    return _context.getServerInfo();
  }

  /**
   * @param arg0
   * @param arg1
   * @see javax.portlet.PortletContext#log(java.lang.String, java.lang.Throwable)
   */
  public void log(String arg0, Throwable arg1)
  {
    _context.log(arg0, arg1);
  }

  /**
   * @param arg0
   * @see javax.portlet.PortletContext#log(java.lang.String)
   */
  public void log(String arg0)
  {
    _context.log(arg0);
  }

  /**
   * @param arg0
   * @see javax.portlet.PortletContext#removeAttribute(java.lang.String)
   */
  public void removeAttribute(String arg0)
  {
    _context.removeAttribute(arg0);
  }

  /**
   * @param arg0
   * @param arg1
   * @see javax.portlet.PortletContext#setAttribute(java.lang.String, java.lang.Object)
   */
  public void setAttribute(String arg0, Object arg1)
  {
    _context.setAttribute(arg0, arg1);
  }

  /**
   * Portlet 2.0 only functionality.  These wrappers are not intended for Portlet 2.0.  Marked
   * final in order to prevent overloading.
   */
  public Enumeration<String> getContainerRuntimeOptions()
  {
    if(!_PORTLET_2_CONTAINER)
    {
      throw new UnsupportedOperationException("This method is only supported in Portlet 2.0 containers");
    }
    
    return _context.getContainerRuntimeOptions();
  } 
  
  private static final boolean _PORTLET_2_CONTAINER = ExternalContextUtils.isRequestTypeSupported(RequestType.RESOURCE);
}
