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
package org.apache.myfaces.trinidadinternal.share.io;

import java.io.File;

import java.net.MalformedURLException;
import java.net.URL;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

/**
 * NameResolver that can look for servlet-based resources.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/ServletNameResolver.java#0 $) $Date: 10-nov-2005.19:00:10 $
 */
public class ServletNameResolver extends DefaultNameResolver
{
  /**
   * Create a ServletNameResolver.
   */
  public ServletNameResolver(
    ServletRequest      request,
    ServletContext      context)
  {
    super(null, null);

    _request = request;
    _context = context;
  }

  @Override
  protected File getFile(String name)
  {
    String rootName;
    if (name.startsWith("/"))
      rootName = name;
    else
      rootName = "/" + name;

    File file;
    if (_context != null)
      file = _getFile(_context.getRealPath(rootName));
    else
      file = null;

    if ((_request != null) && (file == null))
    {
      // Try a second way to make JServ happy
      // -= Simon Lessard =-
      // FIXME: That call is deprecated, is it really needed?
      file = _getFile(_request.getRealPath(rootName));
    }

    if (file != null)
      return file;

    return super.getFile(name);
  }

  @Override
  protected URL getURL(String name)
  {
    if (_context == null)
      return null;
      
    if (! name.startsWith("/"))
      name = "/" + name;

    try
    {
      URL resource = _context.getResource(name);
      return resource;
    }
    catch (MalformedURLException mue)
    {
      return null;
    }
  }

  static private File _getFile(String path)
  {
    if (path != null)
    {
      File file = new File(path);
      if (file.exists())
        return file;
    }

    return null;
  }

  private final ServletRequest _request;
  private final ServletContext _context;
}
