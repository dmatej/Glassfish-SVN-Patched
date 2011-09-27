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
package org.apache.myfaces.trinidad.resource;

import java.io.IOException;

import java.net.URL;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * A resource loader implementation which loads resources
 * using the context class loader.  The returned resource URL will be null
 * for paths that attempt to access paths outside the root directory by having
 * ".." in the path.
 *
 */
public class ClassLoaderResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new root ClassLoaderResourceLoader.
   */
  public ClassLoaderResourceLoader()
  {
    this((String)null);
  }

  /**
   * Constructs a new ClassLoaderResourceLoader with specified parent.
   *
   * @param parent  the parent resource loader
   */
  public ClassLoaderResourceLoader(
    ResourceLoader parent)
  {
    this(null, parent);
  }

  /**
   * Constructs a new root ClassLoaderResourceLoader with specified top
   * level resource package.
   *
   * @param rootPackage  the top level package used to interpret resource paths
   * For example, it could be "META-INF".
   */
  public ClassLoaderResourceLoader(
    String rootPackage)
  {
    _resourcePrefix = _getResourcePrefix(rootPackage);
  }

  /**
   * Constructs a new root ClassLoaderResourceLoader with specified top
   * level resource package and parent resource loader.
   *
   * @param rootPackage  the top level package used to interpret resource paths
   * @param parent  the parent resource loader
   */
  public ClassLoaderResourceLoader(
    String rootPackage,
    ResourceLoader parent)
  {
    super(parent);
    _resourcePrefix = _getResourcePrefix(rootPackage);
  }

  @Override
  protected URL findResource(
    String path) throws IOException
  {
    // Strip off leading slash, since this can
    // trip up ClassLoader.getResource().
    if (path.charAt(0) == '/')
      path = path.substring(1);

    if (_resourcePrefix != null)
    {
      if (_resourcePrefix.endsWith("/"))
      {
        path = _resourcePrefix + path;
      }
      else
      {
        path = _resourcePrefix + "/" + path;
      }
    }

    if (!_isPathWithinRoot(path))
    {
      _LOG.severe("RESOURCE_PATH_OUTSIDE_ROOT", path);
      return null;
    }
    
    return getClassLoader().getResource(path);
  }

  /**
   * Returns the ClassLoader to use when looking up resources under the top
   * level package.  By default, this is the context class loader.
   *
   * @return the ClassLoader used to lookup resources
   */
  protected ClassLoader getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  /**
   * Converts root package into a resource prefix.  For example, converts
   * the package "org.example" into resource prefix "org/example".
   *
   * @param rootPackage  the root package
   *
   * @return the resource prefix
   */
  static private String _getResourcePrefix(
    String rootPackage)
  {
    if (rootPackage == null ||
        rootPackage.length() == 0)
    {
      return null;
    }

    return rootPackage.replace('.', '/');
  }
  
  /**
   * Checks to see if the path when ".." are resolved out is within the root.
   * Returns true if the path is within the root, otherwise returns false.
   * e.g., /afr/../../foo.gif is out of root. It gets resolved to /../foo.gif.
   * /afr/../tmp/../xyz/foo.gif is within the root. It gets resolved to
   * /xyz/foo.gif
   * 
   * @param path -String the path
   * @return boolean true if the path does not get resolved outside the root
   */
  private static boolean _isPathWithinRoot(String path)
  {
    if (path.indexOf("..") == -1)
      return true;
      
    // It would be strange if the path has ".." in it because
    // the browsers (IE7 and Firefox 1.5) resolve ".." out of the path before
    // requesting the resource.
    // Therefore we warn if we find a path with "..".
    // Then, we try to resolve it ourselves to find out if it is outside
    // the root.
    _LOG.warning("RESOURCE_PATH_DOTS", path);
    
    while (path != null && path.length() > 0 )
    {
      // as soon as it starts with .. or /.., we know we are out of bounds
      if (path.startsWith(".."))
        return false;
      int index = path.indexOf("/../");
      if (index == 0)
        return false;
    
      // couldn't find "/../" in the path, so we are within the root
      if (index == -1)
        return true;
    
      // resolve out the first match of "/../" by taking it out as well
      // as the /foo/ before it. 
      String beginning = path.substring(0, index);
      String end = path.substring(index+4);

      int lastIndex = beginning.lastIndexOf("/");
      if (lastIndex == -1)
      {
        //could not find / in the beginning string, so strip the entire thing
        path = end;
      }
      else
      {
        path = beginning.substring(0, lastIndex+1) +  end;
      }
    } 
    
    return true;
  }
  

  private final String _resourcePrefix;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ClassLoaderResourceLoader.class);
}
