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

/**
 * A Dynamic Resource Loader that is capable of agregation and dynamic content via custom
 * URLConnection objects.
 * 
 */
public abstract class DynamicResourceLoader extends ResourceLoader
{  
  /**
   * Creates a DynamicResourceLoader for a specified path.  The dynamic resource loader will only
   * respond to the specified path when findResource is executed with the same path.  If it is not
   * the same, findResource will TRY to redirect the request to the parent if one is provided.  The
   * parent ResourceLoader for this constructor is <code>null</code>
   * 
   * @param path the path that this ResourceLoader will respond to
   */

  public DynamicResourceLoader(String path)
  {
    this(path, null);
  }
  
  /**
   * Creates a CompositeResouceLoader for a specified path.  The dynamic resource loader will only
   * respond to the specified path when findResource is executed with the same path.  If it is not
   * the same, findResource will TRY to redirect the request to the parent if one is provided.
   * 
   * @param path the path that this ResourceLoader will respond to
   * @param parent the parent resource loader
   */
  public DynamicResourceLoader(String path, ResourceLoader parent)
  {
    super(parent);
    
    if(path==null)
    {
      throw new NullPointerException();
    }
    
    _path = path;
  }
  
  /**
   * Finds a resource.  If the path does not match the path that this object was constructed with
   * then it will direct the request to the base ResourceLoader.  If the path is the same, it calls
   * the getURL method.
   * 
   * @param path the path which the ResourceLoader is looking for.
   * @return a URL for this resource
   * @throws IOException when something bad happens
   */
  @Override
  protected URL findResource(String path) throws IOException
  {
    if (_path.equals(path))
    {
      return getURL(path);
    }

    return super.findResource(path);
  }
  
  /**
   * Returns a URL to this resource.  This URL should have a valid connection.'
   * 
   * @param path the path of the findResourceCall
   * @return a url for this Resource
   * @throws IOException when something bad happens
   */
  protected abstract URL getURL(String path) throws IOException;
  
  private String _path;
 
}
