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

import javax.servlet.ServletContext;

/**
 * A resource loader implementation which loads resources
 * using the servlet context.
 *
 */
public class ServletContextResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new ServletContextResourceLoader.
   * 
   * @param context  the servlet context
   */
  public ServletContextResourceLoader(
    ServletContext context)
  {
    _context = context;
  }
  
  /**
   * Constructs a new ServletContextResourceLoader with specified parent
   * resource loader.
   * 
   * @param context  the servlet context
   * @param parent  the parent resource loader
   */
  public ServletContextResourceLoader(
    ServletContext context,
    ResourceLoader parent)
  {
    super(parent);
    _context = context;
  }
  
  @Override
  protected URL findResource(
    String path) throws IOException
  {
    return _context.getResource(path);
  }
 
  /* 
  protected Enumeration findResources(
    String path) throws IOException
  {
    return new IteratorEnumeration(_context.getResourcePaths(path).iterator());
  }
  */
  
  private final ServletContext _context;
}
