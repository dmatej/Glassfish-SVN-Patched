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
import java.net.URL;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * NameResolver that locates files loaded relative to a class.  Files
 * will found using Class.getResource().  If no base Class is specified,
 * ClassLoader.getResource() is used.
 *
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/ClassResourceNameResolver.java#0 $) $Date: 10-nov-2005.19:00:06 $
 */
public class ClassResourceNameResolver extends DefaultNameResolver
{
  public ClassResourceNameResolver(Class<?> base)
  {
    super(null, null);

    _base = base;
  }

  @Override
  protected File getFile(String name)
  {
    return null;
  }

  @Override
  protected URL getURL(String name)
  {
    if (_base == null)
      return ClassLoaderUtils.getResource(name);

    return _base.getResource(name);
  }

  private final Class<?> _base;
}
