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

import java.io.File;
import java.io.IOException;

import java.net.URL;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A resource loader implementation which loads resources
 * from a directory.  The returned resource URL will be null
 * for file resources that do not exist, or for relative paths
 * that attempt to access paths outside the root directory.
 *
 */
public class DirectoryResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new DirectoryResourceLoader.
   *
   * @param directory  the root directory
   */
  public DirectoryResourceLoader(
    File directory)
  {
    if (directory == null)
      throw new NullPointerException();

    if (!directory.isDirectory())
      throw new IllegalArgumentException();

    _directory = directory;
    
    try
    {
      _directoryPath = _directory.getCanonicalPath();
    }
    catch (IOException ex)
    {
      throw new IllegalArgumentException(ex);
    }    
  }

  /**
   * Constructs a new DirectoryResourceLoader.
   *
   * @param directory  the root directory
   * @param parent     the parent resource loader
   */
  public DirectoryResourceLoader(
    File           directory,
    ResourceLoader parent)
  {
    super(parent);

    if (directory == null)
      throw new NullPointerException();

    if (!directory.isDirectory())
      throw new IllegalArgumentException();

    _directory = directory;
    
    try
    {
      _directoryPath = _directory.getCanonicalPath();
    }
    catch (IOException ex)
    {
      throw new IllegalArgumentException(ex);
    }

  }

  @Override
  protected URL findResource(
    String path) throws IOException
  {
    if (path.charAt(0) == '/')
      path = path.substring(1);

    // construct the relative file under the "root" directory
    File file = new File(_directory, path).getCanonicalFile();
    

    // file path should contain the "root" directory path, not be outside it
    boolean isContained = file.getCanonicalPath().startsWith(_directoryPath);
    
    // return null if relative paths were used, 
    // or if the file does not exist,
    // otherwise return an URL to the file resource
    // 2006-08-01: -= Simon Lessard =-
    //             File.toURL is deprecated in JDK 6.0 because the method 
    //             does not escape invalid characters, toURI().toURL is the 
    //             preferred way as of JDK 6.0.
    return (isContained && file.exists()) ? file.toURI().toURL() : null;
  }

  private final File _directory;
  private final String _directoryPath;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    DirectoryResourceLoader.class);
}
