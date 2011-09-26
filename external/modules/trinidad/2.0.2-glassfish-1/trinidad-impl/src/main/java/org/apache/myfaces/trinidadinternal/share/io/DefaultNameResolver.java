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
import java.io.FileNotFoundException;
import java.io.IOException;

import java.net.URL;
import java.net.MalformedURLException;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;



/**
 * Default implementation of NameResolver.  This class
 * provides support for resolving against both file directories
 * and URLs.
 * <p>
 * DefaultNameResolver also adds special functionality for handling
 * the creation of sub-NameResolvers.  Creation of File or URL
 * InputStreamProviders is always deferred back to the original, root
 * DefaultNameResolver.  This allows the root NameResolver to support
 * caching or hook in needed behavior to the providers.  (Note that
 * the locating of the File or URL is always left to the child resolver).
 * <p>
 * DefaultNameResolver will always attempt to resolve names as files
 * first, first relative to the base file (if one exists), then
 * as an absolute path.  If that fails, it will attempt to resolve
 * the path as an URL.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/DefaultNameResolver.java#0 $) $Date: 10-nov-2005.19:00:07 $
 */
public class DefaultNameResolver implements NameResolver
{
  /**
   * Creates a DefaultNameResolver.
   * @param baseFile a base file to resolve names (possibly null)
   * @param baseURL a base URL to resolve names
   */
  public DefaultNameResolver(File baseFile, URL baseURL)
  {
    if (baseFile != null)
    {
      if (!baseFile.isDirectory())
        baseFile = baseFile.getParentFile();
      _baseFile = baseFile;
    }

    _baseURL  = baseURL;
  }

  /**
   * Given a name, returns an InputStreamProvider.  This
   * function should never return null - if the target
   * cannot be resolved, throw an IOException.
   * @param name the name of the target
   */
  public InputStreamProvider getProvider(String name) throws IOException
  {
    // For "root" names (names that start with a slash),
    // give the root NameResolver a crack at it first.
    if (_isRoot(name))
    {
      try
      {
        return _getRootResolver().getProvider(_getRootName(name));
      }
      catch (IOException ioe)
      {
        // Ignore the exception, and try locally
        ;
      }
    }

    File file = getFile(name);
    if (file != null)
      return __getProvider(file);
    
    URL url = getURL(name);
    if (url != null)
      return __getProvider(url);

    throw new FileNotFoundException(_LOG.getMessage(
      "CANNOT_FIND_FILE", name));
  }

  /**
   * Return the new NameResolver that should be used to resolve
   * names relative to a given name.  This function should never
   * return null - if the target cannot be resolved, return a
   * resolver that can only support absolute names.
   * @param name the name of the target
   */
  final public NameResolver getResolver(String name)
  {
    // For "root" names (names that start with a slash),
    // give the root NameResolver a crack at it first.
    if (_isRoot(name))
    {
      return _getRootResolver().getResolver(_getRootName(name));
    }

    DefaultNameResolver base = getResolverImpl(name);

    // Attach a root provider
    if (base != null)
    {
      DefaultNameResolver root = _root;
      if (root == null)
        root = this;

      base.__setRootResolver(root);
    }
    else
    {
      base = this;
    }

    return base;
  }


  /**
   * Return the new NameResolver that should be used to resolve
   * names relative to a given name.  Unlike getResolver(),
   * the resolver returned by getRootResolver() acts as a new root -
   * requests that begin with slashes will be resolved relative to
   * this resolver, not the original root.  This function should never
   * return null - if the target cannot be resolved, return a
   * resolver that can only support absolute names.
   * @param name the name of the target
   */
  final public NameResolver getRootResolver(String name)
  {
    NameResolver resolver = getResolver(name);
    if (resolver instanceof DefaultNameResolver)
    {
      ((DefaultNameResolver) resolver).__setRootResolver(null);
    }

    return resolver;
  }

  @Override
  public String toString()
  {
    String val = super.toString() + "[";
    if (_baseFile != null)
      val += _baseFile.toString();
    if (_baseURL != null)
    {
      if (_baseFile != null)
        val += ",";

      val += _baseURL.toString();
    }

    return val + "]";
  }

  
  /**
   * Resolve a name into a File.  This function
   * should only return files that exist.
   */
  protected File getFile(String name)
  {
    if (name == null)
      return null;

    if ((name.indexOf('/') >= 0) &&
        !(File.separatorChar == '/'))
      name = name.replace('/', File.separatorChar);

    File basePath = _baseFile;
    File file = new File(basePath, name);
    if (!file.exists())
    {
      file = null;
      // If we couldn't find it, see if it was actually a full
      // path to the file
      if (basePath != null)
      {
        file = new File(name);
        if (!file.exists())
          file = null;
      }
    }

    return file;
  }


  /**
   * Resolve a name into an URL.
   */
  protected URL getURL(String name) // ?? throws MalformedURLException
  {
    if (name == null)
      return null;

    URL url;

    try
    {
      if (name.startsWith("http:") ||
          name.startsWith("https:") ||
          name.startsWith("file:") ||
          name.startsWith("ftp:") ||
          name.startsWith("jar:"))
        url = new URL(name);
      else if (_baseURL != null)
        url = new URL(_baseURL, name);
      else
        url = null;
    }
    catch (MalformedURLException mue)
    {
      url = null;
    }

    return url;
  }


  /**
   * Create a child resolver based on a name.
   */
  protected DefaultNameResolver getResolverImpl(String name)
  {
    File file = getFile(name);
    if (file != null)
      return new DefaultNameResolver(file, null);
    
    URL url = getURL(name);
    if (url != null)
      return new DefaultNameResolver(null, url);

    return null;
  }

  
  /**
   * Creates a provider for a File.
   */
  protected InputStreamProvider getProvider(File file)
  {
    return new FileInputStreamProvider(file);
  }


  /**
   * Creates a provider for an URL.
   */
  protected InputStreamProvider getProvider(URL url)
  {
    return new URLInputStreamProvider(url);
  }


  //
  // Get a file provider from the root
  //
  InputStreamProvider __getProvider(File file)
  {
    DefaultNameResolver root = _root;
    if (root != null)
    {
      return root.__getProvider(file);
    }

    return getProvider(file);
  }


  //
  // Get an URL provider from the root
  //
  InputStreamProvider __getProvider(URL url)
  {
    DefaultNameResolver root = _root;
    if (root != null)
    {
      return root.__getProvider(url);
    }

    return getProvider(url);
  }

  // Store the "root" resolver
  void __setRootResolver(DefaultNameResolver root)
  {
    _root = root;
  }


  // Returns the root resolver (or "this" if we're the root)
  private DefaultNameResolver _getRootResolver()
  {
    if (_root == null)
      return this;

    return _root;
  }

  // Returns true if the name is a "root" name
  static private boolean _isRoot(String name)
  {
    if ((name == null) || (name.length() == 0))
      return false;

    return name.charAt(0) == '/';
  }

  // Converts a "root" name into a name that can be 
  // passed to the root resolver
  static private String _getRootName(String name)
  {
    return name.substring(1);
  }

  private DefaultNameResolver _root;

  private File _baseFile;
  private URL  _baseURL;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    DefaultNameResolver.class);
}
