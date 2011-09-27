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
package org.apache.myfaces.trinidadinternal.skin;

import java.io.File;
import java.io.IOException;

import java.net.URL;

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;

import org.apache.myfaces.trinidadinternal.style.StyleContext;


/**
 * Package-private utility class used by StyleSheetEntry to
 * locate style sheet source files. The NameResolver in the StyleSheetNameResolver looks for
 * the imported stylesheets relative to the base stylesheet.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/StyleSheetNameResolver.java#0 $) $Date: 10-nov-2005.18:59:02 $
 */
class StyleSheetNameResolver implements NameResolver
{
  /**
   * Creates a NameResolver which can locate style sheets in the local styles directory. 
   * This method is called from StyleSheetEntry which loads and parses stylesheets (xss or css).
   * The localStylesDir has already been checked to be valid before this method is called.
   * @param context StyleContext
   * @param localStylesDir File
   * @param provider InputStreamProvider. This object was created in StyleSheetEntry based on
   * the best way to load the particular stylesheet file. 
   * It can be an FileInputStreamProvider, an URLInputStreamProvider, etc.
   * @return
   */
  public static NameResolver createResolver(
    StyleContext        context, 
    File                localStylesDir,
    InputStreamProvider provider)
  {   
    return new StyleSheetNameResolver(localStylesDir, provider);
  }

  /**
   * Creates a StyleSheetNameResolver which looks in the specified
   * styles directories.  Note that the constructor is private since
   * StyleSheetEntry always calls createResolver().
   * @param localStylesDirectory The location of the local styles directory
   * @param provider The InputStreamProvider. e.g., FileInputStreamProvider, URLInputStreamProvider,
   * etc.
   */
  private StyleSheetNameResolver(
    File                localStylesDirectory,
    InputStreamProvider provider
    )
  {
    // We should always have some directory
    assert ((localStylesDirectory != null));
    
    // We should always have some provider
    assert ((provider != null));


    _localStylesDir = localStylesDirectory;
    _provider = provider;

  }

  /**
   * Returns the InputStreamProvider for this StyleSheetNameResolver.
   * @param name the stylesheet name.
   * @return the InputStreamProvider. The InputStreamProvider knows how to return an InputStream
   * of the file, it knows if the source has changed, etc.
   * @throws IOException
   * @see InputStreamProvider
   */
  public InputStreamProvider getProvider(String name) throws IOException
  {
    return _provider;
  }

  /**
   * Implementation of NameResolver.getResolver(). This gets the file that is relative to
   * the base file.
   * @param name  String name of the file that is the imported stylesheet.
   * @return NameResolver. A resolver that knows how to resolve files relative to a base file.
   * e.g., DefaultNameResolver
   */
  public NameResolver getResolver(String name)
  {
    URL url = null;
    File file = StyleSheetEntry.resolveLocalFile(_localStylesDir, name);
    if (file == null)
    {
      // Gets an URL for the specified name. 
      // Try a few different means to get the file as an url: 
      // new URL, ExternalContext's getResource, ClassLoaderUtils getResource

      url = StyleSheetEntry.resolveNonStaticURL(name);
      if (url == null)
        url =StyleSheetEntry.resolveClassLoaderURL(name);
    }

    return new DefaultNameResolver(file, url);
  }

  private File _localStylesDir;
  private InputStreamProvider _provider;
  
}

