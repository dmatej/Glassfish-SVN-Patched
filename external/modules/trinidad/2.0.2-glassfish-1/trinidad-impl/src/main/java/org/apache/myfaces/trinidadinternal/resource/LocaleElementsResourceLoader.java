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
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;

import java.net.URL;
import java.net.URLConnection;

import java.util.ArrayList;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.AggregatingResourceLoader;
import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;
import org.apache.myfaces.trinidad.resource.ResourceLoader;

/**
 * A resource loader implementation which serves up the 
 * LocaleElements & ResourceBundle JS libs.
 */
public class LocaleElementsResourceLoader extends AggregatingResourceLoader
{
  public LocaleElementsResourceLoader()
  {
    super("",
          _INIT_LIBRARIES,
          new ClassLoaderResourceLoader());
          
    setSeparator(_NEWLINE_SEPARATOR);
  }
  
  /**
   * Since CoreRenderKitResourceLoader already does the matching, this method is overridden
   * to just call getURL()
   */
  @Override
  protected URL findResource(String path) throws IOException
  {
    return getURL(path);
  }
  
  
  /**
   * Returns a URL which is an aggregate of all the paths.
   *
   * @param path the current path
   * @return a aggregate url
   * @throws IOException when something bad happens
   */
  @Override
  protected URL getURL(String path) throws IOException
  {
    String[] _LIBRARIES = _getLibraries();
    
    int len = _LIBRARIES.length;
    ArrayList<URL> urls = new ArrayList<URL>(len);
    
    for(int i = 0; i < len; i++)
    {
      URL u = _ResourceLoaders[i].getResource(_LIBRARIES[i]);
      if(u != null)
      {
        urls.add(u);
      }
      else
      {
        _LOG.warning("RESOURCE_NOT_FOUND", new Object[]{_LIBRARIES[i], path});
      }
    }

    urls.trimToSize();
    URL[] urlArray = urls.toArray(new URL[0]);

    AggregatingURLStreamHandler handler = new AggregatingURLStreamHandler(urlArray, _NEWLINE_SEPARATOR);
    return new URL("aggregating", null, -1, path, handler);
  }

  @Override
  protected String getContentType(
    URLConnection conn)
  {
    return "text/javascript";
  }
  
  private String[] _getLibraries()
  {
    String[] _LIBRARIES =
    {
      "META-INF" + CoreRenderKitResourceLoader.getLocaleElementsURI("LocaleElements", false),
      "META-INF" + CoreRenderKitResourceLoader.getLocaleElementsURI("Translations", false)
    };
    
    return _LIBRARIES;
  }
  
  // List of all libraries
  static private String[] _INIT_LIBRARIES = { "LocaleElements", "Translations" };
  
  // List of ResourceLoaders
  static private final ResourceLoader[] _ResourceLoaders =
  {
    new ClassLoaderResourceLoader(),
    new TrTranslationsResourceLoader(CoreRenderKitResourceLoader.getLocaleElementsURI("Translations", false))
  };
  
  static private final String _NEWLINE_SEPARATOR = "\n";
  static private final TrinidadLogger _LOG       = TrinidadLogger.createTrinidadLogger(LocaleElementsResourceLoader.class);
}
