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

import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;
import org.apache.myfaces.trinidad.resource.ResourceLoader;


/**
 * A resource loader implementation which loads resources
 * using the context class loader with a rootPackage of META-INF. This will
 * find the resources within the META-INF directory.
 */
public class CoreClassLoaderResourceLoader extends ClassLoaderResourceLoader
{
  /**
   * Creates a new CoreClassLoaderResourceLoader
   */
  public CoreClassLoaderResourceLoader(ResourceLoader parent)
  {
   super("META-INF", parent);
  }


    
  /**
   * Override to pull out the version from the path.
   */
   @Override
  protected URL findResource(
    String path) throws IOException
  {
    // Strip the version string, since it is not present in the actual file name.
    String version = CoreRenderKitResourceLoader.__getVersion();
    int index = path.indexOf(version);
    if (index >= 0)
    {
      path = (path.substring(0, index) +
              path.substring(index + version.length()));
    }

    // Fix up "jsLibs/Debug" -> "jsLibsDebug/".
    path = _fixJavaScriptDebugPath(path);

    return super.findResource(path);
  }

  // LibraryScriptlet prefixes debug JS libraries with the prefix
  // "Debug".  So, for example,  the "Foo.js" library ends up being
  // referred to via an URL of the form "<base>/jsLibs/DebugFoo.js".
  // Unfortunately, this is out of sync with reality, since our
  // JS libraries actually live under a "jsLibsDebug" directory,
  // ie. the debug "Foo.js" lives at "META-INF/adf/jsLibsDebug/Foo.js".
  // As a result, CoreClassLoaderResourceLoader fails to locate
  // scripts that are rendered via a LibraryScriptlet.
  //
  // To work around this problem, this code converts paths of the
  // form "jsLibs/Debug" to "jsLibsDebug/", which allows our
  // library look ups to succeed.
  //
  // Note that an alternate approach might be to tweak LibraryScriptlet
  // to generate the paths that CoreClassLoaderResourceLoader expects.
  // One issue with this is that our "Common" and "LocaleInfo" libraries
  // are handled somewhat differently, so any changes to LibraryScriptlet
  // will impact at least those scripts as well.  Performing this mapping
  // here seems the least intrusive.
  private String _fixJavaScriptDebugPath(String path)
  {
    return (path.indexOf(_JS_DEBUG_PATH) >= 0) ?
      path.replaceFirst(_JS_DEBUG_PATH, _FIXED_JS_DEBUG_PATH) :
      path;
  }

  private static final String _JS_DEBUG_PATH = "jsLibs/Debug";
  private static final String _FIXED_JS_DEBUG_PATH = "jsLibsDebug/";
}
