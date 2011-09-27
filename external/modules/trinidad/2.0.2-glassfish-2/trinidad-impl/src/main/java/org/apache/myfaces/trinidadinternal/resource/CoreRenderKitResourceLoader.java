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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.net.URL;

import javax.faces.context.FacesContext;

import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.RegexResourceLoader;
import org.apache.myfaces.trinidad.resource.ResourceLoader;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * A resource loader implementation which loads resources
 * for the core renderkit.
 *
 *
 * @todo Dynamic version number
 */
public class CoreRenderKitResourceLoader extends RegexResourceLoader
{
  public CoreRenderKitResourceLoader(ResourceLoader parent)
  {
    register("(/.*/Common.*\\.js)",
             new CoreCommonScriptsResourceLoader(false));
    register("(/.*/DebugCommon.*\\.js)",
             new CoreCommonScriptsResourceLoader(true));
    
    register("(/.*/CoreFmt.*\\.js)",
             new CoreFormatScriptsResourceLoader(false));
    register("(/.*/DebugCoreFmt.*\\.js)",
             new CoreFormatScriptsResourceLoader(true));
    
    register("(/.*LocaleElements.*\\.js)",
             new LocaleElementsResourceLoader()); 

    register("(/.*\\.(css|cur|ico|jpg|gif|png|jpeg|svg|js))",
             new CoreClassLoaderResourceLoader(parent));
  }

    
  static public String getLocaleElementsURI(String str, 
                                            Boolean incVersion)
  {
    StringBuffer base = new StringBuffer("/adf/jsLibs/resources/");

    base.append(str);
    base.append("_");

    String locStr = getLocale();
    
    base.append(locStr);
    if(incVersion) base.append(_VERSION);
    base.append(".js");

    return base.toString();
  }

  static public String getLocale()
  {
    String path = ((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest()).getPathInfo();
    String locStr = "";

    int locIndex = path.indexOf("LocaleElements")+ "LocaleElements_".length();
    int index = path.indexOf(_VERSION);

    if (index < 0)
      index = path.indexOf(".js");

    if(index >= 0)
      locStr = path.substring(locIndex, index);

    return locStr;
  }

  static public String __getVersion()
  {
    return _VERSION;
  }

  // Path to ResourceServlet
  // Version string to append to library, style sheet URIs
  static private final String _VERSION;

  static private final TrinidadLogger _LOG =
                          TrinidadLogger.createTrinidadLogger(CoreRenderKitResourceLoader.class);

  static
  {
    // Note: Java Package versioning is useless during development when
    //       we have no JARs, whereas this technique works with non-JAR
    //       classpaths as well.
    String version = "unknown";

    try
    {
      URL resource =
        ClassLoaderUtils.getResource("META-INF/trinidad-version.txt");
      if (resource != null)
      {
        BufferedReader br = null;
        try
        {
          InputStream in = resource.openStream();
          br = new BufferedReader(new InputStreamReader(in));
          version = br.readLine();
        }
        catch (IOException e)
        {
          _LOG.severe(e);
        }
        finally
        {
          if (br != null)
            br.close();
        }
      }
    }
    catch (IOException e)
    {
      _LOG.severe(e);
    }
    finally
    {
      _VERSION = version;
    }
  }
}
