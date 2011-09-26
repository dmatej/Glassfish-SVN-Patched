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
import org.apache.myfaces.trinidad.resource.AggregatingResourceLoader;
import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;

/**
 * A resource loader implementation which serves up Trinidad's
 * JavaScript library.
 */
public class CoreCommonScriptsResourceLoader extends AggregatingResourceLoader
{
  public CoreCommonScriptsResourceLoader(
    boolean debug)
  {
    super("",
          debug ? _DEBUG_LIBRARIES : _LIBRARIES,
          new ClassLoaderResourceLoader());

    // force a newline between libraries to avoid the syntax error when
    // the last line of one library contains a line comment "//"
    // and the first line of the next library starts with a
    // block comment "/*"
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

  @Override
  protected String getContentType(
    URLConnection conn)
  {
    return "text/javascript";
  }

  // List of all libraries
  static private final String[] _LIBRARIES =
  {
    "META-INF/adf/jsLibs/DateField.js",
    "META-INF/adf/jsLibs/DateFieldFormat.js",
    "META-INF/adf/jsLibs/MessageBox.js",
    "META-INF/adf/jsLibs/Core.js",
    "META-INF/adf/jsLibs/Window.js",
    //    "META-INF/adf/jsLibs/PPR.js",
    "META-INF/adf/jsLibs/TableProxy.js",
    "META-INF/adf/jsLibs/Poll.js",
    "META-INF/adf/jsLibs/ColorField.js",
    "META-INF/adf/jsLibs/ColorFieldFormat.js",
    "META-INF/adf/jsLibs/Shuttle.js",
    "META-INF/adf/jsLibs/PanelPopup.js",
    "META-INF/adf/jsLibs/PopupDialog.js",
    "META-INF/adf/jsLibs/Page.js",
    "META-INF/adf/jsLibs/StatusIndicator.js",
    // XMMLHttp libraries
    "META-INF/adf/jsLibs/xhr/RequestQueue.js",
    "META-INF/adf/jsLibs/xhr/XMLRequest.js",
    "META-INF/adf/jsLibs/xhr/XMLRequestEvent.js",
    "META-INF/adf/jsLibs/xhr/IFrameXMLRequestEvent.js",
  };

  // List of all libraries
  static private final String[] _DEBUG_LIBRARIES =
  {
    //"META-INF/adf/jsLibsDebug/CharSets.js",
    //"META-INF/adf/jsLibsDebug/NumberFormat.js",
    //"META-INF/adf/jsLibsDebug/NumberConverter.js",
    "META-INF/adf/jsLibsDebug/TrCollections.js",
    //"META-INF/adf/jsLibsDebug/CoreFormat.js",
    "META-INF/adf/jsLibsDebug/DateField.js",
    "META-INF/adf/jsLibsDebug/DateFieldFormat.js",
    //"META-INF/adf/jsLibsDebug/DateFormat.js",
    "META-INF/adf/jsLibsDebug/Locale.js",
    "META-INF/adf/jsLibsDebug/MessageBox.js",
    "META-INF/adf/jsLibsDebug/Core.js",
    "META-INF/adf/jsLibsDebug/Window.js",
    //    "META-INF/adf/jsLibsDebug/PPR.js",
    "META-INF/adf/jsLibsDebug/TableProxy.js",
    "META-INF/adf/jsLibsDebug/Poll.js",
    "META-INF/adf/jsLibsDebug/ColorField.js",
    "META-INF/adf/jsLibsDebug/ColorFieldFormat.js",
    //"META-INF/adf/jsLibsDebug/ColorFormat.js",
    "META-INF/adf/jsLibsDebug/Shuttle.js",
    "META-INF/adf/jsLibsDebug/PanelPopup.js",
    "META-INF/adf/jsLibsDebug/PopupDialog.js",

    "META-INF/adf/jsLibsDebug/Page.js",
    "META-INF/adf/jsLibsDebug/StatusIndicator.js",
    // XMMLHttp libraries
    "META-INF/adf/jsLibsDebug/xhr/RequestQueue.js",
    "META-INF/adf/jsLibsDebug/xhr/XMLRequest.js",
    "META-INF/adf/jsLibsDebug/xhr/XMLRequestEvent.js",
    "META-INF/adf/jsLibsDebug/xhr/IFrameXMLRequestEvent.js",
  };

  static private final String _NEWLINE_SEPARATOR = "\n";
}
