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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * Scriptlet which defines global variable needed by Common.js
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/GlobalVariablesScriptlet.java#0 $) $Date: 10-nov-2005.19:02:46 $
 */
public class GlobalVariablesScriptlet extends Scriptlet
{
  public GlobalVariablesScriptlet()
  {
  }

  @Override
  public Object getScriptletKey()
  {
    return GLOBAL_VARIABLES_KEY;
  }

  static public Scriptlet sharedInstance()
  {
    if (_sInstance == null)
    {
      _sInstance = new GlobalVariablesScriptlet();
      _sInstance.registerSelf();
    }
    return _sInstance;
  }

  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
      ResponseWriter writer = context.getResponseWriter();
      Object errObj =
        arc.getTranslatedString(_WINDOW_CREATION_ERROR_KEY);
      String jsErr = XhtmlUtils.escapeJS(errObj.toString());
      writer.writeText("var _AdfWindowOpenError='" + jsErr + "';", null);
  }

  public  static final String GLOBAL_VARIABLES_KEY         = "GlobalVariables";
  private static       GlobalVariablesScriptlet _sInstance = null;
  private static final String _WINDOW_CREATION_ERROR_KEY
    = "WINDOW_CREATION_ERROR";

}
