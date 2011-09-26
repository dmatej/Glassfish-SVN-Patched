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

/**
 * Scriptlet which hands off configuration information
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/ConfigurationScriptlet.java#0 $) $Date: 10-nov-2005.19:02:44 $
 */
public class ConfigurationScriptlet extends Scriptlet
{
  public ConfigurationScriptlet()
  {
  }

  @Override
  public Object getScriptletKey()
  {
    return _CONFIGURATION_SCRIPTLET_KEY;
  }

  static public Scriptlet sharedInstance()
  {
    if (_sInstance == null)
    {
      _sInstance = new ConfigurationScriptlet();
      _sInstance.registerSelf();
    }
    return _sInstance;
  }

  /**
   * @todo See if truly necessary.
   */
  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc)
    throws IOException
  {
    JspDirScriptlet.sharedInstance().embedInScriptTag(context, arc);

    ResponseWriter writer = context.getResponseWriter();

    // write the character set encoding
    String characterEncoding = writer.getCharacterEncoding();
    if (characterEncoding != null)
    {
      writer.writeText("var _enc='", null);
      writer.writeText(characterEncoding, null);
      writer.writeText("';", null);
    }
  }

  private static final Object _CONFIGURATION_SCRIPTLET_KEY = new Object();
  private static       ConfigurationScriptlet _sInstance   = null;
}
