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
 * Scriptlet for adding color formatting information.

 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/ColorFormatInfoScriptlet.java#0 $) $Date: 10-nov-2005.19:02:44 $
 */
class ColorFormatInfoScriptlet extends Scriptlet
{
  static public final String COLOR_FORMAT_INFO_KEY = "ColorFormatInfo";

  static public Scriptlet sharedInstance()
  {
    return _sInstance;
  }

  private ColorFormatInfoScriptlet()
  {
  }

  @Override
  public Object getScriptletKey()
  {
    return COLOR_FORMAT_INFO_KEY;
  }

  /**
   * @todo GET TRANSPARENT OFF OF SKIN, NOT HARDCODED BUNDLE
   */
  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    String transparent = arc.getTranslatedString("af_chooseColor.TRANSPARENT");
    writer.writeText("_cfTrans='", null);
    writer.writeText(XhtmlUtils.escapeJS(transparent), null);
    writer.writeText("';", null);
  }

  static private final Scriptlet _sInstance = new ColorFormatInfoScriptlet();
}
