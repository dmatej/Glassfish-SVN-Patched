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

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * Scriptlet which defines the styles needed for lightweight dialogs
 */
public class DialogStyleScriptlet extends Scriptlet
{
  public DialogStyleScriptlet()
  {
  }

  @Override
  public Object getScriptletKey()
  {
    return DIALOG_STYLES_KEY;
  }

  static public Scriptlet sharedInstance()
  {
    if (_sInstance == null)
    {
      _sInstance = new DialogStyleScriptlet();
      _sInstance.registerSelf();
    }
    return _sInstance;
  }

  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    DialogStyleScriptlet.outputStyleMapForDialog(context, arc);
  }
  
  public static void outputStyleMapForDialog(
      FacesContext context, 
      RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Output the style classes to the styleClassMap
    writer.writeText("TrPage.getInstance().addStyleClassMap( {'", null);
    writer.writeText(SkinSelectors.AF_DIALOG_CONTAINER_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_CONTAINER_STYLE_CLASS), null);
    writer.writeText("','" + SkinSelectors.AF_DIALOG_CONTENT_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_CONTENT_STYLE_CLASS), null);
    writer.writeText("','" + SkinSelectors.AF_DIALOG_TITLEBAR_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_TITLEBAR_STYLE_CLASS), null);
    writer.writeText("','" + SkinSelectors.AF_DIALOG_TITLE_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_TITLE_STYLE_CLASS), null);
    writer.writeText("','" + SkinSelectors.AF_DIALOG_CLOSE_ICON_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_CLOSE_ICON_STYLE_CLASS), null);
    writer.writeText("','" + SkinSelectors.AF_DIALOG_BLOCKED_AREA_STYLE_CLASS + "':'", null);
    writer.writeText(arc.getStyleClass(SkinSelectors.AF_DIALOG_BLOCKED_AREA_STYLE_CLASS), null);
    writer.writeText("'} ); ", null);
  }

  public  static final String DIALOG_STYLES_KEY         = "DialogStyles";
  private static       DialogStyleScriptlet _sInstance = null;

}
