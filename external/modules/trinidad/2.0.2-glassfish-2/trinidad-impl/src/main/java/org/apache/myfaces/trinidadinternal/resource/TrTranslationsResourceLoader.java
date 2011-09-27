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

import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.skin.Skin;


public class TrTranslationsResourceLoader extends TranslationsResourceLoader
{
  /**
   * Constructs a dynamic resouce loader for this path which serves up translations
   * 
   * @param path the path of this dynamic resource loader
   */
  public TrTranslationsResourceLoader(String path)
  {
    super(path);
  }


  @Override
  protected int getDefaultSize()
  {
    // We're coming in at about 13K for Japanese, which is probably
    // about as big as this will get
    return 20000;
  }
  
  @Override
  protected String getJSVarName()
  {
    return "TrMessageFactory._TRANSLATIONS";
  }

  @Override
  protected String getBundleName()
  {
    return "org.apache.myfaces.trinidad.resource.MessageBundle";
  }

  // These translations do not go through the skin
  @Override
  protected Skin getSkin(FacesContext context)
  {
    return null;
  }
  
  /*
   * Override to return empty content when loading translations for
   * something other than the page locale. This can happen if components
   * on the page have a different locale than the page. 
   */
  @Override
  protected String getString(String path) throws IOException
  {
    FacesContext context = FacesContext.getCurrentInstance();
    Map params = context.getExternalContext().getRequestParameterMap();
    
    boolean skipTranslations = "true".equals(params.get("skipTranslations"));
    if (skipTranslations)
    {
      return "";
    }
    else
    {
      return (super.getString (path));
    }
    
  }
}
