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
package org.apache.myfaces.trinidadinternal.share.url;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

public class FacesURLEncoder implements URLEncoder
{
  /**
   * @todo Does this ever need to be absolute?
   */
  public FacesURLEncoder(FacesContext context)
  {
    _externalContext = context.getExternalContext();

    String viewId = context.getViewRoot().getViewId();
    _defaultURL =
      context.getApplication().getViewHandler().getActionURL(context, viewId);
  }
  
  public String encodeParameter(String key)
  {
    return key;
  }
  
  /**
   * @todo How to deal with resource URLs?
   */
  public String encodeURL(String url)
  {
    if (url == null)
      return null;
    
    // Don't encode "javascript:" URLs ever.
    if (url.startsWith("javascript:"))
      return url;
    
    return _externalContext.encodeActionURL(url);
  }
  
  public String getDefaultURL()
  {
    return _defaultURL;
  }
  
  private final String _defaultURL;
  private final ExternalContext _externalContext;
}

