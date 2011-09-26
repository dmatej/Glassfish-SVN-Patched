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
package org.apache.myfaces.trinidadinternal.uinode.bind;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * BoundValue that handles context-relative URLs.  URLs that
 * begin with one slash automatically have the context path
 * prepended, but URLs beginning with one slash are treated
 * as server-relative (and have the first slash removed).
 * <p>
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class URLBoundValue implements BoundValue
{
  public URLBoundValue(BoundValue base)
  {
    _base = base;
  }

  public Object getValue(UIXRenderingContext context)
  {
    String url = (String) _base.getValue(context);
    if (url == null)
      return null;
    
    // It'd be much more efficient if we could push this down into the
    // Renderer
    if (url.startsWith("/"))
    {
      // Treat two slashes as server-relative
      if (url.startsWith("//"))
        return url.substring(1);

      FacesContext fContext = (context == null) ? 
          FacesContext.getCurrentInstance() : context.getFacesContext();
      
      url = fContext.getExternalContext().getRequestContextPath() + url;
    }
    
    // =-=AEW The UIX2 Renderers can already be calling encodeURL()
    // and it'd be a bad idea to encode twice
    // return fContext.getExternalContext().encodeResourceURL(url);
    return url;
  }

  private BoundValue _base;
}
