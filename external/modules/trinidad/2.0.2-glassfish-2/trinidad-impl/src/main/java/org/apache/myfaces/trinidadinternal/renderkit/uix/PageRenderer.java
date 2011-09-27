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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.util.Collections;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXPage;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.uinode.UINodeRendererBase;

/**
 * Renderer for page
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/PageRenderer.java#0 $) $Date: 10-nov-2005.19:00:31 $
 */
public class PageRenderer extends UINodeRendererBase
{
  public static final String TREE_SUFFIX = ":_navigationTree";

  @SuppressWarnings("unchecked")
  @Override
  public void decode(
    FacesContext context, 
    UIComponent component)
  {
    Map<String, String> parameters = 
      context.getExternalContext().getRequestParameterMap();
    
    String source = parameters.get(UIConstants.SOURCE_PARAM);
    
    UIXPage page = (UIXPage)component;
    
    // the path needs to be set correctly before calling 
    // page.getClientId
    String treeId = page.getClientId(context) + TREE_SUFFIX;
    
    if ( treeId.equals(source))
    {          
      TreeUtils.decodeExpandEvents(parameters, page, Collections.emptyList());
      RequestContext afContext = RequestContext.getCurrentInstance();
      if (afContext != null)
        afContext.addPartialTarget(component);
    }
  }  
  
}
