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

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.uinode.UINodeRendererBase;

/**
 * Renderer for command components like commandButton and commandLink
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/CommandRenderer.java#0 $) $Date: 10-nov-2005.19:00:27 $
 */
public class CommandRenderer extends UINodeRendererBase
{
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    RequestContext afContext = RequestContext.getCurrentInstance();
    ReturnEvent returnEvent =
      afContext.getDialogService().getReturnEvent(component);
    if (returnEvent != null)
    {
      returnEvent.queue();
    }
    else
    {
      Map<String, String> parameterMap = 
        context.getExternalContext().getRequestParameterMap();
      
      Object source = parameterMap.get("source");
      String clientId = component.getClientId(context);

      if ((source != null) && source.equals(clientId))
      {
        (new ActionEvent(component)).queue();
        Map<String, Object> attrs = component.getAttributes();
        if (Boolean.TRUE.equals(attrs.get("partialSubmit")))
        {
          PartialPageUtils.forcePartialRendering(context);
        }
      }
    }
  }
}
