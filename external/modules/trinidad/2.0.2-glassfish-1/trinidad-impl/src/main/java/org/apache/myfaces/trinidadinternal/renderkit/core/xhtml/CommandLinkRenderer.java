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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;


public class CommandLinkRenderer extends GoLinkRenderer
{
  public CommandLinkRenderer()
  {
    this(CoreCommandLink.TYPE);
  }

  protected CommandLinkRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey = type.findKey("immediate");
    _partialSubmitKey = type.findKey("partialSubmit");
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    FacesBean    facesBean,
    String       clientId)
  {
    RequestContext requestContext = RequestContext.getCurrentInstance();
    ReturnEvent returnEvent =
      requestContext.getDialogService().getReturnEvent(component);
    if (returnEvent != null)
    {
      returnEvent.queue();
    }
    else
    {
      Map<String, String> parameterMap =
        facesContext.getExternalContext().getRequestParameterMap();

      Object source = parameterMap.get("javax.faces.source");
      
      // Support the legacy as well as JSF2 parameter name
      if (source == null) 
      {
        source = parameterMap.get("source");  
      }
      
      if (clientId == null)
      {
        clientId = component.getClientId(facesContext);
      }

      if ((source != null) && source.equals(clientId))
      {
        (new ActionEvent(component)).queue();
        if (getPartialSubmit(component, facesBean))
        {
          PartialPageUtils.forcePartialRendering(facesContext);
        }
      }
    }
  }

  @Override
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    if (getPartialSubmit(comp, bean))
    {
      AutoSubmitUtils.writeDependencies(context, rc);
    }

    String clientId = getClientId(context, comp);
    // Make sure we don't have anything to save
    assert(rc.getCurrentClientId() == null);
    rc.setCurrentClientId(clientId);

    // Find the params up front, and save them off -
    // getOnClick() doesn't have access to the UIComponent
    String extraParams = AutoSubmitUtils.getParameters(comp);
    Object old = rc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY,
                                         extraParams);
    super.encodeBegin(context, rc, comp, bean);
    // Restore any old params, though really, how could that happen??
    rc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY, old);

    rc.setCurrentClientId(null);
  }

  @Override
  public void encodeEnd(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    super.encodeEnd(context, rc, comp, bean);
    FormData fd = rc.getFormData();
    if (fd != null)
      fd.addNeededValue(XhtmlConstants.SOURCE_PARAM);
  }

  @Override
  protected String getDestination(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected String getTargetFrame(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected boolean hasOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    // More efficient
    return true;
  }

  /**
   * Returns the component's onclick
   */
  protected String getComponentOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    return super.getOnclick(component, bean);
  }

  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    String onclick = getComponentOnclick(component, bean);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    String id = arc.getCurrentClientId();
    boolean immediate = getImmediate(component, bean);

    String extraParams = (String)
      arc.getProperties().get(_EXTRA_SUBMIT_PARAMS_KEY);

    String script;
    if (getPartialSubmit(component, bean))
    {
      script = AutoSubmitUtils.getSubmitScript(
                arc, id, immediate, false,
                null/* no event*/,
                extraParams,
                false);
    }
    else
    {
      script = AutoSubmitUtils.getFullPageSubmitScript(
                arc, id, immediate,
                null/*no event*/,
                extraParams,
                false/* return false*/);
    }

    //return XhtmlUtils.buildClientEventHandler(facesContext, component, false, "click",
    //         params, onclick, script);

    return XhtmlUtils.getChainedJS(onclick, script, true);
  }

  protected boolean getImmediate(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getPartialSubmit(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_partialSubmitKey);
    if (o == null)
      o = _partialSubmitKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  private PropertyKey _immediateKey;
  private PropertyKey _partialSubmitKey;

  static private final Object _EXTRA_SUBMIT_PARAMS_KEY = new Object();
}
