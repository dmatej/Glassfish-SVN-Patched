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

import javax.faces.application.StateManager;
import javax.faces.application.ResourceDependency;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;


/**
 * Renderer for the panelPartialRoot.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/DocumentRenderer.java#0 $) $Date: 10-nov-2005.19:01:25 $
 */
@ResourceDependency(target = "head", library = "javax.faces", name = "jsf.js")
public class DocumentRenderer extends XhtmlRenderer
{
  public DocumentRenderer()
  {
    this(CoreDocument.TYPE);
  }

  protected DocumentRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _stateSavingKey = type.findKey("stateSaving");
    _html = new HtmlRenderer(type);
    _head = new Head(type);
    _body = new BodyRenderer(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Value of the 'stateSaving' attribute on <document> component
    String stateSaving = getStateSaving(component, bean);

    // let's make sure the Trinidad StateManagerImpl is used.
    // Otherwise tweaking the stateSaving per <document> does
    // NOT work...
    StateManager sm = context.getApplication().getStateManager();

    if (sm instanceof StateManagerImpl)
    {
      StateManagerImpl smi = (StateManagerImpl) sm;

    // value is "client", "server" or "default"
      smi.setPerViewStateSaving(stateSaving);
    }
    else
    {
      //TODO: We may want this to be i18n...
      _LOG.warning("The 'stateSaving' attribute is only supported with the Trinidad internal StateManager");
    }

    delegateRendererBegin(context, rc, component, bean, _html);

    delegateRendererBegin(context, rc, component, bean, _head);
    UIComponent meta = getFacet(component, CoreDocument.META_CONTAINER_FACET);
    if (meta != null)
      encodeChild(context, meta);
    delegateRendererEnd(context, rc, component, bean, _head);

    delegateRenderer(context, rc, component, bean, _body);
    delegateRendererEnd(context, rc, component, bean, _html);
  }

  protected String getStateSaving(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_stateSavingKey);
    if (o == null)
      o = _stateSavingKey.getDefault();

    return toString(o);
  }

  static private class Head extends HeadRenderer
  {
    public Head(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      String base = super.getClientId(context, component);
      return XhtmlUtils.getCompositeId(base, _HEAD_ID_SUFFIX);
    }
  }

  static private final String _HEAD_ID_SUFFIX = "h";

  private CoreRenderer _html;
  private CoreRenderer _head;
  private CoreRenderer _body;
  private PropertyKey _stateSavingKey;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DocumentRenderer.class);
}