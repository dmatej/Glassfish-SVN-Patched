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
package org.apache.myfaces.trinidadinternal.webapp;

import java.util.Collections;
import java.util.Iterator;

import javax.el.ELContext;
import javax.el.ELResolver;
import javax.el.FunctionMapper;
import javax.el.VariableMapper;

import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.PartialViewContext;
import javax.faces.context.PartialViewContextFactory;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;


/**
 * Pseudo FacesContext, vended by the filter for code that
 * needs to run before (or after) the FacesServlet, but needs
 * access to servlet objects.  This object is only available
 * inside the filter.
 *
 */
class PseudoFacesContext extends FacesContext
{
  public PseudoFacesContext(ExternalContext ec)
  {
    assert ec != null : "External context must not be null";
    _external = ec;
  }

  public void setAsCurrentInstance()
  {
    FacesContext.setCurrentInstance(this);
  }

  @Override
  public void release()
  {
    FacesContext.setCurrentInstance(null);
  }

  @Override
  public ResponseWriter getResponseWriter()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void setResponseWriter(ResponseWriter responseWriter)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Iterator<FacesMessage> getMessages()
  {
    return Collections.<FacesMessage>emptyList().iterator();
  }

  @Override
  public Iterator<FacesMessage> getMessages(String id)
  {
    return Collections.<FacesMessage>emptyList().iterator();
  }

  @Override
  public void addMessage(String id, FacesMessage message)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public FacesMessage.Severity getMaximumSeverity()
  {
    return null;
  }

  @Override
  public Iterator<String> getClientIdsWithMessages()
  {
    return Collections.<String>emptyList().iterator();
  }

  @Override
  public UIViewRoot getViewRoot()
  {
    return null;
  }

  @Override
  public void setViewRoot(UIViewRoot viewRoot)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public RenderKit getRenderKit()
  {
    return null;
  }

  @Override
  public boolean getRenderResponse()
  {
    return false;
  }

  @Override
  public boolean getResponseComplete()
  {
    return false;
  }

  @Override
  public ResponseStream getResponseStream()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void setResponseStream(ResponseStream responseStream)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void renderResponse()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void responseComplete()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Application getApplication()
  {
    if (_application == null)
    {
        _application = ((ApplicationFactory) FactoryFinder.getFactory(
                FactoryFinder.APPLICATION_FACTORY)).getApplication();
    }

    return _application;
  }

  @Override
  public ELContext getELContext()
  {
    if (_elContext == null)
    {
      _elContext = new MockELContext(getApplication());
    }

    return _elContext;
  }

  @Override
  public ExternalContext getExternalContext()
  {
    return _external;
  }

  @Override
  public PartialViewContext getPartialViewContext()
  {
    if (_partialViewContext == null)
    {
      PartialViewContextFactory f = (PartialViewContextFactory)
                      FactoryFinder.getFactory(FactoryFinder.PARTIAL_VIEW_CONTEXT_FACTORY);
      _partialViewContext = f.getPartialViewContext(this);
    }

    return _partialViewContext;
  }

  // This is used to mock up a dummy ELContext to pass into createValueExpression
  // if the FacesContext is null and we can't get FacesContext.getELContext.
  private static class MockELContext extends ELContext
  {
    public MockELContext(Application application)
    {
      _resolver = application.getELResolver();
    }

    @Override
    public ELResolver getELResolver()
    {
      return _resolver;
    }

    @Override
    public FunctionMapper getFunctionMapper()
    {
      return null;
    }

    @Override
    public VariableMapper getVariableMapper()
    {
      return null;
    }

    private final ELResolver _resolver;
  }

  private PartialViewContext _partialViewContext;
  private final ExternalContext _external;
  private ELContext _elContext;
  private Application _application;
}