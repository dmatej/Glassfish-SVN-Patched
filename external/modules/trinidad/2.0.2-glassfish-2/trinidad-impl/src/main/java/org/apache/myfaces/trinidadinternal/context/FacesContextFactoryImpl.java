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
package org.apache.myfaces.trinidadinternal.context;

import java.io.IOException;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.Map;

import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.FacesContextFactory;
import javax.faces.context.FacesContextWrapper;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;

/**
 * Internal class that optimizes retrieval of the RenderKit by caching it
 * on the FacesContext, and hooks ExternalContext.dispatch()
 * to use the PageResolver.
 * <p>
 */
public class FacesContextFactoryImpl
  extends FacesContextFactory
{
  public FacesContextFactoryImpl(FacesContextFactory factory)
  {
    _factory = factory;
  }

  @Override
  @SuppressWarnings ("unchecked")
  public FacesContext getFacesContext(
      Object context,
      Object request,
      Object response, 
      Lifecycle lifecycle)
  {
    return new CacheRenderKit(_factory.getFacesContext(context, request, response, lifecycle));
  }
  
  static public class CacheRenderKit extends FacesContextWrapper
  {
    @SuppressWarnings("unchecked")
    public CacheRenderKit(FacesContext base)
    {
      _base = base;
      ExternalContext baseExternal = base.getExternalContext();
      GlobalConfiguratorImpl config = GlobalConfiguratorImpl.getInstance();

      //This should be done only if beginRequest was not called on the configurator
      //before we retrieve the FacesContext.  If this is the case then we'll need to handle
      //cleanup on the release of the FacesContext.  Otherwise the endRequest should be
      //called by whatever did he origional beginRequest.
      if(!GlobalConfiguratorImpl.isRequestStarted(baseExternal))
      {
        Map<String, Object> requestMap = baseExternal.getRequestMap();
        requestMap.put(_CONFIG_IN_CONTEXT, Boolean.TRUE);
      }

      _external = new OverrideDispatch(config.getExternalContext(baseExternal));
      setCurrentInstance(this);
    }

    @Override
    public ExternalContext getExternalContext()
    {
      return _external;
    }

    @Override
    public RenderKit getRenderKit()
    {
      if (_kit == null)
      {
        _kit = _base.getRenderKit();
      }
      else
      {
        UIViewRoot root = getViewRoot();
        if (root != null)
        {
          String renderKitId = root.getRenderKitId();
          // Yes, instance equality, not .equals();  within a single
          // request and single thread, instance equality should always
          // be sufficient, and behavior will still be correct even
          // if it was somehow not (we'll just spend more time re-getting the
          // RenderKit)
          if (renderKitId != _cachedRenderKitId)
          {
            _cachedRenderKitId = renderKitId;
            _kit = _base.getRenderKit();
          }
        }
      }

      return _kit;
    }

    @Override
    public FacesContext getWrapped()
    {
      return _base;
    }
    
    @Override
    public void release()
    {
      ExternalContext ec = getExternalContext();
      if(Boolean.TRUE.equals(ec.getRequestMap().get(_CONFIG_IN_CONTEXT)))
      {
        GlobalConfiguratorImpl.getInstance().endRequest(ec);
      }
      
      super.release();
    }
    
    private final FacesContext    _base;
    private final ExternalContext _external;
    // An Object, not a String, so that FindBugs won't complain
    // about this usage of instance equality
    private Object    _cachedRenderKitId;
    private RenderKit _kit;
    
    static private final String _CONFIG_IN_CONTEXT = FacesContextFactoryImpl.class.getName()+".CONFIG_IN_CONTEXT";
  }

  static public class OverrideDispatch extends ExternalContextDecorator
  {
    public OverrideDispatch(ExternalContext decorated)
    {
      _decorated = decorated;
    }

    @Override
    public void dispatch(String path) throws IOException
    {
      RequestContext afc = RequestContext.getCurrentInstance();
      if (afc != null)
      {
        path = afc.getPageResolver().getPhysicalURI(path);
      }

      super.dispatch(path);
    }
    
    @Override
    public URL getResource(String path)
                             throws MalformedURLException
    {
      RequestContext afc = RequestContext.getCurrentInstance();
      if (afc != null)
      {
        path = afc.getPageResolver().getPhysicalURI(path);
      }
      return super.getResource(path);
    }


    @Override
    protected ExternalContext getExternalContext()
    {
      return _decorated;
    }

    private final ExternalContext _decorated;
  }

  private final FacesContextFactory _factory;
}
