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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.OutputStream;
import java.io.Writer;

import java.util.Iterator;
import java.util.concurrent.ConcurrentMap;

import javax.faces.FactoryFinder;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.render.ClientBehaviorRenderer;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.Renderer;
import javax.faces.render.ResponseStateManager;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.RequestContextFactory;
import org.apache.myfaces.trinidad.util.Service;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;


abstract public class RenderKitDecorator extends RenderKitBase
                                         implements Service.Provider
{
  public <T> T getService(Class<T> serviceClass)
  {
    return Service.getService(getRenderKit(), serviceClass);
  }

  @Override
  public ResponseWriter createResponseWriter(
    Writer writer,
    String contentTypeList,
    String encoding)
  {
    RenderKit renderKit = getRenderKit();
    ResponseWriter out =
      renderKit.createResponseWriter(writer, contentTypeList, encoding);

    return createDecoratedResponseWriter(out);
  }

  @Override
  public ResponseStream createResponseStream(
    OutputStream out)
  {
    return getRenderKit().createResponseStream(out);
  }

  @Override
  public ResponseStateManager getResponseStateManager()
  {
    return getRenderKit().getResponseStateManager();
  }

  public void addClientBehaviorRenderer(String type, ClientBehaviorRenderer renderer)
  {
    getRenderKit().addClientBehaviorRenderer(type, renderer);
  }

  public ClientBehaviorRenderer getClientBehaviorRenderer(String type)
  {
    return getRenderKit().getClientBehaviorRenderer(type);
  }

  public Iterator<String> getClientBehaviorRendererTypes()
  {
    return getRenderKit().getClientBehaviorRendererTypes();
  }

  protected ResponseWriter createDecoratedResponseWriter(
    ResponseWriter delegate)
  {
    return delegate;
  }

  @Override
  public Renderer findRenderer(
    String componentFamily,
    String rendererType)
  {
    Renderer renderer = super.findRenderer(componentFamily, rendererType);

    // We did not find a renderer in our own render kit, so check
    // the decorated RenderKit
    if (renderer == null)
    {
      RenderKit renderKit = getRenderKit();

      // Use findRenderer() to avoid "not found" warning messages
      if (renderKit instanceof RenderKitBase)
        renderer = ((RenderKitBase) renderKit).findRenderer(
                                        componentFamily, rendererType);
      else
        renderer = renderKit.getRenderer(componentFamily, rendererType);

      // copy-on-read
      if (renderer != null)
        addRenderer(componentFamily, rendererType, renderer);
    }

    return renderer;
  }

  protected RenderKit getRenderKit()
  {
    FacesContext context = FacesContext.getCurrentInstance();

    // There's only one RenderKitFactory per app. The javadoc for RenderKitFactory says:
    //      "There must be one RenderKitFactory instance per web
    //       application that is utilizing JavaServer Faces"
    // The call to FactoryFinder.getFactory is doing locking, Issue 688 was filed with the RI
    // to try to remove the locking there:
    // https://javaserverfaces.dev.java.net/issues/show_bug.cgi?id=688
    // However they were not able to remove the locking.
    // Therefore save off the factory on the app map.
    RequestContext requestContext = RequestContext.getCurrentInstance();
    if (requestContext == null)
    {
      // In JSF 2, this method may be called be before the global configurator has been initialized and if that
      // is the case, then the request context needs to be built
      ExternalContext externalContext = context.getExternalContext();
      GlobalConfiguratorImpl.getInstance().init(externalContext);
      requestContext = RequestContextFactory.getFactory().createContext(externalContext);
    }

    ConcurrentMap<String, Object> appMap =
      requestContext.getApplicationScopedConcurrentMap();

    RenderKitFactory factory = (RenderKitFactory)appMap.get(_RENDER_KIT_FACTORY_KEY);

    if (factory == null)
    {
      factory = (RenderKitFactory)FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
      RenderKitFactory oldFactory =
                          (RenderKitFactory) appMap.putIfAbsent(_RENDER_KIT_FACTORY_KEY, factory);

      if (oldFactory != null)
        factory = oldFactory;
    }

    RenderKit renderKit = factory.getRenderKit(context, getDecoratedRenderKitId());
    assert (renderKit != null);
    return renderKit;
  }

  abstract protected String getDecoratedRenderKitId();
  private static final String _RENDER_KIT_FACTORY_KEY =
         "org.apache.myfaces.trinidadinternal.renderkit.RenderKitDecorator.RENDER_KIT_FACTORY_KEY";
}
