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
package org.apache.myfaces.trinidadinternal.application;

import java.beans.BeanInfo;

import java.io.IOException;
import java.io.InputStream;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import javax.faces.application.Resource;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import javax.faces.view.StateManagementStrategy;
import javax.faces.view.ViewDeclarationLanguage;
import javax.faces.view.ViewDeclarationLanguageFactory;
import javax.faces.view.ViewMetadata;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.InternalView;


public class ViewDeclarationLanguageFactoryImpl
  extends ViewDeclarationLanguageFactory
{
  public ViewDeclarationLanguageFactoryImpl(ViewDeclarationLanguageFactory delegate)
  {
    super();
    _wrapped = delegate;
    
    _loadInternalViews();
    
    InternalViewFinder finder = new InternalViewFinder()
      {
        public InternalView getInternalView(FacesContext context, String viewId)
        {
          return _getInternalView(context, viewId);
        }
      };
    
    _internalViewStrategy = new InternalViewHandlingStrategy(finder);
    
  }

  @Override
  public ViewDeclarationLanguage getViewDeclarationLanguage(String viewId)
  {
    FacesContext context = FacesContext.getCurrentInstance();
    if (_getInternalView(context, viewId) != null)
      return _internalViewStrategy;
    
    // TRINIDAD-1703 - use physical URI (retrieved from the PageResolver) before calling the delegate's 
    // implementation
    viewId = _getPath(context, viewId);
    
    ViewDeclarationLanguage vdl = getWrapped().getViewDeclarationLanguage(viewId);
    // Possiblity of nested VDLs of the same kind
    if (vdl instanceof ChangeApplyingVDLWrapper) 
      return vdl;

    return new ChangeApplyingVDLWrapper(getWrapped().getViewDeclarationLanguage(viewId));
  }
  
  @Override
  public ViewDeclarationLanguageFactory getWrapped()
  {
    return _wrapped;
  }
  
  private InternalView _getInternalView(
    FacesContext context, 
    String       viewId)
  {
    Object cached = _internalViewCache.get(viewId);
    if (cached != null)
    {
      return ((cached == _NOT_FOUND) ? null : (InternalView)cached);
    }
    
    InternalView internal = _internalViews.get(viewId);
    if (internal == null)
    {
      // If we're using suffix-mapping, then any internal viewId will
      // get affixed with ".jsp" or ".jspx";  try trimming that off
      // if present
      ExternalContext external = context.getExternalContext();
      
      // Only bother when using suffix-mapping (path info will always
      // be non-null for prefix-mapping)
      if (external.getRequestPathInfo() == null)
      {
        String suffix = external.getInitParameter("javax.faces.DEFAULT_SUFFIX");
        if (suffix == null)
          suffix = ".jspx";
        
        if (viewId.endsWith(suffix))
        {
          String viewIdWithoutSuffix = viewId.substring(
             0, viewId.length() - suffix.length());
          internal = _internalViews.get(viewIdWithoutSuffix);
        }
      }
    }
    
    _internalViewCache.put(viewId, (internal == null) ? _NOT_FOUND : internal);

    return internal;
  }
  
  //
  // Load the META-INF/org.apache.myfaces.trinidad.render.InternalView.properties
  // files.
  //
  private void _loadInternalViews()
  {
    _internalViews = new HashMap<String, InternalView>();
    List<URL> list = new ArrayList<URL>();
    ClassLoader loader = _getClassLoader();
    try
    {
      Enumeration<URL> en = loader.getResources(
               "META-INF/org.apache.myfaces.trinidad.render.InternalView.properties");
      while (en.hasMoreElements())
      {
        list.add(en.nextElement());
      }

      // And, for some temporary backwards compatibility, also load
      // the incorrect properties without "render"
      en = loader.getResources(
               "META-INF/org.apache.myfaces.trinidad.InternalView.properties");
      while (en.hasMoreElements())
      {
        list.add(en.nextElement());
      }


      // Reverse the list so it is in the proper order (most local
      // entry "wins")
      Collections.reverse(list);
    }
    catch (IOException ioe)
    {
      _LOG.severe(ioe);
    }

    for (URL url : list)
    {
      try
      {
        Properties properties = new Properties();
        _LOG.fine("Loading internal views from {0}",  url);
        InputStream is = url.openStream();
        try
        {
          properties.load(is);
        }
        finally
        {
          is.close();
        }

        for (Map.Entry<Object, Object> entry : properties.entrySet())
        {
          String name = (String) entry.getKey();
          String className = (String) entry.getValue();
          Class<?> clazz = loader.loadClass(className);
          org.apache.myfaces.trinidad.render.InternalView view = (org.apache.myfaces.trinidad.render.InternalView) clazz.newInstance();
          _internalViews.put(name, view);
        }
      }
      catch (IllegalAccessException iae)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(iae);
      }
      catch (InstantiationException ie)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(ie);
      }
      catch (ClassNotFoundException cnfe)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(cnfe);
      }
      catch (IOException ioe)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(ioe);
      }
    }
  }


  static private ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = ViewDeclarationLanguageFactoryImpl.class.getClassLoader();
    return loader;
  }
  
  /**
   * Return the physical path of a particular URI
   */
  static private String _getPath(FacesContext context, String uri)
  {
    UIViewRoot viewRoot = context.getViewRoot();
    boolean viewMatch = false;
    Map<String, String> viewIdMap = (Map<String, String>)context.getAttributes().get(_VIEWID_MAPPING);
    if (viewRoot != null && viewRoot.getViewId().equals(uri))
    {
      viewMatch = true;
      // Only return from cache if requested for the current viewRoot and it matches the arg. 
      // Same rule applies when storing into viewMap
      if (viewIdMap != null)
      {
        String cachedPhysicalURI = viewIdMap.get(uri);
        if (cachedPhysicalURI != null)
          return cachedPhysicalURI;
      }
    }
    
    RequestContext afc = RequestContext.getCurrentInstance();
    if (afc != null)
    {
      String physicalURI = afc.getPageResolver().getPhysicalURI(uri);
      if (viewMatch)
      {
        // Store the viewId 
        if (viewIdMap == null)
        {
          viewIdMap = new HashMap<String, String>();
          context.getAttributes().put(_VIEWID_MAPPING, viewIdMap);
        }
        viewIdMap.put(uri, physicalURI);
      }
      return physicalURI;
    }

    // No RequestContext?  Just return the URI
    return uri;
  }
  
  private final ViewDeclarationLanguageFactory _wrapped;
  private final InternalViewHandlingStrategy _internalViewStrategy;
  private Map<String, InternalView> _internalViews;
  
  private final static Object _NOT_FOUND = new Object();
  private final static String _VIEWID_MAPPING = "org.apache.myfaces.trinidadinternal.application.viewIdMapping";
  private final Map<String, Object> _internalViewCache = 
                              new ConcurrentHashMap<String, Object>();
  
  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(ViewDeclarationLanguageFactoryImpl.class);
  
  /**
   * Package-private interface for use by InternalViewHandlingStrategy as a callback
   * for getting InternalViews
   */
  interface InternalViewFinder
  {
    public InternalView getInternalView(FacesContext context, String viewId);
  }
  
  /**
   * The VDL implementation that wraps an underlying VDL and additionally applies component changes based 
   * customization (usually SessionChangeManager). Note that this works both for Facelets and JSPs.
   */
  private static class ChangeApplyingVDLWrapper extends ViewDeclarationLanguage
  {
    ChangeApplyingVDLWrapper(ViewDeclarationLanguage wrapped)
    {
      _wrapped = wrapped;
    }

    @Override
    public BeanInfo getComponentMetadata(FacesContext facesContext, Resource resource)
    {
      return _wrapped.getComponentMetadata(facesContext, resource);
    }

    @Override
    public ViewMetadata getViewMetadata(FacesContext facesContext, String string)
    {
      return _wrapped.getViewMetadata(facesContext, string);
    }

    @Override
    public Resource getScriptComponentResource(FacesContext facesContext, Resource resource)
    {
      return _wrapped.getScriptComponentResource(facesContext, resource);
    }

    @Override
    public UIViewRoot createView(FacesContext facesContext, String string)
    {
      return _wrapped.createView(facesContext, string);
    }

    @Override
    public UIViewRoot restoreView(FacesContext facesContext, String string)
    {
      return _wrapped.restoreView(facesContext, string);
    }

    @Override
    public void buildView(FacesContext facesContext, UIViewRoot uiViewRoot)
      throws IOException
    {
      _wrapped.buildView(facesContext, uiViewRoot);
      if(PhaseId.RENDER_RESPONSE.equals(FacesContext.getCurrentInstance().getCurrentPhaseId()))
      {          
        ChangeManager cm = RequestContext.getCurrentInstance().getChangeManager();
        cm.applyComponentChangesForCurrentView(FacesContext.getCurrentInstance());
      }
    }

    @Override
    public void renderView(FacesContext facesContext, UIViewRoot uiViewRoot)
      throws IOException
    {
      _wrapped.renderView(facesContext, uiViewRoot);
    }

    @Override
    public StateManagementStrategy getStateManagementStrategy(FacesContext facesContext, String string)
    {
      return _wrapped.getStateManagementStrategy(facesContext, string);
    }
    
    private final ViewDeclarationLanguage _wrapped;
  }
}
