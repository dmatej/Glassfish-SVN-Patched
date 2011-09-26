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

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.faces.FacesException;
import javax.faces.application.ProjectStage;
import javax.faces.application.ViewHandler;
import javax.faces.application.ViewHandlerWrapper;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import javax.faces.view.ViewDeclarationLanguage;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;
import org.apache.myfaces.trinidad.util.URLUtils;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;

/**
 * ViewHandler that adds modification detection to the existing ViewHandler,
 * assuming that the viewId is a valid resource path.
 * <p>
 * And now also supports inserting URLs tokens to preserve PageFlowScope.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/application/ViewHandlerImpl.java#0 $) $Date: 05-jan-2006.13:19:09 $
 * @todo Rename something less generic
 * @todo Support extension mapping (*.faces)
 * @todo The modification detection only works for a single user.  That's
 *   OK for now, because it's intended for use while developing
 */
public class ViewHandlerImpl extends ViewHandlerWrapper
{
  static public final String ALTERNATE_VIEW_HANDLER =
    "org.apache.myfaces.trinidad.ALTERNATE_VIEW_HANDLER";

  public ViewHandlerImpl(
    ViewHandler delegate)
  {
    _delegate = delegate;
    _timestamps = new HashMap<String, Long>();
  }

  public ViewHandler getWrapped()
  {
    return _delegate;
  }
  

  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    _initIfNeeded(context);

    if (_checkTimestamp(context, viewId))
    {
      try
      {
        // Check the timestamp on the physical path
        String path = _getPath(viewId);
        synchronized (_timestamps)
        {
          Long ts = _timestamps.get(path);
          if (ts != _NOT_FOUND)
          {
            URL url = context.getExternalContext().getResource(path);
            Long modified = _getLastModified(url);
            _timestamps.put(path, modified);
          }
        }
      }
      catch (IOException e)
      {
        _LOG.severe(e);
      }
    }

    return super.createView(context, viewId);
  }

  @Override
  public String getActionURL(FacesContext context, String viewId)
  {
    String actionURL = super.getActionURL(context, viewId);
    RequestContext afContext = RequestContext.getCurrentInstance();
    if (afContext != null)
    {
      actionURL = afContext.getPageResolver().encodeActionURI(actionURL);
      actionURL = afContext.getPageFlowScopeProvider().
                     encodeCurrentPageFlowScopeURL(context, actionURL);
    }

    return actionURL;
  }

  @Override
  public String getResourceURL(
    FacesContext context,
    String       path)
  {
    return super.getResourceURL(context, path);
  }


  @Override
  public void renderView(
    FacesContext context,
    UIViewRoot   viewToRender) throws IOException, FacesException
  {
    _initIfNeeded(context);
    
    // Check whether Trinidad's ViewHandler is registered more than once.
    // This happens when the implementation jar is loaded multiple times.
    Map<String, Object> reqMap = context.getExternalContext().getRequestMap();
    if (reqMap.get(_RENDER_VIEW_MARKER) != null)
    {
      _LOG.warning("DUPLICATE_VIEWHANDLER_REGISTRATION");
    }
    else
    {
      reqMap.put(_RENDER_VIEW_MARKER, Boolean.TRUE);
    }

    // See if there is a possiblity of short-circuiting the current
    // Render Response
    ExtendedRenderKitService service = _getExtendedRenderKitService(context);
    if ((service != null) &&
        service.shortCircuitRenderView(context))
    {
      // Yup, we don't need to do anything
      ;
    }
    else
    {
      try
      {
        if (service != null)
          service.encodeBegin(context);
        
        super.renderView(context, viewToRender);

        if (service != null)
          service.encodeEnd(context);
      }
      finally
      {
        if (service != null)
          service.encodeFinally(context);
      }
    }
    
    // Remove the 'marker' from the request map just in case the entire tree is rendered again
    reqMap.remove(_RENDER_VIEW_MARKER);
  }

  @Override
  public UIViewRoot restoreView(
    FacesContext context,
    String       viewId)
  {    
    //This code processes a "return" event.  Most of this logic was moved to 
    //StateManagerImpl because we ran into a problem with JSF where it didn't 
    //set up the JSF mapping properly if we didn't delegate to the default 
    //ViewHandler.  There may be other logic associated with the internalView
    //which might need to be moved to the StateManager as well.  This might also
    //be able to be further optimized if all the other logic in this method passes
    //through.
    if(context.getExternalContext().getRequestMap().get(RequestContextImpl.LAUNCH_VIEW) != null)
    {
      return super.restoreView(context, viewId);
    }
    
    boolean uptodate = true;

    if (_checkTimestamp(context, viewId))
    {
      try
      {
        // Check the timestamp on the physical path
        String path = _getPath(viewId);
        synchronized (_timestamps)
        {
          Long ts = _timestamps.get(path);
          if (ts != _NOT_FOUND)
          {
            URL url = context.getExternalContext().getResource(path);
            Long modified = _getLastModified(url);
            if (modified == _NOT_FOUND)
            {
              _timestamps.put(path, _NOT_FOUND);
            }
            else if ((ts == null) ||
                     (modified.longValue() > ts.longValue()))
            {
              _timestamps.put(path, modified);
              if (ts != null)
              {
                _LOG.fine("View document \"" + path + "\" has been modified, " +
                          "ignoring postback for view \"" + viewId +"\"");
              }
              uptodate = false;
            }
          }
        }
      }
      catch (IOException e)
      {
        _LOG.severe(e);
      }
    }

    if (!uptodate)
    {
      return null;
    }

    UIViewRoot result = super.restoreView(context, viewId);
    // If we've successfully restored a view, then assume that
    // this is a postback request.
    if (result != null)
    {
      TrinidadPhaseListener.markPostback(context);
    }

    return result;
  }

  @Override
  public void writeState(
    FacesContext context) throws IOException
  {
    // After the move of InteralView loading code to the ViewDeclarationFactoryImpl,
    // this class was not supposed to do anything with the InternalViews.
    // Unfortunately, writeState() has not been exposed on ViewDeclarationLanguage,
    // so we have to override this method here. Without an override, JSF save state
    // marker gets written straight to the response

    String viewId = context.getViewRoot().getViewId();
    ViewDeclarationLanguage vdl = getViewDeclarationLanguage(context, viewId);
    if (vdl instanceof InternalViewHandlingStrategy)
    {
      InternalViewHandlingStrategy strategy = (InternalViewHandlingStrategy)vdl;
      if (strategy.__isStateless(context, viewId))
      {
        return;
      }
    }
    
    ExtendedRenderKitService service = _getExtendedRenderKitService(context);
    if ((service != null) &&
        service.isStateless(context))
      return;

    super.writeState(context);
  }
  

  synchronized private void _initIfNeeded(FacesContext context)
  {
    if (!_inited)
    {
      _inited = true;
      String alternateViewHandler =
        context.getExternalContext().getInitParameter(ALTERNATE_VIEW_HANDLER);
      if (alternateViewHandler != null)
      {
        ViewHandler viewHandlerInstance = null;
        try
        {
          ClassLoader loader = Thread.currentThread().getContextClassLoader();
          Class<?> c = loader.loadClass(alternateViewHandler);
          try
          {
            Constructor<?> constructor = c.getConstructor(
               new Class[]{ViewHandler.class});
            viewHandlerInstance =
               (ViewHandler) constructor.newInstance(new Object[]{_delegate});
          }
          catch (NoSuchMethodException nsme)
          {
            viewHandlerInstance = (ViewHandler) c.newInstance();
          }
        }
        catch (Exception e)
        {
          _LOG.warning("CANNOT_LOAD_VIEWHANDLER", alternateViewHandler);
          _LOG.warning(e);
        }

        if (viewHandlerInstance != null)
          _delegate = viewHandlerInstance;
      }
    }
  }

  private ExtendedRenderKitService _getExtendedRenderKitService(
    FacesContext context)
  {
    return Service.getService(context.getRenderKit(),
                              ExtendedRenderKitService.class);
  }

  private boolean _checkTimestamp(FacesContext context, String viewId)
  {
    if (_checkTimestamp == null)
    {
      boolean checkTimestampParam;
      String checkTimestamp =
        context.getExternalContext().getInitParameter(Configuration.CHECK_TIMESTAMP_PARAM);
      
      if (checkTimestamp != null)
      {
        checkTimestampParam = "true".equals(checkTimestamp);  
      }
      else
      {
        // if the CHECK_TIMESTAMP_PARAM parameter has NOT been specified, let us
        // apply the DEFAULT values for the certain Project Stages:
        // -PRODUCTION we want this value to be FALSE;
        // -other stages we use TRUE
        checkTimestampParam = !(context.isProjectStage(ProjectStage.Production));
      }

      boolean developmentStage = context.isProjectStage(ProjectStage.Development);
      
      // if Apache MyFaces Trinidad is running in production stage CHECK_TIMESTAMP_PARAM should
      // be FALSE, otherwise we generate a WARNING message
      boolean productionStage = developmentStage || context.isProjectStage(ProjectStage.Production);
      
      boolean performCheck = checkTimestampParam || developmentStage;
      _checkTimestamp = Boolean.valueOf(performCheck);
      
      if (checkTimestampParam)
      {
        if (productionStage)
        {
          _LOG.warning("TIMESTAMP_CHECKING_ENABLED_SHOULDNOT_IN_PRODUCTION",
              Configuration.CHECK_TIMESTAMP_PARAM);
        }
        else
        {
          _LOG.info("TIMESTAMP_CHECKING_ENABLED_SHOULDNOT_IN_PRODUCTION",
                    Configuration.CHECK_TIMESTAMP_PARAM);
        }
      }
    }

    // Even if _checkTimestamp is TRUE, we do not want to perform the check for the InternalViews
    boolean check = _checkTimestamp.booleanValue();
    
    if (check)
    {
      if (getViewDeclarationLanguage(context, viewId) 
                              instanceof InternalViewHandlingStrategy)
      {
        return false;
      }
    }
    return check;
  }


  /**
   * Return the physical path of a particular URI
   */
  static private String _getPath(String uri)
  {
    RequestContext afc = RequestContext.getCurrentInstance();
    if (afc != null)
    {
      return afc.getPageResolver().getPhysicalURI(uri);
    }

    // No RequestContext?  Just return the URI
    return uri;
  }


  private Long _getLastModified(URL url) throws IOException
  {
    if (url == null)
      return _NOT_FOUND;

    return Long.valueOf(URLUtils.getLastModified(url));
  }


  private Boolean           _checkTimestamp;
  // Mostly final, but see _initIfNeeded()
  private ViewHandler       _delegate;
  private final Map<String, Long> _timestamps;
  private boolean           _inited;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ViewHandlerImpl.class);
  private static final Long   _NOT_FOUND = Long.valueOf(0);
  private static final String _RENDER_VIEW_MARKER = "__trRenderViewEntry";
}
