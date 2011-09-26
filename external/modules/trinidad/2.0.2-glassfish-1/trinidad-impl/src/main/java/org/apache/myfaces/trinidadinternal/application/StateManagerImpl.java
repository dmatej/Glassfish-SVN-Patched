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


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

import javax.faces.FactoryFinder;
import javax.faces.application.StateManager;
import javax.faces.application.StateManagerWrapper;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.ResponseStateManager;
import javax.faces.view.StateManagementStrategy;
import javax.faces.view.ViewDeclarationLanguage;

import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.Window;
import org.apache.myfaces.trinidad.context.WindowManager;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;
import org.apache.myfaces.trinidadinternal.util.ObjectInputStreamResolveClass;
import org.apache.myfaces.trinidadinternal.util.SubKeyMap;
import org.apache.myfaces.trinidadinternal.util.TokenCache;
import org.apache.myfaces.trinidadinternal.util.TokenCacheDebugUtils;


/**
 * StateManager that handles a hybrid client/server strategy:  the state
 * is stored on the server, and only a small token
 * is stored on the client.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/application/StateManagerImpl.java#2 $) $Date: 18-nov-2005.16:12:04 $
 */
public class StateManagerImpl extends StateManagerWrapper
{
  // TODO this should be removed, see comments in restoreView where this constant is used
  // on why this is needed
  public static final String RESPONSE_STATE_MANAGER_STATE_KEY =
    "org.apache.myfaces.trinidadinternal.application.StateManagerImp.RESPONSE_STATE_MANAGER_STATE";

  static public final String CACHE_VIEW_ROOT_INIT_PARAM =
    "org.apache.myfaces.trinidad.CACHE_VIEW_ROOT";

  /**
   * Servlet context initialization parameter used by
   * StateManagerImpl to decide what sort of state should be saved
   * on the client.  Valid values are "token" and "all";  the
   * default is "token".
   */
  static public final String CLIENT_STATE_METHOD_PARAM_NAME =
    "org.apache.myfaces.trinidad.CLIENT_STATE_METHOD";

  /**
   * Servlet context initialization parameter used by
   * StateManagerImpl to decide how many tokens can be stored
   * per user.  The default is 15.
   */
  static public final String CLIENT_STATE_MAX_TOKENS_PARAM_NAME =
    "org.apache.myfaces.trinidad.CLIENT_STATE_MAX_TOKENS";

  /**
   * Servlet context initialization parameter used by
   * StateManagerImpl to decide whether to zip state.
   * Valid values are true and false
   */
  static public final String COMPRESS_VIEW_STATE_PARAM_NAME =
    "org.apache.myfaces.trinidad.COMPRESS_VIEW_STATE";

  /**
   * Value indicating that only a simple token will be stored
   * on the client.
   */
  static public final String CLIENT_STATE_METHOD_TOKEN = "token";

  /**
   * Value indicating that the entire component state will be stored
   * on the client.
   */
  static public final String CLIENT_STATE_METHOD_ALL = "all";

  public StateManagerImpl(
    StateManager delegate)
  {
    _delegate = delegate;
  }

  @Override
  public StateManager getWrapped()
  {
    return _delegate;
  }

  @Override
  public String getViewState(FacesContext context)
  {
    Object state = saveView(context);

    if (state != null)
    {
      return context.getRenderKit().getResponseStateManager().getViewState(context,state);
    }
    else
    {
      return null;
    }

  }

  @SuppressWarnings("deprecation")
  @Override
  public Object saveView(FacesContext context)
  {
    assert(context != null);

    // see if a view has been saved on the request
    Object viewState = _getCachedViewState(context);

    if (viewState != null)
    {
      // TODO gcrawfor
      //        when is this not null, meaning when is saveView being called multiple times
      //        per request?
      return viewState;
    }

    // if the root is transient don't state save
    UIViewRoot viewRoot = context.getViewRoot();

    if (viewRoot.isTransient())
    {
      return null;
    }

    String viewId = viewRoot.getViewId();
    StateManagementStrategy sms = _getStateManagementStrategy(context, viewId);
    Map<Object, Object> contextAttributes = context.getAttributes();
    
    try
    {
        // TODO Once we're dependent on JSF 2.1 we should be using StateManager.IS_SAVING_STATE as the key, but 
        // for now we can use the String value of StateManager.IS_SAVING_STATE
        contextAttributes.put("javax.faces.IS_SAVING_STATE", Boolean.TRUE);
        
        if (sms != null)
        {
          // Force view root to use full state saving
          // This is necessary because we recreate the view root on postback when view root caching
          // is enabled and assume that that we can apply the full state
          if (_useViewRootCache(context))
          {
            viewRoot.clearInitialState();
          }
          
          viewState = sms.saveView(context);
        }
        else
        {
          // if there's no stateManagementStrategy handle saving the state ourselves
          _removeTransientComponents(viewRoot);
    
          Object structure = !_needStructure(context) ? null : new Structure(viewRoot);
          Object state = viewRoot.processSaveState(context);
          viewState = new Object[]{structure, state};
    
        }        
    }
    finally 
    {
      // TODO Once we're dependent on JSF 2.1 we should be using StateManager.IS_SAVING_STATE as the key, but 
      // for now we can use the String value of StateManager.IS_SAVING_STATE
      contextAttributes.remove("javax.faces.IS_SAVING_STATE");
    }

    if (_saveAsToken(context))
    {
      viewState = _saveStateToCache(context, viewState, viewRoot);
    }

    _saveCachedViewState(context, viewState);
    return viewState;
  }

  /**
   * Save a component tree as an Object.
   */
  static public Object saveComponentTree(
    FacesContext context,
    UIComponent  component)
  {
    // Don't remove transient components...
    Object structure = new Structure(component);
    Object state = component.processSaveState(context);
    return new PageState(context, new Object[]{structure, state}, null);
  }

  /**
   * Take an object created by saveComponentTree()
   * and instantiate it as a UIComponent.
   */
  static public UIComponent restoreComponentTree(
    FacesContext context,
    Object       savedState) throws ClassNotFoundException,
                                    InstantiationException,
                                    IllegalAccessException
  {
    if (savedState == null)
      throw new NullPointerException();

    if (!(savedState instanceof PageState))
      throw new IllegalArgumentException(_LOG.getMessage(
        "INVALID_SAVED_STATE_OBJECT"));

    PageState viewState = (PageState) savedState;

    Object[] stateArray = (Object[])viewState.getViewState(context);
    Object structure = stateArray[0];
    Object state = stateArray[1];

    UIComponent component =
      ((Structure) structure).createComponent();

    if (state != null)
      component.processRestoreState(context, state);

    return component;
  }


  /**
   * Save a view root.  Doesn't return a SerializedView because
   * SerializedView is a non-static inner class, and this needs
   * to be a static method.
   */
  static public Object saveViewRoot(
    FacesContext context,
    UIViewRoot   root)
  {
    _removeTransientComponents(root);

    Object structure = new Structure(root);
    Object state = root.processSaveState(context);
    return new PageState(context,  new Object[]{structure, state}, root);
  }

  static public UIViewRoot restoreViewRoot(
    FacesContext    context,
    Object          saved) throws ClassNotFoundException, InstantiationException,
                                  IllegalAccessException

  {
    if (saved == null)
      throw new NullPointerException();

    PageState viewState = (PageState) saved;

    UIViewRoot root = viewState.popRoot(context);
    if (root != null)
    {
      return root; // bug 4712492
    }

    Object[] stateArray = (Object[])viewState.getViewState(context);
    Object structure = stateArray[0];
    Object state = stateArray[1];

    root = (UIViewRoot)
      ((Structure) structure).createComponent();

    if (state != null)
      root.processRestoreState(context, state);

    return root;
  }



  private Object _saveStateToCache(FacesContext context, Object viewState, UIViewRoot root)
  {
    ExternalContext extContext = context.getExternalContext();
    RequestContext trinContext = RequestContext.getCurrentInstance();

    TokenCache cache = _getViewCache(trinContext, extContext);
    assert(cache != null);


    // get per window view cache key with "." separator suffix to separate the SubKeyMap keys
    String subkey = _getViewCacheKey(extContext, trinContext, _SUBKEY_SEPARATOR);

    Map<String, Object> sessionMap = extContext.getSessionMap();
    Map<String, PageState> stateMap = new SubKeyMap<PageState>(sessionMap, subkey);

    // Sadly, we can't save just a SerializedView, because we should
    // save a serialized object, and SerializedView is a *non*-static
    // inner class of StateManager
    PageState pageState = new PageState(
        context,
        viewState,
        // Save the view root into the page state as a transient
        // if this feature has not been disabled
        _useViewRootCache(context) ? root : null);

    String requestToken = _getRequestTokenForResponse(context);
    String token;

    // If we have a cached token that we want to reuse,
    // and that token hasn't disappeared from the cache already
    // (unlikely, but not impossible), use the stateMap directly
    // without asking the cache for a new token
    if ((requestToken != null) && cache.isAvailable(requestToken))
    {
      // NOTE: under *really* high pressure, the cache might
      // have been emptied between the isAvailable() call and
      // this put().  This seems sufficiently implausible to
      // be worth punting on
      stateMap.put(requestToken, pageState);
      token = requestToken;
      // NOTE 2: we have not pinned this reused state to any old state
      // This is OK for current uses of pinning and state reuse,
      // as pinning stays constant within a window, and we're not
      // erasing pinning at all.
    }
    else
    {
      // See if we should pin this new state to any old state
      String pinnedToken = (String)extContext.getRequestMap().get(_PINNED_STATE_TOKEN_KEY);
      token = cache.addNewEntry(pageState,
                                stateMap,
                                pinnedToken);
    }


    assert(token != null);

    // And store the token for this request
    extContext.getRequestMap().put(_REQUEST_STATE_TOKEN_KEY, token);

    // clear out all of the previous PageStates' UIViewRoots and add this page
    // state as an active page state.  This is necessary to avoid UIViewRoots
    // laying around if the user navigates off of a page using a GET
    synchronized(extContext.getSession(true))
    {
      // get the per-window key for the active page state.  We only store the token rather than
      // the view state itself here in order to keep fail-over Serialization from Serializing this
      // state twice, once where it appears here and the second time in the token map itself
      // See Trinidad-1779
      String activePageStateKey = _getActivePageTokenKey(extContext, trinContext);
      String activeToken = (String)sessionMap.get(activePageStateKey);

      // we only need to clear out the state if we're actually changing pages and thus tokens.
      // Since we have already updated the state for
      if (!token.equals(activeToken))
      {
        if (activeToken != null)
        {
          PageState activePageState = stateMap.get(activeToken);

          if (activePageState != null)
            activePageState.clearViewRootState();
        }

        sessionMap.put(activePageStateKey, token);
      }
    }

    // Create a "tokenView" which abuses state to store
    // our token only
    return new Object[]{token, null};
  }

  public static String getActivePageToken(RequestContext trinContext, ExternalContext external)
  {
    String activePageStateKey = _getActivePageTokenKey(external, trinContext);

    if (activePageStateKey != null)
    {
      String tokenPrefix = _getViewCacheKey(external, trinContext, _SUBKEY_SEPARATOR);
      String tokenSuffix = (String)external.getSessionMap().get(activePageStateKey);

      return tokenPrefix + tokenSuffix;
    }
    else
    {
      return null;
    }
  }

  /**
   * Requests that an old state token be "pinned" to the state of
   * the current request.  This means that the view state corresponding
   * to the token will not be released before the state for this request
   * is released.
   */
  @SuppressWarnings("unchecked")
  static public void pinStateToRequest(FacesContext context, String stateToken)
  {
    context.getExternalContext().getRequestMap().put(
            _PINNED_STATE_TOKEN_KEY, stateToken);

  }

  /**
   * @return the state token for the current request
   */
  static public String getStateToken(FacesContext context)
  {
    return (String) context.getExternalContext().getRequestMap().get(
            _REQUEST_STATE_TOKEN_KEY);
  }


  /**
   * Mark the the incoming request token should be used for the response
   */
  @SuppressWarnings("unchecked")
  static public void reuseRequestTokenForResponse(ExternalContext ec)
  {
    ec.getRequestMap().put(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY, Boolean.TRUE);
  }

  /**
   * Clears the flag indicating that the old request token should be used for the response.
   */
  @SuppressWarnings("unchecked")
  static public void clearReuseRequestTokenForResponse(ExternalContext ec)
  {
    ec.getRequestMap().remove(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY);
  }

  /**
   * If we've been asked to reuse the request token for the response,
   * store it off.
   */
  @SuppressWarnings("unchecked")
  static private void _updateRequestTokenForResponse(
    FacesContext context, String token)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    // Go from TRUE -> the saved token
    if (Boolean.TRUE.equals(
          requestMap.get(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY)))
    {
      requestMap.put(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY, token);
    }
  }


  /**
   * Get any cached token for the response.
   */
  @SuppressWarnings("unchecked")
  static private String _getRequestTokenForResponse(
    FacesContext context)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    Object token = requestMap.get(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY);
    // We wanted to, but didn't have anything saved
    if (Boolean.TRUE.equals(token))
      return null;

    return (String) token;
  }

  /**
   * Returns whether a token is a currently valid View State Token
   * @param external The ExternalContext
   * @param token    The state token to check for validity
   * @return
   */
  public static boolean isValidViewStateToken(ExternalContext external, String token)
  {
    if ((token != null) && _calculateTokenStateSaving(external))
    {
      return (_getPageState(external, token) != null);
    }
    else
    {
      return false;
    }
  }

  /**
   * Give a valid state token not associated with a window, change it to be associated with
   * the specified window Id.
   * @param external The ExternalContext
   * @param windowId window id to store the state token under
   * @param token    The state token to remap from windowless to windowed
   */
  public static void remapViewState(ExternalContext external, String windowId, String token)
  {
    // remove the view state stored with no associated window from the session
    String oldSubkey = _getViewCacheKey(external, null, _SUBKEY_SEPARATOR);

    Map<String, Object> sessionMap = external.getSessionMap();

    Map<String, PageState> oldStateMap = new SubKeyMap<PageState>(sessionMap, oldSubkey);

    PageState viewState = oldStateMap.remove(token);

    if (viewState == null)
      throw new IllegalArgumentException();

    // store it under the windowId
    String windowSubkey = _getPerWindowCacheKey(windowId, _VIEW_CACHE_KEY, _SUBKEY_SEPARATOR);

    Map<String, PageState> newStateMap = new SubKeyMap<PageState>(sessionMap, windowSubkey);

    newStateMap.put(token, viewState);
  }

  /**
   * Returns the PageState for a state token
   * @param external
   * @param token
   * @return
   */
  private static PageState _getPageState(ExternalContext external, String token)
  {
    // get view cache key with "." separator suffix to separate the SubKeyMap keys
    String subkey = _getViewCacheKey(external,
                                     RequestContext.getCurrentInstance(),
                                     _SUBKEY_SEPARATOR);

    Map<String, PageState> stateMap = new SubKeyMap<PageState>(
                     external.getSessionMap(),
                     subkey);

    return stateMap.get(token);
  }

  @SuppressWarnings({"unchecked", "deprecation"})
  @Override
  public UIViewRoot restoreView(
    FacesContext context,
    String       viewId,
    String       renderKitId)
  {
    final ExternalContext extContext = context.getExternalContext();

    // If we're being asked to execute a "return" event from, say, a dialog, always
    // restore the "launch view", which was set over in the TrinidadFilter.

    Map<String, Object> requestMap = extContext.getRequestMap();
    UIViewRoot launchView = (UIViewRoot)
                            requestMap.remove(RequestContextImpl.LAUNCH_VIEW);
    if (launchView != null)
    {
      TrinidadPhaseListener.markPostback(context);
      return launchView;
    }

    final Object structure;
    final Object state;

    ResponseStateManager rsm = _getResponseStateManager(context, renderKitId);

    if (_saveAsToken(context))
    {
      // we saved the token in the structure portion of the state, so retrieve the
      // structure portion of the state to get the token.
      String token = (String)rsm.getTreeStructureToRestore(context, viewId);
      if (token == null)
      {
        _LOG.finest("No token in the request for view \"{0}\";  probably a first view.", viewId);
        return null;
      }

      _LOG.finer("Restoring saved view state for token {0}", token);

      // get the PageState for the token
      PageState viewState = _getPageState(extContext, token);

      if (viewState != null)
        _updateRequestTokenForResponse(context, token);

      RequestContext trinContext = RequestContext.getCurrentInstance();

      // Make sure that if the view state is present, the cache still
      // has the token, and vice versa

      // NOTE: it's very important that we call through to the
      // token cache here, not just inside the assert.  If we don't,
      // then we don't actually access the token, so it doesn't
      // get bumped up to the front in the LRU Cache!
      boolean isAvailable = _getViewCache(trinContext, extContext).isAvailable((String) token);
      assert ((viewState != null) == isAvailable);
      
      if (viewState == null)
      {
        _LOG.severe("CANNOT_FIND_SAVED_VIEW_STATE", token);

        if(TokenCacheDebugUtils.debugTokenCache())
        {
          // get the state map
          String subkey = _getViewCacheKey(extContext,
                                           RequestContext.getCurrentInstance(),
                                           _SUBKEY_SEPARATOR);

          Map<String, PageState> stateMap = new SubKeyMap<PageState>(
                           extContext.getSessionMap(),
                           subkey);
          
          // log what's currently in the state map
          TokenCacheDebugUtils.startLog("Restore View");
          TokenCacheDebugUtils.logCacheInfo(stateMap, null, "token '" + token + "' not found"); 
          _LOG.severe(TokenCacheDebugUtils.getLogString());        
        }
        
        return null;
      }

      _LOG.fine("Successfully found view state for token {0}", token);

      UIViewRoot root = viewState.popRoot(context); // bug 4712492
      if (root != null)
      {
        _LOG.finer("UIViewRoot for token {0} already exists. Bypassing restoreState", token);
        return root;
      }

      StateManagementStrategy sms = _getStateManagementStrategy(context, viewId);

      if (sms!= null)
      {
        // TODO This is a hack because stateManagementStrategy doesn't take
        // a state object as a param, instead it always asks the responseStateManager
        // for the state, so push the state onto the request where the CoreResponseStateManager
        // can return it. We will file a bug agains JSF 2.0 asking that the
        // stateManagementStrategy deprecate the current restoreView method in favor of
        // a restoreView method that takes state
        try
        {
          requestMap.put(RESPONSE_STATE_MANAGER_STATE_KEY, viewState.getViewState(context));
          root = sms.restoreView(context, viewId, renderKitId);
        }
        finally
        {
          requestMap.remove(RESPONSE_STATE_MANAGER_STATE_KEY);
        }

        return root;
      }
      else
      {
        Object[] stateArray = (Object[])viewState.getViewState(context);
        structure = stateArray[0];
        state = stateArray[1];
      }
    }
    else
    {

      StateManagementStrategy sms = _getStateManagementStrategy(context, viewId);

      if (sms!= null)
      {
        return sms.restoreView(context, viewId, renderKitId);
      }

      Object[] stateArray = (Object[])rsm.getState(context, viewId);
      structure = stateArray[0];
      state = stateArray[1];
    }


    if (structure == null)
    {

      UIViewRoot root = context.getViewRoot();
      if (root == null && _needStructure(context))
      {
        _LOG.severe("NO_STRUCTURE_ROOT_AVAILABLE");
        return null;
      }

      if (state != null)
        root.processRestoreState(context, state);

      return root;
    }
    else
    {
      if (!(structure instanceof Structure))
      {
        _LOG.severe("NO_STRUCTURE_AVAILABLE");
        return null;
      }

      // OK, we've structure and state; let's see what we can do!
      try
      {
        UIViewRoot root = (UIViewRoot)
        ((Structure) structure).createComponent();

        if (state != null)
          root.processRestoreState(context, state);

        _LOG.finer("Restored state for view \"{0}\"", viewId);
        return root;
      }
      catch (ClassNotFoundException cnfe)
      {
        _LOG.severe(cnfe);
      }
      catch (InstantiationException ie)
      {
        _LOG.severe(ie);
      }
      catch (IllegalAccessException iae)
      {
        _LOG.severe(iae);
      }
    }

    return null;
  }

  /**
   * The given parameter (<code>perViewStateSaving</code>) indicates
   * if we need to enable client- OR server-side state-saving
   * for the current VIEW.
   *
   * <p>
   * <b>This is an internal method, that is ONLY called by the
   * Trinidad Document</b>
   * </p>
   *
   * @param perViewStateSaving <code>default</code>, <code>server</code> or <code>client</code> for stateSaving
   */
  public void setPerViewStateSaving(String perViewStateSaving)
  {
    // tweak the given value into one of the three possible enums
    // TODO: catch wrong/invalid values (aka baby sitting)
    Map<Object, Object> attrs = FacesContext.getCurrentInstance().getAttributes();
    attrs.put(_PER_PAGE_STATE_SAVING, StateSaving.valueOf(perViewStateSaving.toUpperCase()));
  }

  @Override
  public boolean isSavingStateInClient(FacesContext context)
  {
    return _delegate.isSavingStateInClient(context);
  }

  //
  // Protected APIs: we don't want
  //

  @Override
  protected Object getTreeStructureToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected Object getComponentStateToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected UIViewRoot restoreTreeStructure
    (FacesContext context, String viewId, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected void restoreComponentState
    (FacesContext context, UIViewRoot viewRoot, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the key that the TokenCache
   * @param extContext
   * @return
   */
  private static String _getTokenCacheKey(RequestContext trinContext, ExternalContext extContext)
  {
    return _getViewCacheKey(extContext, trinContext, null);
  }

  /**
   * Returns the TokenCache for the current window
   * @param trinContext
   * @param extContext
   * @return
   */
  private TokenCache _getViewCache(RequestContext trinContext, ExternalContext extContext)
  {
    String cacheKey = _getTokenCacheKey(trinContext, extContext);

    return TokenCache.getTokenCacheFromSession(extContext,cacheKey, true,_getCacheSize(extContext));
  }



  /**
   * Returns a key suitable for finding the per-window active page state key
   * @param extContext
   * @param trinContext
   * @return
   */
  static private String _getActivePageTokenKey(
    ExternalContext extContext,
    RequestContext trinContext)
  {
    return _getPerWindowCacheKey(_getCurrWindowId(extContext, trinContext),
                                 _ACTIVE_PAGE_TOKEN_SESSION_KEY,
                                 null);
  }

  /**
   * Returns a key suitable for finding the per-window cache key
   * @param extContext
   * @param trinContext
   * @param suffix
   * @return
   */
  static private String _getViewCacheKey(
    ExternalContext extContext,
    RequestContext trinContext,
    Character suffix)
  {
    return _getPerWindowCacheKey(_getCurrWindowId(extContext, trinContext),
                                 _VIEW_CACHE_KEY,
                                 suffix);
  }

  /**
   * Returns the current windowId, if any
   * @param external
   * @param trinContext
   * @return
   */
  static private String _getCurrWindowId(ExternalContext external, RequestContext trinContext)
  {
    if (trinContext != null)
    {
      WindowManager wm = trinContext.getWindowManager();

      if (wm != null)
      {
        Window currWindow = wm.getCurrentWindow(external);

        if (currWindow != null)
        {
          return currWindow.getId();
        }
      }
    }

    return null;
  }


  /**
   * Returns a key of the form <prefix>.<windowid><suffix> if a window and a suffix are available
   *                           <prefix>.<window> if just a window is available
   *                           <prefix> if neither a window or a suffix is available
   * @param windowId
   * @param prefix
   * @param suffix
   * @return
   */
  private static String _getPerWindowCacheKey(
    String          windowId,
    String          prefix,
    Character       suffix)
  {
    // if we have a current window or a suffix, we need a StringBuilder to calculate the cache key
    if ((windowId != null) || (suffix != null))
    {
      // compute the extra size neeeded to store the windowId and its separator
      int windowPartSize;

      if (windowId != null)
      {
        // add 1 for separator
        windowPartSize = windowId.length() + 1;
      }
      else
      {
        windowPartSize = 0;
      }

      int builderSize =  prefix.length() + windowPartSize;

      // add extra space for the suffix Character
      if (suffix != null)
        builderSize += 1;

      // add the constant part to the StringBuilder
      StringBuilder keyBuilder = new StringBuilder(builderSize);
      keyBuilder.append(prefix);

      // add the windowId and its separator
      if (windowId != null)
      {
        keyBuilder.append('.');
        keyBuilder.append(windowId);
      }

      // add the suffix if any
      if (suffix != null)
        keyBuilder.append(suffix);

      return keyBuilder.toString();
    }
    else
    {
      return prefix;
    }
  }

  /**
   * Returns <code>true</code> if we should use token state saving rather than client state
   * saving
   * @param external
   * @return
   * @see #_saveAsToken
   */
  private static boolean _calculateTokenStateSaving(ExternalContext external)
  {
    Map initParameters = external.getInitParameterMap();

    Object stateSavingMethod = initParameters.get(StateManager.STATE_SAVING_METHOD_PARAM_NAME);

    // on "SERVER" state-saving we return TRUE, since we want send down a token string.
    if ((stateSavingMethod == null) ||
        StateManager.STATE_SAVING_METHOD_SERVER.equalsIgnoreCase((String) stateSavingMethod))
    {
      return true;
    }

    // if the user set state-saving to "CLIENT" *and* the client-state-method to "ALL"
    // we return FALSE, since we want to save the ENTIRE state on the client...
    Object clientMethod = initParameters.get(CLIENT_STATE_METHOD_PARAM_NAME);

    if ((clientMethod != null) &&
        CLIENT_STATE_METHOD_ALL.equalsIgnoreCase((String) clientMethod))
    {
      return false;
    }

    // if the user has used the <document> 'stateSaving' attribute to specify
    // client, we force the state mananger (see above) to render the entire
    // state on the client. The indicator is stashed on the FacesContext and
    // is therefore NOT visible during "restoreView" phase. So if we reach this point
    // here AND we are using "full" client-side-state-saving, this has been tweaked
    // on the previous page rendering phase...
    // In this case we return FALSE to indicate to restore the entire (serialized)
    // state from the client!
    //
    // @see setPerViewStateSaving()
    String viewStateValue =
                      external.getRequestParameterMap().get(ResponseStateManager.VIEW_STATE_PARAM);

    if (viewStateValue != null && !viewStateValue.startsWith("!"))
    {
      return false;
    }

    // In certain situations this method is called from a filter and there's no facesContext, so 
    // make sure to check for a null context
    FacesContext context = FacesContext.getCurrentInstance();

    if (context != null)
    {
      // is vanilla JSF used? No Trinidad render-kit-id give? If so, we need to return FALSE,
      // since we want to save the ENTIRE state on the client...
      UIViewRoot viewRoot = context.getViewRoot();
      
      if (viewRoot != null && RenderKitFactory.HTML_BASIC_RENDER_KIT.equals(viewRoot.getRenderKitId()))
      {
        return false;
      }
    }

    // Last missing option: state-saving is "CLIENT" and the client-state-method uses
    // its default (token), so we return TRUE to send down a token string.
    return true;
  }

  /**
   * Tests whether to send a small string token, or the entire
   * serialized component state to the client-side.
   * @return true, if the small string token is to be sent to the client-side.
   * @see #_calculateTokenStateSaving
   */
  private boolean _saveAsToken(FacesContext context)
  {
    // the setPerViewStateSaving() method stores the PER_PAGE_STATE_SAVING value on the
    // FacesContext attribute map, during rendering
    Map<Object, Object> attrMap = FacesContext.getCurrentInstance().getAttributes();
    StateSaving stateSaving = (StateSaving) attrMap.get(_PER_PAGE_STATE_SAVING);

    // if the <document> 'stateSaving' attribute said "client" we need to return FALSE
    // in order to do "full" client-side-state saving.
    Boolean forceStateSavingPerView = null;

    if (StateSaving.CLIENT.equals(stateSaving))
    {
     forceStateSavingPerView = Boolean.FALSE;
    }
    // for "server" we return TRUE here, as we want client-side
    // state-saving to be turned OFF, regardless of the configuration
    // settings.
    else if (StateSaving.SERVER.equals(stateSaving))
    {
      forceStateSavingPerView = Boolean.TRUE;
    }

    // for the stateSaving "defaul" we just let go and do what it
    // normally would do...
    if (forceStateSavingPerView != null)
    {
      return forceStateSavingPerView.booleanValue();
    }

    return _calculateTokenStateSaving(context.getExternalContext());
  }

  private int _getCacheSize(ExternalContext extContext)
  {
    Object maxTokens =
      extContext.getInitParameterMap().get(CLIENT_STATE_MAX_TOKENS_PARAM_NAME);
    if (maxTokens != null)
    {
      try
      {
        return Math.max(1, Integer.parseInt((String) maxTokens));
      }
      catch (NumberFormatException nfe)
      {
        _LOG.warning("Ignoring servlet init parameter:"+CLIENT_STATE_MAX_TOKENS_PARAM_NAME+
          "\n unable to parse:"+maxTokens, nfe);
        _LOG.warning(nfe);
      }
    }

    return _DEFAULT_CACHE_SIZE;
  }


  private boolean _useViewRootCache(FacesContext context)
  {
    if (_useViewRootCache == null)
    {
      String s = context.getExternalContext().getInitParameter(
                        CACHE_VIEW_ROOT_INIT_PARAM);
      _useViewRootCache =
      (!"false".equalsIgnoreCase(s)) ? Boolean.TRUE : Boolean.FALSE;
    }

    return _useViewRootCache.booleanValue();
  }



  private boolean _needStructure(FacesContext context)
  {
    // TODO - partial state saving is handled by facelets in JSF 2.0,
    //        remove this method and anything that depends on it?
    return true;
  }

  static private ResponseStateManager _getResponseStateManager(
    FacesContext context,
    String       renderKitId)
  {
    RenderKitFactory factory = (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    RenderKit kit = factory.getRenderKit(context, renderKitId);
    return kit.getResponseStateManager();
  }

  @SuppressWarnings("unchecked")
  static private void _removeTransientComponents(
    UIComponent root)
  {
    List<UIComponent> components = new ArrayList<UIComponent>();
    _gatherTransientComponents(root, components);
    Iterator<UIComponent> iter = components.iterator();
    while (iter.hasNext())
    {
      UIComponent kid = iter.next();
      UIComponent parent = kid.getParent();
      // First, see if its a child
      if (parent.getChildCount() > 0)
      {
        List<UIComponent> children = parent.getChildren();
        if (children.remove(kid))
        {
          continue;
        }
      }

      // Nope, guess it's a facet
      // 2006-08-02: -= Simon Lessard
      //             Not 1.5 structure and inefficient loop
      //             values() is more efficient as you don't have
      //             to do a second lookup for the value.
      Map<String, UIComponent> facets = parent.getFacets();
      for(Iterator<UIComponent> facetIter = facets.values().iterator();
          facetIter.hasNext();)
      {
        if(facetIter.next() == kid)
        {
          facetIter.remove();
          // FIXME: -= Simon Lessard
          //        Is that continue need to labeled to go all the way up to
          //        the first while? Currently it won't cause any problem, but
          //        it's a performance loss.
          continue;
        }
      }

      // Couldn't find the component at all in its parent:  that's
      // not good.
      assert false;
    }
  }

  @SuppressWarnings("unchecked")
  static private void _gatherTransientComponents(
    UIComponent component, List<UIComponent> componentsToRemove)
  {
    Iterator<UIComponent> kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      // UIXComponentBase doesn't mind transient components
      // in its saved state, so don't bother with this.
      if (!(component instanceof UIXComponentBase) &&
          kid.isTransient())
      {
        componentsToRemove.add(kid);
      }
      else
      {
        _gatherTransientComponents(kid, componentsToRemove);
      }
    }
  }

  private Object _getCachedViewState(
    FacesContext context)
  {
    return context.getExternalContext().
                 getRequestMap().get(_CACHED_VIEW_STATE);
  }

  private void _saveCachedViewState(
    FacesContext context, Object state)
  {
    context.getExternalContext().getRequestMap().put(_CACHED_VIEW_STATE,
                                                     state);
  }

  private StateManagementStrategy _getStateManagementStrategy(FacesContext context, String viewId)
  {
    ViewDeclarationLanguage vdl =  context.getApplication().getViewHandler().
                                                    getViewDeclarationLanguage(context, viewId);
    if (vdl != null)
    {
      return vdl.getStateManagementStrategy(context, viewId);
    }
    else
    {
      return null;
    }
  }

  private static final class ViewRootState
  {
    public ViewRootState(FacesContext context, UIViewRoot viewRoot)
    {
      if (viewRoot == null)
        throw new NullPointerException();

      _viewRoot = viewRoot;
      _viewRootState = viewRoot.saveState(context);
    }

    public UIViewRoot getViewRoot()
    {
      return _viewRoot;
    }

    public Object getViewRootState()
    {
      return _viewRootState;
    }

    private final UIViewRoot _viewRoot;
    private final Object _viewRootState;
  }

  private static final class PageState implements Serializable
  {
    private static final long serialVersionUID = 1L;

    private Object _viewState;

    // use transient since UIViewRoots are not Serializable.
    private transient ViewRootState _cachedState;

    public PageState(FacesContext context, Object viewState, UIViewRoot root)
    {
      _viewState = viewState;

      boolean zipState = _zipState(context);

      if (zipState || StateUtils.checkComponentTreeStateSerialization(context))
      {

        if (zipState)
        {
          // zip the page state. This will also catch any serialization problems.
          _zipToBytes(context, viewState);
        }
        else
        {
          // if component tree serialization checking is on (in order to validate
          // fail over support, attempt to Serialize all of the component state
          //  immediately
          try
          {
            new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(viewState);
          }
          catch (IOException e)
          {
            throw new RuntimeException(_LOG.getMessage("COMPONENT_TREE_SERIALIZATION_FAILED"), e);
          }
        }
      }

      // we need this state, as we are going to recreate the UIViewRoot later. see
      // the popRoot() method:
      _cachedState = (root != null)
                       ? new ViewRootState(context, root)
                       : null;
    }

    public Object getViewState(FacesContext context)
    {
      if (_zipState(context))
      {
        return _unzipBytes(context, (byte[])_viewState);
      }

      return _viewState;
    }

    public void clearViewRootState()
    {
      synchronized(this)
      {
        _cachedState = null;
      }
    }

    @SuppressWarnings("unchecked")
    public UIViewRoot popRoot(FacesContext fc)
    {
      UIViewRoot root = null;
      Object viewRootState = null;
      // we need to synchronize because we are mutating _root
      // which is shared between simultaneous requests from the same user:
      synchronized(this)
      {
        if (_cachedState != null)
        {
          root = _cachedState.getViewRoot();
          viewRootState = _cachedState.getViewRootState();
          // we must clear the cached viewRoot. This is because UIComponent trees
          // are mutable and if the back button
          // is used to come back to an old PageState, then it would be
          // really bad if we reused that component tree:
          _cachedState = null;
        }
      }

      if (root != null)
      {
        // If an error happens during updateModel, JSF 1.1 does not
        // clear FacesEvents (or FacesMessages, IIRC), so any pending
        // events will still be present, on the subsequent request.
        // so to clear the events, we create a new UIViewRoot.
        // must get the UIViewRoot from the application so that
        // we pick up any custom ViewRoot defined in faces-config.xml:
        UIViewRoot newRoot = (UIViewRoot)
          fc.getApplication().createComponent(UIViewRoot.COMPONENT_TYPE);

        //This code handles automatic namespacing in a JSR-301 environment
        if(ExternalContextUtils.isPortlet(fc.getExternalContext()))
        {
          //IMPORTANT: To avoid introducing a runtime dependency on the bridge,
          //this method should only be executed when we have a portlet
          //request.
          try
          {
            newRoot = (UIViewRoot) root.getClass().newInstance();
          }
          catch (InstantiationException e)
          {
            _LOG.finest("Unable to instantiate new root of type class \"{0}\".", root.getClass());
          }
          catch (IllegalAccessException e)
          {
            _LOG.finest("IllegalAccessException on new root of type class \"{0}\".", root.getClass());
          }
        }


        // must call restoreState so that we setup attributes, listeners,
        // uniqueIds, etc ...
        newRoot.restoreState(fc, viewRootState);

        // we need to use a temp list because as a side effect of
        // adding a child to a UIComponent, that child is removed from
        // the parent UIComponent. So the following will break:
        // newRoot.getChildren().addAll(root.getChildren());
        // because "root"'s child List is being mutated as the List
        // is traversed.
        List<UIComponent> temp = new ArrayList<UIComponent>(root.getChildCount());
        temp.addAll(root.getChildren());
        newRoot.getChildren().addAll(temp);
        return newRoot;
      }

      return null;
    }


    private boolean _zipState(FacesContext fc)
    {
      // default is false
      Object zipStateObject =
                           fc.getExternalContext().getInitParameter(COMPRESS_VIEW_STATE_PARAM_NAME);

      if (zipStateObject == null)
        return false;

      return zipStateObject.toString().equalsIgnoreCase("true");
    }

    private Object _unzipBytes(FacesContext context, byte[] zippedBytes)
    {
      Inflater decompressor = new Inflater();

      try
      {
        decompressor.setInput(zippedBytes);
        ByteArrayOutputStream bos = new ByteArrayOutputStream(zippedBytes.length);
        byte[] buf = new byte[zippedBytes.length*5];

        while (!decompressor.finished())
        {
          try
          {
            int count = decompressor.inflate(buf);
            bos.write(buf, 0, count);
          }
          catch (DataFormatException e)
          {
            throw new RuntimeException(_LOG.getMessage("UNZIP_STATE_FAILED"), e);
          }
        }

        ByteArrayInputStream baos = new ByteArrayInputStream(bos.toByteArray());
        ObjectInputStream ois = new ObjectInputStreamResolveClass(baos);
        Object unzippedState = ois.readObject();
        ois.close();
        return unzippedState;
      }
      catch(ClassNotFoundException cnfe)
      {
        throw new RuntimeException(_LOG.getMessage("UNZIP_STATE_FAILED"), cnfe);
      }
      catch(IOException ioe)
      {
        throw new RuntimeException(_LOG.getMessage("UNZIP_STATE_FAILED"), ioe);
      }
      finally
      {
        decompressor.end();       
      }
    }

    private void _zipToBytes(FacesContext context, Object state)
    {
      Deflater compresser = new Deflater(Deflater.BEST_SPEED);

      try
      {

        //Serialize state
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);

        oos.writeObject(state);
        oos.flush();
        oos.close();

        byte[] ret =  baos.toByteArray();
        compresser.setInput(ret);
        compresser.finish();

        baos.reset();
        byte[] buf = new byte[ret.length/5];

        while (!compresser.finished())
        {
          int count = compresser.deflate(buf);
          baos.write(buf, 0, count);
        }

        _viewState = baos.toByteArray();

      }
      catch (IOException e)
      {
        throw new RuntimeException(_LOG.getMessage("ZIP_STATE_FAILED"), e);
      }
      finally
      {
        compresser.end();
      }
    }
  }

  /**
   * Static ENUM to capture the values of the <document>'s
   * 'stateSaving' attribute
   */
  static private enum StateSaving
  {
    DEFAULT(CoreDocument.STATE_SAVING_DEFAULT),
    CLIENT(CoreDocument.STATE_SAVING_CLIENT),
    SERVER(CoreDocument.STATE_SAVING_SERVER);
     StateSaving(String stateSaving)
    {
      _stateSaving = stateSaving;
    }

    private String _stateSaving;
  }

  // TODO - we used to delegate to the RI when the stateManagement method was server,
  // but we no longer do that, do we really need _delegate any more?
  private final StateManager _delegate;
  private       Boolean      _useViewRootCache;

  private static final Character _SUBKEY_SEPARATOR = new Character('.');

  private static final int _DEFAULT_CACHE_SIZE = 15;

  // base key used to identify the view cache.  The window name, if any, is appended to this
  private static final String _VIEW_CACHE_KEY =
    "org.apache.myfaces.trinidadinternal.application.VIEW_CACHE";

  // key to stash the per_page_state_saving during rendering
  private static final String _PER_PAGE_STATE_SAVING =
    "org.apache.myfaces.trinidadimpl.PER_PAGE_STATE_SAVING";

  private static final String _CACHED_VIEW_STATE=
    "org.apache.myfaces.trinidadinternal.application.CachedViewState";

  private static final String _REQUEST_STATE_TOKEN_KEY =
    "org.apache.myfaces.trinidadinternal.application.REQUEST_STATE_TOKEN";

  private static final String _PINNED_STATE_TOKEN_KEY =
    "org.apache.myfaces.trinidadinternal.application.PINNED_STATE_TOKEN";

  private static final String _REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY =
    "org.apache.myfaces.trinidadinternal.application.REUSE_REQUEST_TOKEN_FOR_RESPONSE";

  // key for saving the toekn to the PageState for the last accessed view in this Session
  private static final String _ACTIVE_PAGE_TOKEN_SESSION_KEY =
              "org.apache.myfaces.trinidadinternal.application.StateManagerImp.ACTIVE_PAGE_STATE";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateManagerImpl.class);
}
