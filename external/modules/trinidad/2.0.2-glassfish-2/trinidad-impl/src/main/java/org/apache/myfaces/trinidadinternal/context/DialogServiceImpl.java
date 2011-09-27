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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.event.LaunchEvent;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.render.DialogRenderKitService;
import org.apache.myfaces.trinidad.util.Service;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;
import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;

public class DialogServiceImpl extends DialogService
{
  public static final String DIALOG_RETURN = "org.apache.myfaces.trinidad.DialogReturn";
    
  public DialogServiceImpl(RequestContextImpl context)
  {
    _context = context;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void pushView(UIViewRoot viewRoot)
  {
    FacesContext context = _getFacesContext();
    Object savedOld = StateManagerImpl.saveViewRoot(context, viewRoot);

    List<Object> list = (List<Object>) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list == null)
      list = new ArrayList<Object>(1);
    else
    {
      List<Object> tmp = new ArrayList<Object>(list.size() + 1);
      tmp.addAll(list);
      list = tmp;
    }

    list.add(savedOld);
    _getPageFlowScope().put(_PUSHED_VIEWS_KEY, list);

    _LOG.fine("Pushed view {0}", viewRoot.getViewId());
  }

  @SuppressWarnings("unchecked")
  @Override
  public void popView(boolean navigateToPoppedView)
  {
    FacesContext context = _getFacesContext();
    // Pop the view, and navigate back
    if (navigateToPoppedView)
    {
      UIViewRoot poppedView = peekView();
      if (poppedView == null)
        throw new IllegalStateException(_LOG.getMessage(
          "POPVIEW_NO_VIEW_PUSHED"));

      // Set the view root
      context.setViewRoot(poppedView);

      // Re-execute any "binding" attributes
      _executeBindings(context, poppedView);
      _LOG.fine("Popped view {0}", poppedView.getViewId());
    }

    // Make a copy of the List;  never mutate the original list
    List<Object> list = (List<Object>) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list == null)
    {
      // For starters, this should only happen if we weren't navigating
      assert(!navigateToPoppedView);
      // But even then, it's an illegal state
      throw new IllegalStateException(_LOG.getMessage(
        ">POPVIEW_NO_VIEW_PUSHED"));
    }
    else if (list.size() == 1)
    {
      _getPageFlowScope().remove(_PUSHED_VIEWS_KEY);
    }
    else
    {
      list = new ArrayList<Object>(list);
      list.remove(list.size() - 1);
      _getPageFlowScope().put(_PUSHED_VIEWS_KEY, list);
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public UIViewRoot peekView()
  {
    FacesContext context = _getFacesContext();
    Object savedRoot = null;

    List<Object> list = (List<Object>) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list != null)
      savedRoot = list.get(list.size() - 1);

    if (savedRoot == null)
      return null;

    try
    {
      return StateManagerImpl.restoreViewRoot(context, savedRoot);
    }
    // Exceptions will be exceptional (if a view could be saved,
    // it should be restorable too)
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

    return null;
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean returnFromDialog(Object returnValue,
                                  Map<Object, Object> returnParameters)
  {
    FacesContext context = _getFacesContext();
    context.getExternalContext().getSessionMap().
      put(_DIALOG_RETURN_KEY, new Object[]{returnValue, returnParameters});

    Object usedRenderKit = _getPageFlowScope().get(_USED_RENDER_KIT_KEY);
    if (usedRenderKit == null)
    {
      _LOG.warning("RETURNFROMDIALOG_KEY_NOT_AVAILABLE");
    }


    if (Boolean.TRUE.equals(usedRenderKit))
    {
      DialogRenderKitService service = _getDialogRenderKitService(context);
      if ((service != null) &&
          service.returnFromDialog(context, returnValue))
        return true;
    }

    // If the render kit didn't handle it, then pop the view ourselves
    popView(true);

    UIViewRoot poppedView = context.getViewRoot();

    // And, if there's parameters that need to be passed to the popped page,
    // do that;  we'll mark the response as complete, because we'll need
    // the AdfFacesFilter to re-execute the faces lifecycle with the
    // new parameters
    Map<Object, Object> launchParameters = (Map<Object, Object>)
      poppedView.getAttributes().get(RequestContextImpl.LAUNCH_PARAMETERS);

    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    if (launchParameters != null)
    {
      // Store the parameters and the UIViewRoot for (respectively)
      // AdfFacesFilterImpl and ViewHandlerImpl
      poppedView.getAttributes().remove(RequestContextImpl.LAUNCH_PARAMETERS);

      requestMap.put(RequestContextImpl.LAUNCH_PARAMETERS, launchParameters);
      requestMap.put(RequestContextImpl.LAUNCH_VIEW, poppedView);

      context.responseComplete();
      _LOG.fine("Returned from dialog and re-executing lifecycle for {0}",
                poppedView.getViewId());
    }
    
    requestMap.put(DIALOG_RETURN, Boolean.TRUE);

    return false;

  }

  @SuppressWarnings("unchecked")
  @Override
  public ReturnEvent getReturnEvent(UIComponent source)
  {
    FacesContext context = _getFacesContext();
    if (TrinidadFilterImpl.isExecutingDialogReturn(context))
    {
      Map<String, String> parameterMap =
        context.getExternalContext().getRequestParameterMap();
      String returnParam = parameterMap.get(_RETURN_PARAM);
      if (returnParam == null)
        return null;

      String clientId = source.getClientId(context);
      if (!returnParam.equals(clientId))
        return null;
    }
    else
    {
      DialogRenderKitService service = _getDialogRenderKitService(context);
      if (service == null)
        return null;
      if (!service.isReturning(context, source))
        return null;
    }

    // OK, we think we are returning to this component.  Create
    // the ReturnEvent.
    // =-=AEW Should we automatically add the component as a partial
    // target???
    Object o = context.getExternalContext().getSessionMap().
             remove(_DIALOG_RETURN_KEY);

    Object returnValue = null;
    Map<Object, Object> returnParams = null;
    if (o != null)
    {
      returnValue = ((Object[]) o)[0];
      returnParams = (Map<Object, Object>)((Object[]) o)[1];
    }

    ReturnEvent returnEvent =
       new ReturnEvent(source, returnValue, returnParams);

    if (_LOG.isFine())
      _LOG.fine("Obtained ReturnEvent {0}", returnEvent);

    return returnEvent;
  }

  @Override
  public void queueLaunchEvent(UIViewRoot viewRoot)
  {
    UIComponent source = getCurrentLaunchSource();
    if (source == null)
    {
      launchDialog(viewRoot, null, null, false, null);
    }
    else
    {
      (new InternalLaunch(source, viewRoot)).queue();
    }
  }

  @Override
  public void queueReturnEvent(
    Object returnValue,
    Map<Object, Object> returnParams)
  {
    UIComponent source = getCurrentLaunchSource();
    if (source == null)
    {
      _LOG.warning("CANNOT_QUEUE_RETURN_EVENT");
    }
    else
    {
      (new ReturnEvent(source, returnValue, returnParams)).queue();
    }
  }

  /**
   * Store the current state token (if a dialog has been launched).
   */
  static public void writeCurrentStateToken(FacesContext context, String token)
  {
    if (token == null)
      return;

    // Locate the String array on the request map, and store the token in
    // there if it exists.  The String array is also going to be on the
    // page flow scope map for the dialog
    Object o =
      context.getExternalContext().getRequestMap().get(_TARGET_FOR_STATE_TOKEN);
    if (o instanceof String[])
    {
      String[] targetForToken = (String[]) o;
      if (targetForToken.length == 1)
        targetForToken[0] = token;
    }
  }

  /**
   * Store the current state token (if a dialog has been launched).
   */
  static public void pinPriorState(FacesContext context)
  {
    RequestContext rc = RequestContext.getCurrentInstance();
    Object o = rc.getPageFlowScope().get(_TARGET_FOR_STATE_TOKEN);
    if (o instanceof String[])
    {
      String[] targetForToken = (String[]) o;
      if (targetForToken.length == 1)
      {
        String token = targetForToken[0];
        if (token != null)
          StateManagerImpl.pinStateToRequest(context, token);
      }
    }
  }
  
  
  /**
   * Launch a dialog.
   * @todo Don't save parameters for state-saving, page-flow scope, etc.
   */
  @SuppressWarnings("unchecked")
  @Override
  public void launchDialog(
    UIViewRoot  dialogRoot,
    Map<String, Object> dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map<String, Object> windowProperties)
  {
    if (dialogParameters == null)
      dialogParameters = new HashMap<String, Object>();

    FacesContext context = _getFacesContext();

    UIViewRoot sourceRoot = context.getViewRoot();

    // Mark down that we're going to (at least try) to use
    // the renderkit to launch the dialog;  which means
    // we'll need to use the renderkit to close the dialog
    dialogParameters.put(_USED_RENDER_KIT_KEY, Boolean.TRUE);
    
    // We also need to pin down the current state token when we're
    // inside the dialog.  But we don't actually know what the token
    // will be until we're done rendering.  So leave a one-element
    // String array that we can write to later.
    String[] targetForToken = new String[1];
    dialogParameters.put(_TARGET_FOR_STATE_TOKEN, targetForToken);

    // Try to launch a window using the render kit. If that
    // fails (or isn't needed), fall through to the same-window
    // dialog code
    DialogRenderKitService service = _getDialogRenderKitService(context);
    if ((service != null) &&
        service.launchDialog(context,
                             dialogRoot,
                             source,
                             dialogParameters,
                             useWindow,
                             windowProperties))
    {
      // The dialog was successfully launched
      _LOG.fine("Launched {0} dialog using RenderKit",
                dialogRoot.getViewId());
      // And we must pop the pageFlow scope immediately;  it'll
      // be restored later.
      _context.getPageFlowScopeProvider().popPageFlowScope(context, false);

      // We only need to pin the existing state when we're using the renderkit
      // to launch the dialog - so only bother putting anything on the
      // request in that case
      context.getExternalContext().getRequestMap().put(_TARGET_FOR_STATE_TOKEN,
                                                       targetForToken);
    }
    else
    {
      _LOG.fine("Launching {0} dialog via fallback mechanism",
                dialogRoot.getViewId());

      // Nope, we're launching it ourselves
      dialogParameters.put(_USED_RENDER_KIT_KEY, Boolean.FALSE);

      // Save the parameters used to launch the dialog so we can
      // simulate a postback when coming back to the dialog;  and
      // write in a "returnId" with the "id" that will be used.
      Map<String, String[]> savedRequestParameters = new HashMap<String, String[]>();
      savedRequestParameters.putAll(
            context.getExternalContext().getRequestParameterValuesMap());
      if (source != null)
      {
        savedRequestParameters.put(_RETURN_PARAM,
                                   new String[]{source.getClientId(context)});
      }
      // And make sure to remove the _RETURN_PARAM for dialogs
      // launched in response to other dialogs.
      else
      {
        savedRequestParameters.remove(_RETURN_PARAM);
      }

      // =-=AEW HACK: attempt to block events from getting retriggered
      // by simply removing the "source" and "event" parameters altogether!
      savedRequestParameters.remove("source");
      savedRequestParameters.remove("event");

      sourceRoot.getAttributes().put(RequestContextImpl.LAUNCH_PARAMETERS,
                                     savedRequestParameters);
      pushView(sourceRoot);

      // Push parameters into the new pageFlow scope
      _getPageFlowScope().putAll(dialogParameters);

      // And navigate to the dialog root
      context.setViewRoot(dialogRoot);
      context.renderResponse();
    }
  }


  //
  // Get the FacesContext.
  //
  private FacesContext _getFacesContext()
  {
    return _context.__getFacesContext();
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> _getPageFlowScope()
  {
    return _context.getPageFlowScope();
  }

  static private DialogRenderKitService _getDialogRenderKitService(
    FacesContext context)
  {
    RenderKit rk = context.getRenderKit();
    DialogRenderKitService service = 
       Service.getService(rk, DialogRenderKitService.class);

    if (service == null)
    {
      _LOG.info("RENDERKIT_NOT_SUPPORT_DIALOGRENDERKITSERVICE", rk);
    }

    return service;
  }

  /**
   * Execute any "binding" attributes so that a popped view
   * is properly set up
   */
  @SuppressWarnings("unchecked")
  private void _executeBindings(FacesContext context, UIComponent component)
  {
    ValueExpression expression = component.getValueExpression("binding");
    if (expression != null)
      expression.setValue(context.getELContext(), component);

    Iterator<UIComponent> kids = component.getFacetsAndChildren();
    while (kids.hasNext())
      _executeBindings(context, kids.next());
  }

  private RequestContextImpl _context;


  /**
   * Inner class that provides LaunchEvent hooking into
   * internalLaunchPageFlow()
   */
  static private class InternalLaunch extends LaunchEvent
  {
    public InternalLaunch(UIComponent source,
                          UIViewRoot viewRoot)
    {
      super(source, viewRoot);
    }

    @Override
    public void launchDialog(boolean useWindow)
    {
      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.getDialogService().launchDialog(
        getViewRoot(),
        getDialogParameters(),
        getComponent(),
        useWindow,
        getWindowProperties());
    }

    private static final long serialVersionUID = 1L;
  }

  static private final String _PUSHED_VIEWS_KEY =
    "org.apache.myfaces.trinidadinternal.PushedViews";
  static private final String _DIALOG_RETURN_KEY =
    "org.apache.myfaces.trinidadinternal.DialogReturnValue";
  static private final String _USED_RENDER_KIT_KEY =
    "org.apache.myfaces.trinidadinternal.DialogUsedRK";
  static private final String _RETURN_PARAM =
    "org.apache.myfaces.trinidadinternal.ReturnParam";
  static private final String _TARGET_FOR_STATE_TOKEN =
    "org.apache.myfaces.trinidadinternal.StateToken";

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(DialogServiceImpl.class);
}
