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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.faces.application.ConfigurableNavigationHandler;
import javax.faces.application.NavigationCase;
import javax.faces.application.NavigationHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

public class NavigationHandlerImpl extends ConfigurableNavigationHandler
{
  public NavigationHandlerImpl(NavigationHandler delegate)
  {
    _delegate = delegate;
  }

  @Override
  public void handleNavigation(
    FacesContext context,
    String       fromAction,
    String       outcome)
  {
    if (_disableNavigationHandler(context))
    {
      _delegate.handleNavigation(context, fromAction, outcome);
      return;
    }
 
    RequestContext afc = RequestContext.getCurrentInstance();
    
    // We are interested for "dialog:" prefixed outcomes 
    if ((outcome != null) && (outcome.startsWith(afc.getDialogService().getDialogNavigationPrefix())))
    {
      // Handle "dialog:" URLs
      
      // First we try find a classic navigation case from faces-config.xml
      NavigationCase navigationCase = getNavigationCase(context, fromAction, outcome);
      
      // Then if there is no rule (but we are in dialog here) try interpret 
      // outcome as view id - JSF 2.0 Implicit Navigation.
      if (navigationCase == null)
      {
        navigationCase = getNavigationCase(context, fromAction, 
            outcome.substring(afc.getDialogService().getDialogNavigationPrefix().length()));
      } 
      
      UIViewRoot newRoot = null;
      UIViewRoot oldRoot = context.getViewRoot();
      
      if (navigationCase == null)
      {
        // Execute the old (pre-ConfigurableNavigation) code in case the navigation case
        // could not be determined
        
        // ViewMap is cleared during navigation, so save it here (we will be undoing the navigation
        // by restoring the old view root below)
        Map<String,Object> viewMap = oldRoot.getViewMap(false);
        Map<String,Object> cloneMap = (viewMap == null) ? null : new HashMap<String, Object>(viewMap);
        
        _delegate.handleNavigation(context, fromAction, outcome);
        
        newRoot = context.getViewRoot();
        
        if (newRoot != oldRoot)
        {
          // Navigate back to the original root
          context.setViewRoot(oldRoot);
          
          // Restore the old ViewMap because it gets cleared during setViewRoot()
          if (cloneMap != null)
          {
            oldRoot.getViewMap().putAll(cloneMap);
          }
        }
      }
      else
      {
        newRoot = context.getApplication().getViewHandler().createView(context, navigationCase.getToViewId(context));
      }
     
      if (newRoot != oldRoot)
      {
        // Give ourselves a new page flow scope
        afc.getPageFlowScopeProvider().pushPageFlowScope(context, true);
        // And ask the component to launch a dialog
        afc.getDialogService().queueLaunchEvent(newRoot);
      }
    }
    else
    {
       // not a dialog, call the wrapped NavigationHandler
      _delegate.handleNavigation(context, fromAction, outcome);
    }
  }
  
  @Override
  public NavigationCase getNavigationCase(FacesContext context, String fromAction,
      String outcome)
  {
    if (!(_delegate instanceof ConfigurableNavigationHandler))
      return null;
    
    return ((ConfigurableNavigationHandler)_delegate).getNavigationCase(context, fromAction, outcome);
  }

  @Override
  public Map<String, Set<NavigationCase>> getNavigationCases()
  {
    if (!(_delegate instanceof ConfigurableNavigationHandler))
      return _emptyCaces;
    
    return ((ConfigurableNavigationHandler)_delegate).getNavigationCases();
  }
  
  /**
   * Returns true if "dialog:" prefixes should be entirely disabled.
   */
  synchronized private boolean _disableNavigationHandler(FacesContext context)
  {
    if (_disabled == null)
    {
      _disabled = Boolean.FALSE;

      // First, look in the application map for "true" or Boolean.TRUE.
      Object disabledAttr = context.getExternalContext().getApplicationMap().
        get(DialogService.DISABLE_DIALOG_OUTCOMES_PARAM_NAME);
      if (disabledAttr != null)
      {
        _disabled = "true".equalsIgnoreCase(disabledAttr.toString());
      }
      else
      {
        String disabledParam =
          context.getExternalContext().getInitParameter(
                        DialogService.DISABLE_DIALOG_OUTCOMES_PARAM_NAME);
        if (disabledParam != null)
        {
          _disabled = "true".equalsIgnoreCase(disabledParam);
        }
      }
    }
    
    return _disabled.booleanValue();
  }

  private Boolean _disabled;
  private NavigationHandler _delegate;
  private final Map<String, Set<NavigationCase>> _emptyCaces = new HashMap<String, Set<NavigationCase>>();
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(NavigationHandlerImpl.class);
}
