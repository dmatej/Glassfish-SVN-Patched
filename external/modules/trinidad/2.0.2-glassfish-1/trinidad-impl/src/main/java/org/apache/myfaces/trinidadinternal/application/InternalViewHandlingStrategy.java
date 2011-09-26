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

import java.util.Locale;

import javax.faces.application.Resource;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.view.StateManagementStrategy;
import javax.faces.view.ViewDeclarationLanguage;
import javax.faces.view.ViewMetadata;

import org.apache.myfaces.trinidad.render.InternalView;

public class InternalViewHandlingStrategy
  extends ViewDeclarationLanguage
{
  public InternalViewHandlingStrategy(ViewDeclarationLanguageFactoryImpl.InternalViewFinder finder)
  {
    super();
    _finder = finder;
  }
  
  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    UIViewRoot root = _finder.getInternalView(context, viewId).createView(context, viewId);
    if (root != null)
      return root;
    
    return _defaultCreateView(context, viewId);
  }
  
  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    return _finder.getInternalView(context, viewId).restoreView(context, viewId);
  }
  
  @Override
  public void renderView(FacesContext context, UIViewRoot viewToRender)
    throws IOException
  {
    _finder.getInternalView(context, viewToRender.getViewId()).renderView(context, viewToRender);
  }
  
  @Override
  public StateManagementStrategy getStateManagementStrategy(FacesContext context, String viewId)
  {
    InternalView internal = _finder.getInternalView(context, viewId);
    
    // Use default StateManager processing for the stateful InternalViews
    if (!internal.isStateless(context, viewId))
      return null;
    
    // Otherwise, return StateManagementStrategy that does nothing
    return _STATELESS_STRATEGY;
  }

  public BeanInfo getComponentMetadata(FacesContext facesContext,
                                       Resource resource)
  {
    return null;
  }

  public ViewMetadata getViewMetadata(FacesContext facesContext,
                                      String string)
  {
    return null;
  }

  public Resource getScriptComponentResource(FacesContext facesContext,
                                             Resource resource)
  {
    return null;
  }

  
  public void buildView(FacesContext facesContext, UIViewRoot uIViewRoot)
    throws IOException
  {
  }
  
  /**
   * Package-private method for use by the ViewHandlerImpl
   * @param context Faces context
   * @param viewId Veiew Id
   * @return true if the internal view is stateless, false otherwise
   */
  boolean __isStateless(FacesContext context, String viewId)
  {
    InternalView internal = _finder.getInternalView(context, viewId);
    return internal.isStateless(context, viewId);
  }

  
  private UIViewRoot _defaultCreateView(FacesContext context, String viewId)
  {
    UIViewRoot result = (UIViewRoot) context.getApplication()
                            .createComponent(UIViewRoot.COMPONENT_TYPE);
    
    Locale locale = null;
    String renderKitId = null;
    
    // use the locale from the previous view if is was one which will be
    // the case if this is called from NavigationHandler. There wouldn't be
    // one for the initial case.
    if (context.getViewRoot() != null) 
    {
      locale = context.getViewRoot().getLocale();
      renderKitId = context.getViewRoot().getRenderKitId();
    }
    
    if (locale == null) 
    {
      locale = context.getApplication().getViewHandler().calculateLocale(context);
    }
    
    if (renderKitId == null) 
    {
      renderKitId = context.getApplication().getViewHandler().calculateRenderKitId(context);
    }
    
    result.setLocale(locale);
    result.setRenderKitId(renderKitId);
    result.setViewId(viewId);

    return result;
  }
  
  private final ViewDeclarationLanguageFactoryImpl.InternalViewFinder _finder;
  
  private static final StateManagementStrategy _STATELESS_STRATEGY = 
    new StateManagementStrategy()
    {
      public UIViewRoot restoreView(FacesContext context, String viewId, 
                                    String renderKitId)
      {
        return null;
      }
      
      public Object saveView(FacesContext context)
      {
        return null;
      }
    };
}
