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
package org.apache.myfaces.trinidadinternal.facelets;

import javax.faces.application.StateManager;
import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.ComponentHandler;
import javax.faces.view.facelets.ComponentConfig;

import javax.faces.view.facelets.MetaRuleset;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * TagLibrary implementation for Apache Trinidad core library.
 * @version
 */
public class TrinidadComponentHandler extends ComponentHandler
{
  public TrinidadComponentHandler(ComponentConfig config) 
  {
    super(config);
    if (_markInitialState == null)
    {
      // Can't imagine why this wouldn't always run during
      // a Faces request...
      FacesContext context = FacesContext.getCurrentInstance();
      if (context != null)
      {
        ExternalContext external = context.getExternalContext();
        String restoreMode = external.getInitParameter(
                                                     StateManager.PARTIAL_STATE_SAVING_PARAM_NAME);
        
        if (Boolean.valueOf(restoreMode))
        {
          _markInitialState = Boolean.TRUE;
          _LOG.severe("PARTIAL_STATE_SAVING_NOT_SUPPORTED");
        }
        else
        {
          _markInitialState = Boolean.FALSE;
        }
      }
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  protected MetaRuleset createMetaRuleset(Class type)
  {
    MetaRuleset m = super.createMetaRuleset(type);
    
    m.addRule(StringArrayPropertyTagRule.Instance);
    m.addRule(TrinidadListenersTagRule.Instance);
    m.addRule(AccessKeyPropertyTagRule.Instance);
    m.addRule(DatePropertyTagRule.Instance);
           
    return m;
  }

  @Override
  public void onComponentPopulated(FaceletContext context,
                                     UIComponent component,
                                     UIComponent parent)
  {
    assert (_markInitialState != null);

    if ((component instanceof UIXComponent) &&
        (_markInitialState == Boolean.TRUE))
    {
      if (component.getId() == null)
        component.setId(context.generateUniqueId(UIViewRoot.UNIQUE_ID_PREFIX));

      ((UIXComponent) component).markInitialState();
    }
  }

  static private Boolean _markInitialState;
  static final private TrinidadLogger _LOG =
                               TrinidadLogger.createTrinidadLogger(TrinidadComponentHandler.class);
}
