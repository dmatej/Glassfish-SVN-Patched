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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.component.UIXSingleStep;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

import org.apache.myfaces.trinidadinternal.uinode.UINodeRendererBase;


/**
 * Renderer for singleStep components
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/SingleStepRenderer.java#0 $) $Date: 10-nov-2005.19:00:37 $
 * @todo do I need to worry about possible exceptions thrown here?
 */
public class SingleStepRenderer extends UINodeRendererBase
{
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> parameters =  
      context.getExternalContext().getRequestParameterMap();
    
    Object event = parameters.get(UIConstants.EVENT_PARAM);

    if (UIConstants.GOTO_EVENT.equals(event))
    {
      Object source = parameters.get(UIConstants.SOURCE_PARAM);

      String id = component.getClientId(context);

      if (id.equals(source))
      {

        Object newValue = parameters.get(UIConstants.VALUE_PARAM);

        int newValueInt = -1;

        if ( newValue != null)
        {
          try{
            newValueInt = Integer.parseInt(newValue.toString());
          }
          catch ( NumberFormatException nfe)
          {
            _LOG.severe(nfe);
          }
        }


        // queue the action on the singleStep component
        // set immediate to true when going back (no validation)
        // and set to false when going forward (validation).
        UIXSingleStep singleStep = (UIXSingleStep) component;

        int oldValueInt = singleStep.getSelectedStep();
        if (newValueInt < oldValueInt )
        {
          singleStep.setActionType(UIXSingleStep.PREVIOUS_ACTION_TYPE);

        }
        else
        {
          singleStep.setActionType(UIXSingleStep.NEXT_ACTION_TYPE);

        }



        // queue an action event
        // This must be added to queue AFTER the actionType is set
        (new ActionEvent(component)).queue();
      }
    }
  }
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SingleStepRenderer.class);
}
