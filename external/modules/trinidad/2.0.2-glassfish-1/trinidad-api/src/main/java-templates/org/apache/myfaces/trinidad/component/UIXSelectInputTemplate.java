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
package org.apache.myfaces.trinidad.component;

import javax.el.MethodExpression;

import javax.faces.component.ActionSource;
import javax.faces.component.ActionSource2;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.LaunchEvent;
import org.apache.myfaces.trinidad.event.ReturnEvent;


/**
 * Base class for selectInput components.
 *
 * @version $Name:  $ ($Revision: 594297 $) $Date: 2007-11-12 13:03:17 -0800 (Mon, 12 Nov 2007) $
 */
abstract public class UIXSelectInputTemplate extends UIXEditableValue
  implements ActionSource, ActionSource2
{
	
/**/ // Abstract methods implemented by code gen
/**/  abstract public MethodExpression getActionExpression();
/**/  abstract public MethodExpression setActionExpression();
/**/  abstract public MethodBinding getActionListener();
/**/  abstract public MethodExpression getReturnListener();

  @Deprecated
  public void setReturnListener(MethodBinding binding)
  {
    setReturnListener(adaptMethodBinding(binding));
  }

  public MethodBinding getAction()
  {
    MethodExpression me = getActionExpression();
    if (me == null)
      return null;

    if (me instanceof MethodBindingMethodExpression)
      return ((MethodBindingMethodExpression) me).getMethodBinding();

    return new MethodExpressionMethodBinding(me);
  }

  public void setAction(MethodBinding binding)
  {
    if (binding instanceof MethodExpressionMethodBinding)
      setActionExpression(((MethodExpressionMethodBinding) binding).getMethodExpression());
    else
      setActionExpression(new MethodBindingMethodExpression(binding));
  }

  /**
   * <p>Intercept <code>queueEvent</code> and mark the phaseId for any
   * ActionEvents to be <code>PhaseId.APPLY_REQUEST_VALUES</code>.
   */
  @Override
  public void queueEvent(FacesEvent e)
  {
    if (e instanceof ActionEvent)
    {
      // Always deliver ActionEvents immediately?
      //  if (isImmediate())
      {
        e.setPhaseId(PhaseId.ANY_PHASE);
      }
      /*
      else
      {
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
        }*/
    }

    super.queueEvent(e);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    if (event instanceof ActionEvent)
    {
      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.getDialogService().setCurrentLaunchSource(this);

      try
      {
        // Perform standard superclass processing
        super.broadcast(event);

        // Notify the specified action listener method (if any),
        // and the default action listener
        broadcastToMethodBinding(event, getActionListener());

        FacesContext context = getFacesContext();
        ActionListener defaultActionListener =
          context.getApplication().getActionListener();
        if (defaultActionListener != null)
        {
          defaultActionListener.processAction((ActionEvent) event);
        }
      }
      finally
      {
        afContext.getDialogService().setCurrentLaunchSource(null);
      }
    }
    else if (event instanceof LaunchEvent)
    {
      // =-=AEW Support launch listeners on SelectInput?
      // super.broadcast(event);
      //
      // __broadcast(event, getLaunchListener());
      ((LaunchEvent) event).launchDialog(true);
    }
    else if (event instanceof ReturnEvent)
    {
      super.broadcast(event);

      broadcastToMethodExpression(event, getReturnListener());
      Object returnValue = ((ReturnEvent) event).getReturnValue();
      if (returnValue != null)
      {
        setSubmittedValue(returnValue);
      }

      // =-=AEW: always jump to render response???  Seems the safest
      // option, because we don't want to immediately update a model
      // or really perform any validation.
      getFacesContext().renderResponse();
    }
    else
    {
      super.broadcast(event);
    }
  }
}
