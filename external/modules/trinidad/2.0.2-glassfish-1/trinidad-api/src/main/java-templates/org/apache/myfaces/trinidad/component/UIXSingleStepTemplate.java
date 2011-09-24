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

import javax.faces.component.ActionSource;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

/**
 * Base class for singleStep components
 *
 * @version $Name:  $ ($Revision: 534284 $) $Date: 2007-05-01 17:12:41 -0700 (Tue, 01 May 2007) $
 */
abstract public class UIXSingleStepTemplate
                extends UIXComponentBase
                implements ActionSource
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public MethodBinding getPreviousActionListener();
/**/  abstract public MethodBinding getNextActionListener();
/**/  abstract public MethodBinding getPreviousAction();
/**/  abstract public MethodBinding getNextAction();
/**/  abstract public void setPreviousActionListener(MethodBinding listener);
/**/  abstract public void setNextActionListener(MethodBinding listener);
/**/  abstract public void setPreviousAction(MethodBinding action);
/**/  abstract public void setNextAction(MethodBinding action);

  @Override
  public void queueEvent(FacesEvent e)
  {
    if (e.getSource() == this)
    {
      if (getActionType() == PREVIOUS_ACTION_TYPE)
        e.setPhaseId(PhaseId.APPLY_REQUEST_VALUES);
      else
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
    }

    super.queueEvent(e);
  }

  // TODO Store as transient PropertyKey
  public Object getActionType()
  {
    return _actionType;
  }


  public void setActionType(Object actionType)
  {
    _actionType = actionType;
  }


  // TODO if I have default listeners, then should
  // remove (then restore) default Listeners before saving state -
  // this note copied from form.submitButtonBase.saveState....
  @Override
  public Object saveState(FacesContext context)
  {
    return super.saveState(context);
  }

  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Perform standard superclass processing
    super.broadcast(event);


    FacesContext context = getFacesContext();

    // Notify the specified listener method (if any)
    if (event instanceof ActionEvent)
    {
      if (getActionType() == PREVIOUS_ACTION_TYPE)
      {
        broadcastToMethodBinding(event, getPreviousActionListener());
      }
      else
      {
        broadcastToMethodBinding(event, getNextActionListener());
      }

      ActionListener defaultActionListener =
                             context.getApplication().getActionListener();

      if (defaultActionListener != null)
        defaultActionListener.processAction((ActionEvent) event);
    }
  }

  public MethodBinding getAction()
  {
    if ( getActionType() == PREVIOUS_ACTION_TYPE)
      return getPreviousAction();

    return getNextAction();
  }

  // TODO  Either make this throw an exception, or make it
  //  execute on both back and next.
  public void setAction(MethodBinding action)
  {
    if ( getActionType() == PREVIOUS_ACTION_TYPE)
      setPreviousAction(action);
    else
      setNextAction(action);
  }

  public MethodBinding getActionListener()
  {
    if ( getActionType() == PREVIOUS_ACTION_TYPE)
      return getPreviousActionListener();

    return getNextActionListener();
  }

  // TODO  Either make this throw an exception, or make it
  //  execute on both back and next.
  public void setActionListener(MethodBinding listener)
  {
    if ( getActionType() == PREVIOUS_ACTION_TYPE)
      setPreviousActionListener(listener);
    else
      setNextActionListener(listener);
  }

  /**
   * returns true if the actionType is PREVIOUS_ACTION_TYPE
   */
  public boolean isImmediate()
  {
    return  (getActionType() == PREVIOUS_ACTION_TYPE);
  }
  /**
   * no-op.
   * For UIXSingleStep components, even though it implements ActionSource,
   * we do no have an immediate attribute.
   * By default, if actionType is PREVIOUS_ACTION_TYPE,
   * we set the phase to PhaseId.APPLY_REQUEST_VALUES, else we set the
   * phase to PhaseId.INVOKE_APPLICATION.
   */
  public void setImmediate(boolean immediate)
  {
    // do nothing
  }

  private Object _actionType = NEXT_ACTION_TYPE;

  public static final Object NEXT_ACTION_TYPE = new Object();
  public static final Object PREVIOUS_ACTION_TYPE = new Object();
  // Someday we might add a cancel button?
  // public static final Object CANCEL_ACTION_TYPE = new Object();


}
