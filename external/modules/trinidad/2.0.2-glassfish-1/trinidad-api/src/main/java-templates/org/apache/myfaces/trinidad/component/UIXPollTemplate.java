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

import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.PollEvent;

/**
 * Base class for Poll component.
 * @version $Name:  $ ($Revision: 594297 $) $Date: 2007-11-12 13:03:17 -0800 (Mon, 12 Nov 2007) $
 */
abstract public class UIXPollTemplate extends UIXComponentBase
{
	
/**/ // Abstract methods implemented by code gen
/**/  abstract public boolean isImmediate();
/**/  abstract public MethodExpression getPollListener();

  @Deprecated
  public void setPollListener(MethodBinding binding)
  {
    setPollListener(adaptMethodBinding(binding));
  }

  //
  // Abstract methods implemented by subclass.
  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Perform standard superclass processing
    super.broadcast(event);

    // Notify the specified Poll listener method (if any)
    if (event instanceof PollEvent)
    {
      broadcastToMethodExpression(event, getPollListener());
    }
  }

  @Override
  public void queueEvent(FacesEvent e)
  {
    if ((e instanceof PollEvent) && (e.getSource() == this))
    {
      if (isImmediate())
      {
        e.setPhaseId(PhaseId.ANY_PHASE);
      }
      else
      {
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
      }
    }

    super.queueEvent(e);
  }
}
