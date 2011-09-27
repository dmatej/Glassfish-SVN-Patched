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

import java.util.Collections;
import java.util.Iterator;

import javax.el.MethodExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Base class for ShowDetail component.
 * @version $Name:  $ ($Revision: 1125570 $) $Date: 2011-05-20 14:10:50 -0700 (Fri, 20 May 2011) $
 */
abstract public class UIXShowDetailTemplate extends UIXComponentBase
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public boolean isDisclosed();
/**/  abstract public void setDisclosed(boolean setDisclosed);
/**/  abstract public boolean isImmediate();
/**/  abstract public MethodExpression getDisclosureListener();
/**/  abstract public boolean isDisclosedTransient();
/**/  abstract public void setDisclosureListener(MethodExpression expression);

  @Deprecated
  public void setDisclosureListener(MethodBinding binding)
  {
    setDisclosureListener(adaptMethodBinding(binding));
  }

  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Perform standard superclass processing
    super.broadcast(event);

    if (event instanceof DisclosureEvent)
    {
      // Do not update the disclosed if "transient"
      if (!isDisclosedTransient())
      {
        // Expand or collapse this showDetail
        boolean isDisclosed = ((DisclosureEvent) event).isExpanded();
        // If the component is already in that disclosure state, we
        // have a renderer bug.  Either it delivered an unnecessary event,
        // or even worse it set disclosed on its own instead of waiting
        // for the disclosure event to do that, which will lead to lifecycle
        // problems.  So in either case, warn the developer.
        if (isDisclosed == isDisclosed())
        {
          _LOG.warning("EVENT_DELIVERED_ALREADY_IN_DISCLOSURE_STATE", event);
        }
        else
        {
          setDisclosed(isDisclosed);
        }

        //pu: Implicitly record a Change for 'disclosed' attribute
        addAttributeChange("disclosed",
                           isDisclosed ? Boolean.TRUE : Boolean.FALSE);
      }

      if (isImmediate())
        getFacesContext().renderResponse();

      // Notify the specified disclosure listener method (if any)
      broadcastToMethodExpression(event, getDisclosureListener());
    }
  }

  @Override
  public void queueEvent(FacesEvent e)
  {
    if ((e instanceof DisclosureEvent) && (e.getSource() == this))
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

  protected Iterator<UIComponent> getRenderedFacetsAndChildren(FacesContext facesContext)
  {
    if (isDisclosed())
    {
      return super.getRenderedFacetsAndChildren(facesContext);
    }
    else
    {
      return Collections.<UIComponent>emptyList().iterator();
    }
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXShowDetail.class);
}
