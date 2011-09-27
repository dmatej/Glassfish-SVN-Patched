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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.ChartDrillDownEvent;
import org.apache.myfaces.trinidad.event.ChartDrillDownListener;

/**
 * Base class for the Table component. The behaviour supported by this base class
 * include record navigation, sorting, selection and detail-disclosure.
 * <p>
 * @version $Name:  $ ($Revision: 431848 $) $Date: 2006-08-16 01:00:16 -0600 (Wed, 16 Aug 2006) $
 */
abstract public class UIXChartTemplate extends UIXComponentBase
{

  /**
   * Delivers an event to the appropriate listeners.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {

    // Deliver to the default ChartDrillDownEvent
    if (event instanceof ChartDrillDownEvent)
    {
      broadcastToMethodExpression(event, getChartDrillDownListener());
    }
    super.broadcast(event);
  }

  /**
   * Adds a drilldown listener.
   *
   * @param listener  the selection listener to add
   */
  final public void addChartDrillDownListener(
    ChartDrillDownListener listener)
  {
    addFacesListener(listener);
  }

  /**
   * Removes a drilldown listener.
   *
   * @param listener  the selection listener to remove
   */
  final public void removeChartDrillDownListener(
    ChartDrillDownListener listener)
  {
    removeFacesListener(listener);
  }

}
