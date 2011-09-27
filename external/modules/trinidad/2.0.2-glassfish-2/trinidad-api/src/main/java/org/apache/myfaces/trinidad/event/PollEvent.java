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
package org.apache.myfaces.trinidad.event;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;

/**
 * Event delivered when the poll component polls the server. 
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/PollEvent.java#0 $) $Date: 10-nov-2005.19:09:03 $
 */
public class PollEvent extends FacesEvent
{
  public PollEvent(UIComponent source)
  {
    super(source);
  }

  @Override
  public void processListener(FacesListener listener)
  {
    ((PollListener) listener).processPoll(this);
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof PollListener);
  }

  @Override
  public int hashCode()
  {
    //A simple implementation to differentiate based on component that
    //  fires the event.
    return (getComponent() == null) ? 0 : getComponent().hashCode();
  }

  @Override
  public boolean equals(Object o)
  {
    if (o instanceof PollEvent)
    {
      PollEvent that = (PollEvent)o;
      return (this.getComponent().equals(that.getComponent()));
    }

    return false;
  }

  @Override
  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(getClass().getName());
    sb.append("[component=");
    sb.append(getComponent());
    sb.append(']');
    return sb.toString();
  }

  private static final long serialVersionUID = 1L;
}
