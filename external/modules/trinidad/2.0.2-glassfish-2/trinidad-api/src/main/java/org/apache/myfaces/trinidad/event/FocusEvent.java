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
 * Event delivered when focusing on a node in a tree. The event includes information
 * about the old and the new focus row keys.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/FocusEvent.java#0 $) $Date: 10-nov-2005.19:09:01 $
 */
public class FocusEvent extends FacesEvent
{
  /**
   * Creates a new FocusEvent
   * @param source source component
   * @param oldKey old focus row key
   * @param newKey new focus row key
   */
  public FocusEvent(UIComponent source, Object oldKey, Object  newKey)
  {
    super(source);
    _oldKey  = oldKey;
    _newKey  = newKey;    
  }

  /**
   * The constructor with no key info is currently need for backwards compatibility.
   * Will be remove at a later time.  
   * @param source
   */
  public FocusEvent(UIComponent source)
  {
    this(source, null, null);
  }
  
  public Object getOldKey()
  {
    return _oldKey;
  }

  public Object getNewKey()
  {
    return _newKey;
  }

  @Override
  public void processListener(FacesListener listener)
  {
    ((FocusListener) listener).processFocus(this);
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof FocusListener);
  }

  @Override
  public int hashCode()
  {
    return (getComponent() == null) ? 0 : getComponent().hashCode();
  }

  @Override
  public boolean equals(Object o)
  {
    if (o instanceof FocusEvent)
    {
      FocusEvent that = (FocusEvent)o;
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
    sb.append(",oldKey=");
    sb.append(getOldKey());
    sb.append(",newKey=");
    sb.append(getNewKey());
    sb.append(']');
    return sb.toString();
  }

  private final Object _oldKey;
  private final Object _newKey;
  private static final long serialVersionUID = 1L;
}