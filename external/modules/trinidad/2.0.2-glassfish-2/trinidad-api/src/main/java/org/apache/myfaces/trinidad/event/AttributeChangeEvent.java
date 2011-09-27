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
 * Event delivered when a renderer-specific attribute has been
 * changed as a result of user interaction.  This event gives
 * advanced <code>Renderers</code> a chance to inform a generic
 * {@link org.apache.myfaces.trinidad.component.UIXComponent} that some portion of it's appearance
 * has been manipulated.  For example, a "splitter" panel renderer
 * could indicate that the proportions of the splitter have changed.
 * <p>
 * <code>AttributeChangeEvents</code> are not delivered in response
 * to programmatic manipulation of a renderer-specific attribute.
 * They must be explicitly queued by a <code>Renderer</code> when
 * it detects that the user had manipulated the component.  Developers
 * should not abuse this event as a one-size-fits-all generic
 * component event.  When a generic component event changes - like
 * "value" or "disclosed", deliver a strongly-typed component event
 * such as <code>ValueChangeEvent</code> or {@link DisclosureEvent}.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/AttributeChangeEvent.java#0 $) $Date: 10-nov-2005.19:08:59 $
 */
public class AttributeChangeEvent extends FacesEvent
{
  public AttributeChangeEvent(
    UIComponent source,
    String      attribute,
    Object      oldValue,
    Object      newValue)
  {
    super(source);
    _attribute = attribute;
    _oldValue  = oldValue;
    _newValue  = newValue;
  }

  public String getAttribute()
  {
    return _attribute;
  }

  public Object getOldValue()
  {
    return _oldValue;
  }

  public Object getNewValue()
  {
    return _newValue;
  }

  @Override
  public void processListener(FacesListener listener)
  {
    ((AttributeChangeListener) listener).processAttributeChange(this);
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof AttributeChangeListener);
  }
  
  @Override
  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(getClass().getName());
    sb.append("[attribute=");
    sb.append(getAttribute());
    sb.append(",component=");
    sb.append(getComponent());
    sb.append(",oldValue=");
    sb.append(getOldValue());
    sb.append(",newValue=");
    sb.append(getNewValue());
    sb.append(']');
    return sb.toString();
  }

  private final String _attribute;
  private final Object _oldValue;
  private final Object _newValue;
  private static final long serialVersionUID = 1L;
}