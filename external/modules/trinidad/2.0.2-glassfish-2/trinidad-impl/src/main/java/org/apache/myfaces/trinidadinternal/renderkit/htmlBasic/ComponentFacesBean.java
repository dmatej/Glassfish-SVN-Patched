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
package org.apache.myfaces.trinidadinternal.renderkit.htmlBasic;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.el.ValueExpression;

import javax.faces.component.UIComponent;
import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.behavior.ClientBehaviorHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;


/**
 * Implementation of FacesBean that purely passes through
 * back to a UIComponent.  This exists so that we can
 * reuse existing rendering code to render on a non-FacesBean-based
 * component.  It's also completely immutable.
 *
 */
public class ComponentFacesBean implements FacesBean
{
  public ComponentFacesBean(UIComponent component)
  {
    _component = component;
  }

  public Type getType()
  {
    throw new UnsupportedOperationException();
  }

  final public Object getProperty(PropertyKey key)
  {
    return _component.getAttributes().get(key.getName());
  }
  
  public Map<String, List<ClientBehavior>> getClientBehaviors()
  {
    Map<String, List<ClientBehavior>> behaviors = _EMPTY_CLIENT_BEHAVIORS_MAP;
    if (_component instanceof ClientBehaviorHolder)
    {
      behaviors = ((ClientBehaviorHolder)_component).getClientBehaviors();
    }
    return behaviors;
  }

  /**
   * @todo Need *good* way of hooking property-sets;  it's
   * currently not called from state restoring, so really, it shouldn't
   * be used as a hook, but EditableValueBase currently
   * requires hooking this method.
   */
  public void setProperty(PropertyKey key, Object value)
  {
    throw new UnsupportedOperationException();
  }

  final public Object getLocalProperty(PropertyKey key)
  {
    throw new UnsupportedOperationException();
  }


  final public ValueBinding getValueBinding(PropertyKey key)
  {
    return _component.getValueBinding(key.getName());
  }

  final public ValueExpression getValueExpression(PropertyKey key)
  {
    return _component.getValueExpression(key.getName());
  }

  final public void setValueExpression(PropertyKey key, ValueExpression expression)
  {
    throw new UnsupportedOperationException();
  }

  final public Object getRawProperty(PropertyKey key)
  {
    throw new UnsupportedOperationException();
  }

  final public void setValueBinding(PropertyKey key, ValueBinding binding)
  {
    throw new UnsupportedOperationException();
  }


  final public void addEntry(PropertyKey listKey, Object value)
  {
    throw new UnsupportedOperationException();
  }

  final public void removeEntry(PropertyKey listKey, Object value)
  {
    throw new UnsupportedOperationException();
  }

  final public Object[] getEntries(PropertyKey listKey, Class<?> clazz)
  {
    throw new UnsupportedOperationException();
  }

  final public boolean containsEntry(PropertyKey listKey, Class<?> clazz)
  {
    throw new UnsupportedOperationException();
  }

  final public Iterator<? extends Object> entries(PropertyKey listKey)
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @todo provide more efficient implementation for copying
   * from other FacesBeanImpl instances
   */
  public void addAll(FacesBean from)
  {
    throw new UnsupportedOperationException();
  }

  final public Set<PropertyKey> keySet()
  {
    throw new UnsupportedOperationException();
  }

  final public Set<PropertyKey> bindingKeySet()
  {
    throw new UnsupportedOperationException();
  }

  public void markInitialState()
  {
    throw new UnsupportedOperationException();
  }

  public boolean initialStateMarked()
  {
    throw new UnsupportedOperationException();
  }

  public void clearInitialState()
  {
    throw new UnsupportedOperationException();
  }

  public void restoreState(FacesContext context, Object state)
  {
    throw new UnsupportedOperationException();
  }

  public Object saveState(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  public void addClientBehavior(String eventName, ClientBehavior behavior)
  {
    throw new UnsupportedOperationException();
  }

  private final UIComponent _component;
  
  private static final Map<String, List<ClientBehavior>> _EMPTY_CLIENT_BEHAVIORS_MAP = Collections.emptyMap();
}
