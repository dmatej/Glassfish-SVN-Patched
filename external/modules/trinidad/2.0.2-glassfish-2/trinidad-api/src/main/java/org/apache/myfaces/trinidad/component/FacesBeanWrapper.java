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

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.el.ValueExpression;

import javax.faces.component.behavior.ClientBehavior;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;


/**
 * Wrapper for {@link org.apache.myfaces.trinidad.bean.FacesBean}
 */
public class FacesBeanWrapper
  implements FacesBean
{
  private FacesBean _wrapped;

  protected FacesBeanWrapper(FacesBean beanToWrap)
  {
    _wrapped = beanToWrap;
  }

  public FacesBean getWrappedBean()
  {
    return _wrapped;
  }

  public FacesBean.Type getType()
  {
    return _wrapped.getType();
  }

  public Object getProperty(PropertyKey key)
  {
    return _wrapped.getProperty(key);
  }

  public void setProperty(PropertyKey key, Object value)
  {
    _wrapped.setProperty(key, value);
  }

  public Object getLocalProperty(PropertyKey key)
  {
    return _wrapped.getLocalProperty(key);
  }

  public ValueExpression getValueExpression(PropertyKey key)
  {
    return _wrapped.getValueExpression(key);
  }

  @SuppressWarnings("deprecation")
  public ValueBinding getValueBinding(PropertyKey key)
  {
    return _wrapped.getValueBinding(key);
  }

  public Object getRawProperty(PropertyKey key)
  {
    return _wrapped.getRawProperty(key);
  }

  public void setValueExpression(PropertyKey key,
                                 ValueExpression expression)
  {
    _wrapped.setValueExpression(key, expression);
  }

  @SuppressWarnings("deprecation")
  public void setValueBinding(PropertyKey key, ValueBinding binding)
  {
    _wrapped.setValueBinding(key, binding);
  }
  
  public void addEntry(PropertyKey listKey, Object value)
  {
    _wrapped.addEntry(listKey, value);
  }

  public void removeEntry(PropertyKey listKey, Object value)
  {
    _wrapped.removeEntry(listKey, value);
  }

  public Object[] getEntries(PropertyKey listKey, Class<?> clazz)
  {
    return _wrapped.getEntries(listKey, clazz);
  }

  public boolean containsEntry(PropertyKey listKey, Class<?> clazz)
  {
    return _wrapped.containsEntry(listKey, clazz);
  }

  public Iterator<? extends Object> entries(PropertyKey listKey)
  {
    return _wrapped.entries(listKey);
  }

  public void addAll(FacesBean from)
  {
    _wrapped.addAll(from);
  }

  public Set<PropertyKey> keySet()
  {
    return _wrapped.keySet();
  }

  public Set<PropertyKey> bindingKeySet()
  {
    return _wrapped.bindingKeySet();
  }

  public Object saveState(FacesContext context)
  {
    return _wrapped.saveState(context);
  }

  public void restoreState(FacesContext context, Object state)
  {
    _wrapped.restoreState(context, state);
  }

  public void clearInitialState()
  {
    _wrapped.clearInitialState();
  }

  public void markInitialState()
  {
    _wrapped.markInitialState();
  }

  public boolean initialStateMarked()
  {
    return _wrapped.initialStateMarked();
  }
}
