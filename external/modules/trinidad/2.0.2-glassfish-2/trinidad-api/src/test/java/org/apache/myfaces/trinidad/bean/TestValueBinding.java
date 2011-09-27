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
package org.apache.myfaces.trinidad.bean;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import java.io.Serializable;

// =-=AdamWiner FIXME We shouldn't have to implement Serializable
// on ValueBindings - FacesBeanImpl is not handling ValueExpressions
// around non-serializable ValueBindings
public class TestValueBinding extends ValueBinding implements StateHolder, 
                                                              Serializable
{
  public TestValueBinding()
  {
  }

  @Override
  public Object getValue(FacesContext context)
  {
    return _value;
  }

  @Override
  public void setValue(FacesContext context, Object value)
  {
    _value = value;
  }

  @Override
  public boolean isReadOnly(FacesContext context)
  {
    return false;
  }

  @Override
  public Class<?> getType(FacesContext context)
  {
    return null;
  }

  public Object saveState(FacesContext context)
  {
    return _value;
  }

  public void restoreState(FacesContext context, Object state)
  {
    _value = state;
  }

  public boolean isTransient()
  {
    return _transient;
  }

  public void setTransient(boolean newTransientValue)
  {
    _transient = newTransientValue;
  }

  private boolean _transient;
  private Object _value;
}
