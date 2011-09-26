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
package org.apache.myfaces.trinidadinternal.taglib;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;

/**
 * @todo move to a reusable location.
 */
public class ConstantMethodBinding extends MethodBinding implements StateHolder
{
  /**
   * Public no-arg constructor for StateHolder _only_.
   */
  public ConstantMethodBinding()
  {
  }

  public ConstantMethodBinding(Object o)
  {
    if (o == null)
      throw new IllegalArgumentException();
    _o = o;
  }
  
  @Override
  public Object invoke(FacesContext context, Object[] params)
  {
    return _o;
  }
  
  @Override
  public Class<?> getType(FacesContext context)
  {
    return _o.getClass();
  }

  public Object saveState(FacesContext context)
  {
    return _o;
  }

  public void restoreState(FacesContext context, Object state)
  {
    _o = state;
  }

  public boolean isTransient()
  {
    return false;
  }
  
  public void setTransient(boolean newTransientValue)
  {
    throw new UnsupportedOperationException();
  }

  private Object _o;
}
