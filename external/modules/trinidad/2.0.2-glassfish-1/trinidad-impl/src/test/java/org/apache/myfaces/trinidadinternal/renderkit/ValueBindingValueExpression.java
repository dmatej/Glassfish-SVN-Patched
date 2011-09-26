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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.Serializable;

import javax.el.ELContext;
import javax.el.ValueExpression;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;


@Deprecated
class ValueBindingValueExpression
  extends ValueExpression implements Serializable
{
  public ValueBindingValueExpression(ValueBinding binding)
  {
    _binding = binding;
  }
  
  public ValueBinding getValueBinding()
  {
    return _binding;
  }
  
  public Object getValue(ELContext elContext)
  {
    return _binding.getValue(FacesContext.getCurrentInstance());
  }

  public void setValue(ELContext elContext, Object object)
  {
    _binding.setValue(FacesContext.getCurrentInstance(), object);
  }

  public boolean isReadOnly(ELContext elContext)
  {
    return _binding.isReadOnly(FacesContext.getCurrentInstance());
  }

  public Class<?> getType(ELContext elContext)
  {
    return _binding.getType(FacesContext.getCurrentInstance());
  }

  public Class<?> getExpectedType()
  {
    return null;
  }

  public String getExpressionString()
  {
    return _binding.getExpressionString();
  }

  public boolean isLiteralText()
  {
    return false;
  }

  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    if (!(o instanceof ValueBindingValueExpression))
      return false;
      
    ValueBindingValueExpression that = (ValueBindingValueExpression) o;
    return that._binding.equals(_binding);
  }

  public int hashCode()
  {
    return _binding.hashCode();
  }

  private final ValueBinding _binding;
}
