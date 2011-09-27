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

import java.io.Serializable;

import javax.el.ELContext;
import javax.el.ELException;
import javax.el.ValueExpression;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.ValueBinding;


class ValueBindingValueExpression extends ValueExpression
{
  /**
   * Given a ValueBinding <code>binding</code>, return a ValueExpression.
   * The returned ValueExpression will implement StateHolder and Serializable interfaces if
   * <code>ve</code> implements these interfaces.
   * @param binding  The ValueBinding
   * @return a ValueExpression equivalent to the ValueBinding
   */
  public static ValueExpression getValueExpression(ValueBinding binding)
  {
    // if we previously wrapped a ValueExpression, unwrap it and return it, otherwise create the
    // correct subclass of ValueBindingValueExpression
    if (binding instanceof ValueExpressionValueBinding)
      return ((ValueExpressionValueBinding)binding).getValueExpression();
    else if (binding instanceof StateHolder)
    {
      if (binding instanceof Serializable)
        return new SerializableStateHolderValueBindingValueExpression(binding);
      else
        return new StateHolderValueBindingValueExpression(binding);      
    }
    else if (binding instanceof Serializable)
    {
      return new SerializableValueBindingValueExpression(binding);
    }
    else
    {
      return new ValueBindingValueExpression(binding);      
    }
  }
  
  private ValueBindingValueExpression(ValueBinding binding)
  {
    if (binding == null)
      throw new NullPointerException();
    
    _binding = binding;
  }
  
  public ValueBinding getValueBinding()
  {
    return _binding;
  }
  
  @SuppressWarnings("deprecation")
  public Object getValue(ELContext elContext)
  {
    try
    {
      return _binding.getValue(FacesContext.getCurrentInstance());
    }
    // Convert EvaluationExceptions into ELExceptions
    catch (EvaluationException ee)
    {
      throw new ELException(ee.getMessage(), ee.getCause());
    }
  }

  @SuppressWarnings("deprecation")
  public void setValue(ELContext elContext, Object object)
  {
    try
    {
      _binding.setValue(FacesContext.getCurrentInstance(), object);
    }
    // Convert EvaluationExceptions into ELExceptions
    catch (EvaluationException ee)
    {
      throw new ELException(ee.getMessage(), ee.getCause());
    }
  }

  @SuppressWarnings("deprecation")
  public boolean isReadOnly(ELContext elContext)
  {
    try
    {
      return _binding.isReadOnly(FacesContext.getCurrentInstance());
    }
    // Convert EvaluationExceptions into ELExceptions
    catch (EvaluationException ee)
    {
      throw new ELException(ee.getMessage(), ee.getCause());
    }
  }

  @SuppressWarnings("deprecation")
  public Class<?> getType(ELContext elContext)
  {
    try
    {
      return _binding.getType(FacesContext.getCurrentInstance());
    }
    // Convert EvaluationExceptions into ELExceptions
    catch (EvaluationException ee)
    {
      throw new ELException(ee.getMessage(), ee.getCause());
    }
  }

  public Class<?> getExpectedType()
  {
    return null;
  }

  @SuppressWarnings("deprecation")
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
  
  public String toString()
  {
    return super.toString() + ", binding=" + _binding;
  }

  private static class SerializableValueBindingValueExpression extends ValueBindingValueExpression
                                                               implements Serializable                                                 
  {
    public SerializableValueBindingValueExpression(ValueBinding binding)
    {
      super(binding);
    }

    private static final long serialVersionUID = 1L;
  }
  
  private static class StateHolderValueBindingValueExpression extends ValueBindingValueExpression
                                                              implements StateHolder
  {
    public StateHolderValueBindingValueExpression(ValueBinding binding)
    {
      super(binding);
      _stateHolder = (StateHolder)binding;
    }
    
    public Object saveState(FacesContext facesContext)
    {
      return _stateHolder.saveState(facesContext);
    }

    public void restoreState(FacesContext facesContext, Object object)
    {
      _stateHolder.restoreState(facesContext, object);
    }

    public boolean isTransient()
    {
      return _stateHolder.isTransient();
    }

    public void setTransient(boolean b)
    {
      _stateHolder.setTransient(b);
    }
    
    private final StateHolder _stateHolder;
    private static final long serialVersionUID = 1L;
  }
  
  private static class SerializableStateHolderValueBindingValueExpression extends 
                                                             StateHolderValueBindingValueExpression
                                                              implements Serializable
  {
    public SerializableStateHolderValueBindingValueExpression(ValueBinding binding)
    {
      super(binding);
    }

    private static final long serialVersionUID = 1L;
  }

  private final ValueBinding _binding;

  private static final long serialVersionUID = 1L;
}
