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

import javax.el.ELException;
import javax.el.ValueExpression;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.ValueBinding;

public class ValueExpressionValueBinding extends ValueBinding
{
  /**
   * Given a ValueExpression <code>ve</code>, return a ValueBinding.
   * The returned ValueBinding will implement StateHolder and Serializable interfaces if
   * <code>ve</code> implements these interfaces.
   * @param ve  The ValueExpression
   * @return a ValueBinding equivalent to the ValueExpression
   */
  public static ValueBinding getValueBinding(ValueExpression ve)
  {
    // if we previously wrapped a ValueBinding, unwrap it and return it, otherwise create the
    // correct subclass of ValueBinding
    if (ve instanceof ValueBindingValueExpression)
      return ((ValueBindingValueExpression)ve).getValueBinding();
    else if (ve instanceof StateHolder)
    {
      if (ve instanceof Serializable)
        return new SerializableStateHolderValueExpressionValueBinding(ve);
      else
        return new StateHolderValueExpressionValueBinding(ve);      
    }
    else if (ve instanceof Serializable)
    {
      return new SerializableValueExpressionValueBinding(ve);
    }
    else
    {
      return new ValueExpressionValueBinding(ve);      
    }    
  }
  
  @SuppressWarnings("deprecation")
  private ValueExpressionValueBinding(ValueExpression ve)
  {
    if (ve == null)
      throw new NullPointerException();
    
    _ve = ve;
  }

  public ValueExpression getValueExpression()
  {
    return _ve;
  }
  
  @SuppressWarnings("deprecation")
  public Object getValue(FacesContext facesContext)
  {
    try
    {
      return _ve.getValue(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  @SuppressWarnings("deprecation")
  public void setValue(FacesContext facesContext, Object object)
  {
    try
    {
      _ve.setValue(facesContext.getELContext(), object);
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  @SuppressWarnings("deprecation")
  public boolean isReadOnly(FacesContext facesContext)
  {
    try
    {
      return _ve.isReadOnly(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  @SuppressWarnings("deprecation")
  public Class getType(FacesContext facesContext)
  {
    try
    {
      return _ve.getType(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }
  }

  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    
    if (!(o instanceof ValueExpressionValueBinding))
      return false;
      
    ValueExpressionValueBinding that = (ValueExpressionValueBinding) o;
    return that._ve.equals(_ve);
  }

  public int hashCode()
  {
    return _ve.hashCode();
  }
  
  public String toString()
  {
    return super.toString() + ", expression=" + _ve;
  }

  private static class SerializableValueExpressionValueBinding extends ValueExpressionValueBinding
                                                               implements Serializable                                                 
  {
    public SerializableValueExpressionValueBinding(ValueExpression ve)
    {
      super(ve);
    }
    private static final long serialVersionUID = 1L;
  }
  
  private static class StateHolderValueExpressionValueBinding extends ValueExpressionValueBinding
                                                              implements StateHolder
  {
    public StateHolderValueExpressionValueBinding(ValueExpression ve)
    {
      super(ve);
      _stateHolder = (StateHolder)ve;
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
  
  private static class SerializableStateHolderValueExpressionValueBinding extends 
                                                             StateHolderValueExpressionValueBinding
                                                              implements Serializable
  {
    public SerializableStateHolderValueExpressionValueBinding(ValueExpression ve)
    {
      super(ve);
    }
    private static final long serialVersionUID = 1L;
  }

  private final ValueExpression _ve;
}
