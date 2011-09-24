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

import java.io.Serializable;

import javax.el.ELContext;
import javax.el.ELException;
import javax.el.MethodExpression;
import javax.el.MethodInfo;

import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;


@Deprecated
class MethodBindingMethodExpression
  extends MethodExpression implements Serializable
{
  // TODO implement serialization correctly?
  public MethodBindingMethodExpression(MethodBinding binding)
  {
    _binding = binding;
  }
  
  public MethodBinding getMethodBinding()
  {
    return _binding;
  }

  public MethodInfo getMethodInfo(ELContext context)
  {
    Class<?> type = _binding.getType(FacesContext.getCurrentInstance());
    return new MethodInfo(null, type, null);
  }

  public Object invoke(ELContext elContext, Object[] params)
  {
    try
    {
      return _binding.invoke(FacesContext.getCurrentInstance(), params);
    }
    // Convert EvaluationExceptions into ELExceptions
    catch (EvaluationException ee)
    {
      throw new ELException(ee.getMessage(), ee.getCause());
    }
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
    if (!(o instanceof MethodBindingMethodExpression))
      return false;
      
    MethodBindingMethodExpression that = (MethodBindingMethodExpression) o;
    return that._binding.equals(_binding);
  }

  public int hashCode()
  {
    return _binding.hashCode();
  }

  private final MethodBinding _binding;
  private static final long serialVersionUID = 1L;
}
