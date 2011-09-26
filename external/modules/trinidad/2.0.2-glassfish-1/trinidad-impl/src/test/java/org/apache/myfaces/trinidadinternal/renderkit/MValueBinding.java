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

import javax.faces.context.FacesContext;
import javax.faces.el.PropertyResolver;
import javax.faces.el.ValueBinding;
import javax.faces.el.VariableResolver;

public class MValueBinding extends ValueBinding
{
  public MValueBinding(String expression)
  {
    _expression = expression.substring(2, expression.length() - 1);
  }

  @Override
  public Object getValue(FacesContext context)
  {
    String varName;
    int period = _expression.indexOf('.');
    if (period < 0)
      varName = _expression;
    else
      varName = _expression.substring(0, period);

    VariableResolver vr = context.getApplication().getVariableResolver();
    Object var =  vr.resolveVariable(context, varName);
    if (period < 0)
      return var;

    return _resolveProperty(context, var, _expression.substring(period + 1));
  }

  private Object _resolveProperty(FacesContext context, Object var, String expr)
  {    
    String propertyName;
    int period = expr.indexOf('.');
    if (period < 0)
      propertyName = expr;
    else
      propertyName = expr.substring(0, period);

    PropertyResolver pr = context.getApplication().getPropertyResolver();
    var =  pr.getValue(var, propertyName);

    if (period < 0)
      return var;
    return _resolveProperty(context, var, expr.substring(period + 1));
  }
  
  @Override
  public void setValue(FacesContext context, Object value)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");    
  }

  @Override
  public boolean isReadOnly(FacesContext context)
  {
    return true;
  }

  @Override
  public Class<?> getType(FacesContext context)
  {
    return Object.class;
  }

  @Override
  public String getExpressionString()
  {
    return "#{" + _expression + "}";
  }
  
  private final String _expression;
}
