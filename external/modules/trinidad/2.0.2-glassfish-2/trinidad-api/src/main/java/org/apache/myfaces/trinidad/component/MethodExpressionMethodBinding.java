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

import javax.el.ELException;
import javax.el.MethodExpression;
import javax.el.MethodInfo;

import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;

@Deprecated
class MethodExpressionMethodBinding extends MethodBinding
{
  public MethodExpressionMethodBinding(MethodExpression me)
  {
    _me = me;
  }

  public MethodExpression getMethodExpression()
  {
    return _me;
  }
  
  public Object invoke(FacesContext facesContext, Object[] params)
  {
    try
    {
      return _me.invoke(facesContext.getELContext(), params);
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }
  }

  public Class getType(FacesContext facesContext)
  {
    MethodInfo mi = _me.getMethodInfo(facesContext.getELContext());
    if (mi == null)
      return null;

    return mi.getReturnType();
  }

  public String getExpressionString()
  {
    return _me.getExpressionString();
  }
  
  private final MethodExpression _me;
}
