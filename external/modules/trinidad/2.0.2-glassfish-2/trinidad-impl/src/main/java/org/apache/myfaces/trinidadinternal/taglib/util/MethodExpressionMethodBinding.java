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
package org.apache.myfaces.trinidadinternal.taglib.util;

import java.io.Serializable;
import javax.el.ELException;
import javax.el.MethodExpression;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;

public class MethodExpressionMethodBinding extends MethodBinding
                                           implements Serializable
{
  public MethodExpressionMethodBinding(MethodExpression expression)
  {
    if (expression == null)
      throw new NullPointerException();

    _expression = expression;
  }

  public String getExpressionString()
  {
    return _expression.getExpressionString();
  }

  public Class getType(FacesContext context)
  {
    return _expression.getMethodInfo(context.getELContext()).getReturnType();
  }

  public Object invoke(FacesContext context, Object[] params) 
  {
    try
    {
      return _expression.invoke(context.getELContext(), params);
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }
  }

  private MethodExpression _expression;
  private static final long serialVersionUID = 1L;
}
