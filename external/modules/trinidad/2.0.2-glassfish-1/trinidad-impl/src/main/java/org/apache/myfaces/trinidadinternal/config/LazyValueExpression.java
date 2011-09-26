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
package org.apache.myfaces.trinidadinternal.config;

import javax.el.ELContext;
import javax.el.ELResolver;
import javax.el.ExpressionFactory;
import javax.el.FunctionMapper;
import javax.el.ValueExpression;
import javax.el.VariableMapper;

import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;
import javax.faces.context.FacesContext;

/**
 * A ValueExpression class that lazily parses the underlying EL expression
 * (in case the Application object is not yet available).  Unfortunately,
 * this implementation means that errors in the syntax of the EL
 * expression won't get detected until use.
 *
 */
public class LazyValueExpression extends ValueExpression
{

  /**
   * Create a ValueExpression
   */
  static public ValueExpression createValueExpression(
    String expression,
    Class<?> expectedType)
  {
    // Try to create the ValueExpression from the Application object
    // This will return null if the ApplicationFactory or Application
    // objects are null.
    ValueExpression valueExpression=
      _createValueExpressionFromApplication(expression, expectedType);

    if (valueExpression != null)
      return valueExpression;

    // We couldn't create a regular ValueExpression, so create a
    // LazyValueExpression. This will try to create a regular
    // ValueExpression later when a 'get' method is called.
    return new LazyValueExpression(expression, expectedType);
  }

  @Override
  public Object getValue(ELContext context)
  {
    return _getValueExpression().getValue(context);
  }

  @Override
  public void setValue(ELContext context, java.lang.Object value)
  {
    _getValueExpression().setValue(context, value);
  }

  @Override
  public boolean isReadOnly(ELContext context)
  {
    return _getValueExpression().isReadOnly(context);
  }

  @Override
  public java.lang.Class<?> getType(ELContext context)
  {
    return _getValueExpression().getType(context);
  }

  @Override
  public java.lang.Class<?> getExpectedType()
  {
    return _expectedType;
  }

  @Override
  public String getExpressionString()
  {
    return _expression;
  }

  @Override
  public boolean isLiteralText()
  {
    return _getValueExpression().isLiteralText();
  }

  @Override
  public boolean equals(Object o)
  {
    return _getValueExpression().equals(o);
  }

  @Override
  public int hashCode ()
  {
    return _getValueExpression().hashCode();
  }

  private LazyValueExpression(
    String expression,
    Class<?> expectedType)
  {
    _expression = expression;
    _expectedType = expectedType;

  }

  // Get the ELContext off of the FacesContext, if the FacesContext is not
  // null. Otherwise, create a dummy ELContext and return that.
  private static ELContext _getELContext(Application application)
  {
    FacesContext fContext = FacesContext.getCurrentInstance();

    if (fContext != null)
    {
      return fContext.getELContext();
    }
    else
    {
      // use a dummy ELContext if FacesContext is null
      return new MockELContext(application);
    }
  }

  // This is called from the LazyValueExpression's 'get' methods.
  // It will try to create a ValueExpression from the Application object
  // if it hasn't done so already.
  private ValueExpression _getValueExpression()
  {

    if (_valueExpression == null)
    {
      _valueExpression =
        _createValueExpressionFromApplication(_expression, _expectedType);
    }

    return _valueExpression;
  }

  // Create a ValueExpression object from the Application object, if the
  // ApplicationFactory and Application objects exist, and return it.
  // Otherwise, return null.
  private static ValueExpression _createValueExpressionFromApplication(
    String expression,
    Class<?> expectedType)
  {
    ApplicationFactory factory = (ApplicationFactory)
      FactoryFinder.getFactory(FactoryFinder.APPLICATION_FACTORY);
    if (factory != null)
    {
      Application application = factory.getApplication();
      if (application != null)
      {
        ELContext elContext = _getELContext(application);

        ExpressionFactory expressionFactory = application.getExpressionFactory();
        if (expressionFactory != null)
        {
          return expressionFactory.createValueExpression(elContext, expression, expectedType);
        }
      }
    }

    return null;
  }

  // This is used to mock up a dummy ELContext to pass into createValueExpression
  // if the FacesContext is null and we can't get FacesContext.getELContext.
  private static class MockELContext extends ELContext
  {
    public MockELContext(Application application)
    {
      _resolver = application.getELResolver();
    }

    @Override
    public ELResolver getELResolver()
    {
      return _resolver;
    }

    @Override
    public FunctionMapper getFunctionMapper()
    {
      return null;
    }

    @Override
    public VariableMapper getVariableMapper()
    {
      return null;
    }

    private final ELResolver _resolver;
  }


  private ValueExpression _valueExpression;
  private final String    _expression;
  private final Class<?>  _expectedType;

  private static final long serialVersionUID = 1L;
}
