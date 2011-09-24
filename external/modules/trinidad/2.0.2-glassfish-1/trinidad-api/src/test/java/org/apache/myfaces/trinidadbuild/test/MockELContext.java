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
package org.apache.myfaces.trinidadbuild.test;

import javax.el.ELContext;
import javax.el.ELResolver;
import javax.el.FunctionMapper;
import javax.el.VariableMapper;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.faces.application.Application;

public class MockELContext extends ELContext
{
  public MockELContext(Application application)
  {
    // =-=AdamWiner:  this is not right;  the Application should
    // be responsible for vending the ELResolver. However, replacing
    // the Application is harder... so for now...
    _resolver = new MockELResolver(application.getVariableResolver(),
                                   application.getPropertyResolver());
  }

  public Object getContext(Class key)
  {
    return _contexts.get(key);
  }
  
  public ELResolver getELResolver() 
  {
    return _resolver;
  }

  public FunctionMapper getFunctionMapper()
  {
    return null;
  }

  public Locale getLocale()
  {
    return _locale;
  }

  public VariableMapper getVariableMapper()
  {
    throw new UnsupportedOperationException();
  }

  public boolean isPropertyResolved()
  {
    return _propertyResolved;
  }

  public void putContext(Class key, Object contextObject)
  {
    _contexts.put(key, contextObject);
  }

  public void setLocale(Locale locale) 
  {
    _locale = locale;
  }

  public void setPropertyResolved(boolean resolved)
  {
    _propertyResolved = resolved;
  }

  private boolean _propertyResolved;
  private Locale  _locale;
  private final Map<Class<?>, Object> _contexts = new HashMap<Class<?>, Object>();
  private final ELResolver _resolver;
}