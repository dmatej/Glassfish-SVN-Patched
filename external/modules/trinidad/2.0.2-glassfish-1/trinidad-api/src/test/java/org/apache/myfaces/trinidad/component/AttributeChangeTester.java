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

import javax.el.ELContext;
import javax.el.MethodExpression;
import javax.el.MethodInfo;

import org.apache.myfaces.trinidad.event.AttributeChangeEvent;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;

/**
 * Utility class for testing out AttributeChange events.
 */
public class AttributeChangeTester extends MethodExpression
 implements AttributeChangeListener
{
  public AttributeChangeTester()
  {
  }

  public void processAttributeChange(AttributeChangeEvent event)
  {
    if (_methodBindingCalled)
      throw new IllegalStateException("Method binding called before listener");
    _listenerCalled = true;
  }

  public MethodInfo getMethodInfo(ELContext context)
  {
    return null;
  }

  @Override
  public Object invoke(ELContext context, Object params[])
  {
    if (params.length != 1)
      throw new IllegalStateException("Params not of length 1");
    if (params[0] == null)
      throw new IllegalStateException("Event is null");
    if (!(params[0] instanceof AttributeChangeEvent))
      throw new IllegalStateException("Event isn't an AttributeChangeEvent");

    _methodBindingCalled = true;
    return null;
  }

  public String getExpressionString()
  {
    return null;
  }

  public boolean isLiteralText()
  {
    return false;
  }

  public int hashCode()
  {
    return 0;
  }

  public boolean equals(Object o)
  {
    return o == this;
  }

  public void verify()
  {
    if (!_methodBindingCalled)
      throw new IllegalStateException("Method binding never called");

    if (!_listenerCalled)
      throw new IllegalStateException("Listener never called");
  }

  private boolean _methodBindingCalled;
  private boolean _listenerCalled;
}
