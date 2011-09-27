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
package org.apache.myfaces.trinidadinternal.binding;

import java.io.Serializable;

import javax.el.ValueExpression;
import javax.el.ELContext;
import javax.el.PropertyNotWritableException;

import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;


/**
 * ValueBinding that wraps a second value binding to extract the
 * access key.
 *
 */
public class AccessKeyBinding extends ValueExpression implements Serializable
{
  /**
   * Constructor purely for serialization.
   */
  public AccessKeyBinding()
  {
  }

  public AccessKeyBinding(ValueExpression expr)
  {
    _base = expr;
  }


  @Override
  public Object getValue(ELContext context)
  {
    Object o = _base.getValue(context);
    if (o == null)
      return null;

    String text = o.toString();
    int accessKeyIndex = StringUtils.getMnemonicIndex(text);
    if (accessKeyIndex == StringUtils.MNEMONIC_INDEX_NONE)
      return null;

    return Character.valueOf(text.charAt(accessKeyIndex + 1));
  }

  @Override
  public void setValue(ELContext context, Object value)
  {
    throw new PropertyNotWritableException();
  }

  @Override
  public Class<?> getType(ELContext context)
  {
    return Character.class;
  }

  @Override
  public Class<?> getExpectedType()
  {
    return Character.class;
  }

  @Override
  public boolean isReadOnly(ELContext context)
  {
    return true;
  }


  @Override
  public boolean isLiteralText()
  {
    return false;
  }

  @Override
  public String getExpressionString()
  {
    return null;
  }

  public int hashCode()
  {
    return 0;
  }

  public boolean equals(Object o)
  {
    return (o == this);
  }

  private ValueExpression _base;
  private static final long serialVersionUID = 1L;
}