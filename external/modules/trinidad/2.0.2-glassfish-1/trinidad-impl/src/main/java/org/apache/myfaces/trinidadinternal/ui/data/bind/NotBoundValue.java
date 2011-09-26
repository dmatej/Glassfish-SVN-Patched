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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * BoundValue that returns <code>Boolean.FALSE</code> if the passed
 * in BoundValue is <code>Boolean.TRUE</code> and returns
 * <code>Boolean.TRUE</code> otherwise.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/NotBoundValue.java#0 $) $Date: 10-nov-2005.18:56:40 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NotBoundValue implements BoundValue
{
  /**
   * Creates a NotBoundValue.  A Null parameter are treated as if its
   * value returns <code>Boolean.FALSE</code>.
   * <p>
   * @param value BoundValue to NOT the result of
   */
  public NotBoundValue(
    BoundValue value
    )
  {
    if (value == null)
      value = FixedBoundValue.FALSE_VALUE;
   
    _value = value;
  }

  public Object getValue(
    UIXRenderingContext context
    )
  {    
    if (Boolean.TRUE.equals(_value.getValue(context)))
    {
      return Boolean.FALSE;
    }
    else
    {
      return Boolean.TRUE;
    }
  }

  private BoundValue _value;
}
