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
 * BoundValue that wraps another BoundValue and returns 
 * <code>Boolean.TRUE</code>
 * if converting its result to a String results in a non-null
 * and non-empty String;  otherwise, returns <CODE>Boolean.FALSE</CODE>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/StringExistsBoundValue.java#0 $) $Date: 10-nov-2005.18:56:42 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class StringExistsBoundValue implements BoundValue
{
  /**
   * Creates a StringExistsBoundValue.
   * @param wrapped the BoundValue to convert into a String.
   */
  public StringExistsBoundValue(
    BoundValue wrapped
    )
  {
    if (wrapped == null)
      throw new IllegalArgumentException();
      
    _wrapped = new ToStringBoundValue(wrapped);
  }

  public Object getValue(
    UIXRenderingContext context
    )
  {
    Object value = _wrapped.getValue(context);
    
    if ((value != null) &&
        ((value.toString()).length() != 0))
      return Boolean.TRUE;
    
    return Boolean.FALSE;
  }
  
  private BoundValue _wrapped;
}
