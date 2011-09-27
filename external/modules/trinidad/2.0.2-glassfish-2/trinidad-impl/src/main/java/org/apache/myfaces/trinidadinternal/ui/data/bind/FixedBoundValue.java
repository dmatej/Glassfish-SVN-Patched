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
 * BoundValue implementation that always returns the value passed into the
 * constructor.  This class is typically used in debugging and performance
 * testing and will rarely be used in production code.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/FixedBoundValue.java#0 $) $Date: 10-nov-2005.18:56:39 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class FixedBoundValue implements BoundValue
{
  /**
   * BoundValue that will always return Boolean.FALSE.
   */
  public static final FixedBoundValue FALSE_VALUE =
                                             new FixedBoundValue(Boolean.FALSE);
  /**
   * BoundValue that will always return Boolean.TRUE.
   */
  public static final FixedBoundValue TRUE_VALUE =
                                             new FixedBoundValue(Boolean.TRUE);

  /**
   * BoundValue that will always return null.
   */
  public static final FixedBoundValue NULL_VALUE =
                                             new FixedBoundValue(null);

  /**
   * Create a BoundValue that always returns the value passed into the
   * constructor.
   * <p>
   * @param value The value to return from <code>getValue</code>.
   * @see #getValue
   */
  public FixedBoundValue(
    Object value
    )
  {
    _value = value;
  }
  
  
  /**
   * Called to retrieve a value based on the current rendering
   * context.  FixedBoundValues always return the value
   * passed into the constructor.
   * <p>
   * @param context the rendering context
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    return _value;
  }
  
  private Object _value;
}
