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
package org.apache.myfaces.trinidadinternal.share.expl;

import java.lang.reflect.InvocationTargetException;

/**
 * Function is the abstraction for an EL Function
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/expl/Function.java#0 $) $Date: 10-nov-2005.19:00:13 $
 */
public abstract class Function
{
  public abstract Object invoke(Object instance, Object[] args)
    throws IllegalAccessException, IllegalArgumentException,
           InvocationTargetException;

  public abstract Class[] getParameterTypes();

  public abstract Class<?> getReturnType();

  // package private constructor:
  Function()
  {
  }
}
