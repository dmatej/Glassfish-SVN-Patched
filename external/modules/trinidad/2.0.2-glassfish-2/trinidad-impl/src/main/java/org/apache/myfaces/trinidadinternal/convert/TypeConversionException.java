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
package org.apache.myfaces.trinidadinternal.convert;

public class TypeConversionException extends RuntimeException
{
  public TypeConversionException(Object source, Class<?> targetType, Throwable error)
  {
    super(_getMessage(source, targetType), error);
  }
  
  public TypeConversionException(Object source, Class<?> targetType)
  {
    super(_getMessage(source, targetType));
  }
  
  private static String _getMessage(Object source, Class<?> targetType)
  {
    return "Could not convert instance:"+source +
      " of type:"+source.getClass()+" into type:"+targetType;
  }

  private static final long serialVersionUID = 1L;
}