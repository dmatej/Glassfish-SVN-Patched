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
package org.apache.myfaces.trinidad.util;

import java.io.Serializable;

/**
 * Holds an unserializable value in a transient variable so that
 * unserializable Objects can be safely placed in distributed Sessions
 * without blowing up.  Users should expect that getValue() will return
 * <code>null</code> if the class has been serialized/de-serialized
 */ 
public final class TransientHolder<T> implements Serializable 
{ 
  private TransientHolder(T value) 
  { 
    _value = value; 
  } 
    
  public static <T> TransientHolder<T> newTransientHolder(T held)
  {
    return new TransientHolder<T>(held);
  }
  
  public T getValue() 
  { 
    return _value; 
  } 
 
  private final transient T _value; 
  private static final long serialVersionUID = 1L; 
} 
