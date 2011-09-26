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
package org.apache.myfaces.trinidadinternal.util;


/**
 * Used for SelectRangeChoiceBar to allow the app developer to customize
 * the labels to not use numbers, but to use the data model.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/Range.java#0 $) $Date: 10-nov-2005.18:49:12 $
 */
public final class Range
{
  public Object getStart()
  {
    return _start;
  }
  
  public void setStart(Object start)
  {
    _start = start;
  } 
  
  public Object getEnd()
  {
    return _end;
  }
  
  public void setEnd(Object end)
  {
    _end = end;
  }  
  
  private Object _start;
  private Object _end;
}
