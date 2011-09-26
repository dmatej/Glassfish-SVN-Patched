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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

/**
 * Private implementation of ColorNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/ColorNode.java#0 $) $Date: 10-nov-2005.18:58:01 $
 */
public class ColorNode
{
  /**
   * Creates a ColorNode with the specified name and value
   */
  public ColorNode(String name, String value)
  {
    _name = name;
    _value = value;
  }

  /**
   * Implementation of ColorNode.getName().
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Implementation of ColorNode.getValue().
   */
  public String getValue()
  {
    return _value;
  }

  private String _name;
  private String _value;
}
