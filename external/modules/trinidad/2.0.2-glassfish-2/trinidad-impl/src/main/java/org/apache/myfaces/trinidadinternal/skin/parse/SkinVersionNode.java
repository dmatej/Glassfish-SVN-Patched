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
package org.apache.myfaces.trinidadinternal.skin.parse;

/**
 * Object which represents a single &lt;version&gt; element in trinidad-skins.xml.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 */
public class SkinVersionNode
{

  /**
   * 
   */
  public SkinVersionNode (
    String name,
    boolean defaultVersion
    )
  {
    _name = name;
    _default = defaultVersion;
  }
  
  public String getName()
  {
    return _name;
  }  
  
  /**
   * Returns the resource bundle name. The actual ResourceBundle will
   * be resolved by the Skin by using the LocaleContext. 
   */    
  public boolean isDefault()
  {
    return _default;
  } 
   
  private final String  _name;
  private final boolean _default;

}
