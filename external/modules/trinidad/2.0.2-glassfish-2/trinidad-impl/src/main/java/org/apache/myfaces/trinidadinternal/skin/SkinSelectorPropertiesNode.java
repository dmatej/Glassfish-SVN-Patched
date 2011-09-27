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
package org.apache.myfaces.trinidadinternal.skin;

import java.util.List;

import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;

/**
 * Stores each selector and its properties that
 * was found in the css file. This is similar to StyleNode
 * for the xss files. 
 * We turn this information into a StyleSheetDocument in 
 * SkinStyleSheetParserUtils.java
 * We might want to add things like imports, so they can at least import other
 * css files if they care to. ??
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinSelectorPropertiesNode.java#0 $) $Date: 10-nov-2005.18:58:58 $

 */
class SkinSelectorPropertiesNode
{

    SkinSelectorPropertiesNode(
      String selectorName,
      List<PropertyNode> propertyNodes
      )
    {
      _selectorName  = selectorName;
      _propertyNodes = propertyNodes;
    }
   
    public String getSelectorName()
    {
      return _selectorName;
    }
    public List<PropertyNode> getPropertyNodes()
    {
      return _propertyNodes;
    }
  
    private String _selectorName;
    private List<PropertyNode> _propertyNodes;

}
