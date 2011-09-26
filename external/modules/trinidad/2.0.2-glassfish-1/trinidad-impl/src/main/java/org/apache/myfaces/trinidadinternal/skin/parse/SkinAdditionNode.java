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
 * Object which represents a single &lt;skin-addition&gt; element in trinidad-skins.xml.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SkinAdditionNode implements Comparable<SkinAdditionNode>
{

  /**
   * 
   */
  public SkinAdditionNode (
    String skinId,
    String styleSheetName,
    String resourceBundleName,
    String translationSourceExpression
    )
  {
    _styleSheetName = styleSheetName;
    _skinId = skinId;
    _resourceBundleName = resourceBundleName;
    _translationSourceExpression = translationSourceExpression;
  }
  
  public String getSkinId()
  {
    return _skinId;
  }  
  
  public void setSkinId(String id)
  {
    _skinId = id;
  }
  
  public String getStyleSheetName()
  {
    return _styleSheetName;
  } 
  
  public void setStyleSheetName(String ssName)
  {
    _styleSheetName = ssName;
  }
  
  /**
   * Returns the resource bundle name. The actual ResourceBundle will
   * be resolved by the Skin by using the LocaleContext. 
   */    
  public String getResourceBundleName()
  {
    return _resourceBundleName;
  } 
  
  public void setResourceBundleName(String rbName)
  {
    _resourceBundleName = rbName;
  }
  
  /**
   * Returns the translationSource value expression String. This could
   * resolve to a Map or a ResourceBundle. 
   */  
  public String getTranslationSourceExpression()
  {
    return _translationSourceExpression;
  } 
  
  // Sort by the name of the stylesheet
  public int compareTo(SkinAdditionNode node)
  {
    return getStyleSheetName().compareTo(node.getStyleSheetName());
  }
  
  private String _skinId;
  private String _styleSheetName;
  private String _resourceBundleName;
  private String _translationSourceExpression;

}
