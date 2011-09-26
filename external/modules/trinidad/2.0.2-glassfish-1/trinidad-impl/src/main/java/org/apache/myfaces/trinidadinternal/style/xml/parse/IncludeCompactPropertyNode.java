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

import java.util.Collection;
import java.util.List;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * The includeCompactProperty node is a data structure to store this syntax:
 * border: 1px solid -tr-property-ref(".AFDarkColor:alias",color);
 * which should resolve to border: 1px solid #cccccc; (if #cccccc is .AFDarkColor:alias's color.
 * This is similar to includeProperty element, but it is used on compact css property values.
 * An IncludePropertyNode is a data structure to store this syntax:
 * color: -tr-property-ref(".AFDarkColor:alias",color);
 * includeProperty element is used to include a single property of one style 
 * within another style. Thus, the includeProperty element is very similar to the 
 * includeStyle element. The only difference is that includeStyle includes all properties 
 * of the referenced style, whereas includeProperty includes only a single property.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/IncludeCompactPropertyNode.java#0 $) $Date: 10-nov-2005.18:58:07 $
 */
public class IncludeCompactPropertyNode
{

  /**
   * Creates an IncludeCompactPropertyNode.
   * If we have border: 1px solid -tr-property-ref(".AFDarkColor:alias",color); in the css file,
   * then localPropertyName == border, propertyValues == {"1px solid"}, and
   * includePropertyNodeList will contain the -tr-property-ref(".AFDarkColor:alias",color);
   * @param includePropertyNodeList
   * @param propertyValues
   * @param localPropertyName
   */
  public IncludeCompactPropertyNode(
    String                    propertyValues,
    List<IncludePropertyNode> includePropertyNodeList,
    String                    localPropertyName)
  {
    // The caller of this constructor must have all these values filled out.
    if (propertyValues == null)
      throw new IllegalArgumentException();
    if (includePropertyNodeList == null)
      throw new IllegalArgumentException();
    if (localPropertyName == null)
      throw new IllegalArgumentException();
    
    _propertyValues = propertyValues;
    _includePropertyNodeList = includePropertyNodeList;
    _localPropertyName = localPropertyName;

  }

  /**
   * Returns the name of the style to include.
   */
  public String getLocalPropertyName()
  {
    return _localPropertyName;
  }
  
  /**
   * Returns the name of the style to include.
   */
  public String getPropertyValues()
  {
    return _propertyValues;
  }
  
  public Collection<IncludePropertyNode> getIncludedProperties()
  {
    return _includePropertyNodeList;
  }

  @Override 
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof IncludeCompactPropertyNode))
      return false;
     
    // obj at this point must be an IncludeCompactPropertyNode
    IncludeCompactPropertyNode test = (IncludeCompactPropertyNode)obj;
    return
      (_localPropertyName == test._localPropertyName 
        || (_localPropertyName != null && _localPropertyName.equals(test._localPropertyName))) 
      && (_propertyValues == test._propertyValues 
        || (_propertyValues != null && _propertyValues.equals(test._propertyValues)))
      && (_includePropertyNodeList == test._includePropertyNodeList 
        || (_includePropertyNodeList != null 
            && _includePropertyNodeList.equals(test._includePropertyNodeList)));       
  }
  
  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _localPropertyName) ? 0 : _localPropertyName.hashCode());
    hash = 37*hash + ((null == _propertyValues) ? 0 : _propertyValues.hashCode());
    hash = 37*hash + ((null == _includePropertyNodeList) ? 0 : _includePropertyNodeList.hashCode());

    return hash; 
  }  
  
  
  @Override
  // TODO jmwOctober. test this
  public String toString()
  {
    return 
      "[propertyValues="   + _propertyValues   + ", " +
      "includePropertyNodeList=" + _includePropertyNodeList + "]";
  }

  private final String                    _propertyValues;
  private final List<IncludePropertyNode> _includePropertyNodeList;
  private final String                    _localPropertyName;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    IncludeCompactPropertyNode.class);
}
