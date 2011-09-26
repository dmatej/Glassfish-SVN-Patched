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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;


/**
 * Private implementation of StyleNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleNode.java#0 $) $Date: 10-nov-2005.18:58:11 $
 */
public class StyleNode
{


  /**
   * Creates a Style with the specified properties
   */
  public StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    PropertyNode[]         skinProperties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties,
    IncludeCompactPropertyNode[] includedCompactProperties,
    Set<String>            inhibitedProperties
    )
  {
    this(name,
         selector,
         properties,
         skinProperties,
         includedStyles,
         includedProperties,
         includedCompactProperties,
         inhibitedProperties,
         false);
  }
  
  // Constructor which also specifies allows resetProperties
  // flag to be specified.
  // skinProperties are server-side properties like -tr-show-last-item: true
  public StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    PropertyNode[]         skinProperties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties,
    IncludeCompactPropertyNode[] includedCompactProperties,
    Set<String>            inhibitedProperties,
    boolean                resetProperties    
    )
  {
    // Initialize _name, _selector, _resetProperties
    // ---------------------------------------------
    _name = name;
    _selector = selector;
    _resetProperties = resetProperties;


    // Structure copying:
    // We have quite a lot of StyleNodes floating around;  so making
    // them lightweight is highly desirable.  Consequently, use
    // the lightest weight lists possible:  emptyList() if null
    // or an empty array, and singletonList() if it's a one-element array

    // Initialize _properties
    // ------------------------------
    if (properties == null || (properties.length == 0))
      _properties = Collections.emptyList();
    else if (properties.length == 1)
      _properties = Collections.singletonList(properties[0]);
    else
      _properties = Collections.unmodifiableList(Arrays.asList(properties));

    // Initialize _includedStyles
    // ------------------------------
    if ((includedStyles == null) || (includedStyles.length == 0))
      _includedStyles = Collections.emptyList();
    else if (includedStyles.length == 1)
      _includedStyles = Collections.singletonList(includedStyles[0]);
    else
      _includedStyles = Collections.unmodifiableList(Arrays.asList(includedStyles));


    // Initialize _includedProperties
    // ------------------------------
    if ((includedProperties == null) || (includedProperties.length == 0))
      _includedProperties = Collections.emptyList();
    else if (includedProperties.length == 1)
      _includedProperties = Collections.singletonList(includedProperties[0]);
    else
      _includedProperties = Collections.unmodifiableList(Arrays.asList(includedProperties));

    // Initialize _includedCompactProperties
    // ------------------------------
    if ((includedCompactProperties == null) || (includedCompactProperties.length == 0))
      _includedCompactProperties = Collections.emptyList();
    else if (includedCompactProperties.length == 1)
      _includedCompactProperties = Collections.singletonList(includedCompactProperties[0]);
    else
      _includedCompactProperties = 
        Collections.unmodifiableList(Arrays.asList(includedCompactProperties));
    
    // Initialize _skinProperties. These are server-side skin properties, like -tr-show-last-item,
    // as opposed to client side, like background-color or color. client side properties get
    // written to css file, but server-side skin properties do not.
    // ------------------------------
    if ((skinProperties == null) || (skinProperties.length == 0))
      _skinProperties = Collections.emptyList();
    else if (skinProperties.length == 1)
      _skinProperties = Collections.singletonList(skinProperties[0]);
    else
      _skinProperties = Collections.unmodifiableList(Arrays.asList(skinProperties));
    
    // Initialize _inhibitAll and _inhibitedProperties
    // -----------------------------------------------    
    boolean inhibitAll = false;
    // Convert inhibitedProperties Set to an unmodifiableList

    if ((inhibitedProperties != null) && !inhibitedProperties.isEmpty())
    {
      List<String> inhibitedPropertiesList = new ArrayList<String>(inhibitedProperties.size());
      for(String property : inhibitedProperties)
      {
        if(_INHIBIT_ALL_VALUE.equalsIgnoreCase(property))
        { // Case insensitivity for "all" value
        
          // we don't want to break when we find inhibitAll because all the inhibitedProperties
          // are needed
          // e.g., 
          /* this should end up with  .foo {padding: 8px} */
          // 
          // This should first inhibit all inherited styles. Then everything else
          // should be included.
          // .foo {
          //   -tr-inhibit: all;
          //   padding: 8px;
          //This should inhibit the background-color that is inherited and/or included,
          //like in .AFLightAccentBackground:alias
          //The order of this does not matter.
          //   -tr-inhibit: background-color;
          //   -tr-rule-ref: selector(".AFLightAccentBackground:alias");
          // }
          inhibitAll = true;
        }
        else
        {
          inhibitedPropertiesList.add(property);
        }
      }

      if (inhibitedPropertiesList.size() == 1)
        _inhibitedProperties = Collections.singletonList(inhibitedPropertiesList.get(0));
      else
        _inhibitedProperties = Collections.unmodifiableList(inhibitedPropertiesList);

    }
    else
      _inhibitedProperties = Collections.emptyList();
    _inhibitAll = inhibitAll;

  }

  /**
   * Implementation of StyleNode.getName().
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Implementation of StyleNode.getSelector().
   */
  public String getSelector()
  {
    return _selector;
  }

  /**
   * Returns true if the style node has no properties. 
   */
  public boolean isEmpty()
  {
    if (!_properties.isEmpty())
      return false;
    if (!_includedStyles.isEmpty())
      return false;
    if (!_includedProperties.isEmpty())
      return false;
    return true;
  }

  /**
   * Implementation of StyleNode.getProperties().
   */
  public Collection<PropertyNode> getProperties()
  {
    return _properties;
  }

  /**
   * Implementation of StyleNode.getSkinProperties().
   */
  public Collection<PropertyNode> getSkinProperties()
  {
    return _skinProperties;
  }
  

  /**
   * Implementation of StyleNode.getIncludedCompactProperties().
   */
  public Collection<IncludeCompactPropertyNode> getIncludedCompactProperties()
  {
    return _includedCompactProperties;
  }  
  
  /**
   * Return a Collection of IncludeStyleNode objects for the StyleNode. 
   * This method will return a Collections.emptyList this StyleNode does
   * not have any IncludeStyleNode.
   * 
   * @return a Collection of the IncludeStyleNode Objects, a Collections.emptyList 
   *         if there are no IncludeStyleNode Objects in this StyleNode.
   */
  public Collection<IncludeStyleNode> getIncludedStyles()
  {
    return _includedStyles;
  }

  /**
   * Return a Collection of IncludePropertyNode objects for the StyleNode. 
   * This method will return a Collections.emptyList this StyleNode does
   * not have any IncludePropertyNodes.
   * 
   * @return a Collection of the IncludePropertyNode Objects, a Collections.emptyList 
   *         if there are no IncludePropertyNode Objects in this StyleNode.
   */
  public Collection<IncludePropertyNode> getIncludedProperties()
  {
    return _includedProperties;
  }
  
  /**
   * Gets the properties specified by this node's parent that should be
   * ignored. This method will return a Collections.emptyList if 
   * {@link #isInhibitingAll()} returns <code>true</code>
   * 
   * @return a Collection of the properties that should be ignored, an 
   *         Collection.emptyList if all properties should be.
   */
  public Collection<String> getInhibitedProperties()
  {
    return _inhibitedProperties;
  }
  
  /**
   * Determines if this node inhibits all of its inherited properties.
   * 
   * @return <code>true</code> if this node ignores all properties defined 
   *         by its parent, <code>false</code> otherwise.
   */
  public boolean isInhibitingAll()
  {
    return _inhibitAll;
  }
  
  @Override  
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof StyleNode))
      return false;
      
    // obj at this point must be a StyleNode
    StyleNode test = (StyleNode)obj;
    // look for equality in the order of what is most likely to be different and what
    // is easiest to check for.
    return
      (_name == test._name || (_name != null && _name.equals(test._name))) &&
      (_selector == test._selector || (_selector != null && _selector.equals(test._selector))) &&
      (_resetProperties == test._resetProperties) &&
      (_inhibitAll == test._inhibitAll) &&
      (_inhibitedProperties.equals(test._inhibitedProperties)) &&
      (_includedStyles.equals(test._includedStyles)) &&
      (_includedProperties.equals(test._includedProperties)) &&
      (_includedCompactProperties.equals(test._includedCompactProperties)) &&
      (_properties.equals(test._properties)) &&
      (_skinProperties.equals(test._skinProperties));
  }
  
  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _name) ? 0 : _name.hashCode());
    hash = 37*hash + ((null == _selector) ? 0 : _selector.hashCode());
    hash = 37*hash + (_resetProperties ? 0 : 1);
    hash = 37*hash + (_inhibitAll ? 0 : 1);
    hash = 37*hash + _inhibitedProperties.hashCode();
    hash = 37*hash + _includedStyles.hashCode();
    hash = 37*hash + _includedProperties.hashCode();
    hash = 37*hash + _includedCompactProperties.hashCode();
    hash = 37*hash + _properties.hashCode();
    hash = 37*hash + _skinProperties.hashCode();
    
    return hash;
  }

  @Override
  public String toString()
  {
    return getClass().getName() + "[" +
      "name="   + _name   + ", " +
      "selector=" + _selector + ", " +
      "properties="  + _properties.toString()  + ", " +
      "skinProperties="  + _skinProperties.toString()  + ", " +
      "includeStyles="  + _includedStyles.toString()  + ", " +
      "includeProperties="  + _includedProperties.toString()  + ", " + 
      "includeCompactProperties="  + _includedCompactProperties.toString()  + ", " + 
      "inhibitedProperties="  + _inhibitedProperties.toString()  + ", " + 
      "resetProperties="  + _resetProperties  + ", " +
      "inhibitAll=" + _inhibitAll + "]";
  }

  // Just leaving this package-private, since only
  // StyleSheetDocument really needs to know about this.
  // Really, we should have a StyleNodeImpl for this kind
  // of implementation detail.
  boolean __getResetProperties()
  {
    return _resetProperties;
  }

  private final String                     _name;
  private final String                     _selector;
  private final List<PropertyNode>         _properties;          // The property nodes
  private final List<PropertyNode>         _skinProperties;      // The skin property nodes
  private final List<IncludeStyleNode>     _includedStyles;      // Included styles
  private final List<IncludePropertyNode>  _includedProperties;  // Included properties
  private final List<IncludeCompactPropertyNode> _includedCompactProperties;  
  private final List<String>               _inhibitedProperties; // Inhibited properties
  
  // These flags checks whether the style should inherit properties
  // from equivalent styles defined in earlier style sheets.
  // This is xss-formatted skin files when resetProperties="true".
  private final boolean                _resetProperties;
  // This is css-formatted skin files when -tr-inhibit: all.
  private final boolean                _inhibitAll;

  private static final String _INHIBIT_ALL_VALUE = "all";

}
