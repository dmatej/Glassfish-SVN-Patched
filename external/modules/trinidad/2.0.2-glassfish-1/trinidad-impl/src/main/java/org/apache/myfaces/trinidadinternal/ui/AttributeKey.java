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
package org.apache.myfaces.trinidadinternal.ui;

import java.util.Hashtable;

/**
 * Key class used to quickly retrieve attribute values from UINodes and
 * AttributeMaps.
 * <p>
 * AttributeKeys can not be instantiated directly, instead,  the factory
 * method <code>getAttributeKey</code> is called with the name of the
 * AttributeKey to retrieve.
 * <p>
 * AttributeKeys have two performance advantages over Strings when used
 * as keys.
 * <ol>
 *   <li>
 *     AttributeKeys are singletons and can thus use instance equality
 *     to check for equality.
 *   </li>
 *   <li>
 *     AttributeKeys that are requested often can be assigned an index
 *     that can then be used by AttributeMap implementations to speed
 *     up value retrieval for these attributes.  Both the
 *     FlaggedAttributeMap and the IndexedAttributeMap take advantage
 *     of this feature.
 *   </li>
 * </ol>  
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/AttributeKey.java#0 $) $Date: 10-nov-2005.18:50:10 $
 * @see org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap
 * @see UINode#getAttributeValue
 * @see MutableUINode#setAttributeValue
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class AttributeKey
{
  /**
   * Returns an AttributeKey for the given name, creating the AttributeKey
   * instance if necessary.  This is the only public way to create an
   * AttributeKey.
   * <p>
   * AttributeKeys created by this method do not have indices.
   */
  // This method must be synchronized;  it'd be very bad if
  // the same attribute key ever got created twice.  We
  // used to use double-checked locking, but that idiom's
  // broken
  synchronized public static AttributeKey getAttributeKey(
    String attrName
    )
  {
    if (attrName == null)
      throw new IllegalArgumentException();

    AttributeKey attr = _sAttrs.get(attrName);
    
    if (attr == null)
      attr = new AttributeKey(attrName, -1);
    
    return attr;
  }

  AttributeKey(
    String attrName
    )
  {
    this(attrName, -1);
  }
    
  AttributeKey(
    String attrName,
    int    attrIndex
    )
  {
    if (attrName == null)
      throw new IllegalArgumentException();
      
    _attrName  = attrName;
    _attrIndex = attrIndex;
    
    _sAttrs.put(attrName, this);
  }
  
  /**
   * Returns the 0-based index of this AttributeKey, or -1 if the
   * AttributeKey has no index.
   */
  public int getAttributeIndex()
  {
    return _attrIndex;
  }
  
  /**
   * Returns the name of this AttributeKey.  This is the name used to
   * create the AttributeKey in <code>getAttributeKey</code>.
   * <p>
   * @see #getAttributeKey
   */
  public String getAttributeName()
  {
    return _attrName;
  }
  
  @Override
  public String toString()
  { 
    return _attrName;
  }
  
  @Override
  public boolean equals(
    Object otherObject
    )
  {
    return (this == otherObject);
  }
  
  @Override
  public int hashCode()
  {
    return _attrName.hashCode();
  }

  private String _attrName;
  private int _attrIndex;

  // -= Simon Lessard =-
  // TODO: Check if synchronization is really required
  private static Hashtable<String, AttributeKey> _sAttrs = 
    new Hashtable<String, AttributeKey>(203);

  // If someone called AttributeKey before UIConstants was
  // loaded, bad things happen.  Prevent this.
  // By bad things:  if the first line of someone's app was:
  //  static public void main(String[] args)
  //  {
  //    AttributeKey attr = AttributeKey.getAttributeKey("source");
  //    ... 
  //  }
  //  Then "attr" would _not_ be the same instance as SOURCE_ATTR.
  //  Which makes stuff blow up.  This line fixes that problem.
  private static final Class<UIConstants> _UICONSTANTS_CLASS = UIConstants.class;
  static
  {
    // Hack to eliminate "unused private field" warning
    _UICONSTANTS_CLASS.getName();
  }
}
