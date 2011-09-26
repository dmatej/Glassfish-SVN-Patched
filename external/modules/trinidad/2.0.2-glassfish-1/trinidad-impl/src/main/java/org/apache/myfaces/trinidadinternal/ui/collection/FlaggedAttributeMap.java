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
package org.apache.myfaces.trinidadinternal.ui.collection;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * AttributeMap that stores a bitflag indicating the presence of
 * an indexed AttributeKey in the AttributeMap.  The use of
 * the bitflags makes determining that no value for an indexed AttributeKey
 * exists extremely fast.  As gets for indexed AttributeKeys that do
 * not exist in the AttributeMap make up the vast majority of gets
 * on AttributeMaps used as storage for UINodes, using a FlaggedAttributeMap
 * is a huge improvement over ArrayAttributeMap for performance.  In fact,
 * given the small size overhead of maintaining the flags,
 * the FlaggedAttributeMap is the best general purpose AttributeMap to
 * use for storing the attributes of UINodes.
 * <p>
 * @see IndexedAttributeMap 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/FlaggedAttributeMap.java#0 $) $Date: 10-nov-2005.18:57:34 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class FlaggedAttributeMap extends ArrayAttributeMap
{
  public FlaggedAttributeMap()
  {
    super();
  }
  
  public FlaggedAttributeMap(
    int size
    )
  {
    super(size);
  }
  
  public FlaggedAttributeMap(
    int size,
    int increment
    )
  {
    super(size, increment);
  }
  
  @Override
  public Object getAttribute(
    UIXRenderingContext context,
    AttributeKey     key
    )
  {
    int attrIndex = key.getAttributeIndex();
    
    if (attrIndex > -1)
    {
      // if we don't have the attribute, don's search the array
      if ((_flags & (1L << attrIndex)) == 0)
      {
        return null;
      }
    }
    
    return super.getAttribute(context, key);
  }
  
  @Override
  protected void putAttribute(
    AttributeKey key,
    Object       value
    )
  {
    int attrIndex = key.getAttributeIndex();
    
    if (attrIndex > -1)
    {
      // set the attribute flag
      _flags |= (1L << attrIndex);
    }
    
    super.putAttribute(key, value);
  }

  @Override
  protected void removeAttribute(
    AttributeKey key
    )
  {
    int attrIndex = key.getAttributeIndex();
    
    if (attrIndex > -1)
    {
      long attrMask = 1L << attrIndex;
      
      // if we don't have the attribute, don's search the array
      if ((_flags & attrMask) == 0)
      {
        return;
      }
      
      // clear the attribute flag
      _flags &= ~attrMask;
    }
    
    super.removeAttribute(key);
  }

  
  private long _flags;
}
