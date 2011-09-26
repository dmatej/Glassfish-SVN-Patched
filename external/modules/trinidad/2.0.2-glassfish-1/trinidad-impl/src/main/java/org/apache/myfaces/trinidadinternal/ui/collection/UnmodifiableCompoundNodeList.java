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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Unmodifiable UINodeList that merges the results of two other node lists.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UnmodifiableCompoundNodeList.java#0 $) $Date: 10-nov-2005.18:57:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UnmodifiableCompoundNodeList implements UINodeList
{
  public UnmodifiableCompoundNodeList(
    UINodeList first,
    UINodeList second
    )
  {
    _first  = first;
    _second = second;
  }
  
  public int size(
    UIXRenderingContext context
    )
  {
    int total = 0;
    
    if (_first != null)
      total = _first.size(context);
    
    if (_second != null)
      return total + _second.size(context);
    
    return total;
  }
  
  
  public UINode getUINode(
    UIXRenderingContext context,
    int              index
    )
  {
    if (_first != null)
    {
      int firstSize = _first.size(context);
      
      if (index < firstSize)
        return _first.getUINode(context, index);
        
      index -= firstSize;
    }
    
    if (_second != null)
    {
      return _second.getUINode(context, index);
    }
    else
    {
      throw new IndexOutOfBoundsException();
    }
  }
  
  public UINode setUINode(
    int    index,
    UINode node
    )
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "ILLEGAL_TO_SET_CHILDREN_ON_UNMODIFIABLECOMPOUNDNODELIST"));
  }
  
  
  public void addUINode(
    int    index,
    UINode node
    )
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "ILLEGAL_TO_ADD_CHILDREN_ON_UNMODIFIABLECOMPOUNDNODELIST"));
  }
  
  
  public void addUINode(
    UINode node
    )
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "ILLEGAL_TO_ADD_CHILDREN_ON_UNMODIFIABLECOMPOUNDNODELIST"));
  }
  
  public UINode removeUINode(
    int index
    )
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "ILLEGAL_TO_REMOVE_CHILDREN_ON_UNMODIFIABLECOMPOUNDNODELIST"));
  }
  
  public void clearUINodes()
  {
    throw new UnsupportedOperationException(_LOG.getMessage(
      "ILLEGAL_TO_REMOVE_CHILDREN_ON_UNMODIFIABLECOMPOUNDNODELIST"));
  }
  
  @Override
  public Object clone()
  {
    try
    {
      return super.clone();
    }
    catch (CloneNotSupportedException cnse)
    {
      // this should never happen
      throw new InternalError();
    }
  }
  
  private UINodeList _first;
  private UINodeList _second;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    UnmodifiableCompoundNodeList.class);
}
