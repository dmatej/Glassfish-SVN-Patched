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


/**
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/ArrayUINodeList.java#0 $) $Date: 10-nov-2005.18:57:31 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ArrayUINodeList implements UINodeList
{
  public ArrayUINodeList()
  {
    this(_DEFAULT_SIZE);
  }
 
  public ArrayUINodeList(
    int initialCapacity
    )
  {
    _nodes = new UINode[initialCapacity];
  }
 
  public int size(UIXRenderingContext context)
  {
    return _nodeCount;
  }
  
  public UINode getUINode(
    UIXRenderingContext context,
    int index
    )
  {
    _checkIndex(index);
    
    return _nodes[index];
  }
  
  public UINode setUINode(
    int    index,
    UINode node
    )
  {
    _checkIndex(index);
    
    if (node == null)
      throw new IllegalArgumentException();
      
    UINode oldNode = _nodes[index];
    _nodes[index] = node;
    
    return oldNode;
  }
  

  public void addUINode(
    UINode node
    )
  {
    addUINode(_nodeCount, node);
  }


  public void addUINode(
    int    index,
    UINode node
    )
  {
    if (node == null)
      throw new IllegalArgumentException();
      
    if ((index < 0) || (index > _nodeCount))
    {
      throw new IndexOutOfBoundsException();
    }
         
    int availSpace = _nodes.length;
    int nodeCount = _nodeCount;
    
    if (nodeCount >= availSpace)
    {
      UINode[] newNodes = new UINode[availSpace * 2];
      System.arraycopy(_nodes,
                       0,
                       newNodes,
                       0,
                       availSpace);
      _nodes = newNodes;
    }
    
    // move the items after the inserted child, if any
    if (index < nodeCount)
    {
      System.arraycopy(_nodes,
                       index,
                       _nodes,
                       index + 1,
                       nodeCount - index);
      
    }
    
    // insert the new item
    _nodes[index] = node;
    
    _nodeCount++;
  }


  public UINode removeUINode(
    int index
    )
  {
    _checkIndex(index);
    
    UINode removedNode = _nodes[index];
    
    _nodeCount--;
    
    if (index != _nodeCount)
    {
      // fill up the space occupied by the removed item
      System.arraycopy(_nodes,
                       index + 1,
                       _nodes,
                       index,
                       _nodeCount - index);
    }
    
    // we've shifted all the nodes up by one. So the last spot can be cleared:
    _nodes[_nodeCount] = null; // bug 3463837
    
    return removedNode;
  }
  
  
  public void clearUINodes()
  {
    int nodeCount  = _nodeCount;
    UINode[] nodes = _nodes;
    
    for (int i = 0; i < nodeCount; i++)
    {
      // free memory
      nodes[i] = null;
    }
    
    _nodeCount = 0;
  }
  
  @Override
  public Object clone()
  {
    try
    { 
      ArrayUINodeList newNodeList = (ArrayUINodeList)super.clone();
      newNodeList._nodes = new UINode[_nodeCount];
      System.arraycopy(_nodes, 0, newNodeList._nodes, 0, _nodeCount);
      
      return newNodeList;
    }
    catch (CloneNotSupportedException e)
    { 
      // this should never happen
      throw new InternalError();
    }
  }
  
  public void trimToSize()
  {
    if (_nodeCount < _nodes.length)
    {
      UINode[] trimmedNodes = new UINode[_nodeCount];
      
      System.arraycopy(_nodes, 0, trimmedNodes, 0, _nodeCount);
      
      _nodes = trimmedNodes;
    }
  }
  
  private void _checkIndex(
    int index
    )
  {
    if ((index < 0) || (index >= _nodeCount))
    {
      throw new IndexOutOfBoundsException();
    }
  }
  
  private static final int _DEFAULT_SIZE = 5;
  
  private int      _nodeCount;
  private UINode[] _nodes;
} 
