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

import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * UINodeList that retrieves its list of children from a UINode.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UINodeUINodeList.java#0 $) $Date: 10-nov-2005.18:57:37 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class UINodeUINodeList implements UINodeList
{
  protected abstract UINode getUINode(UIXRenderingContext context);

  protected UIXRenderingContext getRenderingContext(UIXRenderingContext context)
  {
    return context;
  }

  protected MutableUINode getMutableUINode()
  {
    UINode node = getUINode(null);
    
    if (node instanceof MutableUINode)
    {
      return (MutableUINode)node;
    }
    else
    {
      return null;
    }
  }
  

  public int size(
    UIXRenderingContext context
    )
  {
    UINode node = getUINode(context);
    
    if (node != null)
    {      
      context = getRenderingContext(context);
      return node.getIndexedChildCount(context);
    }
    else
    {
      return 0;
    }
  }
  
  
  public UINode getUINode(
    UIXRenderingContext context,
    int              index
    )
  {
    UINode node = getUINode(context);
    
    if (node != null)
    {
      context = getRenderingContext(context);
      return node.getIndexedChild(context, index);
    }
    else
    {
      return null;
    }
  }
  
  public UINode setUINode(
    int    index,
    UINode node
    )
  {
    MutableUINode mutableNode = getMutableUINode();
    
    if (mutableNode != null)
    {
      UINode returnNode = mutableNode.getIndexedChild(null, index);
      mutableNode.replaceIndexedChild(index, node);
      return returnNode;
    }
    else
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "ILLEGAL_TO_SET_CHILDREN", _getClassName()));
    }
  }
 
    
  public void addUINode(
    int    index,
    UINode node
    )
  {
    MutableUINode mutableNode = getMutableUINode();
    
    if (mutableNode != null)
    {
      mutableNode.addIndexedChild(index, node);
    }
    else
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "ILLEGAL_TO_ADD_CHILDREN", _getClassName()));
    }
  }
  
   
  public void addUINode(
    UINode node
    )
  {
    MutableUINode mutableNode = getMutableUINode();
    
    if (mutableNode != null)
    {
      mutableNode.addIndexedChild(node);
    }
    else
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "ILLEGAL_TO_ADD_CHILDREN", _getClassName()));
    }
  }
  
  
  public UINode removeUINode(
    int index
    )
  {
    MutableUINode mutableNode = getMutableUINode();
    
    if (mutableNode != null)
    {
      return mutableNode.removeIndexedChild(index);
    }
    else
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "ILLEGAL_TO_REMOVE_CHILDREN", _getClassName()));
    }
  }
  
  public void clearUINodes()
  {
    MutableUINode mutableNode = getMutableUINode();
    
    if (mutableNode != null)
    {
      mutableNode.clearIndexedChildren();
    }
    else
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "ILLEGAL_TO_REMOVE_CHILDREN", _getClassName()));
    }
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

  private String _getClassName()
  {
    String name = getClass().getName();
    int indexOfPeriod = name.lastIndexOf('.');
    if (indexOfPeriod < 0)
      return name;
    return name.substring(indexOfPeriod + 1);
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    UINodeUINodeList.class);
} 
