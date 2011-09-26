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
package org.apache.myfaces.trinidadinternal.ui.composite;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.collection.UINodeUINodeList;

/**
 * UINodeList that retrieves the list of indexed children from
 * the root UINode
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/RootUINodeList.java#0 $) $Date: 10-nov-2005.18:56:55 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RootUINodeList extends UINodeUINodeList
{
  public static RootUINodeList getNodeList()
  {
    return _INSTANCE;
  }
  
  protected RootUINodeList()
  {
  }
    
  @Override
  protected UINode getUINode(
    UIXRenderingContext context
    )
  {    
    if (context != null)
    {
      return context.getParentContext().getAncestorNode(0);
    }
    else
    {
      return null;
    }
  }

  @Override
  protected UIXRenderingContext getRenderingContext(UIXRenderingContext context)
  {
    if (context == null)
      return null;

    return context.getParentContext();
  }

  @Override
  public UINode getUINode(
    UIXRenderingContext context,
    int              index
    )
  {
    return ContextPoppingUINode.getUINode(index);
  }
  
  private static final RootUINodeList _INSTANCE = new RootUINodeList();
} 
