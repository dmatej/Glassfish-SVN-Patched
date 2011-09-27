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
 * Interface for lists of UINodes.  This class is implemented by MutableUINode
 * subclasses that wish to change how the list of indexed children
 * in a MutableUINode are stored.
 * <p>
 * Because the RenderingContext is passed to both the <code>size</code> and
 * <code>getUINode</code> methods, the contents of the UINodeList can
 * be proxied through the RenderingContext.  In fact this is how
 * some instances of DataObjectListNodeList work, building a UINodeList
 * from a DataObjectList retrieved from a BoundValue.  However, the amount of
 * proxying that may be accomplished is limited by the fact that the
 * mutating methods do not take a RenderingContext, thus, it is impossible
 * to create a mutable UINodeList that acts as the concatenation of two
 * other UINodeLists.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UINodeList.java#0 $) $Date: 10-nov-2005.18:57:36 $
 * @see org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode#setIndexedNodeList
 * @see DataObjectListNodeList
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface UINodeList extends Cloneable
{
  /**
   * Returns the number of elements in the UINodeList given the specified
   * RenderingContext.
   */
  public int size(UIXRenderingContext context);

  /**
   * Returns the UINode at the specifed index in the UINodeList, given
   * the specified RenderingContext.
   */
  public UINode getUINode(UIXRenderingContext context, int index);
  
  /**
   * Replaces the UINode at the specified index with the new UINode value,
   * returning the old value.
   */
  public UINode setUINode(int index, UINode node);

  /**
   * Inserts the spacified UINode into the UINodeList at the specified index
   */
  public void   addUINode(int index, UINode node);

  /**
   * Appends the specified UINode to the end of the UINodeList.
   */
  public void   addUINode(UINode node);

  /**
   * Removes and returns UINode at the specifed index.
   */ 
  public UINode removeUINode(int index);

  /**
   * Removes all of the UINodes from the UINodeList.
   */
  public void   clearUINodes();

  /**
   * Clones the UINodeList
   */
  public Object clone();
} 
