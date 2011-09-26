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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.path.Path;

/**
 * Callback API for walking the UINode tree.  TreeWalkers
 * can be used (with TreeWalkerUtils) to traverse the tree
 * with proper support for composite rendering.
 * <p>
 * @see TreeWalkerUtils
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/TreeWalker.java#0 $) $Date: 10-nov-2005.18:53:08 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface TreeWalker
{
  /**
   * Callback method that will be called once for every node
   * the tree walker walks.
   * <p>
   * @param context the rendering context for this node
   * @param node    the current UINode
   * @param previousValue the last value returned from walkNode, or
   *      null on the first node walked
   * @param path a Path object from the node where walking started
   * @return a new value.  Frequently, this will be <code>previousValue</code>.
   * @exception IOException if this method writes output
   */
  public Object walkNode(
    UIXRenderingContext context,
    UINode           node,
    Object           previousValue,
    Path             path) throws IOException;


  /**
   * Callback method that will be called once for every node
   * to determine if the children of that node should be walked to.
   * <p>
   * @param context the rendering context for this node
   * @param node    the current UINode
   * @param value the last value returned from walkNode
   * @param path a Path object from the node where walking started
   * @return if true, walking continues to the children of this node.
   *         If false, the siblings of this ndoe will still be walked to.
   */
  public boolean walkChildren(
    UIXRenderingContext context,
    UINode           node,
    Object           value,
    Path             path);
}
