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
package org.apache.myfaces.trinidadinternal.ui.path;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * A path defines a route through a UINode tree.  The
 * Path interface itself is immutable, but implementations
 * are not required to be.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/path/Path.java#0 $) $Date: 10-nov-2005.18:50:28 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface Path extends Cloneable
{
  /**
   * Returns the result of partially following the path from a
   * starting node.  Returns null if any of the steps of the path
   * cannot be followed.
   * @param from the node to start from
   * @param start the number of steps in the path to start from; if 0,
   *   starts from the beginning.   If negative, counts from
   *   the end.
   * @param depth the number of steps in the path to follow
   */
  public UINode followPath(
    UIXRenderingContext context,
    UINode           from,
    int              start,
    int              depth);
  
  
  /**
   * Follows the entire path.
   * Returns null if any of the steps of the path
   * cannot be followed.
   * @param from the node to start from
   */
  public UINode followPath(UIXRenderingContext context, UINode from);
  

  /**
   * Returns the number of elements in the path.
   */
  public int getElementCount();
  
  /**
   * Returns true if the element at the given index is a named child,
   * as opposed to an indexed child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public boolean isElementNamed(int elementIndex);  
  
  /**
   * Returns the child index of a path element;  returns -1
   * if that part of the path is a named child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public int getElementIndex(int elementIndex);
  
  /**
   * Returns the chid name of a path element;  returns null
   * if that part of the path is an indexed child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public String getElementName(int elementIndex);

  
  /**
   * Clones the path.
   */
  public Object clone();
}
