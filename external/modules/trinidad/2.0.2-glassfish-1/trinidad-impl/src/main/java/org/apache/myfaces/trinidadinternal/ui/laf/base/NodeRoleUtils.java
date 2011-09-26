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

import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.path.Path;

/**
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/NodeRoleUtils.java#0 $) $Date: 10-nov-2005.18:53:05 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NodeRoleUtils implements UIConstants
{
  /**
   * Returns the structural role of the node;  if it isn't
   * structural, returns the role of the first structural child.
   * If no children are structural, returns null.
   */
  static public NodeRole getStructuralRole(
    UIXRenderingContext context,
    UINode           node)
  {
    NodeRole role = node.getNodeRole(context);
    if (role.satisfiesRole(STRUCTURAL_ROLE))
    {
      return role;
    }

    try
    {
      return (NodeRole) TreeWalkerUtils.walkTree(context,
                                                 node,
                                                 new FindRole(),
                                                 null);
    }
    catch (IOException ioe)
    {
      // =-=AEW Can't happen
      return null;
    }
  }

  /**
   * Return the first ancestral node that is "structural" - 
   * that is, skip over any nodes that just change state,
   * like DataScopeBean or SwitcherBean.
   */
  static public UINode getStructuralAncestor(UIXRenderingContext context)
  {
    return _getStructuralAncestor(context, 1);
  }


  static private UINode _getStructuralAncestor(
   UIXRenderingContext context,
   int              depth)
  {
    UINode ancestor;

    do
    {
      ancestor = context.getRenderedAncestorNode(depth);
      if (ancestor == null)
        break;

      if (isStructuralButNotComposite(ancestor.getNodeRole(context)))
        break;

      depth++;
    }
    while (true);

    return ancestor;
  }

  static public boolean isStructuralButNotComposite(NodeRole role)
  {
    return !role.satisfiesRole(COMPOSITE_ROLE) &&
            role.satisfiesRole(STRUCTURAL_ROLE);
  }

  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  static private class FindRole implements TreeWalker
  {
    public Object walkNode(
      UIXRenderingContext context,
      UINode           node,
      Object           previousValue,
      Path             path)
    {
      if (previousValue == null)
      {
        NodeRole role = node.getNodeRole(context);
        if (role.satisfiesRole(STRUCTURAL_ROLE))
          return role;
      }

      return previousValue;
    }

    public boolean walkChildren(
      UIXRenderingContext context,
      UINode           node,
      Object           value,
      Path             path)
    {
      return (value == null);
    }
  }
}
