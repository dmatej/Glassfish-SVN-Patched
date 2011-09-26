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

import java.util.Iterator;
import java.util.Collections;


import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.path.PathImpl;

import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;

/**
 * Utilities for walking the tree with TreeWalker.  This currently adds
 * support only for going through context-popping nodes.
 * Features that are not supported currently include:
 * <ol>
 *  <li>Dealing with dataScope nodes</li>
 *  <li>Updating the RenderingContext's rendered and logical stacks</li>
 * </ol>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/TreeWalkerUtils.java#0 $) $Date: 10-nov-2005.18:53:09 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
// =-AEW We also currently don't skip over non-rendered trees;  there
// are some components that need this behavior, like table, so
// a standard TreeWalker base class might be the best idea for
// other components.
@Deprecated
public class TreeWalkerUtils
{
  /**
   * Walks a tree using a TreeWalker.  The TreeWalker will
   * receive a newly instantiated Path object.
   * @param context the initial RenderingContext
   * @param ancestor the initial UINode
   * @param walker the TreeWalker callback used
   * @return the last value returned by walkNode()
   */
  static public Object walkTree(
    UIXRenderingContext context,
    UINode           ancestor,
    TreeWalker       walker) throws IOException
  {
    return walkTree(context, ancestor, walker, new PathImpl());
  }



  /**
   * Walks a tree using a TreeWalker.
   * @param context the initial RenderingContext
   * @param ancestor the initial UINode
   * @param walker the TreeWalker callback used
   * @param path a pre-created PathImpl object;  may be
   *   null if the TreeWalker doesn't need a path
   * @return the last value returned by walkNode()
   */
  static public Object walkTree(
    UIXRenderingContext context,
    UINode           ancestor,
    TreeWalker       walker,
    PathImpl         path) throws IOException
  {
    return _walkTree(context, ancestor, walker, null, path, false);
  }


  /**
   * Walks a tree using a TreeWalker.
   * @param context the initial RenderingContext
   * @param ancestor the initial UINode
   * @param walker the TreeWalker callback used
   * @param path a pre-created PathImpl object;  may be
   *   null if the TreeWalker doesn't need a path
   * @param renderedOnly if true, only walk those nodes
   *    that will be rendered
   * @return the last value returned by walkNode()
   */
  static public Object walkTree(
    UIXRenderingContext context,
    UINode           ancestor,
    TreeWalker       walker,
    PathImpl         path,
    boolean          renderedOnly) throws IOException
  {
    return _walkTree(context, ancestor, walker, null, path, renderedOnly);
  }

  static private Object _walkTree(
    UIXRenderingContext context,
    UINode           ancestor,
    TreeWalker       walker,
    Object           value,
    PathImpl         path,
    boolean          renderedOnly) throws IOException
  {
    UIXRenderingContext childContext =
      _getChildRenderingContext(context, ancestor);

    // Walk the indexed children
    int count = ancestor.getIndexedChildCount(context);
    for(int i = 0; i < count; i++)
    {
      UINode currChild = ancestor.getIndexedChild(context, i);

      if (!renderedOnly || !BaseRenderer.skipNode(childContext, currChild))
      {
        if (path != null)
          path.add(i);

        // Walk the node, then its children
        value = walker.walkNode(childContext, currChild, value, path);
        if (walker.walkChildren(childContext, currChild, value, path))
        {
          value = _walkTree(childContext, currChild, walker, value, path,
                            renderedOnly);
        }

        if (path != null)
          path.removeLastElement();
      }
    }

    // Walk the named children
    Iterator<String> e;

    // For switcher beans, only walk the "CHILD_NAME_ATTR" bean; a
    // SingleItemIterator does the trick quite well...
    if (renderedOnly && _isSwitcherBean(ancestor))
    {
      Object name = ancestor.getAttributeValue(context,
                                               UIConstants.CHILD_NAME_ATTR);
      if (name == null)
        e = null;
      else
        e = Collections.singletonList((String)name).iterator();
    }
    else
    {
      e = ancestor.getChildNames(context);
    }

    if (e != null)
    {
      while (e.hasNext())
      {
        String next = e.next();

        UINode currNamedChild =
                    ancestor.getNamedChild(context, next);
        if ((currNamedChild != null) &&
            (!renderedOnly || !BaseRenderer.skipNode(childContext, currNamedChild)))
        {
          if (path != null)
            path.add(next);

          // Walk the node, then its children
          value = walker.walkNode(childContext, currNamedChild, value, path);
          if (walker.walkChildren(childContext, currNamedChild, value, path))
          {
            value = _walkTree(childContext,
                              currNamedChild, walker, value, path,
                              renderedOnly);
          }

          if (path != null)
            path.removeLastElement();
        }
      }
    }

    return value;
  }

  //
  // Returns the correct RenderingContext to pass to the children of this node.
  //
  static private UIXRenderingContext _getChildRenderingContext(
    UIXRenderingContext context,
    UINode           child)
  {
    if (UIConstants.CONTEXT_POPPING_NAME.equals(child.getLocalName()) &&
        UIConstants.MARLIN_NAMESPACE.equals(child.getNamespaceURI()))
      return context.getParentContext();

    return context;
  }

  static private boolean _isSwitcherBean(
    UINode node)
  {
    return (UIConstants.SWITCHER_NAME.equals(node.getLocalName()) &&
            UIConstants.MARLIN_NAMESPACE.equals(node.getNamespaceURI()));
  }
}
