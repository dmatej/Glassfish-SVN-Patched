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
package org.apache.myfaces.trinidadinternal.ui;

import java.io.IOException;

/**
 * Basic implementation of Renderer.
 * <p>
 * The protected hooks on this class are called in the following order:
 * <ul>
 * <li>prerender()
 *   <ul>
 *     <li>renderContent()
 *       <ul>
 *         <li>renderChild()
 *         <li>renderBetweenIndexedChildren()
 *         <li>renderChild()
 *         <li>...
 *         <li>renderchild()
 *       </ul>
 *   </ul>
 * <li>postrender()
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/BaseRenderer.java#0 $) $Date: 10-nov-2005.18:50:11 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseRenderer implements Renderer
{
  /**
   * Value passed to renderIndexedChild to indicate that no child exists.
   * <p>
   * @see #renderIndexedChild
   */
  protected static final int NO_CHILD_INDEX = -1;

  /**
   * Determines if a specific UINode should be skipped.
   * @return true for nodes with a {@link UIConstants#RENDERED_ATTR}
   * attribute set to Boolean.FALSE.
   * @param context the rendering context
   * @param node the node under consideration
   */
  public static boolean skipNode(UIXRenderingContext context, UINode node)
  {
    Object render = node.getAttributeValue(context, UIConstants.RENDERED_ATTR);
    return Boolean.FALSE.equals(render);
  }

  /**
   * Render a UINode in a RenderingContext.  Subclassers should not need to
   * override this method.
   * @param context the rendering context
   * @param node the current UINode
   */
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Don't do this anymore;  we handle this in the ResponseWriter
    if (false && context.getConfiguration().isDebug())
    {
      // show the node that started this output
      // =-=AEW HACK.  I really don't want comments for TextNodes,
      // since adding the comments messes with whitespace
      // that really does matter (two adjacent TextNodes would
      // get separated).
      if (!"text".equals(node.getLocalName()))
        context.getResponseWriter().writeComment("Start:" + node);
    }

    prerender(context, node);

    renderContent(context, node);

    postrender(context, node);

    // -= Simon Lessard =-
    // Completely useless piece of code...
    //boolean assertEnabled = false;
	  //assert assertEnabled = true;
    //if (false && assertEnabled)
    //{
    //  // show the node that ended this output
    //  context.getResponseWriter().writeComment("End:" + node);
    //}
  }


  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    int childCount = node.getIndexedChildCount(context);

    if (childCount > 0)
    {
      int prevVisChildIndex;
      int currVisChildIndex = -1;
      int ithRenderedChild = 0;

      int nextVisChildIndex = -1;

      do
      {
        prevVisChildIndex = currVisChildIndex;
        currVisChildIndex = nextVisChildIndex;

        // get the index of the next visible child
        nextVisChildIndex = getNextRenderedChildIndex(context,
                                                      node,
                                                      nextVisChildIndex);

        if (currVisChildIndex != -1)
        {
          if (prevVisChildIndex != -1)
          {
            renderBetweenIndexedChildren(context, node, currVisChildIndex);
          }

          // render this indexed child
          renderIndexedChild(context,
                             node,
                             currVisChildIndex,
                             prevVisChildIndex,
                             nextVisChildIndex,
                             ithRenderedChild);

          // update the rendered index
          ithRenderedChild++;
        }
      } while (nextVisChildIndex != NO_CHILD_INDEX);
    }
  }


  /**
   * Returns the next child UINode that will be rendered after the passed in
   * index.
   * <p>
   * @see #getNextRenderedChildIndex
   */
  protected final UINode getNextRenderedChildNode(
    UIXRenderingContext context,
    UINode           parentNode,
    int              afterChildIndex
    )
  {
    int nextVisIndex = getNextRenderedChildIndex(context,
                                                 parentNode,
                                                 afterChildIndex);

    if (nextVisIndex != NO_CHILD_INDEX)
    {
      return parentNode.getIndexedChild(context, nextVisIndex);
    }
    else
    {
      return null;
    }
  }


  /**
   * @param afterChildIndex The indexed children coming after this index, will
   * be considered. To find the first rendered child use {@link
   * #NO_CHILD_INDEX}
   * @return the index of the next child UINode that must be rendered, or
   * {@link #NO_CHILD_INDEX} if there is none.
   * @see #getNextRenderedChildNode
   */
  protected int getNextRenderedChildIndex(
    UIXRenderingContext context,
    UINode           parentNode,
    int              afterChildIndex
    )
  {
    int childCount = parentNode.getIndexedChildCount(context);

    int childIndex = afterChildIndex + 1;

    for (; childIndex < childCount; childIndex++)
    {
      UINode currChild = parentNode.getIndexedChild(context, childIndex);

      if (!skipChild(context, parentNode, currChild))
      {
        return childIndex;
      }
    }

    return NO_CHILD_INDEX;
  }


  /**
   * Called each time an indexed child needs to be rendered.  The
   * previous visible child is passed in to provide renderers with
   * additional context information.
   * <p>
   * The default implementation is call the three-argument
   * version of renderIndexedChild().
   * <p>
   * @see #renderChild
   */
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {
    renderIndexedChild(context,
                       node,
                       currVisChildIndex);
  }


  /**
   * Called to render the portion before the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  }



  /**
   * Called to render the portion after the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  }


  /**
   * Called to render between each set of rendered indexed children.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  }

   /**
   * Called to render between each set of rendered indexed children.
   * @param context the rendering context
   * @param node the current UINode
   * @param index the index of the next child to be rendered
   */
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node,
    int              nextIndex
    ) throws IOException
  {
    renderBetweenIndexedChildren( context, node );
  }



  /**
   * Called to determine if a specific child should be skipped.  This
   * method is called for both named children and indexed children.
   * The default implementation returns true for nodes with a rendered
   * attribute set to Boolean.FALSE.
   * <p>
   * @param context the rendering context
   * @param node the current UINode
   * @param child the child under consideration
   * <p>
   * @see #getVisibleIndexedChildCount
   */
  protected boolean skipChild(
    UIXRenderingContext context,
    UINode           node,
    UINode           child
    )
  {
    return skipNode(context, child);
  }


  /**
   * Called to render an indexed child.  The default implementation
   * will update the context as needed (e.g., calling pushChild()
   * and popChild()) and then call <code>renderChild()</code>.
   * <p>
   * @see #renderChild
   */
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              childIndex
    ) throws IOException
  {
    UINode child = node.getIndexedChild(context, childIndex);

    if (child != null)
    {
      context.pushChild(child, null, childIndex);
      context.pushRenderedChild(context, child);

      try
      {
        renderChild(context, child);
      }
      finally
      {
        context.popRenderedChild(context);
        context.popChild();
      }
    }
  }

  /**
   * Called to render a named child.  Retrieves the child,
   * then calls the four-argument version of this function.
   * <p>
   * @see #renderChild
   */
  protected final void renderNamedChild(
    UIXRenderingContext context,
    UINode           node,
    String           childName
    ) throws IOException
  {
    renderNamedChild(context,
                     node,
                     node.getNamedChild(context, childName),
                     childName);
  }


  /**
   * Called to render a named child.  Renderers that render named
   * children should call this method to do the actual rendering.  The
   * default implementation will update the context as needed (e.g.,
   * calling pushChild() and popChild()) and render the child.
   * <p>
   * @see #renderChild
   */
  protected void renderNamedChild(
    UIXRenderingContext context,
    UINode           node,
    UINode           child,
    String           childName
    ) throws IOException
  {
    if (childName == null)
      throw new NullPointerException();

    if ((child != null) && !skipChild(context, node, child))
    {
      context.pushChild(child, childName, -1);
      context.pushRenderedChild(context, child);

      try
      {
        renderChild(context, child);
      }
      finally
      {
        context.popRenderedChild(context);
        context.popChild();
      }
    }
  }


  /**
   * Called to render a child.  This method does not update the
   * rendering context (by calling pushChild() and popChild()
   * as needed);  subclasses need to use renderIndexedChild() or
   * renderNamedChild() for that purpose.
   * <p>
   * @param context the rendering context
   * @param node the current UINode
   * @param child the child under consideration
   */
  protected void renderChild(
    UIXRenderingContext context,
    UINode           child
    ) throws IOException
  {
    if (child != null)
    {
      child.render(context);
    }
  }


  /**
   * Convenience function to return the number of non-skipped indexed children.
   * <p>
   * @param context the rendering context
   * @param node the current UINode
   * <p>
   * @see #skipChild
   */
  protected int getVisibleIndexedChildCount(
    UIXRenderingContext context,
    UINode           node
    )
  {
    int childCount = node.getIndexedChildCount(context);

    int visibleCount = 0;

    for (int childIndex = 0; childIndex < childCount; childIndex++)
    {
      if (!skipChild(context, node, node.getIndexedChild(context, childIndex)))
      {
        visibleCount++;
      }
    }

    return visibleCount;
  }
}
