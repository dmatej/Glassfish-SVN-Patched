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

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renderer used by composite UINode renderers to render content.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/UINodeRenderer.java#0 $) $Date: 10-nov-2005.18:56:55 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class UINodeRenderer implements Renderer
{
  protected abstract UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node);
    
  /**
   * Render a UINode in a RenderingContext.
   */
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {    
    renderWithNode(context,
                   node,
                   getRenderingUINode(context, node));
  }  

  /**
   * Called to render the portion before the contents.
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
   * @see #renderInCompositeContext(UIXRenderingContext,UINode,UINode)
   */
  protected void renderWithNode(
    UIXRenderingContext context,
    UINode           node,
    UINode           renderingUINode
    ) throws IOException
  {
    if (renderingUINode != null)
    {
      CompositeRenderingContext subContext =
        _startCompositeContext(context, renderingUINode);

      try
      {
        Object rendered =
           renderingUINode.getAttributeValue(subContext,
                                             UIConstants.RENDERED_ATTR);
        if (!Boolean.FALSE.equals(rendered))
        {
          prerender(subContext, renderingUINode);
          
          // render the composite widget 
          renderingUINode.render(subContext);
          
          postrender(subContext, renderingUINode);
        }
      }
      finally
      {
        _endCompositeContext(subContext);
      }
    }
  }

  /**
   * @param context the context that <code>node</code> is defined in.
   * @param node this is the node that will be used for data.
   * @param renderingUINode this UINode subtree will be used to do the actual
   * rendering (ie: this is the template UINode). It may pull attributes
   * and/or indexed/named children from <code>node</code>. It renders in its
   * own RenderingContext.
   */
  public static void renderInCompositeContext(UIXRenderingContext context,
                                              UINode node,
                                              UINode renderingUINode)
    throws IOException
  {
    CompositeRenderingContext subContext =
      _startCompositeContext(context, renderingUINode);

    try
    {
      Object rendered =
        renderingUINode.getAttributeValue(subContext,
                                          UIConstants.RENDERED_ATTR);
      if (!Boolean.FALSE.equals(rendered))
      {
        renderingUINode.render(subContext);
      }
    }
    finally
    {
      _endCompositeContext(subContext);
    }
  }

  static private CompositeRenderingContext _startCompositeContext(
    UIXRenderingContext context,
    UINode renderingUINode
    )
  {
    // get a CompositeRenderingContext from the free list
    CompositeRenderingContext subContext =
      CompositeRenderingContext.__getCompositeRenderingContext(context);
            
    // push the rendering UINode with no path information
    subContext.pushChild(renderingUINode, null, -1);
    subContext.pushRenderedChild(subContext, renderingUINode);
    
    return subContext;
  }

  private static void _endCompositeContext(CompositeRenderingContext crc)
  {
    crc.popRenderedChild(crc);
    crc.popChild();
        
    // return the CompositeRenderingContext to the free list
    CompositeRenderingContext.__recycleContext(crc);
  }
}
