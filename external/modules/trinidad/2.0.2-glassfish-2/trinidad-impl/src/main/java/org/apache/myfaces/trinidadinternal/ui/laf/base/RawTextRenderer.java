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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Renderer for text that shouldn't be escaped.  The renderer
 * uses three node attributes:
 * <ul>
 * <li><code>UIConstants.PRE_TEXT_ATTR</code>: Text to be rendered
 * before any children.
 * <li><code>UIConstants.POST_TEXT_ATTR</code>: Text to be rendered
 * after any children.
 * <li><code>UIConstants.BETWEEN_TEXT_ATTR</code>: Text to be between
 * any two children.
 * <p>
 * RawTextRenderer also understands <code>TEXT_ATTR</code> as a synonym for
 * <code>PRE_TEXT_ATTR</code>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/RawTextRenderer.java#0 $) $Date: 10-nov-2005.18:53:06 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RawTextRenderer extends BaseRenderer implements UIConstants
{
  /**
   * Creates a RawTextRenderer.
   */
  public RawTextRenderer()
  {
  }


  /**
   * Called to render the portion before the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    _renderText(context, node, PRE_TEXT_ATTR);
    _renderText(context, node, TEXT_ATTR);
  }


  /**
   * Called to render the portion after the contents.  Default
   * implementation does nothing.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    _renderText(context, node, POST_TEXT_ATTR);
  }

  /**
   * Called to render between each set of rendered indexed children.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    _renderText(context, node, BETWEEN_TEXT_ATTR);
  }


  private void _renderText(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     attrKey
    ) throws IOException
  {
    Object value = node.getAttributeValue(context, attrKey);
    if (value != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      if (value instanceof char[])
      {
        char[] text = (char[]) value;
        writer.write(text, 0, text.length);
      }
      else
      {
        writer.write(value.toString());
      }
    }
  }
}
