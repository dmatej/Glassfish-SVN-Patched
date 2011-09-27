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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;

import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;

/**
 * Renderer for ShowOneTab
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ShowOneTabRenderer.java#0 $) $Date: 10-nov-2005.18:56:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PanelTabbedRenderer extends HtmlLafRenderer
{
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    ) 
  {
    return SPAN_ELEMENT;
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);


    UIComponent component = NodeUtils.getUIComponent(context, node);

    // Check for partial targets
    String id = component.getClientId(context.getFacesContext());
    String partialTargets = getEncodedPartialTargets(context, node, id);
    setPartialTargets(context, partialTargets);

    int resolvedSelectedIndex = _getResolvedSelectedIndex(context, node);
    SubTabBarUtils.setSelectedIndex(
      context, 
      Integer.valueOf(resolvedSelectedIndex));
      
    if (-1 != resolvedSelectedIndex)
    {
      Object unvalidated = node.getIndexedChild(
        context, 
        resolvedSelectedIndex).getAttributeValue(context, UNVALIDATED_ATTR);
      SubTabBarUtils.setUnvalidated(context, unvalidated);
    }
    
    Object position = node.getAttributeValue(context, POSITION_ATTR);
    if (!POSITION_BELOW.equals(position))
    {
      SubTabBarUtils.setOrientation(context, ORIENTATION_TOP );
      Renderer subTabBarRenderer = 
        context.getRendererManager().getRenderer(
          MARLIN_NAMESPACE, SUB_TAB_BAR_NAME);
      subTabBarRenderer.render(context, node);
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Bug 4054078;  clear out the partial targets while rendering
    // the content of the showOneTab
    String partialTargets =  
      (String) context.getProperty(MARLIN_NAMESPACE,
                 BaseDesktopConstants.LINK_CONTAINER_PARTIAL_TARGETS_PROPERTY);
    if (partialTargets != null)
      setPartialTargets(context, null);

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement( DIV_ELEMENT, null);
    renderStyleClassAttribute(context, AF_SHOW_ONE_TAB_BODY_STYLE_CLASS);
    int selectedChildIndex = _getResolvedSelectedIndex(context, node);
    //Render the content for the selected showItem
    if (selectedChildIndex != -1)
      super.renderContent(
        context, node.getIndexedChild(context, selectedChildIndex));
    writer.endElement( DIV_ELEMENT );

    if (partialTargets != null)
      setPartialTargets(context, partialTargets);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object position = node.getAttributeValue(context, POSITION_ATTR);
    if (!POSITION_ABOVE.equals(position))
    {
      SubTabBarUtils.setOrientation(context, ORIENTATION_BOTTOM );

      int resolvedSelectedIndex = _getResolvedSelectedIndex(context, node);
      SubTabBarUtils.setSelectedIndex(context,
				      resolvedSelectedIndex);

      Renderer subTabBarRenderer = 
        context.getRendererManager().getRenderer(
          MARLIN_NAMESPACE, SUB_TAB_BAR_NAME);
      subTabBarRenderer.render(context, node);
    }
    setPartialTargets(context, null);
    SubTabBarUtils.setOrientation(context, null );    
    super.postrender(context, node);
  }
  
  // Returns the partial targets for this node, if any,
  // in encoded form.
  protected static String getEncodedPartialTargets(
    UIXRenderingContext context,
    UINode           node,
    String           id
    )
  {
    // Make sure partial page rendering is supported
    if (!supportsPartialRendering(context))
      return null;

    if (!PartialPageUtils.isPPRActive(context.getFacesContext()))
      return null;

    return PartialPageRendererUtils.encodePartialTargets(new String[]{id});
  }

  // Sets the partial targets on the RenderingContext
  protected static void setPartialTargets(
    UIXRenderingContext context,
    String           partialTargets)
  {
    context.setProperty(MARLIN_NAMESPACE,
      BaseDesktopConstants.LINK_CONTAINER_PARTIAL_TARGETS_PROPERTY, partialTargets);
  }
  
  /**
  * Returns the index of the first available showItem child that has its  
  *  'selected' property set to true. 
  * If none of children are selected, the index of first such child that is 
  *  enabled is returned.
  * Returns -1 if both of these fail.
  * 
  * @todo pudupa: XhtmlLafRenderer.getResolvedSelectedIndex() is not a clear
  *   reusable code for 3.0, consolidate for code reuse at a later stage when
  *   the selection model (whether parent has this info or the children) is clear.
  */
  private static int _getResolvedSelectedIndex(
    UIXRenderingContext context,
    UINode parentNode)
  {
    int childCount = parentNode.getIndexedChildCount(context);
    int firstEnabledChildIndex = -1;
    for (int childIndex=0; childIndex<childCount; childIndex++)
    {
      UINode childNode = parentNode.getIndexedChild(context, childIndex);
      
      if (Boolean.TRUE.equals(
            childNode.getAttributeValue(context, DISCLOSED_ATTR)))
      {
        return childIndex;
      }
      if (firstEnabledChildIndex == -1  &&
          !Boolean.TRUE.equals(
            childNode.getAttributeValue(context, DISABLED_ATTR)))
      {
        firstEnabledChildIndex = childIndex;
      }
    }
    return firstEnabledChildIndex;
  }
  
}
