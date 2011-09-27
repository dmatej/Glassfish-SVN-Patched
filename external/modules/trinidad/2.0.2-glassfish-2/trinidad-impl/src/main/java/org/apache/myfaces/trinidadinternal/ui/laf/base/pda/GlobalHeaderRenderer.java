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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;

import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ModelRendererUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * Renderer for global headers
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/GlobalHeaderRenderer.java#0 $) $Date: 10-nov-2005.18:54:56 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalHeaderRenderer extends XhtmlLafRenderer
{
  @Override
  public void render(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    if ( getVisibleIndexedChildCount(context, node) < 1 )
    {
      ResponseWriter writer = context.getResponseWriter();

      // write out the horizontal rule
      writer.startElement("hr", node.getUIComponent());
      super.renderAttributes(context, node);
      writer.writeAttribute("size", "2", null);
      writer.endElement("hr");
      return;
    }

    super.render(context, node);
  }

  @Override
  protected void renderContent(
      UIXRenderingContext context,
      UINode           node
      ) throws IOException
    {
      boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);

      UIXHierarchy component = getHierarchyBase(context, node);
      UINode stamp = getStamp(context, node);

      if(stamp != null)
      {

        // Save the current key
        Object oldPath = component.getRowKey();
        boolean isNewPath = setNewPath(context, node, component);

        if (isNewPath)
        {
          int size = component.getRowCount();
          int rowIndex = component.getRowIndex();

          for (int i = 0; i < size; i++)
          {
            component.setRowIndex(i);
            renderStamp(context, stamp,i == rowIndex);

            if ( i < (size - 1))
              renderBetweenIndexedChildren(context,node);

          }

          if (getVisibleIndexedChildCount(context, node) > 0)
            renderBetweenIndexedChildren(context,node);

          // Restore the old path
          component.setRowKey(oldPath);
        }
      }

      super.renderContent(context, node);
      //Reset the selected status, which might have been changed on rendering
      //  indexed children.
      LinkUtils.setSelected(context, initialLinkSelectedStatus);
    }

  protected void renderStamp(
     UIXRenderingContext context,
     UINode           stamp,
     boolean          selected
     )throws IOException
   {
     LinkUtils.setSelected(context,selected);
     if (selected)
     {
       context.setLocalProperty(_SELECTED_KEY, Boolean.TRUE);
       renderChild(context, stamp);
       context.setLocalProperty(_SELECTED_KEY, null);
     }
     else
       renderChild(context, stamp);
   }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return TABLE_ELEMENT;
  }

  @Override
 protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    super.renderAttributes(context, node);
    renderLayoutTableAttributes(context, "0", "100%");


    if ( renderStyleElements(context))
    {

      Style classStyle = XhtmlLafUtils.getClassStyle(context,
                                                     _GLOBAL_HEADER_STYLE_CLASS);

      if (classStyle != null)
      {
        ResponseWriter writer = context.getResponseWriter();
        

        // write the cell's background color property
        writer.writeAttribute("bgcolor",
                              classStyle.getProperties().get("background-color"), null);
      }
    }
    else
      renderStyleClassAttribute(context, _GLOBAL_HEADER_STYLE_CLASS);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    // renderBlockWrapperStart(context, node.getLocalName());
    super.prerender(context, node);

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("tr", null);
    writer.startElement("td", null);

    if ( renderStyleElements(context))
    {
      startRenderingStyleElements(context, null, _GLOBAL_HEADER_STYLE_CLASS);
    }

  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {

    if ( renderStyleElements(context))
    {
      XhtmlLafUtils.endRenderingStyleElements(context);
    }

    ResponseWriter writer = context.getResponseWriter();

    writer.endElement("td");
    writer.endElement("tr");

    super.postrender(context, node);
    // renderBlockWrapperEnd(context);
  }

  @Override
  protected void renderChild(
      UIXRenderingContext context,
      UINode           child
      ) throws IOException
    {
      ResponseWriter writer = context.getResponseWriter();
      boolean selected = _isSelected(context, child);
      if ( selected)
      {
        if ( renderStyleElements(context))
        {
          writer.startElement("b", null);
          super.renderChild(context, child);
          writer.endElement("b");
        }
        else
        {
          writer.startElement("span", null);
          renderStyleClassAttribute(
            context,
            BaseDesktopConstants.AF_MENU_BAR_SELECTED_STYLE_CLASS);
          super.renderChild(context, child);
          writer.endElement("span");
        }
      }
      else
      {
        if ( renderStyleElements(context))
        {
          super.renderChild(context, child);
        }
        else
        {
          writer.startElement("span", null);
          renderStyleClassAttribute(
            context,
            BaseDesktopConstants.AF_MENU_BAR_ENABLED_STYLE_CLASS);
          super.renderChild(context, child);
          writer.endElement("span");
        }
      }
  }

  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              index
    )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    boolean selected = (index == getResolvedSelectedIndexFromCache(
      context, node));

     if ( selected)
    {
      if ( renderStyleElements(context))
      {
        writer.startElement("b", null);

        // Render the tab link
        super.renderIndexedChild( context, node, index);

        writer.endElement("b");
      }
      else
      {
        writer.startElement("span", null);
        renderStyleClassAttribute(
          context,
          BaseDesktopConstants.AF_MENU_BAR_SELECTED_STYLE_CLASS);
        // Render the tab link
        super.renderIndexedChild( context, node, index);
        writer.endElement("span");
      }
    }
    else
    {
      if ( renderStyleElements(context))
      {
        // Render the tab link
        super.renderIndexedChild( context, node, index);
      }
      else
      {
        writer.startElement("span", null);
        renderStyleClassAttribute(
          context,
          BaseDesktopConstants.AF_MENU_BAR_ENABLED_STYLE_CLASS);
        // Render the tab link
        super.renderIndexedChild( context, node, index);
        writer.endElement("span");
      }
    }

  }

  private boolean _isSelected(
          UIXRenderingContext context,
          UINode           node
          )
  {
    Object selectedAttr  = node.getAttributeValue(context, SELECTED_ATTR);

    if (selectedAttr == null)
    {
      selectedAttr = context.getLocalProperty(0, _SELECTED_KEY, null);
      if (selectedAttr != null)
        return true;

      return false;
    }

    return ((Boolean)selectedAttr).booleanValue();
  }

  protected UIXHierarchy getHierarchyBase(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (UIXHierarchy) node.getUIComponent();
  }


  protected UINode getStamp(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getNamedChild(context, NODE_STAMP_CHILD);
  }


  protected boolean setNewPath(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy    component
  )
  {
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    return ModelRendererUtils.setNewPath(component, startDepth,
                                         ((UIXNavigationLevel)component).getFocusRowKey());
  }

  @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.writeText("\u00a0|\u00a0", null);
  }

  // globalHeader style class
  private static final String _GLOBAL_HEADER_STYLE_CLASS = "p_OraGlobalHeader";
  private static final Object _SELECTED_KEY = new Object();
}
