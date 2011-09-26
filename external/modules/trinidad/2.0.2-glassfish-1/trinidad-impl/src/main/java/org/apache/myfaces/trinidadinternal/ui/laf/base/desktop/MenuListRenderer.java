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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ModelRendererUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;


/**
 * Renderer for outputting lists with a number, bullet, letter, etc in front
 * of each child. Works in conjuntion with styledItem.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/MenuListRenderer.java#0 $) $Date: 10-nov-2005.18:55:24 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MenuListRenderer extends HtmlLafRenderer
{
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
  )throws IOException
  {

    super.prerender( context, node);

    //TODO - gc - do we still need this hideShow inline
    // and selected code in adf faces?

    // set the selected property to false
    StyledItemUtils.setSelected(context, false);




    ResponseWriter writer = context.getResponseWriter();
    String style = BaseDesktopUtils.getStringAttributeValue(context,
                                                     node,
                                                     LIST_STYLE_ATTR);

    if ( style == null)
      style = LIST_STYLE_DISC;

    if (LIST_STYLE_LOWER_ALPHA.equals(style) ||
        LIST_STYLE_UPPER_ALPHA.equals(style) ||
        LIST_STYLE_DECIMAL.equals(style)
    )
    {
      writer.startElement( "ol", node.getUIComponent() );
      super.renderAttributes(context, node);
      XhtmlLafRenderer.renderStyleClassAttribute(context, STYLED_LIST_STYLE_CLASS);

      if ( LIST_STYLE_LOWER_ALPHA.equals(style))
        style = _LOWER_ALPHA;
      else if (LIST_STYLE_UPPER_ALPHA.equals(style))
        style = _UPPER_ALPHA;

      writer.writeAttribute( STYLE_ATTRIBUTE,  _LIST_STYLE_TYPE + style, null);
    }
    else
    {
      writer.startElement( "ul", node.getUIComponent()  );
      super.renderAttributes(context, node);
      XhtmlLafRenderer.renderStyleClassAttribute(context, STYLED_LIST_STYLE_CLASS);

      StringBuffer inlineStyleBuffer = null;

      // =-=gc Seems like a bug on ie that when list-style is none, it
      // still indents like there's a bullet. On mozilla it's not
      // indenting. Which browser is right?
      // For now overriding the list-style-position in an inline style
      // on ie
      if ( LIST_STYLE_NONE.equals(style) && isIE(context))
      {
        inlineStyleBuffer = new StringBuffer( _LIST_STYLE_TYPE.length() +
                                              style.length() +
                                              _LIST_STYLE_POSITION.length());
        inlineStyleBuffer.append(_LIST_STYLE_TYPE).append(style);
        inlineStyleBuffer.append(_LIST_STYLE_POSITION);
      }
      else if (style != null )
      {
        inlineStyleBuffer = new StringBuffer( _LIST_STYLE_TYPE.length() +
                                              style.length());
        inlineStyleBuffer.append(_LIST_STYLE_TYPE).append(style);

      }

      if ( inlineStyleBuffer != null && inlineStyleBuffer.length() > 0 )
        writer.writeAttribute("style", inlineStyleBuffer.toString(), null);
    }
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
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

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
          boolean isRendered = 
              getBooleanAttributeValue(context, stamp, RENDERED_ATTR, true);
              
          if (isRendered)
            renderNode(context, stamp,i == rowIndex);
        }

        // Restore the old path
        component.setRowKey(oldPath);
      }
    }

    super.renderContent(context, node);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
  )throws IOException
  {
    ResponseWriter writer = context.getFacesContext().getResponseWriter();

    String style = BaseDesktopUtils.getStringAttributeValue(context,
                                                     node,
                                                     LIST_STYLE_ATTR);
    if (LIST_STYLE_LOWER_ALPHA.equals(style) ||
        LIST_STYLE_UPPER_ALPHA.equals(style) ||
        LIST_STYLE_DECIMAL.equals(style)
    )
    {
      writer.endElement("ol");
    }
    else
    {
      writer.endElement("ul");
    }

    super.postrender( context, node);
  }

  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              childIndex
    ) throws IOException
  {

    UINode child = node.getIndexedChild(context, childIndex);
    boolean selected = isSelected(context, child);

    renderNode(context, child, selected);
  }

  protected void renderNode(
    UIXRenderingContext context,
    UINode           node,
    boolean          selected
    ) throws IOException
  {
    if (selected )
    {
      // set the selected property to true
      StyledItemUtils.setSelected(context, true);
    }

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement( LIST_ITEM_ELEMENT, null );
    writer.startElement(SPAN_ELEMENT, null);
    if ( StyledItemUtils.isSelected(context, node) )
      renderStyleClassAttribute(context, NAV_3_SELECTED_STYLE_CLASS);
    else
      renderStyleClassAttribute(context, NAV_3_STYLE_CLASS);

    boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);

    node.render(context);

    //Reset the selected status, which might have been changed on rendering
    //  indexed children.
    LinkUtils.setSelected(context, initialLinkSelectedStatus);

    writer.endElement(SPAN_ELEMENT);
    writer.endElement( LIST_ITEM_ELEMENT );

    // set the selected property to false
    StyledItemUtils.setSelected(context, false);
  }


  private static final String _LIST_STYLE_POSITION =
             ";list-style-position:outside";
  private static final String _LIST_STYLE_TYPE = "list-style-type:";
  private static final String _LOWER_ALPHA = "lower-alpha";
  private static final String _UPPER_ALPHA = "upper-alpha";
}
