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

import org.apache.myfaces.trinidad.component.UIXNavigationLevel;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.style.Style;

/**
 * GlobalHeader Renderer for the desktop implementation of the
 * Base Look And Feel.
 *
 * The base.desktop.GlobalHeaderRenderer exposes a single customizable
 * icon:
 * <ul>
 * <li>af|menuBar::separator-icon: The separator between global header items.
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/GlobalHeaderRenderer.java#0 $) $Date: 10-nov-2005.18:55:16 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalHeaderRenderer extends HtmlLafRenderer
{
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT;
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    super.renderAttributes(context, node);

    ResponseWriter writer = context.getResponseWriter();
    writer.writeAttribute("width", "100%", null);    
  }  

  /**
   *
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {   
    renderRelatedLinksBlockStart(context, "af_menuBar.BLOCK_TITLE");


    super.prerender(context, node);

    if (isEmpty(context, node))
    {
      renderEmptyGlobalHeader(context, node);
    }
    else
    {
      // Disabled link default style class rendering
      LinkUtils.startDefaultStyleClassDisabled(context);

      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(TABLE_ELEMENT, null);
      renderLayoutTableAttributes(context, "0", null);
      writer.startElement(TABLE_ROW_ELEMENT, null);

      // Get the separator icon and store it away for later
      Icon icon = context.getIcon(AF_MENU_BAR_SEPARATOR_ICON_NAME);
      context.setLocalProperty(_SEPARATOR_ICON_KEY, icon);
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);
    
    UIXNavigationLevel component = (UIXNavigationLevel) node.getUIComponent();     
     
    UINode stamp = node.getNamedChild(context, NODE_STAMP_CHILD);
    
    if(stamp != null)
    { 

      // Save the current key
      Object oldKey = component.getRowKey();      
      int size = component.getRowCount();
      int rowIndex = component.getRowIndex();
      
      for (int i = 0; i < size; i++)
      {
        component.setRowIndex(i);
        renderStamp(context, stamp,i == rowIndex);
        
        if ( i < (size - 1))
          renderBetweenNodes(context);
      }
      
      if (getVisibleIndexedChildCount(context, node) > 0)
        renderBetweenNodes(context);
      
      // Restore the old path
      component.setRowKey(oldKey);

    }
    
    super.renderContent(context, node);
    //Reset the selected status, which might have been changed on rendering
    //  indexed children.
    LinkUtils.setSelected(context, initialLinkSelectedStatus);
  }

  /**
   *
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  { 
    if (!isEmpty(context, node))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.endElement(TABLE_ROW_ELEMENT);         
      writer.endElement(TABLE_ELEMENT);

      // Re-enable link default style class rendering
      LinkUtils.endDefaultStyleClassDisabled(context);
    }

    super.postrender(context, node);

    renderRelatedLinksBlockEnd(context);
  }
  
  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              index
    )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start the table cell
    writer.startElement(TABLE_DATA_ELEMENT, null);

    // Render style attributes
    boolean selected = (index == getResolvedSelectedIndexFromCache(context, node));
    renderItemStyleAttrs(context, node, index, selected);

    //Record the selected status so that in case this child happens to be 
    //  a link, some special aspects like accessibility are taken care of.
    LinkUtils.setSelected(context, selected);

    // Render the global header link
    super.renderIndexedChild( context, node, index);

    writer.endElement(TABLE_DATA_ELEMENT);
  }  

    protected void renderStamp(
    UIXRenderingContext context,
    UINode           stamp,
    boolean          selected
    )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start the table cell
    writer.startElement(TABLE_DATA_ELEMENT, null);
    // todo - the index isn't used so putting -1 for now
    renderItemStyleAttrs(context, stamp, -1, selected);

    //Record the selected status so that in case this child happens to be 
    //  a link, some special aspects like accessibility are taken care of.
    LinkUtils.setSelected(context, selected);

    stamp.render(context);

    writer.endElement(TABLE_DATA_ELEMENT);
  }  

  /**
   * Override of renderBetweenIndexedChildren() which 
   * renders the separator Icon.
   */
    @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderBetweenNodes(context);
  }

/**
   * Override of renderBetweenIndexedChildren() which 
   * renders the separator Icon.
   */
  protected void renderBetweenNodes(
    UIXRenderingContext context
    ) throws IOException
  {
    // Get the separator icon and render it in a table cell
    Icon icon = getSeparatorIcon(context);

    renderTableDataIcon(context, icon, AF_MENU_BAR_SEPARATOR_STYLE_CLASS);
  }

  /**
   * Override of getStyleClass() which forces style class
   * to OraGlobalHeader.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return AF_MENU_BAR_STYLE_CLASS;
  }

  /**
   * Renders the style attributes for global header items
   */
  protected void renderItemStyleAttrs(
    UIXRenderingContext context,
    UINode           node,
    int              index,
    boolean          selected
    ) throws IOException
  {
    String styleClass = null;

    if (selected)
      styleClass = AF_MENU_BAR_SELECTED_STYLE_CLASS;
    else
      styleClass = AF_MENU_BAR_ENABLED_STYLE_CLASS;

    renderStyleClassAttribute(context, styleClass);
  }

  /**
   * Returns the separator Icon
   */
  protected Icon getSeparatorIcon(UIXRenderingContext context)
  {
    return (Icon)context.getLocalProperty(0, _SEPARATOR_ICON_KEY, null);
  }

  /**
   * Checks to see whether the globalHeader is empty (contains no
   * indexed children).
   */
  protected boolean isEmpty(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (getVisibleIndexedChildCount(context, node) != 0)
      return false;
      
          
    UIXNavigationLevel component = (UIXNavigationLevel) node.getUIComponent();   
    int rowCount = component.getRowCount();      
    
    if (rowCount > 0 )
      return false;
      
    return true;
  }

  /**
   * Renders the empty global header
   */
  protected void renderEmptyGlobalHeader(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object text = node.getAttributeValue(context, TEXT_ATTR);

    if (text != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText(text, TEXT_ATTR.getAttributeName());
    }
    else if (isIE(context))
    {
      Style style =XhtmlLafUtils.getClassStyle(context, 
                                              AF_MENU_BAR_STYLE_CLASS);
      if (style != null)
      {
        String minHeight = style.getProperties().get("min-height");
        renderSpacer(context, null, minHeight);
      }
    }
  }
 
  // Key for local property which holds the separator icon
  private static final Object _SEPARATOR_ICON_KEY = new Object();
}
