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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ModelRendererUtils;

/**
 * GlobalHeader Renderer for the desktop implementation of the
 * Simple Look And Feel.
 *
 * This is an extension of the base.desktop.GlobalHeaderRenderer which
 * adds the following customizable icons:
 *
 * <ul>
 * <li>af|menuBar::separator-icon: The separator between menuBar items.
 * <li>af|menuBar::leading-separator-icon: The separator icon which appears
 *                                   before the first item.
 * <li>af|menuBar::trailing-separator-icon: The separator icon which appears
 *                                    after the last item.
 * <li>af|menuBar::background-icon: A background image that fills the menuBar
 * <li>af|menuBar::start-icon: An icon which is rendered at the start
 *                          of the menuBar.
 * <li>af|menuBar::end-icon: An icon which is rendered at the end
 *                           of the menuBar.
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/GlobalHeaderRenderer.java#0 $) $Date: 10-nov-2005.18:51:22 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalHeaderRenderer extends SimpleDesktopRenderer
{
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
    )throws IOException
  {
    super.renderAttributes(context, node);

    ResponseWriter writer = context.getResponseWriter();
    renderLayoutTableAttributes(context, "0", null);
    writer.writeAttribute("width", "100%", null);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    super.prerender(context, node);

    // The globalHeader is implemented using two tables -
    // one nested within the other.  The outer table has a single
    // row has three cells with the following contents:
    //
    // 1. The start icon (if specified)
    // 2. The nested table with all of the links
    // 3. The end icon
    //
    // This structure allows us apply a background image across the
    // entire globalHeader (on the outer table) - and also allows
    // us to shift the globalHeader links (ie. the nested table)
    // horizontally depending on the preferred alignment.

    // Start the single row for the outer table
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we have any content, render the opening table cells
    if (_hasContent(context, node))
    {
      // Render the cell with the start icon
      IconData icons = _getIconData(context);
      renderTableDataIcon(context, icons.start, null);

      // Start the cell which contains the globalHeader content.
      // We render the style class and also the background image
      // on this cell
      writer.startElement(TABLE_DATA_ELEMENT, null);
      renderStyleClassAttribute(context, _getContentStyleClass(context, node));

      org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(context, icons.background);
    }
    else
    {
      // If we are completely empty (no children, no title), just
      // start an empty cell
      writer.startElement(TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(HEIGHT_ATTRIBUTE,"1",null);
    }

  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // If we have children, render them - otherwise render the title
    // text if we have any.
    if (!isEmpty(context, node))
    {
      _renderChildren(context, node);
    }
    else
    {
      // Render the title text if we have any
      Object text = _getText(context, node);
      if (text != null)
        _renderTitle(context, text);
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    // Close up the cell which contains the nested table
    ResponseWriter writer = context.getResponseWriter();
    writer.endElement(TABLE_DATA_ELEMENT);

    if (_hasContent(context, node))
    {
      // Render the end icon
      IconData icons = _getIconData(context);
      renderTableDataIcon(context, icons.end, null);
    }

    // Close up the outer table row
    writer.endElement(TABLE_ROW_ELEMENT);

    // Finally, close up the outer table
    super.postrender(context, node);
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
    boolean selected = (index == getResolvedSelectedIndexFromCache(
      context, node));
    renderItemStyleAttrs(context, node, index, selected);

    // Render the tab link
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
    IconData icons = _getIconData(context);

    renderTableDataIcon(context,
                        icons.separator,
                        AF_MENU_BAR_SEPARATOR_STYLE_CLASS);
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
    // We use one of two style classes
    // - af|menuBar: if we have some contents
    // - af|menuBar::empty: if we are totally empty

    if (_hasContent(context, node))
      return AF_MENU_BAR_STYLE_CLASS;

    return AF_MENU_BAR_EMPTY_STYLE_CLASS;
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

 // Tests whether the globalHeader has any children
  protected boolean isEmpty(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (getVisibleIndexedChildCount(context, node) != 0)
      return false;

    UIXNavigationLevel component = (UIXNavigationLevel) node.getUIComponent();

    if ( component != null)
    {
      int rowCount = component.getRowCount();

      if (rowCount > 0 )
        return false;
    }

    return true;
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

  // Render child links if we have any
  private void _renderChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // We're going to render the links inside of a second
    // table - start that table now
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(TABLE_ELEMENT, null);
    renderLayoutTableAttributes(context, "0", null);

    // Start the single row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // Group related links together
    renderRelatedLinksBlockStart(context, "af_menuBar.BLOCK_TITLE");

    // Disabled link default style class rendering so that
    // we don't render .OraLink styles for our links
    LinkUtils.startDefaultStyleClassDisabled(context);

    // Render the leading separator icon
    IconData icons = _getIconData(context);
    renderTableDataIcon(context, icons.leadingSeparator, null);

    UIXHierarchy component = getHierarchyBase(context, node);
    UINode stamp = getStamp(context, node);

    if(stamp != null)
    {

      // Save the current key
      Object oldKey = component.getRowKey();
      boolean isNewPath = setNewPath(context, node, component);
      if (isNewPath)
      {
        int size = component.getRowCount();
        int rowIndex = component.getRowIndex();        
        boolean needsSeparator = false;
        
        for (int i = 0; i < size; i++)
        {
          component.setRowIndex(i);
          boolean isRendered = 
                getBooleanAttributeValue(context, stamp, RENDERED_ATTR, true);
                
          if (isRendered)
          {
            if (needsSeparator)
              renderBetweenNodes(context);
            else
              needsSeparator = true;
              
            renderStamp(context, stamp,i == rowIndex);  
          }
        }        

        if (getVisibleIndexedChildCount(context, node) > 0)
          renderBetweenNodes(context);

        // Restore the old path
        component.setRowKey(oldKey);
      }
    }

    // Now, render our children
    super.renderContent(context, node);

    // Render the trailing separator icon
    renderTableDataIcon(context, icons.trailingSeparator, null);

    // Done grouping links
    renderRelatedLinksBlockEnd(context);

    // Re-enable link default style class rendering
    LinkUtils.endDefaultStyleClassDisabled(context);

    // And close up our row/table
    writer.endElement(TABLE_ROW_ELEMENT);
    writer.endElement(TABLE_ELEMENT);
  }

  // Render the globalHeader's title.  Only called when
  // globalHeader has no children but does have text.
  private void _renderTitle(
    UIXRenderingContext context,
    Object           text
    ) throws IOException
  {
    context.getResponseWriter().writeText(text, null);
  }

  // Gets the style class for the inner content
  private Object _getContentStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // We use one of two style classes for our inner content:
    // - OraGlobalHeaderBody: if we have children
    // - OraGlobalHeaderTitle: if we don't have children but have a title
    if (!isEmpty(context, node))
      return AF_MENU_BAR_BODY_STYLE_CLASS;

    return AF_MENU_BAR_TITLE_STYLE_CLASS;
  }

  // Gets the title text
  private Object _getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // This is called repeatedly, so use a local property
    return SimpleDesktopUtils.getLocalAttribute(context, node, TEXT_ATTR);
  }

  // Checks to see whether we have some content - either
  // some children or a title
  private boolean _hasContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (!isEmpty(context, node) || (_getText(context, node) != null));
  }


  // Retrieve the Icons used by this globalHeader
  private static IconData _getIconData(UIXRenderingContext context)
  {
    // First check for a local property
    IconData icons = (IconData)context.getLocalProperty(0, _ICONS_KEY, null);

    if (icons == null)
    {
      // Next, check to see if the IconData has already been
      // stored on the Skin
      Skin skin = context.getSkin();
      icons = (IconData)skin.getProperty(_ICONS_KEY);

      if (icons == null)
      {
        // If we still haven't found the IconData, create it now
        icons = _createIconData(context);

        // Store the IconData as a property on the Skin,
        // so that we don't have to re-create it next time round
        skin.setProperty(_ICONS_KEY, icons);

        // Store the IconData as a local property, since we'll be
        // looking it up many times for each globalHeader render.
        context.setLocalProperty(_ICONS_KEY, icons);
      }
    }

    return icons;
  }

  // Create the IconData for the specified Skin
  private static IconData _createIconData(
    UIXRenderingContext context)
  {
    Icon start = context.getIcon(AF_MENU_BAR_START_ICON_NAME);
    Icon end = context.getIcon(AF_MENU_BAR_END_ICON_NAME);
    Icon background = context.getIcon(AF_MENU_BAR_BACKGROUND_ICON_NAME);
    Icon separator = context.getIcon(AF_MENU_BAR_SEPARATOR_ICON_NAME);
    Icon leadingSeparator = context.getIcon(AF_MENU_BAR_LEADING_SEPARATOR_ICON_NAME);
    Icon trailingSeparator = context.getIcon(AF_MENU_BAR_TRAILING_SEPARATOR_ICON_NAME);

    return new IconData(start,
                        end,
                        background,
                        separator,
                        leadingSeparator,
                        trailingSeparator);
  }

  private static class IconData
  {
    public final Icon start;
    public final Icon end;
    public final Icon background;
    public final Icon separator;
    public final Icon leadingSeparator;
    public final Icon trailingSeparator;

    public IconData(
      Icon start,
      Icon end,
      Icon background,
      Icon separator,
      Icon leadingSeparator,
      Icon trailingSeparator
      )
    {
      this.start = start;
      this.end = end;
      this.background = background;
      this.separator = separator;
      this.leadingSeparator = leadingSeparator;
      this.trailingSeparator = trailingSeparator;
    }
  }

  // Key used to retrieve Icons from the Skin - and also
  // from a local property.
  private static final Object _ICONS_KEY = new Object();
}
