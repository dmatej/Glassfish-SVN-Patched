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

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * MessageBox Renderer for the desktop implementation of the
 * Simple Look And Feel.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/MessageBoxRenderer.java#0 $) $Date: 10-nov-2005.18:51:23 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MessageBoxRenderer
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.MessageBoxRenderer
  implements SimpleDesktopConstants
{
  // =-=ags Warning: This class contains code which was copied
  //        from SideBarRenderer.  I didn't have time
  //        to refactor this code into a common support class -
  //        we should try to merge the code from SideBarRenderer,
  //        ContentContainerRenderer and MessageRenderer into
  //        a common code base to make this more maintainable.

  /**
   * Implementation of ElementRenderer.getName();
   */
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return TABLE_ELEMENT;
  }

  /**
   * Override of BaseRenderer.renderAttributes().
   */
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    super.renderAttributes(context, node);

    renderLayoutTableAttributes(context, "0", "100%");
  }

  /**
   * Override of BaseRenderer.prerender()
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    super.prerender(context, node);

    // Get the Icon objects that we'll use to render this messageBox.
    IconData icons = _getIconData(context);
    int columnCount = _getColumnCount(icons);
    
    FacesContext fContext = context.getFacesContext();
    RenderingContext arc = RenderingContext.getCurrentInstance();
    // Render the top row if we have one
    if (_hasTopRow(icons))
      _renderTopRow(fContext, arc, icons, columnCount);

    // Start the contents row
    _startContentsRow(context, fContext, arc, icons, columnCount);
  }

  /**
   * Override of BaseRenderer.postrender()
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    // Get the Icon objects that we'll use to render this messageBox.
    IconData icons = _getIconData(context);
    int columnCount = _getColumnCount(icons);
    
    FacesContext fContext = context.getFacesContext();
    RenderingContext arc = RenderingContext.getCurrentInstance();
    
    // Close up the contents row
    _endContentsRow(fContext, arc, icons);

    // Render the bottom row if we have one
    if (_hasBottomRow(icons))
      _renderBottomRow(fContext, arc, icons, columnCount);

    super.postrender(context, node);
  }

  // Start the table row with the contents
  private void _startContentsRow(
    UIXRenderingContext context,
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    writer.startElement(TABLE_ROW_ELEMENT, null);

    if (icons.startBackground != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.startBackground);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderStyleClassAttribute(context, AF_MESSAGES_BODY_STYLE_CLASS);

    // Write colspan for the body cell.  The body cell takes up
    // space for any empty icon columns.
    Integer bodyColumnCount = _getBodyColumnCount(icons, columnCount);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, bodyColumnCount, null);
  }

  // End the table row with the contents
  private void _endContentsRow(
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    writer.endElement(TABLE_DATA_ELEMENT);

    if (icons.endBackground != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.endBackground);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Tests whether this contentContainer has a bottom row
  private boolean _hasBottomRow(IconData icons)
  {
    // We have a bottom row if we have a bottom start/end icon
    return ((icons.bottomStart != null) || (icons.bottomEnd != null));
  }

  // Renders the bottom row
  private void _renderBottomRow(
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    // Render the contents inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we've got a start icon, render it
    if (icons.bottomStart != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.bottomStart, "", null);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the cell with the bottom background icon.  We first
    // need to determine how many columns the background cell should
    // fill.
    Integer colspan = _getBottomBackgroundColumnCount(icons, columnCount);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, colspan, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, "100%", null);
    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.bottomBackground);
    writer.endElement(TABLE_DATA_ELEMENT);

    // If we've got an end icon, render it
    if (icons.bottomEnd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.bottomEnd, "", null);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Tests whether this contentContainer has a top row
  private boolean _hasTopRow(IconData icons)
  {
    // We have a top row if we have a top start/end icon
    return ((icons.topStart != null) || (icons.topEnd != null));
  }

  // Renders the top row
  private void _renderTopRow(
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    // Render the contents inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we've got a start icon, render it
    if (icons.topStart != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.topStart, "", null);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the cell with the top background icon.  We first
    // need to determine how many columns the background cell should
    // fill.
    Integer colspan = _getTopBackgroundColumnCount(icons, columnCount);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, colspan, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, "100%", null);
    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.topBackground);
    writer.endElement(TABLE_DATA_ELEMENT);

    // If we've got an end icon, render it
    if (icons.topEnd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.topEnd, "", null);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Gets the number of columns that this sideBar renders
  private int _getColumnCount(
    IconData         icons
    )
  {
    int columnCount = 1;

    if ((icons.bottomStart != null)  ||
        (icons.topStart != null)     ||
        (icons.startBackground != null))
      columnCount++;

    if ((icons.bottomEnd != null) ||
        (icons.bottomEnd != null) ||
        (icons.endBackground != null))
      columnCount++;

    return columnCount;
  }

  // Returns the number of columns that the body cell should occupy
  private static Integer _getBodyColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int bodyColumnCount = columnCount;

    // If we have a start background icon, leave room for it
    if (icons.startBackground != null)
      bodyColumnCount--;

    // If we have an end background icon, leave room for it
    if (icons.endBackground != null)
     bodyColumnCount--;

    if (bodyColumnCount == 1)
      return null;

    return bodyColumnCount;
  }

  // Returns the number of columns for the bottom background cell
  private static Integer _getBottomBackgroundColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int backgroundColumnCount = columnCount;

    if (icons.bottomStart != null)
      backgroundColumnCount--;

    if (icons.bottomEnd != null)
      backgroundColumnCount--;

    if (backgroundColumnCount == 1)
      return null;

    return backgroundColumnCount;
  }

  // Returns the number of columns for the top background cell
  private static Integer _getTopBackgroundColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int backgroundColumnCount = columnCount;

    if (icons.topStart != null)
      backgroundColumnCount--;

    if (icons.topEnd != null)
      backgroundColumnCount--;

    if (backgroundColumnCount == 1)
      return null;

    return backgroundColumnCount;
  }

  // Get the IconData to use for rendering this sideBar
  private IconData _getIconData(
    UIXRenderingContext context
    )
  {
    // Check to see whether we have already created
    // the IconData for this background color
    Skin skin = context.getSkin();
    IconData icons    = (IconData)skin.getProperty(_ICONS_KEY);
    Icon bottomStart  = context.getIcon(AF_MESSAGES_BOTTOM_START_ICON_NAME);
    // Add check to see if cached icon is same as icon required for a composite
    // context. Else re-create
    if (icons == null || bottomStart == null ||!(icons.bottomStart.equals(bottomStart)))
    {
      // If we haven't created the IconData yet, create it now

      Icon bottomEnd        = context.getIcon(
                                    AF_MESSAGES_BOTTOM_END_ICON_NAME);
      Icon bottomBackground = context.getIcon(
                                    AF_MESSAGES_BOTTOM_BACKGROUND_ICON_NAME);
      Icon topStart         = context.getIcon(
                                    AF_MESSAGES_TOP_START_ICON_NAME);
      Icon topEnd           = context.getIcon(
                                    AF_MESSAGES_TOP_END_ICON_NAME);
      Icon topBackground    = context.getIcon(
                                    AF_MESSAGES_TOP_BACKGROUND_ICON_NAME);
      Icon startBackground  = context.getIcon(
                                    AF_MESSAGES_START_BACKGROUND_ICON_NAME);
      Icon endBackground    = context.getIcon(
                                    AF_MESSAGES_END_BACKGROUND_ICON_NAME);

      icons = new IconData(bottomStart,
                           bottomEnd,
                           bottomBackground,
                           topStart,
                           topEnd,
                           topBackground,
                           startBackground,
                           endBackground);

      // Stash away the IconData so that we don't have to re-create
      // it on the next render
      skin.setProperty(_ICONS_KEY, icons);
    }

    return icons;
  }

  // A class that we use for storing Icon-related info
  private static class IconData
  {
    public final Icon bottomStart;
    public final Icon bottomEnd;
    public final Icon bottomBackground;
    public final Icon topStart;
    public final Icon topEnd;
    public final Icon topBackground;
    public final Icon startBackground;
    public final Icon endBackground;

    public IconData(
      Icon bottomStart,
      Icon bottomEnd,
      Icon bottomBackground,
      Icon topStart,
      Icon topEnd,
      Icon topBackground,
      Icon startBackground,
      Icon endBackground
      )
    {
      this.bottomStart = bottomStart;
      this.bottomEnd = bottomEnd;
      this.bottomBackground = bottomBackground;
      this.topStart = topStart;
      this.topEnd = topEnd;
      this.topBackground = topBackground;
      this.startBackground = startBackground;
      this.endBackground = endBackground;
    }
  }

  // Keys for looking up IconData properties on the Skin
  private static final Object _ICONS_KEY = new Object();
}
