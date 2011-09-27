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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;


/**
 * Tab bar Renderer for the desktop implementation of the
 * Simple Skin.
 *
 * This is an extension of the Base Skin tab bar which
 * adds the following customizable icons:
 *
 * <ul>
 * <li>af|menuTabs::enabled-start: The start of an unselected tab
 * <li>af|menuTabs::selected-start: The start of a selected tab
 * <li>af|menuTabs::enabled-end: The end of an unselected tab
 * <li>af|menuTabs::selected-end: The end of a selected tab
 * <li>af|menuTabs::enabled-join: The join between two unselected tabs
 * <li>af|menuTabs::enabled-selected-join: A join between an unselected and selected tab
 * <li>af|menuTabs::selected-enabled-join: A join between a selected and unselected
 * <li>af|menuTabs::enabled-background: A background image that fills unselected tabs
 * <li>af|menuTabs::selected-background: A background image that fills selected tabs
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/TabBarRenderer.java#0 $) $Date: 10-nov-2005.18:51:28 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class TabBarRenderer
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.TabBarRenderer
  implements SimpleDesktopConstants

{
  @Override
  protected void renderNode(
    UIXRenderingContext context,
    UINode           child,
    boolean          selected,
    boolean          isFirst,
    boolean          isLast,
    boolean          isNextSelected
    )throws IOException
  {
      // Get the icons array
    IconData icons = _getIconData(context);
    assert (icons != null);


    // We only need to render an icon before the link if
    // we don't have a previous sibling.  (Joins are
    // rendered by the previous sibling).
    if (isFirst)
    {
      Icon startIcon = selected ? icons.selectedStart : icons.enabledStart;

      renderTableDataIcon(context, startIcon, null);
    }

    renderNode(context, child, selected);

    // Either render a join or an end icon depending
    // on whether we have any more siblings
    Icon endIcon = null;

    if (isLast)
    {
      endIcon = selected ? icons.selectedEnd : icons.enabledEnd;
    }
    else
    {
      if (selected)
        endIcon = icons.selectedEnabledJoin;
      else if (isNextSelected)
        endIcon = icons.enabledSelectedJoin;
      else
        endIcon = icons.enabledJoin;
    }

    renderTableDataIcon(context, endIcon, null);
  }  

  /**
   * Override of renderTabStyleAttrs() which adds support
   * for a background image
   */
  @Override
  protected void renderTabStyleAttrs(
    UIXRenderingContext context,
    UINode           node,
    boolean          selected,
    boolean          disabled
    ) throws IOException
  {
    // Render the style class
    super.renderTabStyleAttrs(context, node, selected, disabled);

    // Render the background icon, if we have one
    IconData icons = _getIconData(context);
    Icon icon = (selected && !disabled) ?
                  icons.selectedBackground :
                  icons.enabledBackground;

    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(context, icon);
  }

  /**
   * Override of the base.desktop.TabBarRenderer's
   * renderBetweenNodes().
   */
  @Override
  protected void renderBetweenNodes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // We don't want to render anything between indexed children.
    // We render joins in renderIndexedChild().
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
      Icon enabledStart = context.getIcon(
                                    AF_MENU_TABS_ENABLED_START_ICON_NAME);
   
      if (icons == null || enabledStart == null 
                        || !enabledStart.equals(icons.enabledStart))
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
  private static IconData _createIconData(UIXRenderingContext context)
  {
    Icon enabledStart = context.getIcon(
                                    AF_MENU_TABS_ENABLED_START_ICON_NAME);
    Icon enabledEnd = context.getIcon(
                                  AF_MENU_TABS_ENABLED_END_ICON_NAME);
    Icon selectedStart = context.getIcon(
                                    AF_MENU_TABS_SELECTED_START_ICON_NAME);
    Icon selectedEnd = context.getIcon(
                                  AF_MENU_TABS_SELECTED_END_ICON_NAME);
    Icon enabledJoin = context.getIcon(
                                   AF_MENU_TABS_ENABLED_JOIN_ICON_NAME);
    Icon enabledSelectedJoin = context.getIcon(
                                      AF_MENU_TABS_ENABLED_SELECTED_JOIN_ICON_NAME);
    Icon selectedEnabledJoin = context.getIcon(
                                      AF_MENU_TABS_SELECTED_ENABLED_JOIN_ICON_NAME);
    Icon enabledBackground = context.getIcon(
                                         AF_MENU_TABS_ENABLED_BACKGROUND_ICON_NAME);
    Icon selectedBackground = context.getIcon(
                                        AF_MENU_TABS_SELECTED_BACKGROUND_ICON_NAME);
    return new IconData(enabledStart,
                        enabledEnd,
                        selectedStart,
                        selectedEnd,
                        enabledJoin,
                        enabledSelectedJoin,
                        selectedEnabledJoin,
                        enabledBackground,
                        selectedBackground);
  }

  private static class IconData
  {
    public final Icon enabledStart;
    public final Icon enabledEnd;
    public final Icon selectedStart;
    public final Icon selectedEnd;
    public final Icon enabledJoin;
    public final Icon enabledSelectedJoin;
    public final Icon selectedEnabledJoin;
    public final Icon enabledBackground;
    public final Icon selectedBackground;

    public IconData(
      Icon enabledStart,
      Icon enabledEnd,
      Icon selectedStart,
      Icon selectedEnd,
      Icon enabledJoin,
      Icon enabledSelectedJoin,
      Icon selectedEnabledJoin,
      Icon enabledBackground,
      Icon selectedBackground
      )
    {
      this.enabledStart = enabledStart;
      this.enabledEnd = enabledEnd;
      this.selectedStart = selectedStart;
      this.selectedEnd = selectedEnd;
      this.enabledJoin = enabledJoin;
      this.enabledSelectedJoin = enabledSelectedJoin;
      this.selectedEnabledJoin = selectedEnabledJoin;
      this.enabledBackground = enabledBackground;
      this.selectedBackground = selectedBackground;
    }
  }

  // Key used to retrieve Icons from the Skin - and also
  // from a local property.
  private static final Object _ICONS_KEY = new Object();
}
