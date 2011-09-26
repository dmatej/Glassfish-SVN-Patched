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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import java.io.IOException;

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.NavigationPaneRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;


/**
 * On PDA, do not render NavigationPane children in a table. Instead, render
 * them consecutively with non-breaking spaces. Also, renderTabItems as buttons
 */
public class PdaNavigationPaneRenderer extends NavigationPaneRenderer
{
  public PdaNavigationPaneRenderer()
  {
    super();
  }

  @Override
  protected void renderTabItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    boolean             isRtl
    ) throws IOException
  {
    renderNonOverlappingItem(context, rc, rw, itemData, isRtl, false, false);
  }

  @Override
  protected void renderNonOverlappingItem(
    FacesContext        context,
    RenderingContext    rc,
    ResponseWriter      rw,
    Map<String, Object> itemData,
    boolean             isRtl,
    boolean             isBar,
    boolean             isList
    ) throws IOException
  {
    //Pocket IE, IE Mobile and BlackBerry browsers do not support
    //style="display:inine" attribute. Therefore, instead of putting content in
    //columns of a table, render it inside a span with appropriate styling.
    if (!isList)
    {
        rw.startElement("span", null);
        StringBuilder itemStyleClass = new StringBuilder();
        String userStyleClass =
             toString(itemData.get("styleClass"));
        if (userStyleClass != null)
        {
          itemStyleClass.append(userStyleClass);
          itemStyleClass.append(" "); // more style classes are appended below
        }

        // Assign the event handlers:
        boolean isDisabled =
           getBooleanFromProperty(itemData.get("isDisabled"));
        boolean isActive =
             getBooleanFromProperty(itemData.get("isActive"));
        if (isActive)
        {
          if (isDisabled)
          {
            if (isBar)
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_ACTIVE_DISABLED_STYLE_CLASS);
            }
            else
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_DISABLED_STYLE_CLASS);
            }
          }
          else
          {
            if (isBar)
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_ACTIVE_ENABLED_STYLE_CLASS);
            }
            else
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_ACTIVE_ENABLED_STYLE_CLASS);
            }
          }
        }
        else
        {
          if (isDisabled)
          {
            if (isBar)
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_INACTIVE_DISABLED_STYLE_CLASS);
            }
            else
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_DISABLED_STYLE_CLASS);
            }
          }
          else
          {
            if (isBar)
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BAR_INACTIVE_ENABLED_STYLE_CLASS);
            }
            else
            {
              itemStyleClass.append(SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_INACTIVE_ENABLED_STYLE_CLASS);
            }
          }
        }
        renderStyleClass(context, rc, itemStyleClass.toString());

        rw.startElement("span", null); // centerContent
        if (isList)
        {
          renderStyleClass(context, rc,
                             SkinSelectors.AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS);
        }
        else if (isBar)
        {
          renderStyleClass(context, rc,
                           SkinSelectors.AF_NAVIGATION_LEVEL_BAR_CONTENT_STYLE_CLASS);
        }
        else
        {
          renderStyleClass(context, rc,
                           SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_CONTENT_STYLE_CLASS);
        }

        writeInlineStyles(rw, toString(itemData.get("inlineStyle")), null);
        appendIconAndText(context, rc, rw,
                           toString(itemData.get("icon")),
                           itemData, isDisabled, isRtl);
        rw.endElement("span"); // centerContent

        boolean narrowScreen = supportsNarrowScreen(rc);

        if (!getBooleanFromProperty(itemData.get("isLast")))
        {
          rw.startElement("span", null); // rightContent
          if (isBar)
          {
            renderStyleClass(context, rc,
                             SkinSelectors.AF_NAVIGATION_LEVEL_BAR_SEPARATOR_STYLE_CLASS);
          }
          else
          {
            renderStyleClass(context, rc,
                                SkinSelectors.AF_NAVIGATION_LEVEL_BUTTONS_SEPARATOR_STYLE_CLASS);
          }

          // Narrow-screen PDAs don't need"|", since navigation items
          // are rendered vertically for narrow-screen PDAs.
          if (!narrowScreen)
          {
            rw.write("|");
          }

          rw.endElement("span"); // rightContent
        }
        rw.endElement("span"); // rightContent

        // render vertically for narrow-screen PDAs
        if (narrowScreen)
        {
          rw.startElement("br", null);
          rw.endElement("br");
        }
      }
      // Render as List
      else
      {
         rw.startElement("table", null);
         OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
         String appendedStyle = null;
         writeInlineStyles(rw, toString(itemData.get("inlineStyle")),
           appendedStyle); // user's style + what we must have on top of it
         rw.writeAttribute("title", itemData.get("shortDesc"), null);
         StringBuilder itemStyleClass = new StringBuilder();
         String userStyleClass = toString(itemData.get("styleClass"));
         if (userStyleClass != null)
         {
           itemStyleClass.append(userStyleClass);
           itemStyleClass.append(" "); // more style classes are appended below
         }

         // Assign the event handlers:
         boolean isDisabled = getBooleanFromProperty(itemData.get("isDisabled"));
         boolean isActive = getBooleanFromProperty(itemData.get("isActive"));
         if (isActive)
         {
           if (isDisabled)
           {
             itemStyleClass.append(
                                   SkinSelectors.AF_NAVIGATION_LEVEL_LIST_ACTIVE_DISABLED_STYLE_CLASS);
           }
           else
           {
             itemStyleClass.append(
                 SkinSelectors.AF_NAVIGATION_LEVEL_LIST_ACTIVE_ENABLED_STYLE_CLASS);
           }
         }
         else
         {
           if (isDisabled)
           {
             itemStyleClass.append(
                 SkinSelectors.AF_NAVIGATION_LEVEL_LIST_INACTIVE_DISABLED_STYLE_CLASS);
           }
           else
           {
             itemStyleClass.append(
                 SkinSelectors.AF_NAVIGATION_LEVEL_LIST_INACTIVE_ENABLED_STYLE_CLASS);
           }
         }
         renderStyleClass(context, rc, itemStyleClass.toString());
         rw.startElement("tbody", null);
         rw.startElement("tr", null);
         rw.startElement("td", null); // bulletCell
         renderStyleClass(
            context,
            rc,
            SkinSelectors.AF_NAVIGATION_LEVEL_LIST_BULLET_STYLE_CLASS);
         rw.startElement("div", null); // bulletContent
         rw.write(" ");
         rw.endElement("div"); // bulletContent
         rw.endElement("td"); // bulletCell
         rw.startElement("td", null); // centerCell
         rw.startElement("div", null); // centerContent
         renderStyleClass(context, rc,
           SkinSelectors.AF_NAVIGATION_LEVEL_LIST_CONTENT_STYLE_CLASS);

         appendIconAndText(
            context,
            rc,
            rw,
            toString(itemData.get("icon")),
            itemData,
            isDisabled,
            isRtl);
         rw.endElement("div"); // centerContent
         rw.endElement("td"); // centerCell
         rw.endElement("tr");
         rw.endElement("tbody");
         rw.endElement("table");
      }
  }
}
