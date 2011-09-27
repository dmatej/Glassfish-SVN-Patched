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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelPageHeader;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;


/**
 *  @version $Header: DesktopPanelPageHeaderRenderer.java 15-nov-2005.19:26:45 dosterbe Exp $
 *  made jsf major by gcrawfor
 */

 // NEED TO CHANGE SOME CODE IN TABBARRENDERER WHEN YOU TURN ON THIS CLASS.
 // SEARCH FOR "FACES-MAJOR PANELPAGEHEADER" IN
 // org.apache.myfaces.trinidadinternal.ui.laf.oracle.desktop.TabBarRenderer
public class DesktopPanelPageHeaderRenderer extends XhtmlRenderer
{
  public DesktopPanelPageHeaderRenderer()
  {
    super(CorePanelPageHeader.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _chromeTypeKey = type.findKey("chromeType");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {

   // =-=AEW We don't have any real agent switching
    if (!isDesktop(rc))
    {
      delegateRenderer(context, rc, component, bean, _pdaRenderer);
    }
    else
    {
      encodeAllDesktop(context, rc, component, bean);
    }

  }

  protected void encodeAllDesktop(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {

    UIComponent branding        = getFacet(component,
                                           CorePanelPageHeader.BRANDING_FACET);
    UIComponent brandingApp     = getFacet(component,
                                       CorePanelPageHeader.BRANDING_APP_FACET);
    UIComponent brandingAppCont = getFacet(component,
                            CorePanelPageHeader.BRANDING_APP_CONTEXTUAL_FACET);
    UIComponent navigationGlobal      = getFacet(component,
                                        CorePanelPageHeader.NAVIGATION_GLOBAL_FACET);
    UIComponent navigation1           = getFacet(component,
                                           CorePanelPageHeader.NAVIGATION1_FACET);
    UIComponent navigation2           = getFacet(component,
                                           CorePanelPageHeader.NAVIGATION2_FACET);
    UIComponent menuSwitch      = getFacet(component,
                                        CorePanelPageHeader.MENU_SWITCH_FACET);
    UIComponent search          = getFacet(component,
                                           CorePanelPageHeader.SEARCH_FACET);

    // chromeType is an attribute on panelPageLayout and panelPageHeader.
    // The attribute values are compact or expanded.
    // For now, this controls the brandingApp. If compact, then the
    // brandingApp goes next to the branding, else it goes
    // under the branding.

    Object chromeType = getChromeType(component, bean);

    boolean isCompact = !CorePanelPageHeader.CHROME_TYPE_EXPANDED.equals(chromeType);


    boolean AppContextOrCompact =
      ((brandingAppCont != null) || isCompact);

    boolean isRTL = rc.isRightToLeft();

    boolean hasGlobal = navigationGlobal != null;
    boolean hasSwitch = menuSwitch != null;

    ResponseWriter writer    = context.getResponseWriter();
    writer.startElement("span", component);
    renderAllAttributes(context, rc, component, bean);
    renderId(context, component);


    // start the table element for the next piece
    writer.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", "100%");


    int rowSpan = 0;

    if (hasGlobal || hasSwitch)
    {
      rowSpan++;
    }

    if (navigation1 != null)
    {
      rowSpan++;
    }

    //
    // Determine how many branding areas we have
    //
    int brandingCount = 0;

    if (branding != null)
    {
      brandingCount++;
    }

    if (brandingApp != null)
    {
      brandingCount++;
    }

    if (brandingAppCont != null)
    {
      brandingCount++;
    }


    //
    // Create the middle table, if necessary.
    // brandingCount counts branding,
    // brandingApp, and brandingAppCont.
    // rowSpan counts navigation2, and navigation1.
    //
    if ((rowSpan != 0) || (brandingCount > 0))
    {
      Integer rowSpanObject = (rowSpan > 1)
                                ? rowSpan
                                : null;

      writer.startElement("tr", null);

      if (brandingCount > 0)
      {
        writer.startElement("td", null);
        writer.writeAttribute("nowrap", Boolean.TRUE, null);
        writer.writeAttribute("rowspan", rowSpanObject, null);

        // start the table containing the brandings if any
        if (brandingCount > 0)
        {
          writer.writeAttribute("valign", "top", null);

          writer.startElement("table", null);

          // changed the table from 100% to 1% since it seemed to fix
          // the bug where you have extra space between branding
          // and brandingApp.
          OutputUtils.renderLayoutTableAttributes(context, rc, "2", "1%" );

          writer.startElement("tr", null);
          writer.startElement("td", null);
          writer.writeAttribute("nowrap", Boolean.TRUE, null);

          // what is happening is that the branding and
          // the brandingApp that is next to it have a big space
          // if the browser is wide. I want them to be right next to each other
          // I had put them in one td, but then these two elements don't line
          // up vertically. So I put them in two tds.
          if((brandingApp != null) && (branding != null) && AppContextOrCompact )
          {
            // the 1% width is good for IE and Mozilla, since it keeps the
            // branding and the brandingApp very close together when
            // the browser is resized to be wide.
            writer.writeAttribute("width", "1%", null);
          }

        }

        if (branding != null)
        {
          encodeChild(context, branding);
        }

        // this td is needed to keep the branding and the
        // brandingApp images in line
        if((brandingApp != null) && (branding != null) &&
            AppContextOrCompact)
        {
          // finish branding, and then do brandingApp
          // next to branding if AppContextOrCompact is set.
          writer.endElement("td");
          writer.startElement("td", null);
          writer.writeAttribute("nowrap", Boolean.TRUE, null);
        }

        // If brandingApp && (onlybrandingApp or AppContextOrCompact)
        // According to the BLAF, you must have a branding
        // with brandingApp
        // so I would say, if brandingApp is regular, valign bottom.
        // if brandingApp compact and IE, set width.

        if ( (brandingApp != null) && ((brandingCount == 1) ||
              AppContextOrCompact))
        {
          if (!AppContextOrCompact)
          {
            writer.writeAttribute("valign", "bottom", null);
          }
          else if(isIE(rc))
          {
            // this keeps the table small, so that the tabs can
            // get close. Otherwise, the table was super wide.
            writer.writeAttribute("width", "2%", null);
          }
          encodeChild(context, brandingApp);
        }

        if (brandingCount > 0)
        {
          // finish rendering the first row
          writer.endElement("td");
          writer.endElement("tr");

          // start rendering the second row
          writer.startElement("tr", null);
          writer.startElement("td", null);
          writer.writeAttribute("valign", "top", null);
          // this is for testing only,
          writer.writeAttribute("nowrap", Boolean.TRUE, null);

        }

        if ( (brandingAppCont != null) ||
             (brandingApp != null && isCompact) )
        {
          // Even if we don't have brandingAppCont, but we have a compact
          // brandingApp, we need this space.
          writer.writeAttribute("colspan", "2", null);
          // need the correct height to conform with the BLAF spec
          writer.writeAttribute("height", "17", null);

          if (brandingAppCont != null)
          {
            encodeChild(context, brandingAppCont);
          }
        }
        else if ( (brandingApp != null)  && (branding != null) )
        {
            // in this case, we render the default brandingApp
            encodeChild(context, brandingApp);
        }

        // end the table containing the branding and brandingApp
        if (brandingCount > 0)
        {
          writer.endElement("td");
          writer.endElement("tr");
          writer.endElement("table");

        }
        writer.endElement("td");
      }

      // if rowSpan > 0, then render navigationGlobal,
      // and/or navigation1 each on a different row, if they exist
      // rowSpan is greater than 0 when one or more of these
      // named children exist.
      if (rowSpan > 0)
      {
        boolean startNewRow = false;

        if (hasGlobal || hasSwitch)
        {

          // render global buttons in a td
          writer.startElement("td", null);
          writer.writeAttribute("align", (isRTL)
                                           ? "left"
                                           : "right", null);
          writer.writeAttribute("valign", "bottom", null);

          // we want the navigationGlobal/menuSwitch
          // to layer on top of the branding when the browser is too narrow.
          if (isIE(rc))
          {
            writer.writeAttribute(
              "style",
              "position:relative;z-index:10;padding-bottom:8px", null);
          }
          else
          {
            writer.writeAttribute("style", "padding-bottom:8px", null);
          }

          if ( hasGlobal && hasSwitch)
          {
            writer.startElement("table", null);
            writer.startElement("tr", null);
            writer.startElement("td", null);
            encodeChild(context, navigationGlobal);
            writer.endElement("td");
            writer.startElement("td", null);
            writer.writeAttribute("valign", "bottom", null);
            encodeChild(context, menuSwitch);
            writer.endElement("td");
            writer.endElement("tr");
            writer.endElement("table");
          }
          else if (hasGlobal)
          {
            encodeChild(context, navigationGlobal);
          }
          else
          {
            encodeChild(context, menuSwitch);
          }

          writer.endElement("td");

          startNewRow = true;
        }

        if (navigation1 != null)
        {
          // Let the navigation1 slide under
          // reasons for startNewRow = if cobranding or if navigationGlobal
          if (startNewRow )
          {

            writer.endElement("tr");

            writer.startElement("tr", null);
          }
          // render navigation1. takes up two tds.
          _renderNavigation1(context, rc, writer, navigation1, isRTL);

        }
      }

      writer.endElement("tr");
    }



    // NOW TO RENDER NAVIGATION2 AND SEARCH

    boolean hasSearch = (search != null);
    // Figure out how many columns we have to span
    // for navigation2 and search
    int colSpan = _calculateColSpan(brandingCount,
                                    (navigation1 != null),
                                    (hasGlobal || hasSwitch));

    if (navigation2 != null)
    {
      // render navigation2 in a row
      _renderNavigation2(context, writer, navigation2, colSpan);

    }

    if ( hasSearch )
    {
      // render search in a row
      _renderSearch(context, rc, writer, search, colSpan);
    }

    writer.endElement("table");

    writer.endElement("span");


  }

  protected Object getChromeType(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_chromeTypeKey);
  }

  /**
   * render two tds, one with the navigation1 and one with a spacer
   * @return void
   */
  private void  _renderNavigation1(
    FacesContext     context,
    RenderingContext rc,
    ResponseWriter   writer,
    UIComponent      navigation1,
    boolean          isRTL
    ) throws IOException
  {
    writer.startElement("td", null);
    writer.writeAttribute("align", (isRTL)
                                     ? "left"
                                     : "right", null);
    writer.writeAttribute("valign", "bottom", null);
    encodeChild(context, navigation1);
    writer.endElement("td");

    writer.startElement("td", null);
    renderSpacer(context, rc, _NAVIGATION1_SPACER_SIZE, "1");
    writer.endElement("td");
  }

  /**
   * render a table row with the navigation2
   * @return void
   */
  private void _renderNavigation2(
    FacesContext   context,
    ResponseWriter writer,
    UIComponent    navigation2,
    int            colSpan
    ) throws IOException
  {
      writer.startElement("tr", null);
      writer.startElement("td", null);

      if (colSpan > 1)
        writer.writeAttribute("colspan", colSpan, null);

      writer.writeAttribute("width",
                            "100%", null);

      encodeChild( context, navigation2 );


      writer.endElement("td");
      writer.endElement("tr");
  }

  /**
   * render a table row with the search
   * @return void
   */
  private void _renderSearch(
    FacesContext     context,
    RenderingContext rc,
    ResponseWriter   writer,
    UIComponent      search,
    int              colSpan
    ) throws IOException
  {
    writer.startElement("tr", null);
    writer.startElement("td", null);
    writer.writeAttribute("width",
                          "100%", null);

    if (colSpan > 1)
      writer.writeAttribute("colspan", colSpan, null);

    writer.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, rc,
                                "0",
                                "100%");

    // Add a space under the search so that
    // page content, like panelBoxes, don't hit
    writer.writeAttribute("style", "margin-bottom:3px;", null);
    writer.startElement("tr", null);

    writer.startElement("td", null);
    writer.writeAttribute( "width", "4", null);
    renderSpacer(context, rc, "4", "1");
    writer.endElement("td");

    writer.startElement("td", null);
    writer.writeAttribute("width",
                          "100%", null);
    writer.writeAttribute("colspan","3", null);
    renderStyleClass(context, rc, SkinSelectors.QUICK_SEARCH_BOX_STYLE_CLASS);


    encodeChild(context, search);

    writer.endElement("td");

    writer.startElement("td", null);
    writer.writeAttribute( "width", "4", null);
    renderSpacer(context, rc, "4", "1");
    writer.endElement("td");

    writer.endElement("tr");
    writer.endElement("table");

    writer.endElement("td");
    writer.endElement("tr");
  }

 /**
   * Figure out how many columns we have to span for navigation2
   * and search
   * @return int colSpan number of columns to span.
   */
  private int _calculateColSpan(
    int     brandingCount,
    boolean navigation1Exists,
    boolean navigation2Exists
    )
  {
    int colSpan = 0;
    if (brandingCount > 0)
      colSpan++;
    if ( navigation2Exists || navigation1Exists)
    {
      colSpan++;

      // And one
      if (navigation1Exists)
        colSpan++;
    }
    return colSpan;
  }

  private PropertyKey _chromeTypeKey;

  private CoreRenderer _pdaRenderer = new PdaPanelPageHeaderRenderer();
  private static final String _NAVIGATION1_SPACER_SIZE = "10";
}