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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PageHeaderLayoutRenderer.java#0 $) $Date: 15-nov-2005.19:26:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageHeaderLayoutRenderer extends HtmlLafRenderer
{
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer    = context.getResponseWriter();

    UINode largeAd           = getNamedChild(context, node,
                                             ADVERTISEMENT_LARGE_CHILD);
    UINode mediumAd          = getNamedChild(context, node,
                                             ADVERTISEMENT_MEDIUM_CHILD);
    UINode corporateBranding = getNamedChild(context, node,
                                             BRANDING_CHILD);
    UINode productBranding   = getNamedChild(context, node,
                                             BRANDING_APP_CHILD);
    UINode inContextBranding = getNamedChild(context, node,
                                             BRANDING_APP_CONTEXTUAL_CHILD);
    UINode cobranding        = getNamedChild(context, node,
                                             BRANDING_COOPERATIVE_CHILD);
    UINode globalButtons     = getNamedChild(context, node,
                                             NAVIGATION_GLOBAL_CHILD);
    UINode tabBar            = getNamedChild(context, node,
                                             NAVIGATION1_CHILD);
    UINode globalHeader      = getNamedChild(context, node,
                                             NAVIGATION2_CHILD);
    UINode menuSwitch        = getNamedChild(context, node,
                                             MENU_SWITCH_CHILD);
    UINode quickSearch       = getNamedChild(context, node,
                                             SEARCH_CHILD);

    // chromeType is an attribute on pageLayout and pageHeaderLayout.
    // The attribute values are compact or expanded.
    // For now, this controls the productBranding. If compact, then the
    // productBranding goes next to the corporate branding, else it goes
    // under the corporate branding.

    String chromeType =
      XhtmlLafUtils.getStringAttributeValue(context, node, CHROME_TYPE_ATTR);


    boolean mediumAdInMiddle = (mediumAd != null);
    boolean isRTL = isRightToLeft(context);



    //
    // if largeAd exists, then write out large and medium ads at top of page
    // (if largeAd doesn't exist, but medium does, it goes in a separate area)
    //
    if (largeAd != null)
    {
      // render ads in a table
      _renderAds(context, node, writer, largeAd, mediumAd, isRTL);
      mediumAdInMiddle = false;

    }


    // start the table element for the next piece
    writer.startElement(TABLE_ELEMENT, null);
    renderLayoutTableAttributes(context,
                                  ZERO_ATTRIBUTE_VALUE,
                                  ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE);

    boolean hasGlobal = globalButtons != null;
    boolean hasSwitch = menuSwitch != null;


    //
    // Write out the toolbar/tab bar area
    //
    int rowSpan = 0;

    if (cobranding != null)
    {
      rowSpan++;
    }

    if (hasGlobal || hasSwitch)
    {
      rowSpan++;
    }

    if (tabBar != null)
    {
      rowSpan++;
    }


    //
    // Determine how many branding areas we have
    //
    int brandingCount = 0;

    if (corporateBranding != null)
    {
      brandingCount++;
    }

    if (productBranding != null)
    {
      brandingCount++;
    }

    if (inContextBranding != null)
    {
      brandingCount++;
    }

   // According to the BLAF, Product Branding can be
   // under the Corporate Branding or it can be next to Corporate Branding.
   // the Product Branding is rendered next to Corporate Branding
   // when pageLayout's chromeType attribute is not set to expanded or
   // the page also has an inContextBranding.

    boolean isCompact = !("expanded".equals(chromeType));
    boolean inContextOrCompact =
      ((inContextBranding != null) || isCompact);


    //
    // Create the middle table, if necessary.
    // brandingCount counts corporateBranding,
    // productBranding, and inContextBranding.
    // rowSpan counts cobranding, globalHeader, and tabBar.
    //
    if (mediumAdInMiddle || (rowSpan != 0) || (brandingCount > 0))
    {
      Integer rowSpanObject = (rowSpan > 1)
                                ? getInteger(rowSpan)
                                : null;

      writer.startElement(TABLE_ROW_ELEMENT, null);

      if (brandingCount > 0)
      {
        writer.startElement(TABLE_DATA_ELEMENT, null);
        writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);

        if ((rowSpanObject == null) && mediumAdInMiddle && (tabBar != null))
        {
          writer.writeAttribute(ROWSPAN_ATTRIBUTE, "2", null);
        }
        else
        {
          writer.writeAttribute(ROWSPAN_ATTRIBUTE, rowSpanObject, null);
        }

        // start the table containing the brandings if any
        if (brandingCount > 0)
        {
          writer.writeAttribute(VALIGN_ATTRIBUTE, TOP_ATTRIBUTE_VALUE, null);

          writer.startElement(TABLE_ELEMENT, null);

          // =-= bwa: reducing spacing to tighten layout
          // renderLayoutTableAttributes(context, "5", "100%");
          // changed the table from 100% to 1ince it seemed to fix
          // the bug where you have extra space between corporate branding
          // and product branding.

          renderLayoutTableAttributes(context,
                                      "2",
                                      "1%" );


          writer.startElement(TABLE_ROW_ELEMENT, null);
          writer.startElement(TABLE_DATA_ELEMENT, null);
          renderStyleClassAttribute(context, AF_PANEL_PAGE_BRANDING_STYLE_CLASS);
          writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
          // what is happening is that the corporate branding and
          // the product branding that is next to it have a big space
          // if the browser is wide. I want them to be right next to each other
          // I had put them in one td, but then these two elements don't line
          // up vertically. So I put them in two tds.
          if((productBranding != null) && (corporateBranding != null) &&
              inContextOrCompact && !isNetscape(context))
          {
            // the 1% width on Netscape makes the enclosing table very wide,
            // but it is good for IE and Mozilla, since it keeps the corporate
            // branding and the product branding very close together when
            // the browser is resized to be wide.
            writer.writeAttribute(WIDTH_ATTRIBUTE, "1%", null);
          }

        }

        if (corporateBranding != null)
        {
          renderNamedChild(context, node, corporateBranding,
                           BRANDING_CHILD);
        }
        // this td is needed to keep the corporate branding and the
        // product branding images in line
        if((productBranding != null) && (corporateBranding != null) &&
            inContextOrCompact)
        {
          // finish corporate branding, and then do product branding
          // next to corporate branding if inContextOrCompact is set.
          writer.endElement(TABLE_DATA_ELEMENT);
          writer.startElement(TABLE_DATA_ELEMENT, null);
          writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
        }

        // If productBranding && (onlyProductBranding or inContextOrCompact)
        // According to the BLAF, you must have a corporateBranding
        // with productBranding
        // so I would say, if productBranding is regular, valign bottom.
        // if productBranding compact and IE, set width.

        if ( (productBranding != null) && ((brandingCount == 1) ||
              inContextOrCompact))
        {
          if (!inContextOrCompact)
          {
            writer.writeAttribute(VALIGN_ATTRIBUTE, BOTTOM_ATTRIBUTE_VALUE, null);
          }
          else if(isIE(context))
          {
            // this keeps the table small, so that the tabs can
            // get close. Otherwise, the table was super wide.
            writer.writeAttribute(WIDTH_ATTRIBUTE, "2%", null);
          }
          renderNamedChild(context, node, productBranding,
                           BRANDING_APP_CHILD);
        }

        if (brandingCount > 0)
        {
          // finish rendering the first row
          writer.endElement(TABLE_DATA_ELEMENT);
          writer.endElement(TABLE_ROW_ELEMENT);

          // start rendering the second row
          writer.startElement(TABLE_ROW_ELEMENT, null);
          writer.startElement(TABLE_DATA_ELEMENT, null);
          writer.writeAttribute(VALIGN_ATTRIBUTE, TOP_ATTRIBUTE_VALUE, null);
          // this is for testing only,
          writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);

        }

        if ( (inContextBranding != null) ||
             (productBranding != null && isCompact) )
        {
          // Even if we don't have inContextBranding, but we have a compact
          // productBranding, we need this space.
          writer.writeAttribute(COLSPAN_ATTRIBUTE, "2", null);
          // need the correct height to conform with the BLAF spec
          writer.writeAttribute(HEIGHT_ATTRIBUTE, "17", null);

          if (inContextBranding != null)
          {
            renderNamedChild(context, node, inContextBranding,
                           BRANDING_APP_CONTEXTUAL_CHILD);
          }
        }
        else if ( (productBranding != null)  && (corporateBranding != null) )
        {
            // in this case, we render the default product branding
            renderNamedChild(context, node, productBranding,
                             BRANDING_APP_CHILD);
        }

        // end the table containing the corporate and product branding
        if (brandingCount > 0)
        {
          writer.endElement(TABLE_DATA_ELEMENT);
          writer.endElement(TABLE_ROW_ELEMENT);
          writer.endElement(TABLE_ELEMENT);

        }
        writer.endElement(TABLE_DATA_ELEMENT);
      }

      // mediumAdInMiddle is true when largeAd didn't exist. In this case
      // the mediumAd is rendered in the same row as the branding and
      // globalHeader and such.
      if (mediumAdInMiddle)
      {
        writer.startElement(TABLE_DATA_ELEMENT, null);
        // Let the tab bar slide under the medium advertising
        if (tabBar != null)
        {
          rowSpanObject = (rowSpan > 2) ? getInteger(rowSpan - 1) : null;
        }

        writer.writeAttribute(ROWSPAN_ATTRIBUTE, rowSpanObject, null);
        writer.writeAttribute(VALIGN_ATTRIBUTE, TOP_ATTRIBUTE_VALUE, null);

        // we want the medium advertisement when it is in the middle
        // to layer on top of the branding when the browser is resized to
        // be very narrow.
        if (isIE(context))
        {
          writer.writeAttribute("style", "position:relative;z-index:10", null);
        }
        renderNamedChild(context, node, mediumAd, ADVERTISEMENT_MEDIUM_CHILD);
        writer.endElement(TABLE_DATA_ELEMENT);
      }

      // if rowSpan > 0, then render cobranding, globalButtons,
      // and/or tabBar each on a different row, if they exist
      // rowSpan is greater than 0 when one or more of these
      // named children exist.
      if (rowSpan > 0)
      {
        boolean startNewRow = false;

        if (cobranding != null)
        {
          // render cobranding area in a td
          writer.startElement(TABLE_DATA_ELEMENT, null);
          writer.writeAttribute(ALIGN_ATTRIBUTE,  (isRTL)
                                            ? LEFT_ATTRIBUTE_VALUE
                                            : RIGHT_ATTRIBUTE_VALUE, null);
          writer.writeAttribute(VALIGN_ATTRIBUTE, TOP_ATTRIBUTE_VALUE, null);
          renderNamedChild(context, node, cobranding, BRANDING_COOPERATIVE_CHILD);
          writer.endElement(TABLE_DATA_ELEMENT);

          startNewRow = true;
        }

        if (hasGlobal || hasSwitch)
        {
          // startNewRow if cobranding != null
          if (startNewRow)
          {
            writer.endElement(TABLE_ROW_ELEMENT);
            writer.startElement(TABLE_ROW_ELEMENT, null);
          }

          // render global buttons in a td
          writer.startElement(TABLE_DATA_ELEMENT, null);
          writer.writeAttribute(ALIGN_ATTRIBUTE, (isRTL)
                                           ? LEFT_ATTRIBUTE_VALUE
                                           : RIGHT_ATTRIBUTE_VALUE, null);
          writer.writeAttribute(VALIGN_ATTRIBUTE, BOTTOM_ATTRIBUTE_VALUE, null);

          // we want the global buttons
          // to layer on top of the branding when the browser is too narrow.
          if (isIE(context))
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
            writer.startElement(TABLE_ELEMENT, null);
            writer.startElement(TABLE_ROW_ELEMENT, null);
            writer.startElement(TABLE_DATA_ELEMENT, null);
            renderNamedChild(context, node, globalButtons, NAVIGATION_GLOBAL_CHILD);
            writer.endElement(TABLE_DATA_ELEMENT);
            writer.startElement(TABLE_DATA_ELEMENT, null);
            writer.writeAttribute(VALIGN_ATTRIBUTE, BOTTOM_ATTRIBUTE_VALUE, null);
            renderNamedChild(context, node, menuSwitch, MENU_SWITCH_CHILD);
            writer.endElement(TABLE_DATA_ELEMENT);
            writer.endElement(TABLE_ROW_ELEMENT);
            writer.endElement(TABLE_ELEMENT);
          }
          else if (hasGlobal)
          {
            renderNamedChild(context, node, globalButtons, NAVIGATION_GLOBAL_CHILD);
          }
          else
          {
            renderNamedChild(context, node, menuSwitch, MENU_SWITCH_CHILD);
          }

          writer.endElement(TABLE_DATA_ELEMENT);

          startNewRow = true;
        }

        if (tabBar != null)
        {
          // Let the tab bar slide under the medium-ad
          // reasons for startNewRow = if cobranding or if globalButtons
          if (startNewRow || mediumAdInMiddle)
          {

            writer.endElement(TABLE_ROW_ELEMENT);

            writer.startElement(TABLE_ROW_ELEMENT, null);
          }
          // render tab bar. takes up two tds.
          // if mediumAdInMiddle, then colspan will be 2.
          _renderTabBar(context, node, writer, tabBar, isRTL, mediumAdInMiddle);

        }
      }

      writer.endElement(TABLE_ROW_ELEMENT);
    }


    // NOW TO RENDER GLOBAL HEADER AND QUICKSEARCH

    boolean hasQuickSearch = (quickSearch != null);
    // Figure out how many columns we have to span
    // for global header and quicksearch
    int colSpan = _calculateColSpan(brandingCount,
                                    mediumAdInMiddle,
                                    (cobranding != null),
                                    (hasGlobal || hasSwitch),
                                    (tabBar != null));

    if (globalHeader != null)
    {
      // render globalHeader in a row
      _renderGlobalHeader(context,
                          node,
                          writer,
                          globalHeader,
                          hasQuickSearch,
                          colSpan);

    }

    if ( hasQuickSearch )
    {
      // render quick search in a row
      _renderQuickSearch(context, node, writer, quickSearch, colSpan);
    }

    writer.endElement(TABLE_ELEMENT);

  }

  protected void renderPageHeaderChild(
    UIXRenderingContext context,
    UINode           node,
    UINode           globalHeader,
    boolean          hasQuickSearch
  )throws IOException
  {
    renderNamedChild(context, node, globalHeader, NAVIGATION2_CHILD);
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // put a span around the entire element for the Visual Editor
    // and PPR bug # 2222541
    return SPAN_ELEMENT;
  }
  //
  // private methods
  //
  //
  /**
   * render the large ad and if it exists, the mediumAd as well
   * @return void
   */
  private void _renderAds(
    UIXRenderingContext context,
    UINode           node,
    ResponseWriter   writer,
    UINode           largeAd,
    UINode           mediumAd,
    boolean          isRTL
    ) throws IOException
  {
    if (largeAd == null) return;

    writer.startElement(TABLE_ELEMENT, null);
    renderLayoutTableAttributes(context,
                                ZERO_ATTRIBUTE_VALUE,
                                ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE);

    writer.startElement(TABLE_ROW_ELEMENT, null);
    writer.startElement(TABLE_DATA_ELEMENT, null);

    // if there isn't a medium ad also, then center the large ad
    if (mediumAd == null)
    {
      writer.writeAttribute(ALIGN_ATTRIBUTE, "center", null);
    }

    // write large advertisement
    renderNamedChild(context, node, largeAd, ADVERTISEMENT_LARGE_CHILD);
    writer.endElement(TABLE_DATA_ELEMENT);

    // if medium ad exists, render it along side of the large ad
    if (mediumAd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(ALIGN_ATTRIBUTE, (isRTL)
                                        ? LEFT_ATTRIBUTE_VALUE
                                        : RIGHT_ATTRIBUTE_VALUE, null);
      writer.writeAttribute(VALIGN_ATTRIBUTE, TOP_ATTRIBUTE_VALUE, null);
      renderNamedChild(context, node, mediumAd, ADVERTISEMENT_MEDIUM_CHILD);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
    writer.endElement(TABLE_ELEMENT);

  }

  /**
   * render two tds, one with the tab bar and one with a spacer
   * @return void
   */
  private void  _renderTabBar(
    UIXRenderingContext context,
    UINode           node,
    ResponseWriter   writer,
    UINode           tabBar,
    boolean          isRTL,
    boolean          mediumAdInMiddle
    ) throws IOException
  {
    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(ALIGN_ATTRIBUTE, (isRTL)
                                     ? LEFT_ATTRIBUTE_VALUE
                                     : RIGHT_ATTRIBUTE_VALUE, null);
    writer.writeAttribute(VALIGN_ATTRIBUTE, BOTTOM_ATTRIBUTE_VALUE, null);

    if (mediumAdInMiddle)
    {
      writer.writeAttribute(COLSPAN_ATTRIBUTE, "2", null);
    }

    renderNamedChild(context, node, tabBar, NAVIGATION1_CHILD);
    writer.endElement(TABLE_DATA_ELEMENT);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderSpacer(context, _TAB_SPACER_SIZE, 1);
    writer.endElement(TABLE_DATA_ELEMENT);
  }

  /**
   * render a table row with the global header
   * @return void
   */
  private void _renderGlobalHeader(
    UIXRenderingContext context,
    UINode           node,
    ResponseWriter   writer,
    UINode           globalHeader,
    boolean          hasQuickSearch,
    int              colSpan
    ) throws IOException
  {
      writer.startElement(TABLE_ROW_ELEMENT, null);
      writer.startElement(TABLE_DATA_ELEMENT, null);

      if (colSpan > 1)
        writer.writeAttribute(COLSPAN_ATTRIBUTE, getInteger(colSpan), null);

      writer.writeAttribute(WIDTH_ATTRIBUTE,
                            ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE, null);

      renderPageHeaderChild( context, node, globalHeader, hasQuickSearch );


      writer.endElement(TABLE_DATA_ELEMENT);
      writer.endElement(TABLE_ROW_ELEMENT);
  }


  /**
   * render a table row with the quick search
   * @return void
   */
  private void _renderQuickSearch(
    UIXRenderingContext context,
    UINode           node,
    ResponseWriter   writer,
    UINode           quickSearch,
    int              colSpan
    ) throws IOException
  {

    writer.startElement(TABLE_ROW_ELEMENT, null);
    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE,
                          ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE, null);

    if (colSpan > 1)
      writer.writeAttribute(COLSPAN_ATTRIBUTE, getInteger(colSpan), null);

    writer.startElement(TABLE_ELEMENT, null);
    renderLayoutTableAttributes(context,
                                ZERO_ATTRIBUTE_VALUE,
                                ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE);

    // Add a space under the quickSearch so that
    // content containers don't hit
    writer.writeAttribute("style", "margin-bottom:3px;", null);
    writer.startElement(TABLE_ROW_ELEMENT, null);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute( WIDTH_ATTRIBUTE, "4", null);
    renderSpacer(context, 4, 1);
    writer.endElement(TABLE_DATA_ELEMENT);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE,
                          ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE, null);
    writer.writeAttribute(COLSPAN_ATTRIBUTE,"3", null);
    renderStyleClassAttribute(context, QUICK_SEARCH_BOX_STYLE_CLASS );


    renderNamedChild(context, node, quickSearch, SEARCH_CHILD);

    writer.endElement(TABLE_DATA_ELEMENT);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute( WIDTH_ATTRIBUTE, "4", null);
    renderSpacer(context, 4, 1);
    writer.endElement(TABLE_DATA_ELEMENT);

    writer.endElement(TABLE_ROW_ELEMENT);
    writer.endElement(TABLE_ELEMENT);

    writer.endElement(TABLE_DATA_ELEMENT);
    writer.endElement(TABLE_ROW_ELEMENT);
  }

  /**
   * Figure out how many columns we have to span for global header
   * and quicksearch
   * @return int colSpan number of columns to span.
   */
  private int _calculateColSpan(
    int brandingCount,
    boolean mediumAdInMiddle,
    boolean cobrandingExists,
    boolean globalButtonsExists,
    boolean tabBarExists
    )
  {
    int colSpan = 0;
    if (brandingCount > 0)
      colSpan++;
    if (mediumAdInMiddle)
      colSpan++;
    if (cobrandingExists || globalButtonsExists || tabBarExists)
    {
      colSpan++;
      // And one
      if (tabBarExists)
        colSpan++;
    }
    return colSpan;
  }

  private static final int _TAB_SPACER_SIZE = 10;

}
