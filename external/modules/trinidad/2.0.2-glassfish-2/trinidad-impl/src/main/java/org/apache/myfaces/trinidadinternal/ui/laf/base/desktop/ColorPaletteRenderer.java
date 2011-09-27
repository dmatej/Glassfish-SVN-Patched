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

import java.awt.Color;

import java.io.IOException;

import java.util.Arrays;
import java.util.List;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.share.text.RGBColorFormat;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.expl.ColorPaletteUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;



/**
 * Renders the color palette.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ColorPaletteRenderer.java#0 $) $Date: 10-nov-2005.18:55:12 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ColorPaletteRenderer extends HtmlLafRenderer
{
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode node) throws IOException
  {
    super.prerender(context, node);
    BaseDesktopUtils.addLib(context, "TrColorConverter()");
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode node) throws IOException
  {
    List<Color> colorData = _getColors(context, node, COLOR_DATA_ATTR);
    if (colorData == null)
    {
      colorData = 
        ColorPaletteUtils.getColorPaletteMap().get("default49");
    }

    List<Color> customColorData = _getColors(context, node, CUSTOM_COLOR_DATA_ATTR);

    int width = getWidth(context, node);
    int height = getHeight(context, node);

    if (colorData != null && !colorData.isEmpty())
    {
      _renderColorPalette(context, node, colorData, width, height);
    }

    if (customColorData != null && !customColorData.isEmpty())
    {
      renderSpacer(context, -1, 8);
      _renderColorPalette(context, node, customColorData, width, 0);
    }
  }

  private void _renderColorPalette(
    UIXRenderingContext context,
    UINode node,
    List<Color> colorData,
    int width,
    int height) throws IOException
  {
    int colorCount = colorData.size();
    if (colorCount > 0)
    {
      if (width <= 0 && height <= 0)
      {
        // default to a square
        width = height = (int)Math.ceil(Math.sqrt(colorCount));
      }
      else if (width <= 0) // height > 0
      {
        // cast to double to avoid integer math
        width = (int)Math.ceil(colorCount / (double)height);
      }
      else if (height <= 0) // width > 0
      {
        // cast to double to avoid integer math
        height = (int)Math.ceil(colorCount / (double)width);
      }
    }

    // if colors were provided, width and height must be determined
    assert (colorCount == 0 || (width > 0 && height > 0));

    boolean isNetscape = isNetscape(context);
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("table", null);
    renderLayoutTableAttributes(context, "0", "1", null);

    if (!isNetscape)
    {
      renderStyleClassAttribute(context, COLOR_PALETTE_STYLE_CLASS);
    }
    else // isNetscape
    {
      writer.startElement("tr", null);
      writer.startElement("td", null);
      renderStyleClassAttribute(context, COLOR_PALETTE_STYLE_CLASS);
      writer.startElement("table", null);
      renderLayoutTableAttributes(context, "0", "1", null);
    }

    Color color;
    int index;

    Object onColorSelect = getOnColorSelect(context, node);
    boolean hasOnColorSelect = (onColorSelect != null);

    boolean scriptWritten =
      Boolean.TRUE.equals(getRenderingProperty(context,
                                               _SCRIPT_WRITTEN_KEY));

    // render dependent script
    if (!scriptWritten)
    {
      // mark the script as written
      setRenderingProperty(context, _SCRIPT_WRITTEN_KEY, Boolean.TRUE);

      writer.startElement("script", null);
      XhtmlLafRenderer.renderScriptDeferAttribute(context);

      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      XhtmlLafRenderer.renderScriptTypeAttribute(context);
      if (isNetscape)
      {
        writer.write(_ON_CP_SEL_NS);
      }
      else if (isIE(context))
      {
        writer.write(_ON_CP_SEL_IE);
      }
      else // Mozilla
      {
        writer.write(_ON_CP_SEL_MZ);
      }

      writer.endElement("script");
    }

    StringBuilder onCellClick = null;
    int onCellClickLength = 0;

    if (isNetscape)
    {
      Object id = getID(context, node);
      if (id == null)
        id = "null";
      onCellClick = new StringBuilder(id.toString().length() +
                                     onColorSelect.toString().length() +
                                     16);
      onCellClick.append("_onCPSel('");
      onCellClick.append(id);
      onCellClick.append("','");
      BaseDesktopUtils.escapeJS(onCellClick,
                         onColorSelect.toString(),
                         true /* inQuotes */);
      onCellClick.append('\'');
      onCellClickLength = onCellClick.toString().length();
    }

    String pattern = "#RRGGBB";

    for(int y=0; y < height; y++)
    {
      writer.startElement("tr", null);
      writer.writeAttribute("height", _CELL_SIZE, null);
      for(int x=0; x < width; x++)
      {
        // completed 'y' rows, each with 'width' cells,
        // plus 'x' more cells in this row
        index = (y * width) + x;

        if (index < colorCount)
        {
          //colorData/customColorData binding is always expected to resolve to
          //  a java.util.List of java.awt.Color. An error otherwise.
          color = colorData.get(index);
        }
        else
        {
          color = null;
        }

        writer.startElement("td", null);
        if (color != null)
        {
          if (color.getAlpha() == 0) // transparent
          {
            String destination = null;
            String onClick = null;

            if (hasOnColorSelect)
            {
              destination = "#";

              if (onCellClick != null)
              {

                onCellClick.setLength(onCellClickLength);
                onCellClick.append("); return false");
                onClick = onCellClick.toString();
              }
            }

            renderIcon(context,
                       getBaseImageURI(context) +
                       COLOR_PALETTE_TRANSPARENT_ICON_NAME,
                       "af_chooseColor.TRANSPARENT",
                       destination,
                       null,
                       onClick,
                       null,
                       null, true);
          }
          else
          {
            String colorString = CSSUtils.getColorValue(color);

            if (isNetscape)
              writer.writeAttribute("bgcolor", colorString, null);

            if (hasOnColorSelect)
            {
              writer.startElement("a", null);
              if (onCellClick != null)
              {
                onCellClick.setLength(onCellClickLength);
                onCellClick.append(",'");
                onCellClick.append(colorString);
                onCellClick.append("'); return false");
                writer.writeAttribute("onclick", onCellClick.toString(), null);
              }
              writer.writeAttribute("href", "#", null);
            }

            writer.startElement("img", null);
            writer.writeAttribute("border", "0", null);
            writer.writeAttribute("width", _CELL_SIZE, null);
            writer.writeAttribute("height", _CELL_SIZE, null);
            if (!isNetscape)
            {
              writer.writeAttribute("style", "background-color:" + colorString,
                  null);
              XhtmlLafRenderer.renderStyleClassAttribute(context,
                  "p_OraDisplayBlock");
            }
            writeAbsoluteImageURI(context, "src", TRANSPARENT_GIF);
            renderAltAndTooltipForImage(context,
                    new RGBColorFormat(pattern).format(color));

            writer.endElement("img");

            if (hasOnColorSelect)
            {
              writer.endElement("a");
            }
          }
        }
        else
        {
          // empty cell
          renderStyleClassAttribute(context,
                                    COLOR_PALETTE_EMPTY_CELL_STYLE_CLASS);
          if (isNetscape)
          {
            writer.startElement("img", null);
            writer.writeAttribute("border", "0", null);
            writer.writeAttribute("width", _CELL_SIZE, null);
            writer.writeAttribute("height", _CELL_SIZE, null);
            writeAbsoluteImageURI(context, "src", TRANSPARENT_GIF);
            writer.endElement("img");
          }
          else
          {
            writer.writeAttribute("width", _CELL_SIZE, null);
          }
        }
        writer.endElement("td");
      }
      writer.endElement("tr");
    }

    if (isNetscape)
    {
      writer.endElement("table");
      writer.endElement("td");
      writer.endElement("tr");
    }

    writer.endElement("table");
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode node)
  {
    return "span";
  }

  protected int getWidth(
    UIXRenderingContext  context,
    UINode            node)
  {
    return getIntAttributeValue(context, node, WIDTH_ATTR, 0);
  }

  protected int getHeight(
    UIXRenderingContext  context,
    UINode            node)
  {
    return getIntAttributeValue(context, node, HEIGHT_ATTR, 0);
  }

  protected Object getOnColorSelect(
    UIXRenderingContext  context,
    UINode            node)
  {
    return getAttributeValue(context, node,
                             ON_COLOR_SELECT_ATTR,
                             "_cfbs(event)");
  }

  @Override
  protected Object getOnClick(
    UIXRenderingContext  context,
    UINode            node)
  {
    Object onClick = null;

    if (!isNetscape(context))
    {
      Object onColorSelect = getOnColorSelect(context, node);

      if (onColorSelect != null)
      {
        onClick = context.getLocalProperty(0, ON_CLICK_ATTR, null);

        if (onClick == null)
        {
          Object id = getID(context, node);
          if (id == null)
            id = "null";
          StringBuilder sb = new StringBuilder(id.toString().length() +
                                             onColorSelect.toString().length() +
                                             21);
          sb.append("_onCPSel('");
          sb.append(id);
          sb.append("','");
          BaseDesktopUtils.escapeJS(sb,
                             onColorSelect.toString(),
                             true /* inQuotes */);
          // IE stores the event on the window
          if (isIE(context))
            sb.append("')");
          else
            sb.append("',event)");
          sb.append("; return false");
          onClick = sb.toString();
          context.setLocalProperty(ON_CLICK_ATTR, onClick);
        }
      }
    }
    //pu: Chain it with the 'onclick' attribute value
    return XhtmlLafUtils.getChainedJS(super.getOnClick(context, node),
                                     onClick, 
                                     true);
  }

  @SuppressWarnings("unchecked")
  private static List<Color> _getColors(
    UIXRenderingContext  context,
    UINode            node,
    AttributeKey     attributeKey
    )
  {
    Object attribValue = getAttributeValue(context, node, attributeKey, null);
    if (attribValue instanceof Color[])
    {
      return Arrays.asList( (Color[]) attribValue );
    }
    //If not Color[], colorData/customColorData binding is always expected to
    //  resolve to java.util.List of java.awt.Color. An error otherwise.
    return (List<Color>) attribValue;
  }

  private static final String _CELL_SIZE = "11";

  private static final Object _SCRIPT_WRITTEN_KEY = new Object();

  private static final String _ON_CP_SEL_NS =
    // param s  source id
    // param b  handler body
    // param v  color value
    "function _onCPSel(s,b,v) {" +
     "var parser = new TrColorConverter(\"#RRGGBB\"); " +
     "var color = (v != null) ? parser.getAsObject(v) : new TrColor(0,0,0,0); " +
     "_handleClientEvent('colorSelect', s, {value:color}, b); }";

  private static final String _ON_CP_SEL_IE =
    // param s  source id
    // param b  handler body
    "function _onCPSel(s,b) {" +
    "var ele=window.event.srcElement;" +
    "if (ele.tagName=='A') ele=ele.childNodes[0];" +
     "  if (ele.tagName == 'IMG') {" +
        "var parser = new TrColorConverter(\"#RRGGBB\"); " +
        "var color = parser.getAsObject(ele.style.backgroundColor); " +
        "if (color == null) { color = new TrColor(0,0,0,0) }; " +
        "_handleClientEvent('colorSelect', s, {value:color}, b);" +
       "}" +
     "}";


  private static final String _ON_CP_SEL_MZ =
    // param s  source id
    // param b  handler body
    // param e  the click event
    "function _onCPSel(s,b,e) {" +
      "var ele=e.target; if (ele.tagName=='A') ele=ele.childNodes[0];"+
      " if (ele.tagName == 'IMG') {" +
        "var color = ele.style.backgroundColor;" +
        "var parser = new TrColorConverter([\"'rgb'(r, g, b)\",\"'rgb'(r,g,b)\",\"#RRGGBB\"]); " +
        "color = parser.getAsObject(color); " +
        "if (color == null) { color = new TrColor(0,0,0,0) }; " +
        "_handleClientEvent('colorSelect', s, {value:color}, b);" +
       "}" +
     "}";

}
