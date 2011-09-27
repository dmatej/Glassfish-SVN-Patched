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
import org.apache.myfaces.trinidad.component.core.layout.CorePanelBox;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class PanelBoxRenderer
  extends XhtmlRenderer
{
  public PanelBoxRenderer()
  {
    this(CorePanelBox.TYPE);
  }

  protected PanelBoxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _iconKey = type.findKey("icon");
    _backgroundKey = type.findKey("background");
    _contentStyleKey = type.findKey("contentStyle");
  }

  @Override
  public String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    String background = getBackground(component, bean);
    if(_BACKGROUND_TRANSPARENT.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS;
    }
    else if(_BACKGROUND_LIGHT.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS;
    }
    else if( _BACKGROUND_MEDIUM.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_MEDIUM_STYLE_CLASS;
    }
    else if( _BACKGROUND_DARK.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_DARK_STYLE_CLASS;
    }
    else
    {
      return _BACKGROUND_DEFAULT_STYLE_CLASS;
    }
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected boolean hasChildren(
    UIComponent component)
  {
    return component.getChildCount() > 0;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    super.encodeAll(context, rc, component, bean);

    String icon = getIcon(component, bean);
    String text = getText(component, bean);

    ResponseWriter writer = context.getResponseWriter();
        
    boolean isPIE = Agent.PLATFORM_PPC.equalsIgnoreCase(
                               rc.getAgent().getPlatformName());
                               
    // While handling a PPR response, Windows Mobile cannot DOM replace
    // a table element. Wrapping a table element with a div element fixes
    // the problem.                               
    if (isPIE)
    {  
      writer.startElement("div", component);
      renderId(context, component);
      // The frame table
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, component);
      renderId(context, component);
    }
    
    renderAllAttributes(context, rc, component, bean);
    writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);

    if (hasChildren(component) || text != null || icon != null)
    {
      // There's something to render to let build the frame
      _renderContainerTopRow(context, rc);

      _renderMiddleRow(context, rc, component, bean, icon, text);

      _renderContainerBottomRow(context, rc);
    }

    writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_ELEMENT);
    
    if (isPIE)
      writer.endElement("div");
  }

  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    super.renderAllAttributes(context, rc, component, bean);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
  }

  private void _renderContainerTopRow(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    if(rc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }

  private void _renderContainerBottomRow(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    if(rc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }

  private void _renderMiddleRow(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    Object           icon,
    Object           text
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);

    // Render left edge
    if(rc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS);

      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }

    // Render body
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    renderBody(context, rc, component, bean, icon, text);
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

    // Render right edge
    if(rc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context,
                       rc,
                       SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }

  protected void renderBody(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    Object           icon,
    Object           text
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    renderStyleClass(context,
                     rc,
                     SkinSelectors.AF_PANEL_BOX_BODY_STYLE_CLASS);

    if (hasChildren(component) && (text != null || icon != null))
    {
      // There's both a header and a content, use a table.
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
      OutputUtils.renderLayoutTableAttributes(context, rc, "0", "100%");
      writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);

      // Render header
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      _renderHeader(context, rc, icon, text);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);

      // Render content
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderContent(context, rc, bean, component);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);

      writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ELEMENT);
    }
    else if(text != null || icon != null)
    {
      // We only have a header, use a div as style class placeholder
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      _renderHeader(context, rc, icon, text);
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }
    else
    {
      // We only have a content, use a div as style class placeholder
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      renderContent(context, rc, bean, component);
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }
  }

  private void _renderHeader(
    FacesContext     context,
    RenderingContext rc,
    Object           icon,
    Object           text
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    renderStyleClass(context,
                     rc,
                     SkinSelectors.AF_PANEL_BOX_HEADER_STYLE_CLASS);

    if(rc.isRightToLeft())
    {
      if(text != null)
      {
        writer.writeText(text, _textKey.getName());
      }

      if(icon != null)
      {
        writer.startElement("img", null);
        OutputUtils.renderAltAndTooltipForImage(context,
                                                rc,
                                                XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);

        renderEncodedResourceURI(context, "src", icon);
        writer.endElement("img");
      }
    }
    else
    {
      if(icon != null)
      {
        writer.startElement("img", null);
        OutputUtils.renderAltAndTooltipForImage(context,
                                                rc,
                                                XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);

        renderEncodedResourceURI(context, "src", icon);
        writer.endElement("img");
      }

      if(text != null)
      {
        writer.writeText(text, _textKey.getName());
      }
    }
  }

  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    FacesBean        bean,
    UIComponent      component
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    renderStyleClass(context,
                     rc,
                     SkinSelectors.AF_PANEL_BOX_CONTENT_STYLE_CLASS);

    String style = getContentStyle(component, bean);
    if(style != null)
    {
      writer.writeAttribute("style", style, null);
    }

    encodeAllChildren(context, component);
  }

  protected String getText(
    UIComponent component,
    FacesBean   bean)
  {
    if (_textKey == null)
      return null;
    return toString(bean.getProperty(_textKey));
  }

  protected String getIcon(
    UIComponent component,
    FacesBean   bean)
  {
    if (_iconKey == null)
      return null;
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
  }

  protected String getContentStyle(
    UIComponent component,
    FacesBean   bean)
  {
    if (_contentStyleKey == null)
      return null;
    return toString(bean.getProperty(_contentStyleKey));
  }

  protected String getBackground(
    UIComponent component,
    FacesBean   bean)
  {
    if (_backgroundKey == null)
      return null;
    return toString(bean.getProperty(_backgroundKey));
  }

  private PropertyKey _textKey;
  private PropertyKey _iconKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _backgroundKey;

  private static final String _BACKGROUND_LIGHT       = "light";
  private static final String _BACKGROUND_TRANSPARENT = "transparent";
  private static final String _BACKGROUND_MEDIUM      = "medium";
  private static final String _BACKGROUND_DARK        = "dark";

  private static final String _BACKGROUND_DEFAULT_STYLE_CLASS =
    SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS;
}
