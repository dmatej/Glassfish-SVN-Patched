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
package org.apache.myfaces.trinidadinternal.skin.icon;

import java.io.IOException;

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;


/**
 * An Icon implementation which renders a text string as the icon.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/TextIcon.java#0 $) $Date: 10-nov-2005.18:59:05 $
 */
public class TextIcon extends Icon
{
  /**
   * Creates the TextIcon with the specified text string.
   */
  public TextIcon(String text)
  {
    this(text, null, null, null);
  }

  /**
   * Creates a TextIcon which uses different text depending on the
   * reading direction.
   */
  public TextIcon(
    String text,
    String rtlText
    )
  {
    this(text, rtlText, null, null);
  }

  /**
   * Creates the TextIcon with the specified text string, style class,
   * and inline style.
   */
  public TextIcon(
    String text,
    String rtlText,
    String styleClass,
    Style  inlineStyle
    )
  {
    _text = text;
    _rtlText = rtlText;
    _styleClass = styleClass;
    _inlineStyle = inlineStyle;
  }

  /**
   * Renders the Icon.
   *
   * @param context The RenderingContext for the current request.
   * @param attrs A Map which provides access to
   *             values that might be useful to Icon implementations
   *             TextIcon looks for Icon.ID_KEY, Icon.SHORT_DESC_KEY, and
   *             Icon.EMBEDDED_KEY. It does not render SHORT_DESC_KEY if it
   *             is null or "", because there is no point to this for TextIcons.
   */
  @Override
  public void renderIcon(
    FacesContext context,
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    ) throws IOException
  {
    // See if we have an id
    Object id = null;
    Object styleClass = _styleClass;
    Object title = null;
    Object attrInlineStyle = null;
    Object attrStyles = null;
    boolean embedded = false;

    if (attrs != null)
    {
      attrInlineStyle = _getInlineStyle(attrs);
      attrStyles = _getStyles(attrs);
      id = attrs.get(Icon.ID_KEY);
      title = _getTitle(attrs);
      embedded = _isEmbedded(attrs);
    }


    // If we have an id or style information, render the text contents
    // within a span
    ResponseWriter writer = context.getResponseWriter();

    boolean useSpan = _useSpan(styleClass, _inlineStyle, title, embedded);

    if (useSpan)
      writer.startElement("span", null);

    if (id != null)
      writer.writeAttribute("id", id, null);

    // If we have a title that isn't "", render it on the span unless in screen reader mode
    boolean screenReader = arc.getAccessibilityMode() == RequestContext.Accessibility.SCREEN_READER;
    boolean hasTitle = (title != null) && !"".equals(title);
    if (hasTitle && !screenReader)
    {
      writer.writeAttribute("title", title, null);
    }

    // Handle style attributes (or elements)

    // we map the styleClass in case it is used in a composite
    StringBuilder styleClasses = new StringBuilder();
    if (styleClass != null)
    {
      // FIXME: since we go through the rendering context,
      // there should be no need to go to StyleUtils
      String convertedStyleClass =
        StyleUtils.convertToValidSelector(arc.getStyleClass(_styleClass));

      styleClasses.append(convertedStyleClass);
    }

    if (attrStyles != null)
    {
      if (styleClasses.length() > 0)
        styleClasses.append(" ");

      styleClasses.append(arc.getStyleClass(attrStyles.toString()));
    }

    if (styleClasses.length() > 0)
      writer.writeAttribute("class", styleClasses.toString(), null);

    StringBuilder inline = new StringBuilder(100);
    if (_inlineStyle != null)
    {
      inline.append(_inlineStyle.toInlineString());
    }

    if ((attrInlineStyle != null) && !"".equals(attrInlineStyle))
    {
      inline.append(attrInlineStyle.toString());
    }

    if (inline.length() > 0)
      writer.writeAttribute("style", inline.toString(), null);

    String text = getText(arc);

    // don't know how to map this back to the source, so using null...
    writer.writeText(text, null);

    if (hasTitle && screenReader)
    {
      // Render the title in-line for screen reader mode as JAWS will not read the title
      // attribute of a SPAN element
      writer.writeText(" ", null);
      writer.writeText(title, null);
    }

    if (useSpan)
      writer.endElement("span");
  }

  /**
   * Returns the inlineStyle to render.
   */
  protected Style getInlineStyle()
  {
    return _inlineStyle;
  }

  /**
   * Returns the text to render when in RTL mode.
   */
  protected String getRtlText()
  {
    return _rtlText;
  }

  /**
   * Returns the styleClass to render.
   */
  protected String getStyleClass()
  {
    return _styleClass;
  }

  /**
   * Returns the text to render.
   */
  protected String getText(RenderingContext arc)
  {
    if ((_rtlText != null) && arc.isRightToLeft() )
      return _rtlText;

    return _text;
  }

  /**
   * Sets the inlineStyle of the icon
   */
  public void setInlineStyle(Style inlineStyle)
  {
    _inlineStyle = inlineStyle;
  }

  /**
   * Sets the text to render if in RTL mode
   */
  public void setRtlText(String rtlText)
  {
    _rtlText = rtlText;
  }

  /**
   * Sets the styleClass of the icon
   */
  public void setStyleClass(String styleClass)
  {
    _styleClass = styleClass;
  }


  /**
   * Sets the text to render.
   */
  public void setText(String text)
  {
    _text = text;
  }


  // Returns the inlineStyle for the icon
  private Object _getInlineStyle(Map<String, ? extends Object> attrs)
  {
    assert (attrs != null);

    return attrs.get(Icon.INLINE_STYLE_KEY);
  }

  // Returns the styleClasses for the icon
  private Object _getStyles(Map<String, ? extends Object> attrs)
  {
    assert (attrs != null);

    return attrs.get(Icon.STYLE_CLASS_KEY);
  }

  // Returns the title text for the icon
  private Object _getTitle(Map<String, ? extends Object> attrs)
  {
    assert (attrs != null);

    return attrs.get(Icon.SHORT_DESC_KEY);
  }

  private boolean _isEmbedded(Map<String, ? extends Object> attrs)
  {
    assert (attrs != null);

    return Boolean.TRUE.equals(
             attrs.get(Icon.EMBEDDED_KEY));
  }

  // Tests whether we need to render a span
  private static boolean _useSpan(
    Object styleClass,
    Object inlineStyle,
    Object title,
    boolean embedded
    )
  {
    return (!embedded &&
             ((styleClass != null)  ||
              (title != null && !"".equals(title)) ||
              (inlineStyle != null) ||
              (title != null)));
  }

  private String _text;
  private String _rtlText;
  private String _styleClass;
  private Style  _inlineStyle;
}
