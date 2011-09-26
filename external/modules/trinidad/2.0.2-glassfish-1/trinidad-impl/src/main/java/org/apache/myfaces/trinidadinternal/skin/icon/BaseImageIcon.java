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

import org.apache.myfaces.trinidad.skin.Icon;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;

import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;

/**
 * A base class for ImageIcon implementations.
 * The base class produces the full image URI by combining a
 * subclass-specific base URI with an image URI that is
 * specified when the BaseImageIcon instance is created.
 * This allows subclasses to implement different strategies
 * for determining the base image URI - eg. ContextImageIcon
 * uses the servlet context path.  Subclasses must implement
 * the getBaseURI() method, which provides the base URI to prepend
 * to the image URI.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/BaseImageIcon.java#0 $) $Date: 10-nov-2005.18:59:02 $
 */
abstract public class BaseImageIcon extends Icon
{
  /**
   * Creates an image Icon which has a different image URI depending on the
   * reading direction.
   * @param uri The URI to the image to use when the
   *                reading direction is left-to-right.
   * @param rtlURI The URI to the image to use when
   *                   the reading direction is right-to-left.
   * @param width The width of the image
   * @param height The height of the image
   * @param styleClass The style class for the image
   * @param inlineStyle The inline style for the image
   */
  public BaseImageIcon(
    String  uri,
    String  rtlURI,
    Integer width,
    Integer height,
    String  styleClass,
    Style   inlineStyle
    )
  {
    _width = width;
    _height = height;
    _uri = uri;
    _rtlURI = rtlURI;
    _styleClass = styleClass;
    _inlineStyle = inlineStyle;
  }

  /**
   * Implementation of ImageIcon.getImageURI().
   */
  @Override
  public Object getImageURI(
    FacesContext        context,
    RenderingContext arc)
  {
    // Get the base URI
    String baseURI = _getBaseURI(context, arc);

    // Get the icon name
    String uri = getRelativeURI(context, arc);

    // If we don't have a base URI, just return the icon name
    if (baseURI == null)
      return uri;

    // Otherwise, combine the base URI with the icon name
    // to produce the full URI.
    return baseURI + uri;
  }

  /**
   * Implementation of Icon.getImageWidth().
   */
  @Override
  public Integer getImageWidth(RenderingContext arc)
  {
    return _width;
  }

  /**
   * Implementation of Icon.getImageHeight().
   */
  @Override
  public Integer getImageHeight(RenderingContext arc)
  {
    return _height;
  }

  /**
   * Implementation of Icon.renderIcon().
   */
  @Override
  public void renderIcon(
    FacesContext context,
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start the img element
    writer.startElement("img", null);

    // Write out the id attr
    Object id  = _getId(attrs);
    if (id != null)
      writer.writeAttribute("id", id, null);

    // Write out the src attr
    String baseURI = _getBaseURI(context, arc);
    String uri = getRelativeURI(context, arc);
    
    if (baseURI == null)
      writer.writeURIAttribute("src", context.getExternalContext().encodeResourceURL(uri), null);
    else
      writer.writeURIAttribute("src", 
                            context.getExternalContext().encodeResourceURL(baseURI + uri), null);

    // Write out the width/height attrs
    Object width = _getWidth(arc, attrs);
    if (width != null)
      writer.writeAttribute("width", width, null);

    Object height = _getHeight(arc, attrs);
    if (height != null)
      writer.writeAttribute("height", height, null);

    // Write out the border width
    writer.writeAttribute("border", "0", null);

    // Probably should check to see whether style attrs are supported
    // and if not render style elements.  However, it's not clear
    // whether any of the style properties that would be set on images
    // would actually have equivalent style element attributes.
    /**** don't use RenderingContext
    if (_styleClass != null)
      XhtmlLafRenderer.renderStyleClassAttribute(rcontext, _styleClass);
    else
    {
      if (attrs != null)
      {
        XhtmlLafRenderer.renderStyleClassAttribute(rcontext,
                                          attrs.get(Icon.STYLE_CLASS_KEY));
      }
    }
    */
    // =-=jmwIcon@todo: do what XhtmlLafRenderer.renderStyleClassAttribute
    // did -- check if class attribute is supported, use short class names,
    // map style classes
    String styleClass = null;
    if (_styleClass != null)
    {
      styleClass = _styleClass;
    }
    else if (attrs != null)
    {
      styleClass = (String) attrs.get(Icon.STYLE_CLASS_KEY);

    }

    // =-=jmwIcon need to shorten it!
    // we map the styleClass in case it is used in a composite
    // Then, since we aren't shortening it, which we need to!, we need
    // to at least make sure it is valid.
    if (styleClass != null)
    {
      String convertedStyleClass =
        StyleUtils.convertToValidSelector(arc.getStyleClass(styleClass));

      writer.writeAttribute("class", convertedStyleClass, null);
    }

    String inlineStyle = null;
    if (_inlineStyle != null)
    {
      inlineStyle = _inlineStyle.toInlineString();
    }
    else if (attrs != null)
    {
      inlineStyle = (String) attrs.get(Icon.INLINE_STYLE_KEY);
    }

    if (inlineStyle != null)
    {
      if (!("".equals(inlineStyle)))
        writer.writeAttribute("style", inlineStyle, null);
    }

    // Write out alt/title attrs
    Object altText = _getAltText(attrs);
    OutputUtils.renderAltAndTooltipForImage(context, arc, altText);

    // Write out the vertical alignment attr
    _writeVerticalAlignment(writer, attrs);

    writer.endElement("img");
  }

  /**
   * Returns the base URI to prepend to the icon name.
   * This base URI must be terminated with a trailing
   * URI separator ('/').
   */
  abstract protected String getBaseURI(
    FacesContext        context,
    RenderingContext arc);

  /**
   * Returns the URI of the image relative to the base URI.
   */
  protected String getRelativeURI(
    FacesContext        context,
    RenderingContext arc)
  {
    // We only need to check the reading direction if
    // we actually have a RTL version of the icon
    if ((_rtlURI != null) && arc.isRightToLeft())
        return _rtlURI;

    return _uri;
  }

  // Gets the baseURI, checking to make sure that it is properly terminated.
  private String _getBaseURI(
    FacesContext        context,
    RenderingContext arc)
  {
    String baseURI = getBaseURI(context, arc);

    // Just to be safe, make sure that the base URI is terminated properly
    assert ((baseURI == null) ||
                       (baseURI.charAt(baseURI.length() - 1) == '/'));


    return baseURI;
  }

  // Returns the alt text for the image
  private Object _getAltText(
    Map<String, ? extends Object> attrs
    )
  {
    if (attrs == null)
      return null;

    return attrs.get(Icon.SHORT_DESC_KEY);
  }

  // Returns the ID for the image
  private Object _getId(Map<String, ? extends Object> attrs)
  {
    if (attrs == null)
      return null;

    return attrs.get(Icon.ID_KEY);
  }

  // Returns the width of the icon
  private Object _getWidth(
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    )
  {
    Object width = null;

    // First, check to see if the width is specified as an attribute.
    // If so, this takes precedence over our own width value.
    if (attrs != null)
      width = attrs.get(Icon.WIDTH_KEY);

    // If width wasn't passed in as an attribute, use our own width value.
    if (width == null)
      width = getImageWidth(arc);

    return width;
  }

  // Returns the height of the icon
  private Object _getHeight(
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    )
  {
    Object height = null;

    // First, check to see if the height is specified as an attribute.
    // If so, this takes precedence over our own height value.
    if (attrs != null)
      height = attrs.get(Icon.HEIGHT_KEY);

    // If height wasn't passed in as an attribute, use our own height value.
    if (height == null)
      height = getImageHeight(arc);

    return height;
  }

  // Write out the vertical alignment
  private void  _writeVerticalAlignment(
    ResponseWriter writer,
    Map<String, ? extends Object> attrs
    ) throws IOException
  {
    if (attrs != null)
    {
      // Some components (such as hideShow)
      // need to set the img align attribute in order to force
      // an icon to line up with its associated text.
      Object align = attrs.get(Icon.ALIGN_KEY);

      if (align != null)
      {
        // absmiddle isn't actually a valid alignment value according
        // to the HTML specification.  We might want to convert absmiddle
        // alignments to style="vertical-align:middle", but that doesn't
        // seem to have the desired results in all cases.  In particular,
        // when we switch from absmiddle to vertical-align:middle, the
        // dateField's button drops down by two pixels.

        writer.writeAttribute("align", align, null);
      }
    }
  }


  private String  _uri;    // URI for ltr icons
  private String  _rtlURI; // URI for rtl icons
  private Integer _width;  // Width of the icon
  private Integer _height; // Height of the icon
  private String  _styleClass;  // Style class for the image
  private Style   _inlineStyle; // Inline style for the image
}
