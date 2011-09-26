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
import org.apache.myfaces.trinidad.component.core.output.CoreOutputDocument;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 */
public class OutputDocumentRenderer extends ValueRenderer
{
  public OutputDocumentRenderer()
  {
    super(CoreOutputDocument.TYPE);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement(_ROOT_ELEMENT, comp);

    renderId(context, comp);
    renderAllAttributes(context, rc, comp, bean);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void encodeEnd(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    String value = getConvertedString(context, comp, bean);

    _renderTitle(context, rc, comp, bean);

    boolean firstParagraphRendered = false;

    UIComponent separator = getFacet(comp, CoreOutputDocument.SEPARATOR_FACET);
    // Cache the separator existence in a variable in order to
    // evaluate a potential rendered EL only once.
    boolean hasSeparator = separator != null;

    int length = value.length();
    StringBuilder builder = new StringBuilder(length);

    for (int i = 0; i < length; i++)
    {
      char character = value.charAt(i);
      if (_isLineFeed(character))
      {
        // Paragraph change, but only if the last one was not empty
        // That might happen often whe \n\r sequence is found
        if (builder.length() > 0)
        {
          _renderParagraph(context, builder);
          firstParagraphRendered = true;
        }
      }
      else
      {
        if (builder.length() == 0)
        {
          // The first character that is not a line break found
          // for the current paragraph, so start a new one
          if (firstParagraphRendered && hasSeparator)
          {
            // If a separator was specified and the paragraph being rendered
            // is not the first, render the separator
            _renderSeparator(context, rc, separator, bean);
          }

          _renderParagraphStart(context, rc);
        }

        builder.append(character);
      }
    }

    // There's more characters to print
    if(builder.length() > 0)
    {
      _renderParagraph(context, builder);
    }

    rw.endElement(_ROOT_ELEMENT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _separatorClassKey = type.findKey(_SEPARATOR_CLASS_ATTRIBUTE);
    _titleClassKey = type.findKey(_TITLE_CLASS_ATTRIBUTE);
  }

  /**
   * Gets the separator class of the rendered document. That class will be
   * combined with the skin class, not override it.
   *
   * @param bean the property holder for the rendered document.
   *
   * @return the separator class for the rendered document or <code>null</code>
   *         if none was specified.
   */
  protected String getSeparatorClass(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_separatorClassKey));
  }

  /**
   * Gets the title class of the rendered document. That class will be
   * combined with the skin class, not override it.
   *
   * @param bean the property holder for the rendered document.
   *
   * @return the title class for the rendered document or <code>null</code>
   *         if none was specified.
   */
  protected String getTitleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_titleClassKey));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    renderStyleAttributes(context, rc, component, bean, _ROOT_SELECTOR);
  }

  /**
   * Gets the array of style classes to be used by the currently rendered
   * document's facet.
   *
   * @param arc          the current adf rendering context
   * @param bean         the property holder for the rendered document.
   * @param key          the property key of the style class property for the
   *                     current facet being rendered.
   * @param skinSelector the skin selector associated with the currently
   *                     rendered facet.
   *
   * @return the array of style classes to add to the current facet.
   */
  private String[] _getStyleClasses(
    FacesBean bean,
    PropertyKey key,
    String skinSelector)
  {
    // Pre-conditions that would cause NullPointerException if not met
    assert bean != null;

    String specifiedClass = toString(bean.getProperty(key));
    if (specifiedClass != null)
    {
      return new String[] { specifiedClass, skinSelector };
    }
    else
    {
      return new String[] { skinSelector };
    }
  }

  /**
   * Determiens if the specified character can be considered as a line feed
   * for the purpose of separating the paragraphs
   *
   * @param character the character to evaluate
   *
   * @return <code>true</code> if the specified character can be considered a
   *         line feed, <code>false</code> otherwise.
   */
  private boolean _isLineFeed(
    char character)
  {
    return character == _LINE_FEED || character == _CARRIAGE_RETURN;
  }

  /**
   * Renders the currently opened paragraph with the currently buffered content,
   * escaping it if necessary, then close it and flush the buffer.
   *
   * @param rw      the response writer used to put content in the underlying
   *                response.
   * @param bean    the property holder for the rendered document.
   * @param builder the content buffer
   *
   * @throws IOException if a problem occurs while writing the content to the
   *                     underlying response.
   */
  private void _renderParagraph(
    FacesContext  context,
    StringBuilder builder
    ) throws IOException
  {
    // Pre-conditions that would cause NullPointerException if not met
    assert builder != null;
    assert context != null;

    renderFormattedText(context, builder.toString());

    context.getResponseWriter().endElement(_PARAGRAPH_ELEMENT);

    // Clear buffer content
    builder.delete(0, builder.length());
  }

  /**
   * Renders the beginning of a paragraph.
   *
   * @param context the current Faces context
   * @param rc     the current rendering context
   * @param bean    the property holder for the rendered document.
   *
   * @throws IOException if a problem occurs while writing the content to the
   *                     underlying response.
   */
  private void _renderParagraphStart(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    rw.startElement(_PARAGRAPH_ELEMENT, null);

    renderStyleClasses(context, rc, new String[]{_PARAGRAPH_SELECTOR});
  }

  /**
   * Renders a separator between 2 paragraphs.
   *
   * @param context   the current Faces context
   * @param rc       the current rendering context
   * @param separator the component to render as a separator
   * @param bean      the property holder for the rendered document.
   *
   * @throws IOException if a problem occurs while writing the content to the
   *                     underlying response.
   */
  private void _renderSeparator(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      separator,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    rw.startElement(_SEPARATOR_ELEMENT, null);

    String[] classes = _getStyleClasses(bean,
                                        _separatorClassKey,
                                        _SEPARATOR_SELECTOR);

    renderStyleClasses(context, rc, classes);

    encodeChild(context, separator);

    rw.endElement(_SEPARATOR_ELEMENT);
  }

  /**
   * Renders the title of the rendered document.
   *
   * @param context the current Faces context
   * @param rc     the current rendering context
   * @param comp    the document component
   * @param bean    the property holder for the rendered document.
   *
   * @throws IOException if a problem occurs while writing the content to the
   *                     underlying response.
   */
  private void _renderTitle(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    UIComponent title = getFacet(comp, CoreOutputDocument.TITLE_FACET);
    if (title != null)
    {
      ResponseWriter rw = context.getResponseWriter();

      rw.startElement(_TITLE_ELEMENT, null);

      String[] classes = _getStyleClasses(bean,
                                          _titleClassKey,
                                          _TITLE_SELECTOR);

      renderStyleClasses(context, rc, classes);

      encodeChild(context, title);

      rw.endElement(_TITLE_ELEMENT);
    }
  }

  // Line feed charaters
  private static final char _LINE_FEED = '\n';
  private static final char _CARRIAGE_RETURN = '\r';

  // Elements
  private static final String _PARAGRAPH_ELEMENT = XhtmlConstants.PARAGRAPH_ELEMENT;

  private static final String _ROOT_ELEMENT = XhtmlConstants.DIV_ELEMENT;

  private static final String _SEPARATOR_ELEMENT = XhtmlConstants.DIV_ELEMENT;

  private static final String _TITLE_ELEMENT = XhtmlConstants.DIV_ELEMENT;

  // Property names
  private static final String _SEPARATOR_CLASS_ATTRIBUTE = "separatorClass";

  private static final String _TITLE_CLASS_ATTRIBUTE = "titleClass";

  // Skin selectors
  private static final String _PARAGRAPH_SELECTOR = SkinSelectors.AF_OUTPUT_DOCUMENT_PARAGRAPH_STYLE_CLASS;

  private static final String _ROOT_SELECTOR = SkinSelectors.AF_OUTPUT_DOCUMENT_STYLE_CLASS;

  private static final String _SEPARATOR_SELECTOR = SkinSelectors.AF_OUTPUT_DOCUMENT_SEPARATOR_STYLE_CLASS;

  private static final String _TITLE_SELECTOR = SkinSelectors.AF_OUTPUT_DOCUMENT_TITLE_STYLE_CLASS;

  // Property keys
  private PropertyKey _separatorClassKey;

  private PropertyKey _titleClassKey;
}
