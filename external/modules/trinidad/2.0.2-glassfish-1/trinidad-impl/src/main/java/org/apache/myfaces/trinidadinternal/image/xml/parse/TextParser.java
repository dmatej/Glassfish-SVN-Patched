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
package org.apache.myfaces.trinidadinternal.image.xml.parse;

import java.util.ResourceBundle;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseErrorUtils;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for text and translatedText elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/TextParser.java#0 $) $Date: 10-nov-2005.19:04:43 $
 */
public class TextParser extends BaseNodeParser
{
  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _bundleName = attrs.getValue(XMLConstants.BUNDLE_ATTR);
    _key = attrs.getValue(XMLConstants.KEY_ATTR);
  }

  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    // If the text has been specified explicitly, return it
    if ((_text != null) || (_key == null) && (_bundleName == null))
      return _text;

    ResourceBundle bundle = (ResourceBundle)context.getProperty(
                              ImageConstants.TECATE_NAMESPACE,
                              XMLConstants.RESOURCE_BUNDLE_PROPERTY);

    if (_bundleName != null)
    {
      LocaleContext localeContext = (LocaleContext)context.getProperty(
                                      ImageConstants.TECATE_NAMESPACE,
                                      XMLConstants.LOCALE_CONTEXT_PROPERTY);
      bundle = localeContext.getBundle(_bundleName);
    }

    if ((bundle == null) || (_key == null))
    {
      if (_LOG.isWarning())
      {
        String message = (bundle == null) ? _BUNDLE_ERROR : _KEY_ERROR;
        _LOG.warning(ParseErrorUtils.getErrorMessage(context, message));
      }
      return null;
    }

    Object value = bundle.getObject(_key);
    if (value == null)
      return null;

    return value.toString();
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    return null;
  }

  /**
   * Implementation of NodeParser.addText().
   */
  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
    String moreText = new String(text, start, length);

    if (_text == null)
      _text = moreText;
    else
      _text = _text + moreText;
  }

  /**
   * Implemenation of NodeParser.addWhitespace().
   */
  @Override
  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
    // Believe it or not, preserve white space, as the white space may
    // represent whitespace within user presentable text (such as button
    // labels) and as such should be preserved.  So, until we come up with a
    // better plan, we're preserving all whitespace.
    addText(context, text, start, length);
  }

  private String _bundleName;
  private String _key;
  // -= Simon Lessard =-
  // FIXME: Using a StringBuilder could be MUCH more efficient
  //        Currently String concatenation within a loop process
  //        which is bad.
  private String _text;

  // Error messages
  private static final String _BUNDLE_ERROR =
    "Missing translatedText bundle attribute.";
  private static final String _KEY_ERROR =
    "Missing translatedText key attribute.";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TextParser.class);
}
