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
package org.apache.myfaces.trinidadinternal.share.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A base implementation of NodeParser that performs
 * no actions at all.  Clients can use this as a base
 * class for their node parsers, or as a way to ignore
 * an element (and all of its children).
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/BaseNodeParser.java#0 $) $Date: 10-nov-2005.18:59:06 $
 */
public class BaseNodeParser implements NodeParser
{
  /**
   * Returns a shared instance of this class.  The instance
   * may be used anywhere that a node and all of its children
   * should be ignored (because of an error, for instance)
   */
  static public NodeParser getIgnoreParser()
  {
    return _sInstance;
  }

  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
  }


  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException
  {
    return null;
  }


  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    return this;
  }


  public void endChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException
  {
  }

  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException
  {
  }

  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
  }


  /**
   * Get the value of an attribute;  if not found,
   * send a warning to the error log.
   * @param context the ParseContext
   * @param attrs the SAX2 attribute list
   * @param qName the XML 1.0 qualified name of the attribute
   */
  final protected String getRequiredAttribute(
    ParseContext context,
    Attributes   attrs,
    String       qName)
  {
    String value = attrs.getValue(qName);
    if ((value == null) && (_LOG.isWarning()))
      _LOG.warning("Required attribute \"" + qName + "\" not found.");

    return value;
  }

  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
  }

  protected void logError(
    ParseContext context,
    String       message,
    Exception    e) throws SAXParseException
  {
    throw new SAXParseException(message, context.getLocator(), e);
  }

  protected void logUnexpectedElement(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    if (_LOG.isWarning())
      _LOG.warning("NOT_UNDERSTOOD_CHILD_NAME", localName);
  }

  protected void logUnexpectedAttribute(
    ParseContext context,
    String       qName)
  {
    if (_LOG.isWarning())
      _LOG.warning("\"" + qName + "\" is not an understood attribute");
  }

  static private final NodeParser _sInstance = new BaseNodeParser();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BaseNodeParser.class);
}
