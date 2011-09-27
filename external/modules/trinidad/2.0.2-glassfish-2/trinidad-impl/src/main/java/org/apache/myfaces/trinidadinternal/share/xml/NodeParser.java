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

/**
 * NodeParser is the interface used to process each XML element.
 * NodeParsers are responsible for building up a Java object
 * based on an XML element and all of its contents, though they
 * will generally delegate the handling of any child XML elements
 * to other NodeParsers.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/NodeParser.java#0 $) $Date: 10-nov-2005.18:59:11 $
 */
public interface NodeParser
{
  /**
   * Receives notification of the start of an element.
   * @param context the parsing context
   * @param namespaceURI the namespace of the element
   * @param localName the local name of the element
   * @param attrs the attributes attached to the element
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException;


  /**
   * Receives notification of the end of the element.
   * @param context the parsing context
   * @param namespaceURI the namespace of the element
   * @param localName the local name of the element
   * @return the fully-initialized Java object corresponding
   *   to this release
   */
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException;

  /**
   * Receives notification of the start of a child element.  If this
   * method returns "this", further processing of that child element
   * will be continue with this same parser.  If it returns "null",
   * the element will be ignored, and a warning logged. Otherwise, handling
   * will be delegated to the new parser, and the next call made to
   * this interface will be <code>addCompletedChild()</code>
   * <p>
   * @see #endChildElement
   * @see #addCompletedChild
   *
   * @param context the parsing context
   * @param namespaceURI the namespace of the child element
   * @param localName the local name of the child element
   * @param attrs the attributes attached to the element
   * @return the node parser that should be used for this
   *   child element, or null if processing should continue with
   *   this <code>NodeParser</code>
   */
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException;
  

  /**
   * Receives notification of the end of a child element;  called
   * only if <code>startChildElement</code> returned "this".
   * <p>
   * @param context the parsing context
   * @param namespaceURI the namespace of the child element
   * @param localName the local name of the child element
   */
  public void endChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException;

  /**
   * Receives notification of the end of a delegated child element.
   * Called  only if <code>startChildElement</code> returns a value
   * other than "null" or "this".
   * <p>
   * @see #endElement
   * 
   * @param context the parsing context
   * @param namespaceURI the namespace of the child element
   * @param localName the local name of the child element
   * @param child the Java object produced for that child by
   *   its NodeParser's <code>endElement()</code> method.
   */
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException;


  /**
   * Receives notification of text content of this element.
   * @param context the parsing context
   * @param text an array of characters
   * @param start the start position in the array
   * @param length the number of characters to read from the array
   */
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException;

  /**
   * Receives notification of ignorable whitespace.
   * @param context the parsing context
   * @param text an array of characters
   * @param start the start position in the array
   * @param length the number of characters to read from the array
   */
  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException;
}
