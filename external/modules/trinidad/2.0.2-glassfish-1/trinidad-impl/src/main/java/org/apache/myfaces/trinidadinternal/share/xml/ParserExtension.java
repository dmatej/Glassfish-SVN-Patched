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

import org.xml.sax.SAXParseException;
import org.xml.sax.Attributes;
import java.util.Map;

/**
 * The ParserExtension interface allows developers to extend
 * node parsing remotely developed by adding new attributes
 * or child elements.  Clients can register one global ParserExtension
 * per namespace.  (We might restrict this to one ParserExtension
 * per object Class, but this would require NodeParser API changes.)
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParserExtension.java#0 $) $Date: 10-nov-2005.18:59:13 $
 */
public interface ParserExtension
{
  /**
   * Called before parsing of the parent element has started.
   * <p>
   * @param context the parsing context
   * @param namespaceURI the namespace of the element being processed
   * @param localName the local name of the element
   * @param attributes a Map of all the attributes in the
   *          namespace of this extension. This dictionary does
   *          not include values handled by extension NodeParsers.
   */
  public void elementStarted(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Map<?, ?>    attributes) throws SAXParseException;


  /**
   * Called to notify that a child element has been found
   * that was not handled by the outer parser.  If a NodeParser
   * is returned here, it will be used for handling that child element.
   * The value that NodeParser returns will be stored in the Map
   * eventually passed to elementEnded() - the key used is the local name
   * of the element.
   * <p>
   * @param context the parsing context
   * @param namespaceURI the namespace of the child element
   * @param localName the local name of the element
   * @param attrs the attributes attached to the element.
   * @return a node parser if one should be used, or null otherwise
   */
  public NodeParser startExtensionElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException;


  /**
   * Called after parsing of the parent element has completed
   * with extension values still to be processed.
   * <p>
   * @param context the parsing context
   * @param namespaceURI the namespace of the element being processed
   * @param localName the local name of the element
   * @param parsed the object instantiated for the element
   * @param values a Map of all the attributes in the
   *          namespace of this extension, plus values returned
   *          by child elements.
   */
  public Object elementEnded(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       parsed,
    Map<?, ?>    values) throws SAXParseException;
}
