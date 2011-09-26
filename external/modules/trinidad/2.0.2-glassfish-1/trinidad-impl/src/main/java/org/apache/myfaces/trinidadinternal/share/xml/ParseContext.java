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

import org.xml.sax.Locator;

import org.apache.myfaces.trinidadinternal.share.expl.ExpressionContext;

/**
 * An interface providing contextual information for the current
 * parse state.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParseContext.java#0 $) $Date: 10-nov-2005.18:59:11 $
 */
public interface ParseContext extends Cloneable
{
  /**
   * Returns the default node parser that should be used
   * for a specific element name, given the type of expected object.
   * <p>
   * @param expectedType the Class of the Java object expected for this element
   * @param namespaceURI the namespace of the XML element
   * @param localName the local name of the XML element
   */
  public NodeParser getParser(
    Class<?>   expectedType,
    String     namespaceURI,
    String     localName);


  /**
   * Returns a parser extension.
   * <p>
   * @param namespaceURI the namespace of the XML element or attribute
   */
  public ParserExtension getExtension(String namespaceURI);



  /**
   * Returns the parser manager.
   */
  public ParserManager getParserManager();

  /**
   * gets the bindingContext
   */
  public ExpressionContext getExpressionContext();

  /**
   * Convert a string prefix to a full namespace URI.
   * @param prefix the string prefix of the namspace, or the empty
   *     string for the default namespace
   * @return the URI of that namespace, or null if the prefix
   *    hasn't been mapped
   * @deprecated since 2.2.0 use {@link #getExpressionContext()} and
   * {@link ExpressionContext#getPrefixMapper()}
   */
  @Deprecated
  public String getNamespaceURI(String prefix);


  /**
   * Return a SAX Locator object for identifying the document location.
   * @return a locator, or null if none is available
   */
  public Locator getLocator();


  /**
   * Get an XMLProvider.
   */
  public XMLProvider getXMLProvider();


  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(String namespace, Object key);


  /**
   * Stores a property on the context.
   */
  public void setProperty(String namespace, Object key, Object value);

  /**
   * Clones the ParseContext so that it can be used for a new
   * set of parsing.
   */
  public Object clone();
}
