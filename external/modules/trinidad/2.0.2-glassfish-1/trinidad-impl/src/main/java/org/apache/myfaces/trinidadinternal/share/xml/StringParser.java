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

/**
 * An implementation of NodeParser that will simply buffer up
 * the string contents of an element.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/StringParser.java#0 $) $Date: 10-nov-2005.18:59:15 $
 */
public class StringParser extends BaseNodeParser
{
  /**
   * Create a StringParser.
   */
  public StringParser()
  {
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    return _buffer.toString();
  }

  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length)
  {
    if (_savedWhitespace != null)
    {
      _buffer.append(_savedWhitespace);
      _savedWhitespace = null;
    }

    _buffer.append(text, start, length);
  }
  
  @Override
  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length)
  {
    // If we encounter "whitespace" in the middle of a string, save it off.
    // This should not ever happen - whitespace should only be at the beginning
    // and end - but somehow it does happen on rare occasions with the Oracle
    // XML parser.
    if (_buffer.length() > 0)
    {
      _savedWhitespace = new String(text, start, length);
    }
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    // All child elements should be ignored
    return null;
  }
  

  private String _savedWhitespace;
  private StringBuffer _buffer = new StringBuffer();
}
