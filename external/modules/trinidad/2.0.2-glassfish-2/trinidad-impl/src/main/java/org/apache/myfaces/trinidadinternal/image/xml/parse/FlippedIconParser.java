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

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;
/**
 * NodeParser for flippedIcon elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/FlippedIconParser.java#0 $) $Date: 10-nov-2005.19:04:04 $
 */
public class FlippedIconParser extends BaseImageProviderRequestParser
{
  public FlippedIconParser()
  {
    super(_PROPERTY_COUNT);
  }

  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    super.startElement(context, namespaceURI, localName, attrs);

    String source = getRequiredAttribute(context,
                                         attrs,
                                         XMLConstants.SOURCE_ATTR);

    if (source != null)
      setProperty(ImageConstants.SOURCE_KEY, source);

 }

  // Flipped icons have the following properties:
  // 1. SOURCE_KEY
  // 2. DIRECTION_KEY        (handled by BaseImageProviderRequestParser)
  // 3. WIDTH_RESPONSE_KEY   (handled by BaseImageProviderRequestParser)
  // 4. HEIGHT_RESPONSE_KEY  (handled by BaseImageProviderRequestParser)
  // 5. NAME_KEY             (handled by BaseImageProviderRequestParser)
  private static final int _PROPERTY_COUNT = 5;
}
