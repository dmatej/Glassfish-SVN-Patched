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

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;

/**
 * NodeParser for ImageGenerator elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ImageGeneratorParser.java#0 $) $Date: 10-nov-2005.19:04:06 $
 */
public class ImageGeneratorParser extends BaseNodeParser
{

  /**
   * Implementations of NodeParser.startElement();
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    super.startElement(context, namespaceURI, localName, attrs);

    // Enable mnemonic stripping.
    context.setProperty(ImageConstants.TECATE_NAMESPACE,
                     BaseImageProviderRequestParser.__STRIP_MNEMONICS_PROPERTY,
                     Boolean.TRUE);
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
    // Disabled mnemonic stripping
    context.setProperty(ImageConstants.TECATE_NAMESPACE,
                     BaseImageProviderRequestParser.__STRIP_MNEMONICS_PROPERTY,
                     Boolean.FALSE);

    int count = _requests.size();
    ImageProviderRequest[] requests = new ImageProviderRequest[count];
    _requests.copyInto(requests);

    return requests;
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
    return context.getParser(ImageProviderRequest.class, 
                             namespaceURI, 
                             localName);
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    _requests.addElement((ImageProviderRequest)child);
  }

  // -= Simon Lessard =-
  // FIXME: Wow! A Vector... Change that to ArrayList
  //        or Collections.synchronizedList(ArrayList)
  private Vector<ImageProviderRequest> _requests = new Vector<ImageProviderRequest>();
}

