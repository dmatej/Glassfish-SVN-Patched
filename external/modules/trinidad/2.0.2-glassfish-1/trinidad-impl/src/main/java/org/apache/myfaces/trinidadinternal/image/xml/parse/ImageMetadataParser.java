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


import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.xml.XMLConstants;

/**
 * NodeParser for ImageMetadata elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ImageMetadataParser.java#0 $) $Date: 10-nov-2005.19:04:07 $
 */
public class ImageMetadataParser extends BaseNodeParser
{
  /**
   * Implementation of NodeParser.startElement
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    super.startElement(context, namespaceURI, localName, attrs);

    String version = attrs.getValue(XMLConstants.VERSION_ATTR);
    if ((!_20VERSION.equals(version)) && !_sLoggedVersionError)
    {
      if (_LOG.isFine())
        _LOG.fine(_VERSION_ERROR + context.getLocator().getSystemId());
      _sLoggedVersionError = true;
    }
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
    return _request;
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
    assert (_request == null);
    _request = (ImageProviderRequest)child;
  }

  private ImageProviderRequest _request;

  // We keep track of whether we have already logged the _VERSION_ERROR
  // message so that we only log it once.
  private static boolean _sLoggedVersionError = false;

  // The IMX version string for Cabo 2.0
  private static final String _20VERSION = "2.0";

  // Error message to display if old IMX files are detected
  private static final String _VERSION_ERROR =
    "Parsing error while loading the image cache.\n" +
    "A Cabo 1.0 IMX file has been detected.  For best results, please\n" +
    "remove any old files from the image cache directory containing:\n";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ImageMetadataParser.class);
}
