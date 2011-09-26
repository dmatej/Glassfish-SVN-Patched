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
package org.apache.myfaces.trinidadinternal.agent.parse;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import java.net.URL;
import java.net.MalformedURLException;

/**
 * NodePaser for include nodes in the capabilities file
 */
class IncludeNodeParser extends BaseNodeParser implements XMLConstants
{

  @Override
  public void startElement (ParseContext context,
                            String       namespaceURI,
                            String       localName,
                            Attributes   attrs )
          throws SAXParseException
  {
    if (!NS_URI.equals(namespaceURI))
    {
      throw new SAXParseException(_LOG.getMessage(
        "INVALID_NAMESPACE", namespaceURI), context.getLocator());
    }

    String refId = attrs.getValue(ATTRIBUTE_REFID);
    String src = attrs.getValue(ATTRIBUTE_SRC);
    if (((refId == null) && (src == null)) ||
        ((refId != null) && (refId.length() <= 0)) ||
        ((src != null) && (src.length() <= 0)))
    {
      _LOG.warning("ELEMENT_MISSING_ATTRIBUTES", ELEMENT_INCLUDE);
      return;
    }

    _src = src;
    _refId = refId;
  }

  @Override
  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    URL srcUrl = _getUrl(context, _src);
    if ((_refId == null) && (srcUrl == null))
      return null;

    return new IncludeNode(_refId, srcUrl);
  }

  private URL _getUrl(ParseContext context, String url)
  {
    if (url == null)
      return null;


    URL baseURL = (URL) context.getProperty(NS_URI,
                                            CapabilitiesDocumentParser.__BASE_URL);
    try
    {
      if (baseURL != null)
        return  new URL(baseURL, url);

      return new URL(url);
    }
    catch (MalformedURLException mue)
    {
       _LOG.warning("INVALID_CAPABILITY_DATA_URL", _src);
    }

    return null;
  }


  private String _src;
  private String _refId;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(IncludeNodeParser.class);
}
