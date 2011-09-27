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

import org.xml.sax.SAXException;
import org.xml.sax.InputSource;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.ArrayList;
import java.net.URL;
import java.net.URLConnection;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.CapabilityKey;
import org.apache.myfaces.trinidadinternal.agent.CapabilityValue;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;

/**
 * CapabilitiesData document parser
 */
public class CapabilityDataDocumentParser  extends BaseNodeParser
        implements XMLConstants
{

  private CapabilityDataDocumentParser()
  {
    _capList = new ArrayList<Object>();
  }

  static public Object[] parse (URL srcUrl)
  {
    //if URL is null return empty documen
    if (srcUrl == null)
      return new Object[0];

    CapabilityDataDocumentParser parser = new CapabilityDataDocumentParser();
    InputStream stream = null;
    try
    {
      URLConnection connection = srcUrl.openConnection();
      stream = connection.getInputStream();

      InputSource source = new InputSource(stream);
      source.setSystemId(srcUrl.toExternalForm());

      ParseContextImpl pc = new ParseContextImpl ();
      TreeBuilder builder =  new  TreeBuilder();
      return (Object[]) builder.parse(null, source, pc, parser);
    }
    catch (SAXException saxe)
    {
      _LOG.severe("FAIL_PARSE_CAPABILITIES_DATA_DOCUMENT", saxe);
    }
    catch (IOException ioe)
    {
      _LOG.severe("FAIL_PARSE_CAPABILITIES_DATA_DOCUMENT", ioe);
    }
    finally
    {
      try
      {
        if (stream != null)
          stream.close();
      }
      catch (IOException e)
      {
        //do nothing
        ;
      }
    }

    return new Object[0];
  }

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
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    if (ELEMENT_CAPABILITY.equals(localName))
    {
      String name = attrs.getValue(ATTRIBUTE_NAME);
      String value = attrs.getValue(ATTRIBUTE_VALUE);

      if (name != null)
        name = name.trim();
      if (value != null)
        value = value.trim();


      if ((name == null) || (name.length() <= 0) ||
          (value == null) || (value.length() <= 0))
      {
        _LOG.warning("ELEMENT_MISSING_ATTRIBUTES", ELEMENT_CAPABILITIES);
      }

      CapabilityKey key =
              CapabilityKey.getCapabilityKey(name, true);
      Object valueObject =
              CapabilityValue.getCapabilityValue(key, value);

      _capList.add(key);
      _capList.add(valueObject);

      return this;
    }

    return null;
  }


  @Override
  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    if (ELEMENT_CAPABILITY_DATA.equals(localName))
       return _capList.toArray(new Object[_capList.size()]);

    return null;
  }

  private List<Object> _capList;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CapabilityDataDocumentParser.class);

}
