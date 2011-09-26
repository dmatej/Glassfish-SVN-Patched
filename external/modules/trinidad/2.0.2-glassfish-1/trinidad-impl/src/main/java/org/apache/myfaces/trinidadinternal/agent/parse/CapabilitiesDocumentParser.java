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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;

import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;


import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.net.URL;
import java.net.URLConnection;

/**
 * Parser for parsing capabilities document
 */
public class CapabilitiesDocumentParser extends BaseNodeParser
        implements XMLConstants
{

  private CapabilitiesDocumentParser()
  {
    _capabilitiesNodes = new ArrayList<CapabilitiesNode>();
    _deviceNodes = new ArrayList<DeviceNode>();
  }

  static public CapabilitiesDocument createInstance (URL capUrl)
  {
    //if URL is null return empty documen
    if (capUrl == null)
      return CapabilitiesDocument.EMPTY_DOCUMENT;

    CapabilitiesDocumentParser parser = new CapabilitiesDocumentParser();
    InputStream stream = null;
    try
    {
      URLConnection connection = capUrl.openConnection();
      stream = connection.getInputStream();

      InputSource source = new InputSource(stream);
      source.setSystemId(capUrl.toExternalForm());

      ParseContextImpl pc = new ParseContextImpl ();
      pc.setProperty(NS_URI, __BASE_URL, capUrl);
      TreeBuilder builder =  new  TreeBuilder();
      return (CapabilitiesDocument) builder.parse(null, source, pc, parser);

    }
    catch (SAXException saxe)
    {
      //Logging handled by tree builder
      //_LOG.severe("FAIL_PARSE_CAPABILITIES_DOCUMENT", saxe);
      ;
    }
    catch (IOException ioe)
    {
      _LOG.severe("FAIL_PARSE_CAPABILITIES_DOCUMENT", ioe);
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

    return CapabilitiesDocument.EMPTY_DOCUMENT;
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

    if (!ELEMENT_ROOT.equals(localName))
    {
      throw new SAXParseException(_LOG.getMessage(
        "INVALID_ROOT_ELEMENT", localName), context.getLocator());
    }
  }


  @Override
  public NodeParser startChildElement(ParseContext context,
                                      String       namespaceURI,
                                      String       localName,
                                      Attributes   attrs)
          throws SAXParseException
  {
    if (ELEMENT_AGENT_CAPABILITIES.equals(localName))
    {
      return this;
    }
    else if (ELEMENT_DEVICES.equals(localName))
    {
      return this;
    }
    else if (ELEMENT_CAPABILITIES.equals(localName))
    {
      return new CapabilitiesNodeParser();
    }
    else if (ELEMENT_DEVICE.equals(localName))
    {
      return new DeviceNodeParser();
    }

    //return null, if not a known element
    return null;
  }


  @Override
  public void addCompletedChild (ParseContext context,
                                 String       namespaceURI,
                                 String       localName,
                                 Object       child)
          throws SAXParseException
  {
    if (child == null)
      return;

    if (ELEMENT_CAPABILITIES.equals(localName))
      _capabilitiesNodes.add((CapabilitiesNode)child);

    if (ELEMENT_DEVICE.equals(localName))
      _deviceNodes.add((DeviceNode)child);
  }


  @Override
  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    if (!ELEMENT_ROOT.equals(localName))
      return null;

    CapabilitiesNode[] agents = null;
    if (_capabilitiesNodes.size() > 0)
    {
      agents = _capabilitiesNodes.toArray
              (new CapabilitiesNode[_capabilitiesNodes.size()]);
    }

    DeviceNode[] devices = null;
    if (_deviceNodes.size() > 0)
    {
      devices = _deviceNodes.toArray
              (new DeviceNode[_deviceNodes.size()]);
    }

    CapabilitiesDocument document = new CapabilitiesDocument(agents, devices);
    return document;
  }

  private List<CapabilitiesNode> _capabilitiesNodes;
  private List<DeviceNode> _deviceNodes;

  static final String __BASE_URL = "baseURL";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CapabilitiesDocumentParser.class);
}
