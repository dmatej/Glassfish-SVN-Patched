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

import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import java.util.ArrayList;
import java.util.List;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Node parser for component nodes in the capabilities file
 */
class ComponentNodeParser extends BaseNodeParser implements XMLConstants
{
  ComponentNodeParser()
  {
    _includeNodes = new ArrayList<IncludeNode>();
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

    _type = attrs.getValue(ATTRIBUTE_TYPE);

  }

  @Override
  public NodeParser startChildElement(ParseContext context,
                                      String       namespaceURI,
                                      String       localName,
                                      Attributes   attrs)
          throws SAXParseException
  {
    if (ELEMENT_INCLUDE.equals(localName))
      return new IncludeNodeParser();

    //return null; if unknown element
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

    _includeNodes.add((IncludeNode)child);
  }

  @Override
  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    ArrayList<IncludeNode> nodesWithRefList = new ArrayList<IncludeNode>(_includeNodes.size());
    ArrayList<IncludeNode> nodesWithSrcList = new ArrayList<IncludeNode>(_includeNodes.size());
    for (int i = 0; i < _includeNodes.size(); i++)
    {
      IncludeNode node = _includeNodes.get(i);
      if (node.__getRefId() != null)
        nodesWithRefList.add(node);
      else
        nodesWithSrcList.add(node);
    }
    IncludeNode[] nodesWithRef = 
            nodesWithRefList.toArray(new IncludeNode[nodesWithRefList.size()]);
    IncludeNode[] nodesWithSrc = 
            nodesWithSrcList.toArray(new IncludeNode[nodesWithSrcList.size()]);
    return new DeviceComponentNode(_type, nodesWithRef, nodesWithSrc);    
  }

  private List<IncludeNode> _includeNodes;
  private String _type;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ComponentNodeParser.class);
}
