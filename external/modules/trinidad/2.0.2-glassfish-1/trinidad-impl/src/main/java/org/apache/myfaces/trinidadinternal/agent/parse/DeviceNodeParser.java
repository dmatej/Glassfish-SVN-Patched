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
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import java.util.List;
import java.util.ArrayList;
import java.text.ParseException;

/**
 * NodePaser for device nodes in the capabilities file
 */
class DeviceNodeParser extends BaseNodeParser implements XMLConstants
{
  DeviceNodeParser()
  {
    _componentNodes = new ArrayList<DeviceComponentNode>();
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

    String id = attrs.getValue(ATTRIBUTE_ID);
    String model = attrs.getValue(ATTRIBUTE_MODEL);
    String extendsId = attrs.getValue(ATTRIBUTE_EXTENDS);
    if (((id == null) && (model == null)) ||
        ((id != null) && (id.trim().length() <= 0)) ||
        ((model != null) && (model.trim().length() <= 0)))
    {
      _LOG.warning("ELEMENT_MISSING_ATTRIBUTES", ELEMENT_DEVICE);
    }

    _id = id;
    _extendsId = extendsId;
    if (model != null)
    {
      try
      {
        //Really should be using only the name part. There is no version here
        _model = new NameVersion(model);
      }
      catch (ParseException pe)
      {
        _LOG.warning("UNABLE_PARSE_MODEL_STRING");
      }
    }
  }


  @Override
  public NodeParser startChildElement(ParseContext context,
                                      String       namespaceURI,
                                      String       localName,
                                      Attributes   attrs)
          throws SAXParseException
  {
    if (ELEMENT_COMPONENT.equals(localName))
      return new ComponentNodeParser();

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

    _componentNodes.add((DeviceComponentNode)child);
  }

  @Override
  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    if ((_id == null) && (_model == null))
      return null;

    DeviceComponentNode[] cNodes = 
            _componentNodes.toArray(
                    new DeviceComponentNode[_componentNodes.size()]);
    return new DeviceNode(_id, _model, _extendsId, cNodes);
  }


  private String _id;
  private NameVersion _model;
  private String _extendsId;
  private List<DeviceComponentNode> _componentNodes;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DeviceNodeParser.class);
}



