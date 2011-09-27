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
package org.apache.myfaces.trinidadinternal.skin.parse;

import java.util.ArrayList;

import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * NodeParser for &lt;skins&gt; element in trinidad-skins.xml
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinsNodeParser.java#0 $) $Date: 10-nov-2005.18:50:46 $
 */
public class SkinsNodeParser extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    if ("skin-addition".equals(localName))
      return context.getParser(SkinAdditionNode.class, namespaceURI, localName);
    else
      return context.getParser(SkinNode.class, namespaceURI, localName);
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    assert ((child == null) || 
            (child instanceof SkinNode) ||
            (child instanceof SkinAdditionNode));
    
    if ((child instanceof SkinAdditionNode))    
      _skinAdditions.add((SkinAdditionNode)child);
    else
      _skins.add((SkinNode)child);
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    return new SkinsNode(_skins,
                         _skinAdditions);
  }

  private List<SkinNode> _skins = new ArrayList<SkinNode>();
  private List<SkinAdditionNode> _skinAdditions = new ArrayList<SkinAdditionNode>();
  
}
