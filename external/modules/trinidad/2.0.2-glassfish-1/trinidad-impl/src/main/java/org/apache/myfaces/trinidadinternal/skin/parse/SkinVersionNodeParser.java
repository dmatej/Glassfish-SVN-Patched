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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.xml.StringParser;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * This class is used when we parse the trinidad-skins.xml file and there is a 'version' element
 * as a child of the 'skin' element. A 'version' has both a 'name' (required) and 
 * a 'default' (optional) element. After parsing, a SkinVersion object is created and attached
 * to the Skin object.
 * @see SkinVersionNode
 * @see org.apache.myfaces.trinidadinternal.skin.SkinUtils
 * @see org.apache.myfaces.trinidad.skin.SkinVersion
 */
public class SkinVersionNodeParser  extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    // allow no name in version.
    if (_name == null)
      _name = "";

    return new SkinVersionNode(_name, "true".equals(_default));
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {

    if ("name".equals(localName) ||
        "default".equals(localName))

    {
      return new StringParser();
    }

    return null;
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {

    if ("name".equals(localName))
      _name = (String) child;
    else if ("default".equals(localName))
    {
      _default = (String) child;
      // <default/> means true
      if (_default == null || "".equals(_default))
        _default = "true";
    }
  }

  private String _name;
  private String _default;

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SkinVersionNodeParser.class);
}
