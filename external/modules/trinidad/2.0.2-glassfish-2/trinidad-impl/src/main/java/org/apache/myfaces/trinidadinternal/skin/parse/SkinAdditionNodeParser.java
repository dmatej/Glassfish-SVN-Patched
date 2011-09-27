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
 * NodeParser for &lt;skin-addition&gt; node in trinidad-skins.xml
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinExtensionParser.java#0 $) $Date: 10-nov-2005.18:50:44 $
 * @todo ELIMINATE NAMESPACE
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SkinAdditionNodeParser extends BaseNodeParser
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

    // id is required for a SkinAddition. log a severe error if it is null.
    if (_skinId == null)
      _LOG.severe("REQUIRED_ELEMENT_SKINID_NOT_FOUND");

    if ((_resourceBundleName != null) && (_translationSourceExpression != null))
    {
      _LOG.severe("BOTH_BUNDLENAME_TRANSLATIONSOURCE_SET");
      _translationSourceExpression = null;
    }

    if (_translationSourceExpression != null &&
        !(_translationSourceExpression.startsWith("#{") &&
        _translationSourceExpression.endsWith("}")))
    {
      _LOG.severe("TRANSLATION_SOURCE_NOT_EL");
      _translationSourceExpression = null;
    }

    return new SkinAdditionNode(_skinId, _styleSheetName,
                                _resourceBundleName, _translationSourceExpression);
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {

    if ("skin-id".equals(localName) ||
        "style-sheet-name".equals(localName) ||
        "bundle-name".equals(localName) ||
        "translation-source".equals(localName))

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

    if ("skin-id".equals(localName))
      _skinId = (String) child;
    else if ("style-sheet-name".equals(localName))
      _styleSheetName = (String) child;
    else if ("bundle-name".equals(localName))
      _resourceBundleName = (String) child;
    else if ("translation-source".equals(localName))
      _translationSourceExpression = (String) child;
  }

  private String _skinId;
  private String _styleSheetName;
  private String _resourceBundleName;
  private String _translationSourceExpression;


  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SkinAdditionNodeParser.class);

}
