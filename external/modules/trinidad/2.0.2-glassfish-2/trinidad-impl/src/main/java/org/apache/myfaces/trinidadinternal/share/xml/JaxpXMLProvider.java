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
package org.apache.myfaces.trinidadinternal.share.xml;

import java.io.IOException;

import java.lang.reflect.UndeclaredThrowableException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.w3c.dom.Document;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

/**
 * Implementation of XMLProvider that uses JAXP to access a parser.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/JaxpXMLProvider.java#0 $) $Date: 10-nov-2005.18:59:08 $
 */
public class JaxpXMLProvider implements XMLProvider
{
  /**
   * Returns an implementation of the SAX2 XMLReader interface.
   */
  public XMLReader getXMLReader()
  {
    try
    {
      return _SAX_PARSER_FACTORY.newSAXParser().getXMLReader();
    }
    catch (ParserConfigurationException pce)
    {
      _LOG.severe(pce);
    }
    catch (SAXException saxe)
    {
      throw new UndeclaredThrowableException(saxe);
    }
    catch (Error e)
    {
      _LOG.severe(e);
    }

    return null;
  }

  /**
   * Implementation of XMLProvider.parseDocument().
   */
  public Document parseDocument(InputSource source)
     throws IOException, SAXException
  {
    try
    {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware(true);
      return factory.newDocumentBuilder().parse(source);
    }
    catch (ParserConfigurationException pce)
    {
      _LOG.severe(pce);
    }
    catch (Error e)
    {
      _LOG.severe(e);
    }

    return null;
  }
  
  private static final SAXParserFactory _SAX_PARSER_FACTORY;
  static
  {
      _SAX_PARSER_FACTORY = SAXParserFactory.newInstance();
      _SAX_PARSER_FACTORY.setNamespaceAware(true);
  } 
  
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(JaxpXMLProvider.class);
}
