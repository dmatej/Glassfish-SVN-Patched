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

import java.awt.Color;

import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;


/**
 * Factory for creating ColorParsers
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ColorParserFactory.java#0 $) $Date: 10-nov-2005.19:04:02 $
 */
public class ColorParserFactory implements ParserFactory
{
  /**
   * Gets the shared ColorParserFactory instance.
   */
  public static ColorParserFactory sharedInstance()
  {
    return _sInstance;
  }

  /**
   * Registers this factory on a ParserManager.
   */
  public void registerSelf(ParserManager manager, String namespace)
  {
    // We support parsing of both a single ImageProviderRequest as 
    // well as a set of ImageProviderRequests.
    manager.registerFactory(Color.class, namespace, this);
  }

  /**
   * Returns the parser used for creating ImageProviderRequest
   * instances with the given namespace and local name.
   */
  public NodeParser getParser(
    ParseContext context,                                
    String       namespaceURI,
    String       localName)
  {
    return new ColorParser();
  }

  private ColorParserFactory() {}

  private static final ColorParserFactory _sInstance = 
    new ColorParserFactory();
}
