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
package org.apache.myfaces.trinidadinternal.share.xml.beans;

import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;

import java.util.Map;

/**
 * A ParserFactory that uses BeanDefs to define how to parse
 * elements.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/BeanParserFactory.java#0 $) $Date: 10-nov-2005.18:59:19 $
 */
public class BeanParserFactory implements ParserFactory
{
  /**
   * Creates a BeanParserFactory.
   * @param parserType the type of objects this factory is used to create
   * @param namespaceURI the namespace of elements this factory handles
   * @param beanType a Map that associates local names with
   *          BeanDef objects
   */
  public BeanParserFactory(
    Class<?>             parserType,
    String               namespaceURI,
    Map<String, BeanDef> beanTypes)
  {
    _parserType   = parserType;
    _namespaceURI = namespaceURI;
    _beanTypes    = beanTypes;
  }

  /**
   * Registers this factory on a ParserManager.
   */
  public void registerSelf(ParserManager manager)
  {
    manager.registerFactory(_parserType, _namespaceURI, this);
  }

  /**
   * Returns the handler used for creating Beans off XML
   * elements with the given namespace and local name.
   */
  public NodeParser getParser(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    BeanDef beanDef = getBeanDef(namespaceURI, localName);

    if (beanDef != null)
      return createBeanParser(context, beanDef);

    return null;
  }

  public BeanDef getBeanDef(
    String namespaceURI,
    String localName)
  {
    return _beanTypes.get(localName);
  }

  protected NodeParser createBeanParser(
    ParseContext context,
    BeanDef      beanDef)
  {
    return new BeanParser(beanDef);
  }

  private Class<?>             _parserType;
  private String               _namespaceURI;
  private Map<String, BeanDef> _beanTypes;
}
