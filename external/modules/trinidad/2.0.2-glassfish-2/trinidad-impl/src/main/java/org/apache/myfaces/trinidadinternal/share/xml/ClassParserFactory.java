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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * ParserFactory that instantiates parsers for
 * a given class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ClassParserFactory.java#0 $) $Date: 10-nov-2005.18:59:08 $
 */
public class ClassParserFactory implements ParserFactory
{
  /**
   * Creates a ClassParserFactory.
   * @param classObject a Class instance that must be a subclass of
   *   NodeParser
   */
  public ClassParserFactory(Class<?> classObject)
  {
    if (classObject == null)
      throw new NullPointerException();

    _class = classObject;
    _checkClass();
  }


  /**
   * Creates a ClassParserFactory.
   * @param className the full name of a class that must be a subclass
   *   of NodeParser
   */
  public ClassParserFactory(String className)
  {
    try
    {
      _class = ClassLoaderUtils.loadClass(className);
    }
    catch (ClassNotFoundException e)
    {
      _LOG.severe(e);
    }

    _checkClass();
  }


  /**
   * Returns a new NodeParser from the class.
   */
  public NodeParser getParser(
    ParseContext context,
    String       namespaceURI,
    String       localName)
  {
    NodeParser parser = null;

    try
    {
      parser = (NodeParser)_class.newInstance();
    }
    catch (IllegalAccessException e)
    {
      _LOG.severe(e);
    }
    catch (InstantiationException e)
    {
      _LOG.severe(e);
    }

    return parser;
  }


  // Make sure the class is non-null and is a subclass of NodeParser
  private void _checkClass()
  {
    assert (_class != null);
    assert (NodeParser.class.isAssignableFrom(_class));
  }

  private Class<?> _class;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ClassParserFactory.class);
}
