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

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.share.expl.ExpressionParser;
import org.apache.myfaces.trinidadinternal.share.expl.Function;
import org.apache.myfaces.trinidadinternal.share.util.NamespaceMap;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ParserManager maintains a table of ParserFactories, keyed
 * by return-type Class.  Clients can use a single ParserManager, or
 * create their own.  By default, none of the ParserManagers
 * have any registered ParserFactories.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParserManager.java#0 $) $Date: 10-nov-2005.18:59:15 $
 */

public class ParserManager implements Cloneable
{
  /**
   * Creates a new, empty ParserManager.
   */
  public ParserManager()
  {
  }

  /**
   * Returns a globally shared instance of ParserManager.
   */
  static public ParserManager getDefaultParserManager()
  {
    return _sDefaultInstance;
  }


  /**
   * Utility method for retrieving a NodeParser.
   */
  final public NodeParser getParser(
    ParseContext context,
    Class<?>     expectedType,
    String       namespaceURI,
    String       localName)
  {
    ParserFactory factory = getFactory(expectedType, namespaceURI);
    if (factory == null)
      return null;

    return factory.getParser(context, namespaceURI, localName);
  }



  /**
   * Gets the factory registered for the namespace.
   */
  public ParserFactory getFactory(
    Class<?> expectedType,
    String   namespaceURI)
  {
    return (ParserFactory)
      _factories.get(resolveNamespaceAlias(namespaceURI),
                     expectedType);
  }


  /**
   * Registers a factory for a type and namespace.
   */
  synchronized public void registerFactory(
     Class<?>      expectedType,
     String        namespaceURI,
     ParserFactory factory)
  {
    _unshareState();
    _factories.put(resolveNamespaceAlias(namespaceURI),
                   expectedType,
                   factory);
  }


  /**
   * Unregisters a factory for a type and namespace.
   */
  synchronized public void unregisterFactory(
    Class<?> expectedType,
    String   namespaceURI)
  {
    _unshareState();
    _factories.remove(resolveNamespaceAlias(namespaceURI),
                      expectedType);
  }


  /**
   * Gets the extension registered for the namespace.
   */
  public ParserExtension getExtension(
    String namespaceURI)
  {
    return _extensions.get(resolveNamespaceAlias(namespaceURI));
  }


  /**
   * Registers an extension for a namespace.
   */
  synchronized public void registerExtension(
     String          namespaceURI,
     ParserExtension extension)
  {
    _unshareState();
    _extensions.put(resolveNamespaceAlias(namespaceURI), extension);
  }


  /**
   * Unregisters an extension for a namespace.
   */
  synchronized public void unregisterExtension(String namespaceURI)
  {
    _unshareState();
    _extensions.remove(resolveNamespaceAlias(namespaceURI));
  }

  /**
   * Registers a namespace alias, used for backwards
   * compatibility.  Note that this (mostly) only affects the
   * finding of parser factories and extensions - the
   * NodeParser will still see the old namespace.  This
   * method also affects ParseContext.getNamespaceURI().
   */
  synchronized public void registerNamespaceAlias(
    String mainNamespaceURI,
    String aliasURI)
  {
    _unshareState();
    _aliases.put(aliasURI, mainNamespaceURI);
  }

  /**
   *
   */
  public String resolveNamespaceAlias(String namespaceURI)
  {
    if (namespaceURI == null)
      return null;

    Object o = _aliases.get(namespaceURI);
    if (o == null)
      return namespaceURI;

    return (String) o;
  }

  /**
   * gets a registered ExpressionParser with the given name.
   * @param name the name of the ExpressionParser. If this is null, the
   * default ExpressionParser will be returned.
   * @see #registerExpressionParser
   * @see #setDefaultExpressionParser
   */
  public final ExpressionParser getExpressionParser(String name)
  {
    return name == null ? _defaultExpressionParser : _bindingParsers.get(name);
  }

  /**
   * registers a binding parser. the parser is registered under its name.
   */
  public final synchronized void registerExpressionParser(
    ExpressionParser bindingParser)
  {
    _unshareState();
    _bindingParsers.put(bindingParser.getName(), bindingParser);
  }

  /**
   * sets the default bindingParser
   */
  public final void setDefaultExpressionParser(ExpressionParser parser)
  {
    if (parser==null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_PARSER"));
    _defaultExpressionParser = parser;
  }

  /**
   * gets a method for the given QName
   */
  public final Function getFunction(String namespace, String name)
  {
    return (Function) _functions.get(namespace, name);
  }

  /**
   * registers a method for a given QName
   * @see XMLUtils#registerFunctions
   */
  public final synchronized void registerFunction(String namespace,
                                                  String name, Function method)
  {
    _unshareState();
    _functions.put(namespace, name, method);
  }

  /**
   * Makes a deep copy of the ParserManager.
   */
  @Override
  synchronized public Object clone()
  {
    try
    {
      // it is very important that we set this to true, before we clone:
      _sharedState = true;

      // Optimize to lazily clone the contents
      ParserManager pm = (ParserManager) super.clone();
      return pm;
    }
    catch (CloneNotSupportedException cnse)
    {
      // Shouldn't happen
      throw new IllegalStateException();
    }
  }

  // Unshare any parts of the state that have been shared.
  // Must be called by synchronized functions!
  @SuppressWarnings("unchecked")
  private synchronized void _unshareState()
  {
    if (_sharedState)
    {
      _functions      = (NamespaceMap) _functions.clone();
      _factories      = (NamespaceMap) _factories.clone();
      _extensions     = (ArrayMap<String, ParserExtension>)  _extensions.clone();
      _aliases        = (ArrayMap<String, String>)           _aliases.clone();
      _bindingParsers = (ArrayMap<String, ExpressionParser>) _bindingParsers.clone();

      _sharedState = false;
    }
  }

  private NamespaceMap _functions = new NamespaceMap();
  private NamespaceMap _factories = new NamespaceMap();
  private ArrayMap<String, ParserExtension>  _extensions     = new ArrayMap<String, ParserExtension>(5);
  private ArrayMap<String, String>           _aliases        = new ArrayMap<String, String>(5);
  private ArrayMap<String, ExpressionParser> _bindingParsers = new ArrayMap<String, ExpressionParser>(2);
  private ExpressionParser _defaultExpressionParser = null;

  // If true, our state is shared with another parser manager - so
  // make a copy before mutating.  This value can easily have
  // false positives;  for instance, we don't store _which_ PM
  // we're sharing state with, so if both mutate their state,
  // then both will make copies even though only one really needed to.
  private boolean      _sharedState = false;

  static private final ParserManager _sDefaultInstance =
    new ParserManager();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ParserManager.class);
}
