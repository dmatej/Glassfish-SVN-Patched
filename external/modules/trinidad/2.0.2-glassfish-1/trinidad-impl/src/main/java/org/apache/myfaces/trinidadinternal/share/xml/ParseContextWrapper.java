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

import org.xml.sax.Locator;

import org.apache.myfaces.trinidadinternal.share.expl.ExpressionContext;
import org.apache.myfaces.trinidadinternal.share.expl.Function;
import org.apache.myfaces.trinidadinternal.share.expl.PrefixMapper;
import org.apache.myfaces.trinidadinternal.share.expl.NSFunctionMapper;

/**
 * Wrapper class that turns a client-created ParseContext into
 * one sufficient for ParseContextImpl
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParseContextWrapper.java#0 $) $Date: 10-nov-2005.18:59:12 $
 */
final class ParseContextWrapper implements ParseContext
{
  /**
   * Creates a ParseContext that uses the provided
   * ParserManager to get NodeParsers.
   */
  public ParseContextWrapper(
    ParseContext  base,
    ParserManager manager,
    XMLProvider   provider)
  {
    _base = base;
    _manager = manager;
    _provider = provider;

    final PrefixMapper pmapper = new PrefixMapper()
      {
        public String getNamespaceURI(String prefix)
        {
          return getParserManager().resolveNamespaceAlias
            (_namespaces.getURI(prefix));
        }
      };

    final NSFunctionMapper fmapper =
      new NSFunctionMapper()
      {
        @Override
        public Function resolveFunction(String namespaceURI,
                                        String name)
        {
          return getParserManager().getFunction(namespaceURI, name);
        }
      };

    _bindingContext = new ExpressionContext()
      {
        public NSFunctionMapper getFunctionMapper()
        {
          return fmapper;
        }

        public PrefixMapper getPrefixMapper()
        {
          return pmapper;
        }
      };
  }


  /**
   * Returns the default node parser that shold be used
   * for a specific element name, given the type of expected object.
   * <p>
   * @param expectedType the Class of the Java object expected for this element
   * @param namespaceURI the namespace of the XML element
   * @param localName the local name of the XML element
   */
  public NodeParser getParser(
    Class<?>   expectedType,
    String     namespaceURI,
    String     localName)
  {
    return getParserManager().getParser(this,
                                        expectedType,
                                        namespaceURI,
                                        localName);
  }

  /**
   * Returns a parser extension.
   * <p>
   * @param currentType the Class of the Java object being built
   * @param namespaceURI the namespace of the XML element or attribute
   */
  public ParserExtension getExtension(
    String     namespaceURI)
  {
    return getParserManager().getExtension(namespaceURI);
  }

  /**
   * Returns the parser manager.
   */
  public ParserManager getParserManager()
  {
    return _manager;
  }


  /**
   * gets the bindingContext
   */
  public ExpressionContext getExpressionContext()
  {
    return _bindingContext;
  }

  /**
   * Convert a string prefix to a full namespace URI.
   * @param prefix the string prefix of the namspace, or the empty
   *     string for the default namespace
   * @return the URI of that namespace, or null if the prefix
   *    hasn't been mapped
   * @deprecated since 2.2.0 use {@link #getExpressionContext()} and
   * {@ExpressionContext#getPrefixMapper()}
   */
  @Deprecated
  public final String getNamespaceURI(String prefix)
  {
    return getExpressionContext().getPrefixMapper().getNamespaceURI(prefix);
  }


  /**
   * Return a SAX Locator object for identifying the document location.
   * @return a locator, or null if none is available
   */
  public Locator getLocator()
  {
    return _locator;
  }

  /**
   * Get an XMLProvider.
   */
  public XMLProvider getXMLProvider()
  {
    return _provider;
  }


  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(String namespace, Object key)
  {
    return _base.getProperty(namespace, key);
  }

  /**
   * Stores a property on the context.
   */
  public void setProperty(String namespace, Object key, Object value)
  {
    _base.setProperty(namespace, key, value);
  }


  /**
   *
   */
  @Override
  public Object clone()
  {
    return _base.clone();
  }


  /**
   * Set the locator.  A callback used by TreeBuilder.
   */
  void __setLocator(Locator locator)
  {
    _locator = locator;
  }


  /**
   * A callback used by TreeBuilder.
   */
  void __startElement()
  {
    if (!_contextPushed)
      _namespaces.pushContext();
    else
      _contextPushed = false;
  }

  /**
   * A callback used by TreeBuilder.
   */
  void __endElement()
  {
    _namespaces.popContext();
  }

  /**
   * A callback used by TreeBuilder.
   */
  void __addPrefixMapping(
    String prefix,
    String namespaceURI)
  {
    if (!_contextPushed)
    {
      _contextPushed = true;
      _namespaces.pushContext();
    }

    _namespaces.declarePrefix(prefix, namespaceURI);
  }


  ParseContext __getWrappedContext()
  {
    return _base;
  }

  private final ExpressionContext _bindingContext;
  private final ParseContext   _base;
  private final ParserManager  _manager;
  private final XMLProvider    _provider;

  // A SAX helper object that keeps track of namespace prefixes
  private NamespaceSupport _namespaces = new NamespaceSupport();

  // Boolean to keep track of whether we've called "pushContext()" yet.
  private boolean _contextPushed;

  // The SAX locator
  private Locator          _locator;
}
