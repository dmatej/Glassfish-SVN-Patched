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
import org.apache.myfaces.trinidadinternal.share.expl.PrefixMapper;
import org.apache.myfaces.trinidadinternal.share.expl.NSFunctionMapper;

import org.apache.myfaces.trinidadinternal.share.util.NamespaceMap;

/**
 * The base implementation of ParseContext.  Clients can subclass
 * this if they wish to change the behavior, but a more common
 * use is simply creating a ParseContext so you can set properties.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParseContextImpl.java#0 $) $Date: 10-nov-2005.18:59:12 $
 */
public class ParseContextImpl implements ParseContext, Cloneable
{
  /**
   * Creates a ParseContext that uses the provided
   * ParserManager to get NodeParsers.
   */
  public ParseContextImpl()
  {
    this(null);
  }

  protected ParseContextImpl(ExpressionContext bc)
  {
    if (bc!=null)
      _bindingContext = bc;
    else
    {

      final PrefixMapper mapper = new PrefixMapper()
        {
          public String getNamespaceURI(String prefix)
          {
            // this is backwards: we are calling a deprecated method here;
            // however, we need to do this to maintain backwards compatibility:
            return ParseContextImpl.this.getNamespaceURI(prefix);
          }
        };

      _bindingContext= new ExpressionContext()
        {
          public PrefixMapper getPrefixMapper()
          {
            return mapper;
          }

          public NSFunctionMapper getFunctionMapper()
          {
            return null;
          }
        };

    }
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
    return null;
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
    return null;
  }

  /**
   * Returns the parser manager.
   */
  public ParserManager getParserManager()
  {
    return null;
  }

  /**
   * gets the bindingContext
   */
  public final ExpressionContext getExpressionContext()
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
   * {@link ExpressionContext#getPrefixMapper()}
   */
  @Deprecated
  public String getNamespaceURI(String prefix)
  {
    return null;
  }


  /**
   * Return a SAX Locator object for identifying the document location.
   * @return a locator, or null if none is available
   */
  public Locator getLocator()
  {
    return null;
  }

  /**
   * Get an XMLProvider.
   */
  public XMLProvider getXMLProvider()
  {
    return null;
  }


  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(String namespace, Object key)
  {
    return _properties.get(namespace, key);
  }

  /**
   * Stores a property on the context.
   */
  public void setProperty(String namespace, Object key, Object value)
  {
    if (value != null)
    {
      _properties.put(namespace, key, value);
    }
    else
    {
      _properties.remove(namespace, key);
    }
  }


  /**
   *
   */
  @Override
  public Object clone()
  {
    try
    {
      ParseContextImpl copy = (ParseContextImpl) super.clone();
      copy._properties = (NamespaceMap) _properties.clone();
      return copy;
    }
    catch (CloneNotSupportedException cnse)
    {
      return null;
    }
  }

  private final ExpressionContext _bindingContext;
  private NamespaceMap _properties = new NamespaceMap();
}
