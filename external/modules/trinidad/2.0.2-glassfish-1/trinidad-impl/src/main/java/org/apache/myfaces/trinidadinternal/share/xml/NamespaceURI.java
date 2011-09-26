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

import org.apache.myfaces.trinidadinternal.share.expl.PrefixMapper;
import org.apache.myfaces.trinidadinternal.share.expl.ExpressionContext;

/**
 * Utility class to parse a string of the form
 * <CODE>namespacePrefix:name</CODE>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/NamespaceURI.java#0 $) $Date: 10-nov-2005.18:59:10 $
 */
public final class NamespaceURI
{
  public NamespaceURI(String namespace, String name)
  {
    _ns = namespace.intern();
    _name = name;
  }

  /**
   * @return the namespace of this URI
   */
  public String getNamespace()
  {
    return _ns;
  }

  /**
   * @return the name portion of this URI
   */
  public String getName()
  {
    return _name;
  }

  @Override
  public boolean equals(Object obj)
  {
    if (obj instanceof NamespaceURI)
    {
      NamespaceURI ns = (NamespaceURI) obj;
      return (ns._ns.equals(_ns) && ns._name.equals(_name));
    }
    return false;
  }

  @Override
  public int hashCode()
  {
    // =-= ACW: we need a better function to combine hashcodes. for now lets
    // just add. we should also cache this at some point.
    return _ns.hashCode() + _name.hashCode();
  }

  /**
   * Create a NamespaceURI instance from a qualified name of the form
   * <CODE>prefix:name</CODE>, using a default namespace uri for the
   * cases when no prefix is specified.
   *
   * @param pmapper    the prefix to namespace mapper
   * @param qname      the qualified name
   * @param defaultURI the default namespace uri

   * @return a NamespaceURI instance
   *
   * @throws IllegalArgumentException when prefix is not declared
   */
  public static NamespaceURI create(ExpressionContext context,
                                    String qname,
                                    String defaultURI)
  {
    PrefixMapper pmapper = context.getPrefixMapper();

    String uri;
    String name;
    // jmw test
    // af|breadCrumbs::separator-icon fails, because it thinks that
    // af|breadCrumbs is the prefix.
    // distinguish between single quotes and double quotes for now.

    // jmw test
    // shuttleRemoveAllIcon:alias fails. Ignore anything before :alias
    int doubleColonIndex = qname.indexOf("::");
    int colonIndex = qname.indexOf(':');
    boolean isAlias = (qname.indexOf(":alias") > -1);
    if (colonIndex > 0 && (colonIndex != doubleColonIndex) && !isAlias)
    {
      String prefix = qname.substring(0, colonIndex);

      uri = pmapper.getNamespaceURI(prefix);

      if (uri == null)
        throw new IllegalArgumentException(_LOG.getMessage(
          "UNDECLARED_PREFIX", prefix));

      name = qname.substring(colonIndex + 1);
    }
    else
    {
      name = qname;
      uri  = defaultURI;
    }

    return new NamespaceURI(uri, name);
  }

  /**
   * convinience for creating NamspaceURIs. this method calls
   * {@link #create(ExpressionContext, String,String)}
   * @deprecated since 2.2.0. use
   * {@link #create(ExpressionContext,String,String)}
   */
  @Deprecated
  public static NamespaceURI create(ParseContext context,
                                    String qname,
                                    String defaultURI)
  {
    try
    {
      return create(context.getExpressionContext(), qname, defaultURI);
    }
    catch (RuntimeException e)
    {
      _LOG.severe(e);
    }
    return null;
  }

  private final String _ns;
  private final String _name;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(NamespaceURI.class);
}
