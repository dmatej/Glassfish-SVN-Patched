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

import java.io.InputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.config.ConfigurationImpl;

import org.apache.myfaces.trinidadinternal.share.io.CachingInputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;

import org.apache.myfaces.trinidadinternal.share.expl.JavaMethod;

/**
 * Utility class for XML parsing.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/XMLUtils.java#0 $) $Date: 10-nov-2005.18:59:17 $
 */
public class XMLUtils
{
  /**
   * Parses an include of an XML file.  The include will be located using
   * an already-stored NameResolver object.
   *
   * @see #getResolver
   * @see #setResolver
   * @param context the current ParseContext, which will be cloned
   * @param sourceName the name of the target, relative to the current file
   * @param expectedType the expected Java type of the target.
   */
  @SuppressWarnings("unchecked")
  static public Object parseInclude(
    ParseContext context,
    String       sourceName,
    Class<?>     expectedType) throws IOException, SAXException
  {
    // Step 1. Find the name resolver.
    NameResolver resolver = getResolver(context);
    if (resolver == null)
    {
      _log(context, "Internal error: couldn't find NameResolver");
      return null;
    }

    // Step 2. Find an InputStreamProvider.  Mark a dependency on the base
    // provider (if necessary)
    InputStreamProvider provider = resolver.getProvider(sourceName);
    InputStreamProvider baseProvider = getInputStreamProvider(context);
    if (baseProvider instanceof CachingInputStreamProvider)
    {
      // set the dependency; hasSourceChanged also checks if the 
      // dependencies have changed
      ((CachingInputStreamProvider) baseProvider).addCacheDependency(provider);
    }

    // Step 3. Detect if this will be a circular include.
    ArrayList<Object> list = 
      (ArrayList<Object>) context.getProperty(_SHARE_NAMESPACE, "_includeStack");
    Object identifier = provider.getIdentifier();
    
    if ((list != null) && (list.contains(identifier)))
    {
      // =-=AEW Just logging an error isn't really enough - the include
      // will fail, but parsing continues.  So, instead, we throw
      // an exception...
      throw new SAXParseException(_LOG.getMessage(
        "CIRCULAR_INCLUDE_DETECTED", sourceName), context.getLocator());
    }

    // Step 4. Try to get a cached version
    // =-=jmw I don't see when this cached gets a non-null value other than if the same file
    // is included twice.
    Object cached = provider.getCachedResult();
    if ((cached != null) && expectedType.isInstance(cached))
      return cached;


    // Step 5. Set up the new context;  first, clone the original
    ParseContext newContext = (ParseContext) context.clone();

    // Add the current identifer to the stack (used for detecting
    // circular includes) placed on the ParseContext
    if (list == null)
      list = new ArrayList<Object>();
    else
      list = (ArrayList<Object>) list.clone();
    list.add(identifier);
    newContext.setProperty(_SHARE_NAMESPACE, "_includeStack", list);


    ParserManager parserManager =context.getParserManager();
    TreeBuilder builder = new TreeBuilder(parserManager,
                                          expectedType);
    InputStream stream = provider.openInputStream();

    try
    {
      InputSource source = new InputSource(stream);
      source.setSystemId(sourceName);

      setResolver(newContext, resolver.getResolver(sourceName));
      setInputStreamProvider(newContext, provider);

      // Step 6. Parse!
      Object value = builder.parse(context.getXMLProvider(),
                                   source,
                                   newContext);

      // Step 7. Store the cached result (if successful)
      if (value != null)
        provider.setCachedResult(value);

      return value;
    }
    finally
    {
      stream.close();
    }
  }


  /**
   * Gets the NameResolver stored on the ParseContext.
   */
  static public NameResolver getResolver(ParseContext context)
  {
    NameResolver source = (NameResolver)
      context.getProperty(_SHARE_NAMESPACE, "_nameResolver");
    if (source == null)
      source = new DefaultNameResolver(null, null);

    return source;
  }

  /**
   * Stores a NameResolver on a ParseContext.
   */
  static public void setResolver(ParseContext context, NameResolver source)
  {
    context.setProperty(_SHARE_NAMESPACE, "_nameResolver", source);
  }



  /**
   * Gets the InputStreamProvider stored on the ParseContext.
   */
  static public InputStreamProvider getInputStreamProvider(
     ParseContext context)
  {
    return (InputStreamProvider)
      context.getProperty(_SHARE_NAMESPACE, "_provider");
  }

  /**
   * Stores a InputStreamProvider on a ParseContext.
   */
  static public void setInputStreamProvider(
    ParseContext context, InputStreamProvider provider)
  {
    context.setProperty(_SHARE_NAMESPACE, "_provider", provider);
  }



  /**
   * Convenience function for setting an XML provider on a
   * configuration.
   */
  static public void setXMLProvider(
    ConfigurationImpl config,
    String            providerClassName)
  {
    config.putProperty(Configuration.XML_PROVIDER,
                       providerClassName);
  }


  /**
   * Sets a "local" ParseContext property.  ParseContext properties
   * set with the standard setProperty() method will still be
   * available in included files, and if set inside an included
   * file will be available to the parent file.  Local properties
   * are available to this document only.
   * @see #getLocalProperty
   */
  static public void setLocalProperty(
    ParseContext context,
    String       namespace,
    Object       key,
    Object       value)
  {
    if (value != null)
      value = new Object[]{context, value};

    context.setProperty(namespace, key, value);
  }



  /**
   * Gets a "local" ParseContext property.  ParseContext properties
   * set with the standard setProperty() method will still be
   * available in included files, and if set inside an included
   * file will be available to the parent file.  Local properties
   * are available to this document only.
   * @see #setLocalProperty
   */
  static public Object getLocalProperty(
    ParseContext context,
    String       namespace,
    Object       key)
  {
    Object o = context.getProperty(namespace, key);
    if (o instanceof Object[])
    {
      Object[] array = (Object[]) o;
      if ((array.length == 2) && (array[0] instanceof ParseContext))
      {
        ParseContext contextWhenSet = (ParseContext) array[0];

        if (_unwrapContext(contextWhenSet) == _unwrapContext(context))
        {
          return array[1];
        }
      }
    }

    return null;
  }

  /**
   * Convenience object for getting an XML provider off
   * a Configuration object.
   * @param config the configuration object
   * @param log an (optional) error log
   * @return an XMLProvider implementation
   */
  static public XMLProvider getXMLProvider(
    Configuration config)
  {
    return new JaxpXMLProvider();
  }

  /**
   * Parses a whitespace separated series of name tokens.
   * @param stringValue the full string
   * @return an array of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   */
  static public String[] parseNameTokens(String stringValue)
  {
    List<String> list = parseNameTokensAsList(stringValue);

    if (list == null)
      return null;
    else
      return list.toArray(new String[list.size()]);

  }

  /**
   * Parses a whitespace separated series of name tokens.
   * @param stringValue the full string
   * @return a set of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   */
  static public Set<String> parseNameTokensAsSet(String stringValue)
  {
    List<String> list = parseNameTokensAsList(stringValue);

    if (list == null)
      return null;
    else
      return new HashSet(list);
  }

  /**
   * Parses a whitespace separated series of name tokens.
   * @param stringValue the full string
   * @return a list of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   */
  static public List<String> parseNameTokensAsList(String stringValue)
  {
    if (stringValue == null)
      return null;

    ArrayList<String> list = new ArrayList<String>(5);

    int     length = stringValue.length();
    boolean inSpace = true;
    int     start = 0;
    for (int i = 0; i < length; i++)
    {
      char ch = stringValue.charAt(i);

      // We're in whitespace;  if we've just departed
      // a run of non-whitespace, append a string.
      // Now, why do we use the supposedly deprecated "Character.isSpace()"
      // function instead of "isWhitespace"?  We're following XML rules
      // here for the meaning of whitespace, which specifically
      // EXCLUDES general Unicode spaces.
      if (Character.isWhitespace(ch))
      {
        if (!inSpace)
        {
          list.add(stringValue.substring(start, i));
          inSpace = true;
        }
      }
      // We're out of whitespace;  if we've just departed
      // a run of whitespace, start keeping track of this string
      else
      {
        if (inSpace)
        {
          start = i;
          inSpace = false;
        }
      }
    }

    if (!inSpace)
      list.add(stringValue.substring(start));

    if (list.isEmpty())
      return null;

    return list;
  }

  /**
   * add the methods of a class to the list of available EL functions.
   * @param namespace the namespace to register the methods under
   * @param klass only the public static methods declared on this class are
   * inspected
   */
  public static void registerFunctions(
      ParserManager manager,
      String namespace,
      Class<?> klass)
  {
    Method[] methods = klass.getDeclaredMethods();
    for(int i=0; i<methods.length; i++)
    {
      Method met = methods[i];
      int mod = met.getModifiers();
      if (Modifier.isStatic(mod) && Modifier.isPublic(mod))
      {
        manager.registerFunction(namespace, met.getName(),
                                 new JavaMethod(met));
      }
    }
  }


  static private ParseContext _unwrapContext(ParseContext context)
  {
    if (context instanceof ParseContextWrapper)
      context = ((ParseContextWrapper) context).__getWrappedContext();

    return context;
  }

  private static void _log(ParseContext context, String message)
  {
    if (_LOG.isWarning())
      _LOG.warning(ParseErrorUtils.getErrorMessage(context, message));
  }

  private XMLUtils()
  {
  }

  // Perhaps move to ShareConstants
  static private final String _SHARE_NAMESPACE  =
    "http://myfaces.apache.org/uix/share";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(XMLUtils.class);
}
