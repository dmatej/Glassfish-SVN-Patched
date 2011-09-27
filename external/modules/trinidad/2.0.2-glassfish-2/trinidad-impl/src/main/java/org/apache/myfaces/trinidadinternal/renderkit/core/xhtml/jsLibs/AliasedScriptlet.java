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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs;

import java.io.IOException;
import java.io.InputStream;

import java.util.Enumeration;
import java.util.HashSet;
import java.util.Properties;


import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/AliasedScriptlet.java#0 $) $Date: 10-nov-2005.19:02:43 $
 */
public class AliasedScriptlet extends Scriptlet
{
  /**
   * Registers all the base libraries.
   */
  static public void registerAliases()
  {
    HashSet<String> aliases = new HashSet<String>();
    Enumeration<?> names = _ALIASES.propertyNames();
    while (names.hasMoreElements())
    {
      aliases.add(_ALIASES.getProperty((String) names.nextElement()));
    }

    for(String libName : aliases)
    {
      Scriptlet scriptlet = null;

      if ("Common".equals(libName))
      {
        scriptlet = new LibraryScriptlet(libName, null)
        {
          // =-=AEW  The Common library needs to be rendered even
          // when it's outside of a partial page request, since the
          // partial page library itself needs it.
          // See comments in Scriptlet.__isOutsidePartialPage()
          @Override
          boolean __isOutsidePartialPage(RenderingContext arc)
          {
            return false;
          }
        };
      }
      else
      {
        scriptlet = new LibraryScriptlet(libName, null);
      }

      scriptlet.registerSelf();
    }
  }

  public AliasedScriptlet(
    String name,
    Object[] functions)
  {
    this(name, functions, null);
  }

  public AliasedScriptlet(
    String name,
    Object[] functions,
    Object[] dependencies)
  {
    _name = name;
    _functions = (functions == null) ? null : functions.clone();
    _dependencies = (dependencies == null) ? null : dependencies.clone();
    _actualLibrary = _getAlias(name);
  }

  @Override
  public Object getScriptletKey()
  {
    return _name;
  }

  @Override
  public void registerSelf()
  {
    super.registerSelf();
    if (_functions != null)
    {
      for (int i = 0; i < _functions.length; i++)
        registerSelfWithKey(_functions[i]);
    }
  }

  @Override
  protected void outputScriptletImpl(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    if (_dependencies != null)
    {
      for (int i = 0; i < _dependencies.length; i++)
        outputDependency(context, arc, _dependencies[i]);
    }

    outputDependency(context, arc, _actualLibrary);
  }

  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    // Nothing
  }

  private final String _name;
  private final Object[] _functions;
  private final Object[] _dependencies;
  private final Object _actualLibrary;

  /**
   * If the library has to be aliased return the alias name for it;
   * if not return the same name.
   */
  static private String _getAlias(String theLibraryName)
  {
    return _ALIASES.getProperty(theLibraryName, theLibraryName);
  }

  private static final Properties _ALIASES            = new Properties();

  private static final String _JAVASCRIPT_ALIAS_FILE  =
    "javascriptmap.properties";


  private static final TrinidadLogger _LOG;

  static
  {
    _LOG = TrinidadLogger.createTrinidadLogger(AliasedScriptlet.class);

    // Load library versions
    Class<AliasedScriptlet> cl = AliasedScriptlet.class;
    InputStream in = cl.getResourceAsStream(_JAVASCRIPT_ALIAS_FILE);

    if (in == null)
    {
      // Failure to get an InputStream here indicates a
      // serious problem - likely in the UIX build system -
      // so make some noise
      _LOG.severe("UNABLE_GET_RESOURCE",_JAVASCRIPT_ALIAS_FILE);
      throw new AssertionError();
    }
    else
    {
      try
      {
        _ALIASES.load(in);
        in.close();
      }
      catch (IOException ioException)
      {
        // Not sure why we would ever get an IOException here,
        // but let's make sure that it is obvious that something
        // bad has happened.
        throw new AssertionError(ioException);
      }
    }
  }
}
