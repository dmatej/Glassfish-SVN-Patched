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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import javax.faces.application.ProjectStage;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.context.RenderingContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.resource.CoreRenderKitResourceLoader;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/LibraryScriptlet.java#0 $) $Date: 10-nov-2005.19:02:47 $
 */
public class LibraryScriptlet extends Scriptlet
{
  public LibraryScriptlet(String libraryName, String[] functions)
  {
    this(libraryName, functions, null);
  }

  public LibraryScriptlet(String libraryName,
                          Object[] functions,
                          Object[] dependencies)
  {
    _libraryName  = libraryName;
    _functions = (functions == null) ? null : functions.clone();
    _dependencies = (dependencies == null) ? null : dependencies.clone();
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
  public Object getScriptletKey()
  {
    return _libraryName;
  }

  /**
   * given a libraryName, return the versioned-name
   * if versioning if on.
   * @param context
   * @param libraryName
   * @return
   */
  public static String getLibraryNameWithVersion(
    FacesContext context,
    String libraryName
  )
  {
    if (_useLibraryVersions())
      return libraryName + _LIBRARY_VERSION;

    return libraryName;
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

    XhtmlUtils.writeLibImport(context, arc, getLibraryURL(context, arc));
  }

  @Override
  protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    // No-op.
  }

  @Override
  protected void embedInScriptTagImpl(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    // =-=AEW REVISIT.  You can't currently "embed" a library.
    // Of course, we're also not using this feature for LibraryScriptlets,
    // so it's not relevant now.
    throw new IllegalStateException();
  }

  protected String getLibraryName(
    FacesContext        context,
    RenderingContext arc)
  {
    String libraryName = _libraryName;

    if (_isDebug(context))
      libraryName = "Debug" + libraryName;

    return libraryName;
  }
  
  /* return extra parameters, like "?loc=en".
  /* returns null if no extra parameters */
  protected String getExtraParameters(
  FacesContext        context,
  RenderingContext    arc)
  {
    return null;
  }
  
  protected String getLibraryURL(
    FacesContext        context,
    RenderingContext arc)
  {
    String libraryName = getLibraryName(context, arc);

    // start off with the base URL
    StringBuffer libURL = new StringBuffer(80);

    libURL.append(context.getExternalContext().getRequestContextPath());
    libURL.append(getBaseLibURL());
    libURL.append(libraryName);

    if (_useLibraryVersions())
      libURL.append(_LIBRARY_VERSION);
    
    libURL.append(".js");

    String extraParams = getExtraParameters(context, arc);
    if (extraParams != null)
      libURL.append(extraParams);    

    return libURL.toString();
  }

  public static String getBaseLibURL()
  {
    return _JSLIBS_DIRECTORY;
  }

  // Tests whether or not to use library versions for this request
  /**
   * @todo Re-enable disabling versioning??
   */
  private static boolean _useLibraryVersions()
  {
    if (_LIBRARY_VERSION == null)
      return false;

    return true;
  }

  static private boolean _isDebug(FacesContext context)
  {
    if (_debugJavascript == null)
    {
      String debugJavascript = context.
          getExternalContext().getInitParameter(_DEBUG_JAVASCRIPT);

      if (debugJavascript != null)
      {
        if (debugJavascript.equalsIgnoreCase("true"))
        {
          _debugJavascript = Boolean.TRUE;
        
          // if Apache MyFaces Trinidad is running in production stage
          // running with JavaScript debugging is not desired, therefore
          // we generate a WARNING message; otherwise we just inform the user
          if (context.isProjectStage(ProjectStage.Production))
          {
            _LOG.warning("RUNNING_DEBUG_JAVASCRIPT_IN_PRODUCTION_STAGE");
          }
          else
          {
            _LOG.info("RUNNING_DEBUG_JAVASCRIPT"); 
          }
        }
        else
        {
          _debugJavascript = Boolean.FALSE; 
        }
      }
      else
      {
        // if the _DEBUG_JAVASCRIPT parameter has NOT been specified, let us
        // apply the DEFAULT values for the certain Project Stages:
        // -PRODUCTION we want this value to be FALSE;
        // -other stages we use TRUE
        _debugJavascript = !(context.isProjectStage(ProjectStage.Production));
        if (_debugJavascript)
        {
          _LOG.info("RUNNING_DEBUG_JAVASCRIPT"); 
        }
      }
    }

    return _debugJavascript.booleanValue();
  }

  private final String _libraryName;
  private final Object[] _functions;
  private final Object[] _dependencies;


  private static Boolean _debugJavascript;
  private static final String _DEBUG_JAVASCRIPT =
     "org.apache.myfaces.trinidad.DEBUG_JAVASCRIPT";

  // Library version properties
  private static String _LIBRARY_VERSION = null;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(LibraryScriptlet.class);

  static private final String _JSLIBS_DIRECTORY = "/adf/jsLibs/";
  static
  {
    // Use whatever the resource loader thinks is the version, 'cause
    // they've got to match
    _LIBRARY_VERSION = CoreRenderKitResourceLoader.__getVersion();
  }
}
