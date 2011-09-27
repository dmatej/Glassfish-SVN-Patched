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
package org.apache.myfaces.trinidadinternal.share.config;

import java.io.File;

import java.util.Hashtable;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * <code>ConfigurationImpl</code> is the default implementation of
 * <code>Configuration</code>.  See that interface's documentation
 * for information on the configuration architecture.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/config/ConfigurationImpl.java#0 $) $Date: 10-nov-2005.19:00:19 $
 */
public class ConfigurationImpl extends Configuration
{
  /**
   * Returns whether the configuration is in debug mode.
   */
  @Override
  public boolean isDebug()
  {
    return _debug;
  }


  /**
   * Returns whether the configuration is in debug mode.
   */
  public void setDebug(boolean debug)
  {
    _debug = debug;
  }


  // Constructor for _the_ default ConfigurationImpl
  ConfigurationImpl()
  {
    super();
  }


  /**
   * Return a URI for a UIX directory.
   * @param key the key used to identify the directory
   * @param contextURI the current contextURI;  this will
   *   be preprended to the returned URI if this directory
   *   is registered as (or defaulting to) context-relative.
   *   This path must not be terminated with a separator ("/").
   * @return a URI, which will always be terminated with a separator
   * @exception DirectoryUnavailableException if the directory is
   *    unavailable
   */
  @Override
  public String getURI(Object key, String contextURI)
  {
    String uri = _getURI(key);
    if (_isContextURI(uri))
    {
      if (contextURI == null)
        throw new DirectoryUnavailableException(_LOG.getMessage(
          "NULL_CONTEXT_URL"), key);

      if (contextURI.endsWith(_URI_DELIMITER))
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "CONTEXT_URI_ENDS_WITH_SLASH", key));
      }

      return contextURI + uri;
    }

    if (uri.indexOf(':') < 0)
      return _URI_DELIMITER + uri;
    return uri;
  }


  /**
   * Return a path for a UIX directory.
   * @param key the key used to identify the directory
   * @param contextPath the current context path;  this will
   *   be preprended to the returned path if this directory
   *   is registered as (or defaulting to) context-relative.
   * @return a full file system path, which will always be
   *   terminated with the appropriate separator for the file system
   * @exception DirectoryUnavailableException if the directory is
   *    unavailable
   */
  @Override
  public String getPath(Object key, String contextPath)
  {
    String uri = _getURI(key);
    if (!_isContextURI(uri))
    {
      return _getPath(key);
    }

    if (contextPath == null)
      throw new DirectoryUnavailableException(_LOG.getMessage(
        "NULL_CONTEXTPATH"), key);

    // =-=AEW Or should we require one set of behavior?
    if (contextPath.endsWith(File.separator))
      contextPath = contextPath.substring(0, contextPath.length() - 1);

    return contextPath + uri.replace(_URI_DELIMITER_CHAR,
                                     File.separatorChar);
  }


  /**
   * Return a registered property.
   * @param key the key used to identify the property
   * @return the registered object, or null if no object
   *    was registered.
   */
  @Override
  public Object getProperty(Object key)
  {
    return _properties.get(key);
  }


  /**
   * Register a full URI and path.
   * @param key the key used to identify the directory
   * @param uri the full URI pointing to the desired location
   * @param path the full path corresponding to the given URI on the
   *    webserver.
   */
  public void putFullURIAndPath(
    Object key,
    String uri,
    String path)
  {
    if (uri == null)
      throw new NullPointerException(_LOG.getMessage(
        "REGISTERED_NULL_URI"));

    uri = _endWithDelimiter(uri);
    if (uri.startsWith(_URI_DELIMITER))
      uri = uri.substring(1);

    _uris.put(key, uri);
    _resolvedURIs.clear();

    if (path == null)
    {
      _paths.put(key, _NULL_PATH);
    }
    else
    {
      if (!path.endsWith(File.separator))
        path = path + File.separator;

      _paths.put(key, path);
    }
  }


  /**
   * Register a context-relative URI .
   * @param key the key used to identify the directory
   * @param uri a URI pointing to the desired location, relative
   *   to the root of a web application
   */
  public void putRelativeURI(
    Object key,
    String uri)
  {
    if (uri == null)
      throw new NullPointerException(_LOG.getMessage(
        "REGISTERED_NULL_URI"));

    uri = _endWithDelimiter(uri);
    if ((uri.indexOf(':') < 0) &&
        (uri.charAt(0) != _URI_DELIMITER_CHAR))
      uri = _URI_DELIMITER + uri;

    _uris.put(key, uri);
    _resolvedURIs.clear();
    _paths.remove(key);
  }


  /**
   * Register a propery.  DO NOT use this method to
   * set up paths for UIX resources.  Paths must be
   * set using <code>putRelativeURI()</code> and
   * <code>putFullURIAndPath()</code>
   * @param key the key used to identify the directory
   * @param value the registered value
   * @see #putRelativeURI
   * @see #putFullURIAndPath
   */
  public void putProperty(Object key, Object value)
  {
    if (value == null)
      _properties.remove(key);
    else
      _properties.put(key, value);
  }

  // Return true if the URI is a context-relative URI.
  // Context-relative URIs are stored with a leading '/',
  // and context-dependent URIs are stored without one.
  private boolean _isContextURI(String uri)
  {
    return ((uri != null) &&
            (uri.length() > 0) &&
            (uri.charAt(0) == _URI_DELIMITER_CHAR));
  }

  // Append a delimiter to the URI if needed
  private String _endWithDelimiter(String uri)
  {
    if (!uri.endsWith(_URI_DELIMITER))
      uri = uri + _URI_DELIMITER;
    return uri;
  }

  // Turn a key into a URI, deriving it if needed
  private String _getURI(Object key)
  {
    String uri = _resolvedURIs.get(key);
    if (uri != null)
      return uri;

    uri = _uris.get(key);
    if (uri == null)
    {
      if (BASE_DIRECTORY.equals(key))
      {
        uri = _DEFAULT_BASE_DIRECTORY;
      }
      else if (IMAGES_DIRECTORY.equals(key))
      {
        uri = _getURI(BASE_DIRECTORY) + _DEFAULT_IMAGES_SUBDIRECTORY;
      }
      else if (IMAGES_CACHE_DIRECTORY.equals(key))
      {
        uri = _getURI(IMAGES_DIRECTORY) + _DEFAULT_CACHE_SUBDIRECTORY;
      }
      else if (STYLES_DIRECTORY.equals(key))
      {
        uri = _getURI(BASE_DIRECTORY) + _DEFAULT_STYLES_SUBDIRECTORY;
      }
      else if (STYLES_CACHE_DIRECTORY.equals(key))
      {
        uri = _getURI(STYLES_DIRECTORY) + _DEFAULT_CACHE_SUBDIRECTORY;
      }
      else if (JSLIBS_DIRECTORY.equals(key))
      {
        uri = _getURI(BASE_DIRECTORY) + _DEFAULT_JSLIBS_SUBDIRECTORY;
      }
      else if (JSPS_DIRECTORY.equals(key))
      {
        uri = _getURI(BASE_DIRECTORY) + _DEFAULT_JSPS_SUBDIRECTORY;
      }
    }

    assert (uri != null);

    _resolvedURIs.put(key, uri);

    return uri;
  }


  private String _getPath(Object key)
  {
    String path = _resolvedPaths.get(key);
    if (path != null)
      return path;

    Object o = _paths.get(key);
    if (o == _NULL_PATH)
      throw new IllegalStateException(_LOG.getMessage(
        "NULL_PATH_REGISTERED", key));

    path = (String) o;
    if (path == null)
    {
      if (BASE_DIRECTORY.equals(key))
      {
        // This case differs from directories.  There is no
        // default.
        throw new IllegalStateException(_LOG.getMessage(
          "NO_BASE_PATH_REGISTERED"));
      }
      else if (IMAGES_DIRECTORY.equals(key))
      {
        path = _getPath(BASE_DIRECTORY) + _DEFAULT_IMAGES_SUBPATH;
      }
      else if (IMAGES_CACHE_DIRECTORY.equals(key))
      {
        path = _getPath(IMAGES_DIRECTORY) + _DEFAULT_CACHE_SUBPATH;
      }
      else if (STYLES_DIRECTORY.equals(key))
      {
        path = _getPath(BASE_DIRECTORY) + _DEFAULT_STYLES_SUBPATH;
      }
      else if (STYLES_CACHE_DIRECTORY.equals(key))
      {
        path = _getPath(STYLES_DIRECTORY) + _DEFAULT_CACHE_SUBPATH;
      }
      else if (JSLIBS_DIRECTORY.equals(key))
      {
        path = _getPath(BASE_DIRECTORY) + _DEFAULT_JSLIBS_SUBPATH;
      }
      else if (JSPS_DIRECTORY.equals(key))
      {
        path = _getPath(BASE_DIRECTORY) + _DEFAULT_JSPS_SUBPATH;
      }
    }

    assert (path != null);

    _resolvedPaths.put(key, path);

    return path;
  }


  // context-dependent URI's DO NOT start with '/'
  // full URI's MUST start with '/'.  This bogusness does
  // mean that we can't support registration of URIs to external
  // webservers.
  //-= Simon Lessard =-
  //TODO: Check is synchronization is required
  private Hashtable<Object, Object> _paths         = new Hashtable<Object, Object>(11);
  private Hashtable<Object, String> _uris          = new Hashtable<Object, String>(11);
  private Hashtable<Object, String> _resolvedURIs  = new Hashtable<Object, String>(11);
  private Hashtable<Object, String> _resolvedPaths = new Hashtable<Object, String>(11);
  private Hashtable<Object, Object> _properties    = new Hashtable<Object, Object>(11);
  private boolean   _debug;

  private static final String _DEFAULT_BASE_DIRECTORY   = "/adf/";
  private static final String _DEFAULT_IMAGES_SUBDIRECTORY = "images/";
  private static final String _DEFAULT_JSLIBS_SUBDIRECTORY = "jsLibs/";
  private static final String _DEFAULT_JSPS_SUBDIRECTORY   = "jsps/";
  private static final String _DEFAULT_STYLES_SUBDIRECTORY = "styles/";
  private static final String _DEFAULT_CACHE_SUBDIRECTORY  = "cache/";

  private static final String _URI_DELIMITER = "/";
  private static final char   _URI_DELIMITER_CHAR = _URI_DELIMITER.charAt(0);

  private static final String _DEFAULT_IMAGES_SUBPATH =
    _DEFAULT_IMAGES_SUBDIRECTORY.replace(_URI_DELIMITER_CHAR,
                                         File.separatorChar);
  private static final String _DEFAULT_JSLIBS_SUBPATH =
    _DEFAULT_JSLIBS_SUBDIRECTORY.replace(_URI_DELIMITER_CHAR,
                                         File.separatorChar);
  private static final String _DEFAULT_JSPS_SUBPATH   =
    _DEFAULT_JSPS_SUBDIRECTORY.replace(_URI_DELIMITER_CHAR,
                                         File.separatorChar);
  private static final String _DEFAULT_STYLES_SUBPATH =
    _DEFAULT_STYLES_SUBDIRECTORY.replace(_URI_DELIMITER_CHAR,
                                         File.separatorChar);
  private static final String _DEFAULT_CACHE_SUBPATH  =
    _DEFAULT_CACHE_SUBDIRECTORY.replace(_URI_DELIMITER_CHAR,
                                         File.separatorChar);

  private static final Object _NULL_PATH = new Object();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ConfigurationImpl.class);
}
