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
package org.apache.myfaces.trinidadinternal.share.io;

import java.beans.Beans;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;

import java.net.URL;
import java.net.URLConnection;

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.util.URLUtils;


/**
 * An InputStreamProvider for opening URLs.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/URLInputStreamProvider.java#0 $) $Date: 10-nov-2005.19:00:10 $
 */
public class URLInputStreamProvider implements InputStreamProvider
{
  /**
   * Create an URLInputStreamProvider.
   */
  public URLInputStreamProvider(URL url)
  {
    if (url == null)
      throw new NullPointerException();

    _url = url;
  }

  public InputStream openInputStream() throws IOException
  {
    // Get the inputstream from the connection to avoid duplicate calls
    // to URL.openConnection
    _lastModifiedTime = URLUtils.getLastModified(_url);
    URLConnection connection = _url.openConnection();
    // prevent caching during DT where the source may change...
    if (Beans.isDesignTime())
    {
      connection.setUseCaches(false);
    }
    // In theory, should not need to close
    InputStream base = connection.getInputStream();
    
    if (base instanceof BufferedInputStream)
      return base;
    else
      return new BufferedInputStream(base);
  }

  public String getDisplayName()
  {
    return _url.toExternalForm();
  }

  public Object getIdentifier()
  {
    return _url;
  }

  /**
   * Returns true if the underlying target has changed
   * since the last call to openInputStream()
   */
  public boolean hasSourceChanged()
  {
    try
    {
      long currentModifiedTime = URLUtils.getLastModified(_url);
      return currentModifiedTime != _lastModifiedTime;
    }
    catch (IOException ioe)
    {
      // default to the old answer of "no"
      return false;
    }
  }

  public Object getCachedResult()
  {
    return _cached;
  }

  public void setCachedResult(Object value)
  {
    _cached = value;
  }

  private final URL  _url;
  private Object     _cached;
  private long _lastModifiedTime = -1;
}
