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
package org.apache.myfaces.trinidad.resource;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.List;
import java.util.Map;
import java.io.OutputStream;
import java.security.Permission;

import org.apache.myfaces.trinidad.util.URLUtils;

/**
 * A resource loader implementation that proxies another
 * resource loader, controlling the URLConnection.
 *
 */
public class ProxyResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new ProxyResourceLoader with specified parent.
   * 
   * @param parent  the parent resource loader
   */
  public ProxyResourceLoader(
    ResourceLoader parent)
  {
    super(parent);
  }
  
  @Override
  public URL getResource(
    String path) throws IOException
  {
    URL url = super.getResource(path);
    return (url != null) 
              ? new URL("proxy", null, -1, url.toExternalForm(),  
                        new ProxyURLStreamHandler(url))
              : null;
  }

  /**
   * Helper class used to manage decorated URL input stream.
   */
  private class ProxyURLStreamHandler extends URLStreamHandler
  {
    public ProxyURLStreamHandler(
      URL proxied)
    {
      _proxied = proxied;
    }
    
    @Override
    protected URLConnection openConnection(
      URL url
      ) throws IOException
    {
      return new ProxyURLConnection(url, _proxied);
    }
    
    private final URL _proxied;
  }

  /**
   * Helper class used to manage aggregated URL input stream.
   */
  protected class ProxyURLConnection extends URLConnection
  {
    public ProxyURLConnection(
      URL url,
      URL proxied
      ) throws IOException
    {
      super(url);
      
      _delegate = proxied.openConnection();
    }

    @Override
    public void addRequestProperty(String key, String value)
    {
      getURLConnection().addRequestProperty(key, value);
    }

    @Override
    public void connect() throws IOException
    {
      getURLConnection().connect();
    }

    @Override
    public boolean getAllowUserInteraction()
    {
      return getURLConnection().getAllowUserInteraction();
    }

    @Override
    public Object getContent() throws IOException
    {
      return getURLConnection().getContent();
    }

    @Override
    public Object getContent(Class[] classes) throws IOException
    {
      return getURLConnection().getContent(classes);
    }

    @Override
    public String getContentEncoding()
    {
      return getURLConnection().getContentEncoding();
    }

    @Override
    public int getContentLength()
    {
      return getURLConnection().getContentLength();
    }

    @Override
    public String getContentType()
    {
      return ProxyResourceLoader.this.getContentType(getURLConnection());
    }

    @Override
    public long getDate()
    {
      return getURLConnection().getDate();
    }

    @Override
    public boolean getDefaultUseCaches()
    {
      return getURLConnection().getDefaultUseCaches();
    }

    @Override
    public boolean getDoInput()
    {
      return getURLConnection().getDoInput();
    }

    @Override
    public boolean getDoOutput()
    {
      return getURLConnection().getDoOutput();
    }

    @Override
    public long getExpiration()
    {
      return getURLConnection().getExpiration();
    }

    @Override
    public String getHeaderField(int n)
    {
      return getURLConnection().getHeaderField(n);
    }

    @Override
    public String getHeaderField(String name)
    {
      return getURLConnection().getHeaderField(name);
    }

    @Override
    public long getHeaderFieldDate(String name, long Default)
    {
      return getURLConnection().getHeaderFieldDate(name, Default);
    }

    @Override
    public int getHeaderFieldInt(String name, int Default)
    {
      return getURLConnection().getHeaderFieldInt(name, Default);
    }

    @Override
    public String getHeaderFieldKey(int n)
    {
      return getURLConnection().getHeaderFieldKey(n);
    }

    @Override
    public Map<String, List<String>> getHeaderFields()
    {
      return getURLConnection().getHeaderFields();
    }

    @Override
    public long getIfModifiedSince()
    {
      return getURLConnection().getIfModifiedSince();
    }

    @Override
    public InputStream getInputStream() throws IOException
    {
      return getURLConnection().getInputStream();
    }

    @Override
    public long getLastModified()
    {
      try
      {
        return URLUtils.getLastModified(getURLConnection());
      }
      catch (IOException exception)
      {
        return -1;
      }
    }

    @Override
    public OutputStream getOutputStream() throws IOException
    {
      return getURLConnection().getOutputStream();
    }

    @Override
    public Permission getPermission() throws IOException
    {
      return getURLConnection().getPermission();
    }

    @Override
    public Map<String, List<String>> getRequestProperties()
    {
      return getURLConnection().getRequestProperties();
    }

    @Override
    public String getRequestProperty(String key)
    {
      return getURLConnection().getRequestProperty(key);
    }

    @Override
    public boolean getUseCaches()
    {
      return getURLConnection().getUseCaches();
    }

    @Override
    public void setAllowUserInteraction(boolean allowuserinteraction)
    {
      getURLConnection().setAllowUserInteraction(allowuserinteraction);
    }

    @Override
    public void setDefaultUseCaches(boolean defaultusecaches)
    {
      getURLConnection().setDefaultUseCaches(defaultusecaches);
    }

    @Override
    public void setDoInput(boolean doinput)
    {
      getURLConnection().setDoInput(doinput);
    }

    @Override
    public void setDoOutput(boolean dooutput)
    {
      getURLConnection().setDoOutput(dooutput);
    }

    @Override
    public void setIfModifiedSince(long ifmodifiedsince)
    {
      getURLConnection().setIfModifiedSince(ifmodifiedsince);
    }

    @Override
    public void setRequestProperty(String key, String value)
    {
      getURLConnection().setRequestProperty(key, value);
    }

    @Override
    public void setUseCaches(boolean usecaches)
    {
      getURLConnection().setUseCaches(usecaches);
    }
    
    protected URLConnection getURLConnection()
    {
      return _delegate;
    }
    
    private final URLConnection _delegate;
  }
}
