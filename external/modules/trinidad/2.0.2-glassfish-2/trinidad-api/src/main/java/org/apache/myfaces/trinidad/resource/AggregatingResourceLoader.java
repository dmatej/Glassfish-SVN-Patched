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

import java.io.InputStream;
import java.io.SequenceInputStream;
import java.net.URL;

import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;

import java.io.IOException;
import java.util.Enumeration;
import java.util.NoSuchElementException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.URLUtils;

/**
 * A resource loader implementation which combines multiple resources
 * into a single stream.  This version leverages the DynamicResourceLoader.
 *
 */
public class AggregatingResourceLoader extends DynamicResourceLoader
{
  /**
   * Creates a new AggregatingResourceLoader.
   *
   * @param path    the aggregated resource path
   * @param paths   the target resource paths to aggregate
   * @param target  the resource loader use to find target resource paths
   * @param parent  the parent resource loader
   */
  public AggregatingResourceLoader(
    String         path,
    String[]       paths,
    ResourceLoader target,
    ResourceLoader parent)
  {
    super(path, parent);
    if (paths == null)
      throw new NullPointerException();

    if (target == null)
      throw new NullPointerException();

    _paths = paths.clone();
    _target = target;
  }

  /**
   * Creates a new AggregatingResourceLoader.
   *
   * @param path    the aggregated resource path
   * @param paths   the target resource paths to aggregate
   * @param target  the resource loader use to find target resource paths
   */
  public AggregatingResourceLoader(
    String         path,
    String[]       paths,
    ResourceLoader target)
  {
    this(path, paths, target, null);
  }

  /**
   * Sets the separator to use in between streams.  This will typically contain a newline character.
   * By default the value is <code>null</code> which implies no separator.
   *
   * @param separator a string containing the separator characters
   */
  public void setSeparator(String separator)
  {
    _separator = separator;
  }

  /**
   * Returns a URL which is an aggregate of all the paths.
   *
   * @param path the current path
   * @return a aggregate url
   * @throws IOException when something bad happens
   */
  @Override
  protected URL getURL(String path) throws IOException
  {
    int len = _paths.length;
    ArrayList<URL> urls = new ArrayList<URL>(len);
    for(int i = 0; i < len; i++)
    {
      URL u = _target.getResource(_paths[i]);
      if(u != null)
      {
        urls.add(u);
      }
      else
      {
        _LOG.warning("RESOURCE_NOT_FOUND", new Object[]{_paths[i], path});
      }
    }

    urls.trimToSize();
    URL[] urlArray = urls.toArray(new URL[urls.size()]);

    AggregatingURLStreamHandler handler = new AggregatingURLStreamHandler(urlArray, _separator);
    return new URL("aggregating", null, -1, path, handler);
  }

  private String[] _paths;
  private ResourceLoader _target;
  private String _separator;
  static private final TrinidadLogger _LOG = 	TrinidadLogger.createTrinidadLogger(AggregatingResourceLoader.class);

  /**
   * This is a Stream Handler which can be used to construct a URL that is an Aggregate of a list of
   * other urls.
   *
   */
  public class AggregatingURLStreamHandler extends URLStreamHandler
  {
    /**
     * Constructs and AggregatingURLStreamHandler from an array of URLs containing other data.
     * This constructor assumes a null separator.
     *
     * @param urls the urls
     */
    public AggregatingURLStreamHandler(URL[] urls)
    {
      this(urls, null);
    }

    /**
     * Constructs and AggregatingURLStreamHandler from an array of URLs containing other data.
     *
     * @param urls the urls
     * @param separator a String containing a separator.  This will typically be an newline character
     *                  or null.
     */
    public AggregatingURLStreamHandler(URL[] urls, String separator)
    {
      if(urls == null)
      {
        throw new NullPointerException();
      }
      _urls = urls.clone();
      _separator = separator;
    }

    /**
     * Opens a connection containing all of the data from the provided urls.  The seperator character,
     * if one is provided, will seperate the content of each seperate stream.
     *
     * @param u the parent URL object
     * @return a URLConnection
     * @throws IOException when something bad happens
     */
    @Override
    protected URLConnection openConnection(URL u) throws IOException
    {
      int len = _urls.length;
      URLConnection[] conn = new URLConnection[len];
      for(int i = 0; i < len; i++)
      {
        conn[i] = _urls[i].openConnection();
      }

      return new AggregatingURLConnection(u, conn, _separator);
    }

    private URL[] _urls;
    private String _separator;

  }

  static private class AggregatingURLConnection extends URLConnection
  {
    public AggregatingURLConnection(URL url, URLConnection[] connections, String separator)
    {
      super(url);
      _connections = connections;

      if(separator != null)
      {
        _separator = separator.getBytes();
      }
    }

    @Override
    public void connect() throws IOException
    {
      for (int i=0, len = _connections.length; i < len; i++)
      {
        _connections[i].connect();
      }
    }

    @Override
    public InputStream getInputStream() throws IOException
    {
      boolean hasseparator = (_separator!=null);
      InputStream[] streams;
      if(hasseparator)
      {
        streams = new InputStream[(_connections.length *2)-1];
      }
      else
      {
        streams = new InputStream[_connections.length];
      }

      for (int i=0, len=_connections.length, sublen = len -1, streamCounter = 0; i < len; i++, streamCounter++)
      {
        streams[streamCounter] = _connections[i].getInputStream();

        //Add the separator if needed
        if(hasseparator && (i < sublen))
        {
          streams[++streamCounter] = new SeparatorInputStream(_separator);
        }
      }
      return new SequenceInputStream(new ArrayEnumeration<InputStream>(streams));
    }

    @Override
    public String getContentType()
    {
      return _connections[0].getContentType();
    }

    @Override
    public int getContentLength()
    {
      int totalContentLength = _contentLength;

      // Calculate the total content length
      // If any piece is unknown in length, then the total is unknown also
      if (totalContentLength == Integer.MIN_VALUE)
      {
        totalContentLength = 0;

        URLConnection[] connects = _connections;

        // Ensure that separator calculation happens first
        // to avoid adding extra length in the case when
        // the total content length is unknown.
        if (_separator != null)
        {
          totalContentLength += ((connects.length - 1)*_separator.length);
        }

        for (int i=0, len = _connections.length; i < len; i++)
        {
          int contentLength = _connections[i].getContentLength();
          if (contentLength < 0)
          {
            // Unknown content length for one part implies
            // unknown content length for aggregated whole
            totalContentLength = -1;
            break;
          }
          totalContentLength += contentLength;
        }

        _contentLength = totalContentLength;
      }

      return totalContentLength;
    }

    @Override
    public long getLastModified()
    {
      long maxLastModified = -1;

      for (int i=0, len = _connections.length; i < len; i++)
      {
        long lastModified;
        try
        {
          lastModified = URLUtils.getLastModified(_connections[i]);
        }
        catch (IOException exception)
        {
          maxLastModified = -1;
          break;
        }

        if (lastModified < 0)
        {
          maxLastModified = lastModified;
          break;
        }
        maxLastModified = Math.max(maxLastModified, lastModified);
      }

      return maxLastModified;
    }

    @Override
    public String getHeaderField(
      String name)
    {
      if ("content-length".equals(name))
      {
        return String.valueOf(getContentLength());
      }
      else if ("content-type".equals(name))
      {
        return getContentType();
      }
      else if ("last-modified".equals(name))
      {
        return String.valueOf(getLastModified());
      }
      else
      {
        return super.getHeaderField(name);
      }
    }

    private int _contentLength = Integer.MIN_VALUE;
    private URLConnection[] _connections;
    private byte[] _separator;

  }

  static private class ArrayEnumeration<T> implements Enumeration<T>
  {
    public ArrayEnumeration(T[] array)
    {
      _array = array;
      _len = array.length;
    }

    public boolean hasNext()
    {
      return _pointer < _len;
    }

    public T nextElement() throws NoSuchElementException
    {
      try
      {
        return _array[_pointer++];
      }
      catch (IndexOutOfBoundsException e)
      {
        throw new NoSuchElementException();
      }
    }

    public boolean hasMoreElements()
    {
      return hasNext();
    }

    private T[] _array;
    private int _len;
    private int _pointer = 0;
  }

  static private class SeparatorInputStream extends InputStream
  {
    public SeparatorInputStream (byte[] separatorBytes)
    {
      _separator = separatorBytes;
      _length = _separator.length;
    }

    @Override
    public int read() throws IOException
    {
      if(_index < _length)
      {
        return _separator[_index++];
      }

      return -1;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException
    {
      int bytesLeft = available();

      if(len <= bytesLeft)
      {
        System.arraycopy(_separator,_index,b,off,len);
        _index += len;
        return len;
      }
      else
      {
        System.arraycopy(_separator, _index, b,off, bytesLeft);
        _index += bytesLeft;
        return bytesLeft;
      }
    }

    @Override
    public long skip(long n) throws IOException
    {
      int bytesLeft = available();

      if(n < 0)
      {
        return 0;
      }
      else if(n < bytesLeft || n == bytesLeft)
      {
        _index += n;
        return n;
      }
      else
      {
        _index += bytesLeft;
        return bytesLeft;
      }
    }

    @Override
    public int available() throws IOException
    {
      return _length - _index;
    }

    private byte[] _separator;
    private int    _length;
    private int    _index = 0;
  }
}
