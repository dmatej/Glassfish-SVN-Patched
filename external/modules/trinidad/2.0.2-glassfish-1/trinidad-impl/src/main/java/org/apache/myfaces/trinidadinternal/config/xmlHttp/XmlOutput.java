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
package org.apache.myfaces.trinidadinternal.config.xmlHttp;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

import java.io.Writer;

import javax.servlet.ServletOutputStream;

/**
 * This class filters out any whitespace or doctype or any other
 * bad characters that might have been added by the JspWriter to the
 * beginning of an ppr xml response.  This makes sure that the
 * declaration is right up 
 * 
 */
final class XmlOutput
{
  XmlOutput(final PrintWriter writer)
  {
    _baseWriter = writer;
    _baseStream = null;
  }
  
  public XmlOutput(final OutputStream out)
  {
    _baseWriter = null;
    _baseStream = new PrintStream(out);
  }
  
  public ServletOutputStream getOutputStream()
  {
    assert _baseStream != null;
    
    return new ServletOutputStream()
    {
      public void write(int b) throws IOException
      {
        byte[] bs = new byte[]{(byte) b};
        write(bs);
      }

      public void close() throws IOException
      {
        _baseStream.close();
      }

      public void flush() throws IOException
      {
        _baseStream.flush();
      }

      public void write(byte[] b, int off, int len) throws IOException
      {
        if (_strip)
        {
          _filter(null, b, off, len);
          return;
        }
        _baseStream.write(b, off, len);
      }
    };
  }
  
  public PrintWriter getWriter()
  {
    assert _baseWriter != null;
    
    Writer writer = new Writer()
    {
      public void close() throws IOException
      {
        _baseWriter.close();
      }

      public void flush() throws IOException
      {
        _baseWriter.flush();
      }

      public void write(char[] cbuf, int off, int len) throws IOException
      {
        if (_strip)
        {
          _filter(cbuf, null, off, len);
          return;
        }
        
        _baseWriter.write(cbuf, off, len);
      }
    };
    
    return new PrintWriter(writer);
  }

  private void _filter(final char[] cbuf, final byte[] b, final int off, int len) throws IOException
  {
    // strip out any whitespace or doctype declarations, until we reach the
    // xml declaration: bug 5176371:
    for(int i=off, sz=off+len; i<sz; i++)
    {
      char ch = _charAt(cbuf, b, i);
      if (ch == _XML_STR.charAt(_searchIndex))
      {
        _searchIndex++;
        if (_searchIndex >= _XML_STR.length())
        {
          _strip = false;
          _write(_XML_STR);
          len = sz - i - 1;
          if (len > 0)
          {
            _write(cbuf, b, i + 1, len);
          }
          break;
        }
      }
      else if (_searchIndex > 0)
      {
        _searchIndex = 0;
        i--; // retest the last character.
      }
    }
  }

  private char _charAt(final char[] cbuf, final byte[] b, final int index)
  {
    return (b != null)
      ? (char) b[index]
      : cbuf[index];
  }

  private void _write(final char[] cbuf, final byte[] b, final int off, final int len) throws IOException
  {
    if (b != null)
      _baseStream.write(b, off, len);
    else
      _baseWriter.write(cbuf, off, len);
  }
  
  private void _write(final String s) throws IOException
  {
    if (_baseStream != null)
      _baseStream.print(s);
    else
      _baseWriter.write(s);
  }
  
  private boolean _strip = true;
  private int _searchIndex = 0;
  private final PrintWriter _baseWriter;
  private final PrintStream _baseStream;

  // this is the string that we're going to search for:
  private static final String _XML_STR = "<?xml ";
}
