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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import javax.faces.context.ResponseWriter;

import java.io.Writer;
import java.io.IOException;

import java.util.ArrayList;

/**
 * Note: this class is completely unsynchronized;  an instance
 * may only be used from one thread at a time.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/FullyBufferedWriter.java#0 $) $Date: 10-nov-2005.18:53:00 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class FullyBufferedWriter extends Writer
{
  public FullyBufferedWriter(ResponseWriter writer)
  {
    if (writer == null)
      throw new NullPointerException();

    _writer        = writer;

    // _buffer = null;
    // _bufferSize = 0;
    // _nextChar   = 0;
  }

  /**
   * Write a single character.
   *
   * @exception  IOException  If an I/O error occurs
   */
  @Override
  public void write(int c) throws IOException
  {
    int nextChar = _nextChar;
    if (nextChar >= _bufferSize)
    {
      _getNewBuffer(1);
      nextChar = 0;
    }

    _buffer[nextChar] = (char) c;

    _nextChar = nextChar + 1;
  }


  /**
   * Write a portion of an array of characters.
   *
   * @param  cbuf  A character array
   * @param  off   Offset from which to start reading characters
   * @param  len   Number of characters to write
   *
   * @exception  IOException  If an I/O error occurs
   */
  @Override
  public void write(char cbuf[], int off, int len) throws IOException
  {
    if ((off < 0) || (off > cbuf.length) || (len < 0) ||
        ((off + len) > cbuf.length) || ((off + len) < 0))
    {
      throw new IndexOutOfBoundsException();
    } 

    if (len == 0)
      return;

    int nextChar = _nextChar;
    int remainingChars = _bufferSize - nextChar;
    if (len > remainingChars)
    {
      if (_buffer != null)
        System.arraycopy(cbuf, off, _buffer, nextChar, remainingChars);
      off += remainingChars;
      len -= remainingChars;

      _getNewBuffer(len);
      nextChar = 0;
    }
    
    System.arraycopy(cbuf, off, _buffer, nextChar, len);
    _nextChar = nextChar + len;
  }

  /**
   * Write a portion of a String.
   *
   * @param  s     String to be written
   * @param  off   Offset from which to start reading characters
   * @param  len   Number of characters to be written
   *
   * @exception  IOException  If an I/O error occurs
   */
  @Override
  public void write(String s, int off, int len) throws IOException
  {
    if (len == 0)
      return;

    int nextChar = _nextChar;
    int remainingChars = _bufferSize - nextChar;
    if (len > remainingChars)
    {
      if (_buffer != null)
        s.getChars(off, off + remainingChars, _buffer, nextChar);
      off += remainingChars;
      len -= remainingChars;

      _getNewBuffer(len);
      nextChar = 0;
    }

    s.getChars(off, off + len, _buffer, nextChar);

    _nextChar = nextChar + len;
  }

  /**
   * Flush the writer.  This is a no-op, as nothing should
   * be flushed until explicitly requested.
   *
   * @exception  IOException  If an I/O error occurs
   */
  @Override
  public void flush() throws IOException
  {
  }

  /**
   * Close the writer.  This does _not_ close the underlying output method.
   *
   * @exception  IOException  If an I/O error occurs
   */
  @Override
  public void close() throws IOException
  {
    if (_writer == null)
      return;
    reallyFlush();
    _writer = null;
  }


  /**
   * Actually flushes the writer.
   *
   * @exception  IOException  If an I/O error occurs
   */
  public void reallyFlush() throws IOException
  {
    int nextChar = _nextChar;
    ArrayList<char[]> usedBuffers = _usedBuffers;
    if ((nextChar == 0) && (usedBuffers == null))
      return;

    if (usedBuffers != null)
    {
      int bufferCount = usedBuffers.size();
      for (int i = 0; i < bufferCount; i++)
      {
        char[] usedBuffer = usedBuffers.get(i);
        _writer.write(usedBuffer, 0, usedBuffer.length);
      }
    }

    _writer.write(_buffer, 0, nextChar);
    _nextChar = 0;
    
    _writer.flush();
  }

  /**
   * Dispose all the buffered IO of the writer
   *
   * @exception  IOException  If an I/O error occurs
   */
  public void dispose()
  {
    if (_writer == null)
      return;
    _writer = null;
    _nextChar = 0;
    _usedBuffers = null;
  }


  private void _getNewBuffer(int neededSize)
  {
    // Push the current buffer (if one exists) onto the list
    if (_buffer != null)
    {
      if (_usedBuffers == null)
        _usedBuffers = new ArrayList<char[]>(5);
      _usedBuffers.add(_buffer);
    }
    
    int size = Math.max(_DEFAULT_BUFFER_SIZE, neededSize);
    _buffer = new char[size];
    _nextChar = 0;
    _bufferSize = size;
  }
  

  private ResponseWriter _writer;

  private ArrayList<char[]> _usedBuffers;
  private char[] _buffer;
  // Size of the above buffer, or zero if _buffer is null
  private int    _bufferSize;
  private int    _nextChar;

  // Default buffer size - we use a larger buffer only when
  // we get a request for writing out a large block of
  // text all at once.
  
  // This may seem small, but because we build up a chain of buffers
  // this value could be even smaller with only a minor performance
  // penalty for large pages;  for tiny blocks of buffered text,
  // the smaller the better.
  static private final int _DEFAULT_BUFFER_SIZE = 1024;
}
