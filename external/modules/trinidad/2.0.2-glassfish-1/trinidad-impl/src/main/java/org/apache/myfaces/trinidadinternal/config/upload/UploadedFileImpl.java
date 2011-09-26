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
package org.apache.myfaces.trinidadinternal.config.upload;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;

import org.apache.myfaces.trinidad.model.UploadedFile;

/**
 * UploadedFileImpl defines a single file that has been uploaded
 * to the server.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/UploadedFileImpl.java#0 $) $Date: 10-nov-2005.18:49:03 $
 */
public class UploadedFileImpl implements UploadedFile, Serializable
{
  UploadedFileImpl()
  {
  }

  /**
   * Returns the filename reported from the client.
   */
  public String getFilename()
  {
    return _filename;
  }

  /**
   * Returns the MIME type of the file.
   */
  public String getContentType()
  {
    return _contentType;
  }

  /**
   * Returns the total length (in bytes) of the file.
   */
  public long getLength()
  {
    return _length;
  }

  public Object getOpaqueData()
  {
    return null;
  }

  /**
   * Returns an InputStream that can be used to read the file.
   * This method can be called repeatedly.
   */
  public InputStream getInputStream() throws IOException
  {
    if (_buffers != null)
    {
      return new BufferIS(_buffers, _sizeOfLastBuffer);
    }
    else if (_file != null)
    {
      return new BufferedInputStream(new FileInputStream(_file));
    }

    return null;
  }

  /**
   * Writes the entire contents of the file to an OutputStream.
   */
  private void _writeFile(OutputStream out) throws IOException
  {
    if (_buffers != null)
    {
      int size = _buffers.size();
      for (int i = 0; i < size; i++)
      {
        byte[] buffer = _buffers.get(i);
        int bytes;
        if (i == (size - 1))
          bytes = _sizeOfLastBuffer;
        else
          bytes = buffer.length;

        out.write(buffer, 0, bytes);
      }
    }
    else if (_file != null)
    {
      FileInputStream in = new FileInputStream(_file);
      try
      {
        byte[] buffer = new byte[_DISK_BUFFER_SIZE];
        while (true)
        {
          int bytes = in.read(buffer);
          if (bytes < 0)
            break;
          out.write(buffer, 0, bytes);
        }
      }
      finally
      {
        in.close();
      }
    }
  }

  /**
   * Disposes of all resources used to store this file.
   */
  public void dispose()
  {
    _length = 0;
    _buffers = null;
    if (_file != null)
    {
      _file.delete();
      _file = null;
    }
  }

  /**
   * Loads the file from a MultipartFormItem.
   */
  public void loadFile(
    UploadedFile      file,
    long              remainingMemory,
    long              remainingDiskSpace,
    String            directory) throws IOException
  {
    _filename    = file.getFilename();
    _contentType = file.getContentType();

    InputStream in = file.getInputStream();

    // First step: try to read it into memory.  Instead of trying
    // to build up one big buffer, we create a list of buffers
    // that are each at most 4K.  This is vastly faster to create
    // and insignificantly slower to read.
    while (_length < remainingMemory)
    {
      // Read another block of buffered memory.  We'll read up to 4K
      // at a time, but no more than the remaining amount of memory plus
      // one byte.  Why plus one?  Well, we want to know when we've
      // exceeded the memory size - not just reached it.
      int size = Math.min(((int) (remainingMemory - _length)) + 1,
                          _MEMORY_BUFFER_SIZE);
      byte[] buffer = new byte[size];
      int bytes = _fillBuffer(in, buffer, size);

      _sizeOfLastBuffer = bytes;
      _length           = _length + bytes;

      if (_buffers == null)
        _buffers = new ArrayList<byte[]>();
      _buffers.add(buffer);

      // If we're done, bail right here.
      if (bytes < size)
        return;
    }

    // If we didn't have enough space to read the file into memory,
    // and we also don't have enough space available on disk, then
    // punt right here and now.
    if (_length > remainingDiskSpace)
    {
      _buffers = null;
      _length  = 0;
      throw new EOFException("Per-request disk space limits exceeded.");
    }

    OutputStream out = _createOutputStream(directory);

    try
    {
      // First, copy the file from memory to the file
      if (_length > 0)
      {
        _writeFile(out);
      }

      // Free the buffers, since we're
      _buffers = null;

      // Now, write directly to the file.
      while (_length < remainingDiskSpace)
      {
        byte[] buffer = new byte[_DISK_BUFFER_SIZE];
        int bytes = _fillBuffer(in, buffer, _DISK_BUFFER_SIZE);
        out.write(buffer, 0, bytes);
        _length = _length + bytes;

        if (bytes < _DISK_BUFFER_SIZE)
          break;
      }
    }
    finally
    {
      out.close();

      // If we read too much - then drop the file, and bail.
      if (_length > remainingDiskSpace)
      {
        _file.delete();
        _file = null;
        _length = 0;

        throw new EOFException("Per-request disk space limits exceeded.");
      }
    }
  }

  @Override
  public int hashCode()
  {
    if (_filename == null)
      return 0;
    return _filename.hashCode();
  }

  @Override
  public boolean equals(Object o)
  {
    // UploadedFiles are only equal to themselves.
    return (this == o);
  }

  //
  // Create the OutputStream.
  //
  private OutputStream _createOutputStream(String directory) throws IOException
  {
    File tempDir;
    if (directory == null)
      tempDir = null;
    else
      tempDir = new File(directory);

    // Create our temporary file.
    _file = File.createTempFile("uix", null, tempDir);

    // Even though we're supposed to clean up these files ourselves,
    // make sure they get deleted even if an exception terminates
    // our code.
    _file.deleteOnExit();

    // No need for additional buffering of the output - we always
    // buffer the writes - so _don't_ add a BufferedOutputStream.
    return new FileOutputStream(_file);
  }


  //
  // If true, we're either in memory (or empty);  if false,
  // we're on disk.
  //
  boolean __isInMemory()
  {
    return (_buffers != null);
  }

  //
  // Fill a byte buffer from an InputStream.
  //
  private int _fillBuffer(
    InputStream in,
    byte[]      buffer,
    int         size) throws IOException
  {
    int offset = 0;

    // Read until the buffer is full, or the InputStream runs out.
    do
    {
      int bytes = in.read(buffer, offset, size - offset);
      if (bytes < 0)
        break;

      offset = offset + bytes;
    }
    while (offset < size);

    return offset;
  }


  //
  // InputStream implementation that reads from an in-memory buffer.
  //
  static private class BufferIS extends InputStream
  {
    public BufferIS(List<byte[]> bufferList, int sizeOfLastBuffer)
    {
      _bufferList = bufferList;
      _sizeOfLastBuffer = sizeOfLastBuffer;
    }

    // Read a single byte.
    @Override
    public int read()
    {
      int bufferIndex = _bufferIndex;
      if (bufferIndex < 0)
        return -1;

      byte[] buffer = _getBuffer(bufferIndex);
      byte currByte = buffer[_bufferPos];

      int bufferSize = _getBufferSize(buffer, bufferIndex);
      _advanceTo(_bufferPos + 1, bufferSize);

      return currByte & 0xff;
    }

    // Read into a buffer.
    @Override
    public int read(byte b[], int off, int len)
    {
      int bufferIndex = _bufferIndex;
      if (bufferIndex < 0)
        return -1;

      byte[] buffer = _getBuffer(bufferIndex);
      int bufferSize = _getBufferSize(buffer, bufferIndex);

      // Read as many bytes are available in the current buffer block,
      // up to "len" bytes.  If "len" is larger, then in theory
      // we could loop and read multiple blocks, but that'd complicate
      // our logic here without actually simplifying the developer's
      // logic any (they can't assume an InputStream behaves that way).
      int bufferPos = _bufferPos;
      int bytes = Math.min(len, bufferSize - bufferPos);
      System.arraycopy(buffer, bufferPos, b, off, bytes);
      _advanceTo(bufferPos + bytes, bufferSize);

      return bytes;
    }

    //
    // Returns the number of bytes that will be made avaialable
    // in the next call to read(byte[], int, int);
    //
    @Override
    public int available()
    {
      int bufferIndex = _bufferIndex;
      if (bufferIndex < 0)
        return 0;

      byte[] buffer = _getBuffer(bufferIndex);
      int bufferSize = _getBufferSize(buffer, bufferIndex);

      return bufferSize - _bufferPos;
    }

    // Advance the current buffer to a certain position.
    private void _advanceTo(int pos, int bufferSize)
    {
      if (pos >= bufferSize)
      {
        _bufferPos = 0;
        _bufferIndex = _bufferIndex + 1;
        if (_bufferIndex >= _bufferList.size())
          _bufferIndex = -1;
      }
      else
      {
        _bufferPos = pos;
      }
    }

    // Return the size of a given buffer. (The last buffer
    // may not be completely full.)
    private int _getBufferSize(byte[] buffer, int bufferIndex)
    {
      if (bufferIndex == _bufferList.size() - 1)
        return _sizeOfLastBuffer;

      return buffer.length;
    }

    // Return a buffer.
    private byte[] _getBuffer(int bufferIndex)
    {
      return _bufferList.get(bufferIndex);
    }

    // ArrayList of all the buffers
    private List<byte[]> _bufferList;
    // The number of bytes in the last buffer (which may not be full)
    private int  _sizeOfLastBuffer;

    // Current index into _bufferList
    private int  _bufferIndex;

    // Current position in _bufferList[_bufferIndex]
    private int  _bufferPos;
  }


  private String     _filename;
  private String     _contentType;

  // Total length fo the content, whether in memory or on disk
  transient private long       _length;

  // File where the content is stored (or null if the content
  // is in memory)
  transient private File       _file;

  // ArrayList of all the in-memory buffers (or null if it's in
  // a File)
  transient private ArrayList<byte[]> _buffers;
  // The number of bytes in the last buffer (which may not be full)
  transient private int        _sizeOfLastBuffer;

  // Buffer sizes.  The memory buffer size can be very small
  // because of our list-of-small-buffers technique;  for disk I/O,
  // use a larger buffer.
  static private final int _MEMORY_BUFFER_SIZE = 2048;
  static private final int _DISK_BUFFER_SIZE   = 8192;
  static private final long serialVersionUID = 1592383482116327478L;
}
