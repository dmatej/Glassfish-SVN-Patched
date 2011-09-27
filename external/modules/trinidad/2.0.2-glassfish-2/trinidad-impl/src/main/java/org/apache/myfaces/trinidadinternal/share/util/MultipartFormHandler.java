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
package org.apache.myfaces.trinidadinternal.share.util;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.StringTokenizer;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * MultipartFormHandler - parses an incoming file upload post.
 * This classs parses the incoming post and returns MultipartStreamItems
 * for each form item.
 * <p>
 * If setCharacterEncoding() is called (with anything other than null),
 * then strings returned from this class will be character-set decoded.
 * Otherwise, strings returned from this class are not character-set
 * decoded.  In this way, MultipartFormHandler precisely matches
 * the behavior of ServletRequest.  Clients should use
 * <code>CaboHttpUtils.decodeRequestParameter()</code> to decode
 * any returned parameters.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/util/MultipartFormHandler.java#1 $) $Date: 11-nov-2005.14:59:39 $
 */
public class MultipartFormHandler
{

  /**
   * Returns true if the servlet request is a multipart request.
   */
  static public boolean isMultipartRequest(final ExternalContext externalContext)
  {
    final String contentType = ExternalContextUtils.getContentType(externalContext);
    
    if (contentType == null)
    {
      return false;
    }

    return contentType.startsWith(_MULTIPART_REQUEST_TYPE) && externalContext.getRequestMap().get(_HANDLED) == null;
  }

  /**
   * Create a MultipartFormHandler for the given servlet request.
   */
  @SuppressWarnings("unchecked")
  public MultipartFormHandler(final ExternalContext externalContext) throws IOException
  {
    
    this(ExternalContextUtils.getContentType(externalContext), ExternalContextUtils.getRequestInputStream(externalContext));

    // make sure that we don't try to decode this multi part request at a
    // later time; ie: if we do a forward.
    Map<String, Object> requestMap = externalContext.getRequestMap();
    requestMap.put(_HANDLED, Boolean.TRUE);
    _contentStreamSize = ExternalContextUtils.getContentLength(externalContext);
  }

  /**
   * Create a MultipartFormHandler for the given InputStream and content
   * type.
   * @param type The content type, including the boundary string.
   *             The content type must be "multipart/form-data", and
   *             must define the multipart boundary value.
   * @param in The InputStream which provides the multipart/form-data content
   */
  public MultipartFormHandler(final String type, final InputStream in) throws IOException
  {
    if (!type.startsWith(_MULTIPART_REQUEST_TYPE))
    {
      throw new IllegalStateException(_LOG.getMessage(
        "CONTENT_NOT_MULTIPART_FORM_DATA"));
    }

    _boundary = _parseBoundary(type);
    _in = in;
    _skipBoundary();
  }

  /**
   * Gets the character enocoding.
   */
  public String getCharacterEncoding()
  {
    return _characterEncoding;
  }

  /**
   * Sets the character encoding.  If left to default,
   * Strings will be decoded for ISO-8859-1.  Clients
   * should set the character encoding if they do not
   * expect another part of their processing to decode
   * the strings for them.
   */
  public void setCharacterEncoding(final String characterEncoding) throws UnsupportedEncodingException
  {
    CaboHttpUtils.validateEncoding(characterEncoding);
    _characterEncoding = characterEncoding;
  }

  /**
   * Returns the total number of bytes yet read.
   */
  public long getBytesRead()
  {
    return _totalBytesRead;
  }

  /**
   * Returns the total size of the incoming content, or -1
   * if the length is not known.
   * @return an integer containing the length of the request
   *        or -1 if the length is not known
   */
  public long getTotalBytes()
  {
    return _contentStreamSize;
  }

  /**
   * Sets the maximum number of bytes that MultipartFormItem.writeFile()
   * will be allowed to write.  This value may be set immediately
   * before or between calls to MultipartFormItem.writeFile().  If
   * any call to writeFile() exceeds this value, an EOFException
   * will be thrown.
   * <p>
   * @param maxAllowedBytes the maximum number of bytes that
   * MultipartFormItem.writeFile() will be allowed to write.  Defaults
   * to 128MB.
   * @see org.apache.myfaces.trinidadinternal.share.util.MultipartFormItem#writeFile
   */
  public void setMaximumAllowedBytes(final long maxAllowedBytes)
  {
    _maxAllowedBytes = Math.max(0L, maxAllowedBytes);
  }

  /**
   * Gets the maximum number of bytes that MultipartFormItem.writeFile()
   * will be allowed to write.
   */
  public long getMaximumAllowedBytes()
  {
    return _maxAllowedBytes;
  }

  /**
   * Returns the next MultipartStreamItem from the request, or null if no
   * more are present.
   */
  public MultipartFormItem getNextPart() throws IOException
  {
    final MultipartFormItemImpl previous = _currentItem;
    if (previous != null)
    {
      previous.finish();
    }

    //The first line is content-disposition
    final String dispositionText = _readLine(false);
    if (dispositionText == null)
    {
      return null;
    }
    final Disposition disposition = new Disposition(dispositionText);

    final String contentTypeText = _readLine(false);
    if (contentTypeText == null)
    {
      return null;
    }
    String contentType = _parseContentType(contentTypeText);

    if (contentType == null)
    {
      contentType = _DEFAULT_CONTENT_TYPE;
    }
    else
    {
      //Eat empty line
      final String emptyLine = _readLine(true);
      if (emptyLine.length() > 0)
      {
        // =-=AEW Better exception?
        throw new IOException();
      }
    }

    // Create the MultipartFormItem using the previously created
    // information
    _currentItem = new MultipartFormItemImpl(disposition, contentType);
    return _currentItem;
  }

  //Reads a line and makes sure it is a boundary.  Throws an exception
  //if the line is not a boundary
  private void _skipBoundary() throws IOException
  {
    final String line = _readLine(true);

    //A boundary must be a boundary, otherwise the stream is corrupt
    if (!line.startsWith(_boundary))
    {
      // A better exception would be nice.
      throw new IllegalStateException("Boundary information not found. Unable to process upload.");   
    }
  }

  //Reads a line from the stream.  If required is true then throws an
  //Exception if the line cannot be read.
  private String _readLine(final boolean required) throws IOException
  {
    return _readLine(required, false, true);
  }

  //Reads a line from the stream.  If required is true then throws an
  //Exception if the line cannot be read.
  private String _readLine(final boolean required, final boolean decodeEncoding, final boolean stripNewLines)
  throws IOException
  {
    final byte[] data = _lineBuffer;
    final int bytes = _readLine(data, 0, data.length);

    String line = null;
    if (bytes < 0)
    {
      if (required)
      {
        throw new EOFException();
      }
    }
    else
    {
      line = _dataToString(data, 0, bytes, decodeEncoding, stripNewLines);
    }
    return line;
  }

  // This is a replacement for ServletInputStream.readLine().  We use
  // this utility method instead as we don't always have a ServletInputStream.
  private int _readLine(final byte[] buffer, int offset, final int length) throws IOException
  {
    if (length <= 0)
    {
      return 0;
    }

    int count = 0;
    int c;

    // =-=AEW  This could be optimized quite a bit;  we could
    // read in a large number of bytes at once, and only return
    // enough to fill the incoming buffer or finish up a line.
    // However, if the input stream is itself buffered, this would
    // be of doubtful value.
    while ((c = _in.read()) != -1)
    {
      buffer[offset++] = (byte) c;
      count++;

      // Found a newline;  we're done.
      if (c == '\n')
      {
        break;
      }

      // Out of space; we're done too.
      // Read one character less so that we can account for CR
      if (count == length - 1)
      {
        // If we've found a CR, then we're not quite done;  we'd
        // better read over the next character (which might be a LF);
        // othewise, the LF gets processed as a bonus newline.
        if (c == '\r')
        {
          final int nextchar = _in.read();
          buffer[offset++] = (byte) nextchar;
          count++;
        }

        break;
      }
    }

    _totalBytesRead += count;
    return count > 0 ? count : -1;
  }

  private String _dataToString(final byte[] data, final int start, int bytes, final boolean decodeEncoding,
      boolean stripNewLines)
  {
    if (bytes > 0)
    {
      int i = 0;

      // Strip off up to the last two CR/LF's automatically
      while (i < 2 && bytes > 0)
      {
        final byte lastChar = data[start + bytes - 1];
        if (lastChar == '\r' || lastChar == '\n')
        {
          bytes--;
        }
        else
        {
          break;
        }

        i++;
      }

      // If we *don't* want to strip new lines, but we just did,
      // then put back a '\n' (doing it this way means that
      // we force any combo of CR/LF/etc. into '\n', which is our intention.)
      if (!stripNewLines && i > 0)
      {
        bytes++;
        data[start + bytes - 1] = '\n';
      }

      // Not NLS compliant;  but that's OK.  We're
      // precisely as NLS compliant as ServletRequest, so clients
      // have to do exactly the same handling there as here.
      // Note also that the "zero" below is important.  The
      // String(byte[], int, int) constructor uses the default
      // platform byte converter.  Whereas we explicitly want
      // to do _no_ byte conversion whatsoever, which is what
      // this constructor does.
      if (decodeEncoding && _characterEncoding != null)
      {
        try
        {
          return new String(data, start, bytes, _characterEncoding);
        }
        catch (final UnsupportedEncodingException uee)
        {
          // Shouldn't happen - we trap unsupported encodings
          // in setCharacterEncoding()... but fall through anyway
          assert false;
        }
      }
      else
      {
        try
        {
          return new String(data, start, bytes, "ISO-8859-1");
        }
        catch (final UnsupportedEncodingException uee)
        {
          // Shouldn't happen - we trap unsupported encodings
          // in setCharacterEncoding()... but fall through anyway
          assert false;
        }
      }
    }

    return "";
  }

  // Parse out the boundary text from the content type
  static private String _parseBoundary(final String contentType)
  {
    final int boundaryStart = contentType.indexOf(_BOUNDARY_PARAMETER);
    if (boundaryStart < 0)
    {
      return null;
    }

    String boundary = contentType.substring(boundaryStart + _BOUNDARY_PARAMETER.length());                   
    final int semicolonIndex = boundary.indexOf(";");                                                        
    if (semicolonIndex > -1) {                                                                               
        boundary = boundary.substring(0, semicolonIndex);                                                    
    }                                                                                                        

    // Boundary always starts with "--"                                                                      
        return "--" + boundary;
  }                                          

  //Reads the ContentType string out of a line of the incoming request
  private String _parseContentType(String line) throws IOException
  {
    String contentType = null;

    line = line.toLowerCase();

    if (line.startsWith(_CONTENT_TYPE_PARAMETER))
    {
      final int start = line.indexOf(" ");

      if (start == -1)
      {
        // =-=AEW Better exception?
        throw new IOException();
      }

      contentType = line.substring(start + 1);
    }
    else if (line.length() > 0)
    {
      // If the line after content-disposition has anything other
      // than content-type or an empty line, the data's bad.
      // =-=AEW Better exception?
      throw new IOException();
    }

    return contentType;
  }

  // Implementation of MultipartFormItem
  private class MultipartFormItemImpl implements MultipartFormItem
  {
    MultipartFormItemImpl(final Disposition disposition, final String contentType) throws IOException
    {
      _disposition = disposition;
      _contentType = contentType;

      // This is a parameter
      if (disposition.getFilename() == null)
      {
        _parameterValue = _readParameter();
      }

    }

    public void finish() throws IOException
    {
      if (_parameterValue == null && !_finished)
      {
        if (_inputStream == null)
        {
          writeFile(null);
        }
        else
        {
          _inputStream.finish();
        }

        _finished = true;
      }
    }

    public String getValue()
    {
      return _parameterValue;
    }

    public String getName()
    {
      return _disposition.getName();
    }

    public String getFilename()
    {
      return _disposition.getFilename();
    }

    public String getContentType()
    {
      return _contentType;
    }

    public long writeFile(final OutputStream out) throws IOException
    {
      // This isn't a file!
      if (_parameterValue != null)
      {
        // =-=AEW Better exception?  We could just write
        // the value out to their output stream instead
        // of complaining, but this is probably not what
        // they really want.
        throw new IOException(_LOG.getMessage(
          "ITEM_NOT_A_FILE"));
      }

      // The file's already been written, or at least
      // skipped over.
      if (_finished)
      {
        // =-=AEW Better exception?
        throw new IOException(_LOG.getMessage(
          "ITEM_ALREADY_BEEN_READ_PAST"));
      }

      if (_inputStream != null)
      {
        // =-=AEW Better exception?
        throw new IOException(_LOG.getMessage(
          "INPUT_STREAM_ALREADY_REQUESTED"));
      }

      long totalBytesWritten = 0;

      // ServletInputStream.readLine() has the annoying habit of adding a \r\n
      // to the end of the last line.
      // Since we want a byte-for-byte transfer, don't write the \r\n from the
      // end of a line until we have verified that we have another line.
      boolean addCRLF = false;
      int numbuf = 0;
      final byte[] buffer = __getStreamBuffer();
      final int bufferSize = buffer.length;
      while ((numbuf = _readLine(buffer, 0, bufferSize)) != -1)
      {
        // Check for boundary
        if (numbuf > 2 && buffer[0] == '-' && buffer[1] == '-') // quick pre-check
        {
          final String line = _dataToString(buffer, 0, numbuf, false, true);
          if (line.startsWith(_boundary))
          {
            break;
          }
        }

        // Are we supposed to write \r\n from the last iteration?
        if (addCRLF)
        {
          if (out != null)
          {
            out.write('\r');
            out.write('\n');
            totalBytesWritten += 2;
          }
          addCRLF = false;
        }
        // Postpone any ending \r\n until the next iteration
        if (numbuf >= 2 && buffer[numbuf - 2] == '\r' && buffer[numbuf - 1] == '\n')
        {
          numbuf -= 2; // skip the last 2 chars
          addCRLF = true; // make a note to write them on the next iteration
        }
        if (out != null)
        {
          totalBytesWritten += numbuf;
          if (totalBytesWritten <= _maxAllowedBytes)
          {
            out.write(buffer, 0, numbuf);
          }
        }
      }

      _finished = true;

      if (totalBytesWritten >= _maxAllowedBytes)
      {
        throw new EOFException(_LOG.getMessage(
          "UPLOADED_FILE_EXCEEDED_MAXIMUM_ALLOWED_LENGTH", new Object[]{totalBytesWritten, _maxAllowedBytes}));
      }
      return totalBytesWritten;
    }

    public InputStream getInputStream() throws IOException
    {
      if (_parameterValue != null)
      {
        // =-=AEW Better exception?  We could just give
        // them a StringInputStream, but this probably
        // isn't what they want
        throw new IOException(_LOG.getMessage(
          "ITEM_NOT_A_FILE"));
      }

      if (_finished)
      {
        // =-=AEW Better exception?
        throw new IOException(_LOG.getMessage(
          "ITEM_ALREADY_BEEN_READ_PAST"));
      }

      if (_inputStream != null)
      {
        // =-=AEW Better exception?
        throw new IOException(_LOG.getMessage(
          "INPUT_STREAM_ALREADY_REQUESTED"));
      }

      _inputStream = new MultipartInputStream();
      return _inputStream;
    }

    private String _readParameter() throws IOException
    {
      // Create the buffer.  It's no use reusing the buffer,
      // since the String object sent out will end up "owning"
      // the storage of the StringBuffer
      final StringBuffer buffer = new StringBuffer(200);
      for (String line = _readLine(false, true, false); line != null
      && !line.startsWith(_boundary); line = _readLine(false, true, false))
      {
        buffer.append(line);
      }

      // Trim the final newline
      final int length = buffer.length();
      if (buffer.charAt(length - 1) == '\n')
      {
        buffer.deleteCharAt(length - 1);
      }

      // =-=AEW Trim the buffer before toString()'ing?  The
      // usual time/space tradeoff.
      return buffer.toString();
    }

    private MultipartInputStream _inputStream;

    private Disposition          _disposition;

    private String               _contentType;

    private String               _parameterValue;

    // For a file item, has the file been read?
    private boolean              _finished;

    private class MultipartInputStream extends InputStream
    {
      MultipartInputStream() throws IOException
      {
        // This isn't a file!
        if (_parameterValue != null)
        {
          // =-=AEW Better exception?  We could just write
          // the value out to their output stream instead
          // of complaining, but this is probably not what
          // they really want.
          throw new IOException(_LOG.getMessage(
            "ITEM_NOT_A_FILE"));
        }

        // The file's already been written, or at least
        // skipped over.
        if (_finished)
        {
          // =-=AEW Better exception?
          throw new IOException(_LOG.getMessage(
            "ITEM_ALREDY_BEEN_READ_PAST"));
        }

        _begin = 0;
        _end = 0;
        _addCRLF = false;
        _buffer = __getStreamBuffer();
      }

      public void finish()
      {
        try
        {
          while (read(_buffer) > 0)
          {
            // do nothing
            ;
          }
        }
        catch (final IOException e)
        {
          // Don't care...
          ;
        }
      }

      //Fills up the _buffer parameter with
      //Returns false on EOF
      private void readLine() throws IOException
      {
        if (_finished)
        {
          throw new IOException(_LOG.getMessage(
            "END_OF_FILE"));
        }

        if (_addCRLF)
        {
          _buffer[_end++] = (byte) '\r';
          _buffer[_end++] = (byte) '\n';
          _addCRLF = false;
        }

        final int bufferSize = _buffer.length;
        int numbuf = _readLine(_buffer, _end, (bufferSize - _end));
        if (numbuf < 0)
        {
          _finished = true;
          return;
        }

        if (numbuf > 2 && _buffer[_end] == '-' && _buffer[_end + 1] == '-') // quick pre-check
        {
          // Check for boundary
          final String line = _dataToString(_buffer, _end, numbuf, false, true);
          if (line.startsWith(_boundary))
          {
            _finished = true;
            return;
          }
        }

        if (numbuf >= 2 && _buffer[_end + numbuf - 2] == '\r' && _buffer[_end + numbuf - 1] == '\n')
        {
          // Postpone any ending \r\n until the next iteration
          numbuf -= 2; // skip the last 2 chars
          _addCRLF = true; // make a note to write them on the next iteration
        }
        _end += numbuf;
      }

      @Override
      public int read(final byte[] buffer, final int offset, final int length) throws IOException
      {
        int bytes = -1; // default to EOF

        //Either there are more bytes in the stream, or there are more
        //bytes in the cache
        if (!_finished || _end > 0)
        {
          int cachedBytes;

          //No more data, must get some from servlet
          if (_end == 0)
          {
            readLine();
          }
          if (!_finished)
          {
            cachedBytes = _end - _begin;

            bytes = length > cachedBytes ? cachedBytes : length;

            System.arraycopy(_buffer, _begin, buffer, offset, bytes);
            _begin += bytes;

            //If we've written all the data out of the array, then reset
            //to the beginning of the array
            if (_begin == _end)
            {
              _begin = _end = 0;
            }
          }
        }
        return bytes;
      }

      @Override
      public int read(final byte[] buffer) throws IOException
      {
        return read(buffer, 0, buffer.length);
      }

      @Override
      public int read() throws IOException
      {
        final byte[] temp = new byte[1];
        int value;

        value = read(temp, 0, 1);

        if (value > 0)
        {
          value = (char) temp[0];
        }

        return value;
      }
    }

    //Where data begins in the buffer
    private int     _begin;

    //Where date ends in the buffer
    private int     _end;

    //Data read from the servlet
    private byte[]  _buffer;

    //If true then no more bytes can be read
    private boolean _addCRLF;
  }

  private class Disposition
  {
    Disposition(final String line) throws IOException // =-=AEW Better exception?
    {
      // =-=AEW This could be more efficient
      final StringTokenizer tokenizer = new StringTokenizer(line, ";");

      if (!tokenizer.hasMoreTokens())
      {
        throw new IOException();
      }

      // The first token has to be "content-disposition: something"
      String disposition = tokenizer.nextToken().toLowerCase();
      if (!disposition.startsWith(_CONTENT_DISPOSITION_PARAMETER))
      {
        throw new IllegalArgumentException();
      }

      // Get everything after content-disposition, lose the white space...
      disposition = disposition.substring(_CONTENT_DISPOSITION_PARAMETER.length()).trim();

      // ... and then make sure it's form-data.
      if (!disposition.equals(_FORM_DATA_DISPOSITION))
      {
        throw new IOException();
      }

      StringBuilder filenameBuffer = null;
      while (tokenizer.hasMoreTokens())
      {
        final String keyValue = tokenizer.nextToken().trim();
        if (_name == null)
        {
          _name = _extractName(keyValue);
        }
        else
        {
          if (filenameBuffer == null)
          {
            filenameBuffer = new StringBuilder(keyValue);
          // Don't quit on the first semicolon - keep appending
          }
          else
          {
            filenameBuffer.append(";").append(keyValue);
          }
        }
      }

      if (filenameBuffer != null)
      {
        _filename = _extractFilename(filenameBuffer.toString());
      }
    }

    public final String getName()
    {
      return _name;
    }

    public final String getFilename()
    {
      return _filename;
    }

    private String _extractName(final String keyValue)
    {
      return _extractValue(keyValue, _NAME_PARAMETER);
    }

    private String _extractFilename(final String keyValue)
    {
      String fileName = _extractValue(keyValue, _FILENAME_PARAMETER);
      try
      {
        fileName = CaboHttpUtils.decodeRequestParameter(fileName, getCharacterEncoding(), null);
      }
      catch (final UnsupportedEncodingException uee)
      {
        // Must never happen, because we always check the validity
        // of the encoding before it gets set
        assert false;
      }

      // Strip off anything that corresponds to a path.
      if (fileName != null)
      {
        final int index = Math.max(fileName.lastIndexOf('/'), fileName.lastIndexOf('\\'));

        if (index != -1)
        {
          fileName = fileName.substring(index + 1);
        }
      }

      return fileName;
    }

    private String _extractValue(final String keyValue, final String param)
    {
      final int length = param.length();
      if (keyValue.regionMatches(true, 0, param, 0, length))
      {
        // Remove the leading and trailing quotes
        int start = length;
        if (keyValue.charAt(start) == '"')
        {
          start++;
        }
        int end = keyValue.length();
        if (keyValue.charAt(end - 1) == '"')
        {
          end--;
        }
        return keyValue.substring(start, end);
      }

      return null;
    }

    private String _name;

    private String _filename;
  }

  byte[] __getStreamBuffer()
  {
    if (_streamBuffer == null)
    {
      _streamBuffer = new byte[_STREAM_BUFFER_SIZE];
    }
    return _streamBuffer;
  }

  private static final String   _MULTIPART_REQUEST_TYPE        = "multipart/form-data";

  private static final String   _DEFAULT_CONTENT_TYPE          = "application/octet-stream";

  //Parameter of the content type used to identify the boundary string
  private static final String   _BOUNDARY_PARAMETER            = "boundary=";

  private static final String   _NAME_PARAMETER                = "name=";

  private static final String   _FILENAME_PARAMETER            = "filename=";

  private static final String   _CONTENT_TYPE_PARAMETER        = "content-type";

  private static final String   _CONTENT_DISPOSITION_PARAMETER = "content-disposition:";

  private static final String   _FORM_DATA_DISPOSITION         = "form-data";

  private static final String   _HANDLED                       = "org.apache.myfaces.trinidadinternal.share.util.MultipartFormHandler.handled";

  private static final int      _STREAM_BUFFER_SIZE            = 65000;

  private static final int      _LINE_BUFFER_SIZE              = 8000;

  // Use one buffer for each of file streaming and line reading.
  // Not multithread
  // safe, but this class explicitly _can't_ multithread anyway.
  private final byte[]                _lineBuffer                    = new byte[_LINE_BUFFER_SIZE];

  private byte[]                _streamBuffer;

  private InputStream           _in;

  private String                _boundary;

  private MultipartFormItemImpl _currentItem;

  private String                _characterEncoding;

  private long                  _maxAllowedBytes               = 1L << 27;

  private int                   _totalBytesRead;

  private int                   _contentStreamSize             = -1;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    MultipartFormHandler.class);
}


