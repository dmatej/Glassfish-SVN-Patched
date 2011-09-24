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
package org.apache.myfaces.trinidad.util;


import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;


/**
 * An InputStream that decodes data from base64 representation into a binary
 * format. 
 * It takes a Reader as its single argument to its constructor and base64 
 * characters read in from the Reader are correspondingly made available to be 
 * read out in the corresponding binary format via the read() method.
 */
public class Base64InputStream extends InputStream
{
  /*
   * @param in  a character stream of valid base64 characters.  the reader 
   * should not contain invalid base64 characters (such as newlines).
   *
   */
  public Base64InputStream(Reader in)
  {
    _in = in;
    _byteBufferIndex = Integer.MAX_VALUE;
    _maxByteBufferIndex = 0;
    _byteBuffer = new byte[(int)(_QUANTUM_SIZE*0.75)];
  }
  
  
  
  /**
   * Read a single character. 
   * 
   * Overrides InputStream.read()
   * 
   * @return The byte read, as an integer, 
   *         or -1 if the end of the stream has been reached 
   * 
   */
  @Override
  public int read() throws IOException
  {
    int result = -1;
    
    if ( _byteBufferIndex <= _maxByteBufferIndex ) 
    {
      result = _byteBuffer[_byteBufferIndex];
      result = result & 0xff;
      _byteBufferIndex++;
    } 
    else 
    {
      // exhausted our buffer, so fill it up and try again.
      // Only try to read another byte if byteBuffer has been refilled 
      // with something new.
      
      int b = _fillByteBuffer();
      if (b>-1) {  
        return read();  
      } 
    }
    
    return result;
  }
  
  @Override
  public void close() throws IOException 
  {
    _in.close();
  }

  @Override
  public int available() throws IOException 
  {
    return _in.ready() ? 1 : 0;
  }

  /**
   * Reads in _QUANTUM_SIZE number of base64 characters from the reader 
   * and converts them into bytes and places these bytes into the decodedBuffer
   * array.
   * 
   * Note: This method assumes that the reader contains ONLY valid Base64 
   * characters.  
   * 
   * @return the highest index of the decodedbuffer that this method filled up
   *  
   *
   */
  private int _fillByteBuffer() throws IOException
  {
    //fill encodedBuffer with up to _QUANTUM_SIZE valid base64 chars from reader
    int numValidCharsRead;
    char[]  encodedBuffer = new char[ _QUANTUM_SIZE ]; // base64 encoded chars
    
    numValidCharsRead = _in.read( encodedBuffer, 0, _QUANTUM_SIZE );

    // reset the index
    _byteBufferIndex = -1;
    
    char eb1, eb2, eb3, eb4;        
    char c1, c2, c3, c4;
    
    for (int i = 0; i < numValidCharsRead; i += 4) 
    {   
      eb1 = encodedBuffer[i];
      eb2 = encodedBuffer[i+1];
      eb3 = encodedBuffer[i+2];
      eb4 = encodedBuffer[i+3];
         
      c1 = _decode(eb1);
      c2 = _decode(eb2);
      c3 = _decode(eb3);
      c4 = _decode(eb4);
      
      _byteBufferIndex++;
      _byteBuffer[_byteBufferIndex] = (byte)( (c1<<2) | ((c2>>4)&0x03) );
      if (eb3 != '=')
      {
        _byteBufferIndex++;  
        _byteBuffer[_byteBufferIndex] = (byte)( (c2<<4) | ((c3>>2)&0x0f) );
      }
      if (eb4 != '=')
      {
        _byteBufferIndex++;
        _byteBuffer[_byteBufferIndex] = (byte)( (c3<<6) | (c4&0x3f) );
      }
    }
    
    // set the maximum index to be the number of bytes written to the buffer
    _maxByteBufferIndex = _byteBufferIndex;
    // reset the index so we can walk through the byte buffer
    _byteBufferIndex = 0;
    
    return _maxByteBufferIndex;
      
  }
  
  
  /**
   * Converts a base64 character into its decimal equivalent.
   * 
   * @param   c the base64 character as an int
   * 
   * @return decoded value of the input base64 character
   */
  static private char _decode(int c) 
  {
    if (c >= 'A' && c <= 'Z')
    {
      return (char)(c - 'A');
    }
    else if (c >= 'a' && c <= 'z') 
    {
      return (char)(c - 'a' + 26);
    }
    else if (c >= '0' && c <= '9') 
    {
      return (char)(c - '0' + 52);
    } 
    else if (c == '+') 
    {
      return 0x3e;
    } 
    else if (c == '/')
    {
      return 0x3f;
    }
    else if (c == '=') 
    {
      return '=';
    } 
    else 
    {
      return 0;
    }
  }

  /** the number of base64 encoded characters to read from reader at a time **/
  /** this number should be a multiple of 4 **/
  private static final int _QUANTUM_SIZE = 512;
  
  /** input stream of base64 encoded data **/
  private final Reader  _in;
  
  /** an index into the decoded byte buffer **/
  private int     _byteBufferIndex;
  
  /** the number of bytes in the byte buffer **/
  private int     _maxByteBufferIndex;
 
  /** contains decoded bytes  **/
  private byte[]  _byteBuffer;
}
