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
package org.apache.myfaces.trinidadinternal.util;

import java.io.UnsupportedEncodingException;

import java.net.URLEncoder;

import org.apache.myfaces.trinidad.util.StringUtils;


final public class MimeUtility
{
  /**
   * Encode a string for HTTP header value
   * 
   * @param word The string data to encode.
   * @param isIE If true, use URL UTF-8 encoding, which MSIE expects.
   *             If false, use RFC-2047, UTF-8 quoted-printable, according to HTTP 1.1 spec.
   */
  public static String encodeHTTPHeader(String word, boolean isIE)
  {
    if (isIE)
    {
      // IE requires UTF-8 URL encoded content-disposition header value
      try
      {
        // IE does not understand "+ = space", workaround here
        return StringUtils.replace(URLEncoder.encode(word, "UTF-8"), "+", "%20");
      }
      catch (UnsupportedEncodingException e)
      {
        // this will not happen
        return null;
      }
    }
    else
    {
      // FF requires RFC-2047 encoded content-disposition header value
      // according to HTTP 1.1
      // use UTF-8 quoted-printable here

      // this is the maximum size of a segment of encoded data, which is based off 
      // of a 75 character size limit and all of the encoding overhead elements.
      int sizeLimit = 63; // 75 - 7 - "UTF-8".length

      StringBuffer result = new StringBuffer();

      try
      {
        // The Apache MimeUtillity is designed to support different encoding mechanism, 
        // quoted-printable, base64, UUEncode currently.
        // In our case, we just need one, either base64 or quoted-printable
        _encodeQuotedPrintable(word, result, sizeLimit, "UTF-8", true, false, _QP_WORD_SPECIALS);
      }
      catch (UnsupportedEncodingException e)
      {
        // this will not happen as UTF-8 is hardwired.
      }
      return result.toString();
    }
  }

  /**
   * Encode a string into quoted printable encoding, taking into 
   * account the maximum segment length. 
   * 
   * @param data      The string data to encode.
   * @param out       The output buffer used for the result.
   * @param sizeLimit The maximum amount of encoded data we're allowed
   *                  to have in a single encoded segment.
   * @param charset   The character set marker that needs to be added to the
   *                  encoding header.
   * @param firstSegment
   *                  If true, this is the first (left-most) segment in the
   *                  data.  Used to determine if segment delimiters need to
   *                  be added between sections.
   * @param foldSegments
   *                  Indicates the type of delimiter to use (blank or newline sequence).
   * @param specials  The set of special characters that we require to encoded.
   */
  private static void _encodeQuotedPrintable(
    String       data, 
    StringBuffer out, 
    int          sizeLimit, 
    String       charset, 
    boolean      firstSegment, 
    boolean      foldSegments, 
    String       specials)
    throws UnsupportedEncodingException
  {
    // this needs to be converted into the appropriate transfer encoding. 
    // The Apache MimeUtility can support encoding and decoding of different 
    // charsets, UTF-8, Shift-JIS, etc.  
    // To support this, it requires a Java-IANA charset name mapping.  
    // For example, Java name = 8859_1 vs IANA name = ISO-8859-1.
    // In our case, we need only UTF-8 only.  
    // The Java name and IANA name are same here.  
    // So, simplified the logics and remove this. 
    // byte [] bytes = data.getBytes(javaCharset(charset)); 
    byte[] bytes = data.getBytes(charset);

    int estimatedSize = _estimateQPEncodedLength(bytes, specials);

    // if the estimated encoding size is over our segment limit, split the string in half and 
    // recurse.  Eventually we'll reach a point where things are small enough.  
    if (estimatedSize > sizeLimit)
    {
      // the first segment indicator travels with the left half. 
      _encodeQuotedPrintable(data.substring(0, data.length() / 2), out, sizeLimit, charset, 
                             firstSegment, foldSegments, specials);
      // the second half can never be the first segment 
      _encodeQuotedPrintable(data.substring(data.length() / 2), out, sizeLimit, charset, false, 
                             foldSegments, specials);
    }
    else
    {
      // if this is not the first sement of the encoding, we need to add either a blank or 
      // a newline sequence to the data 
      if (!firstSegment)
      {
        if (foldSegments)
        {
          out.append("\r\n");
        }
        else
        {
          out.append(' ');
        }
      }
      // do the encoding of the segment.
      _encodeQPWord(bytes, out, charset, specials);
    }
  }

  /**
   * Perform RFC-2047 word encoding using Base64 data encoding.
   * 
   * @param data     The source for the encoded data.
   * @param out      The output stream where the encoded data is to be written.
   * @param charset  The charset tag to be added to each encoded data section.
   * @param specials The set of special characters that we require to encoded.
   * 
   */
  private static void _encodeQPWord(
    byte[]       data, 
    StringBuffer out, 
    String       charset, 
    String       specials)
  {
    // append the word header 
    out.append("=?");
    out.append(charset);
    out.append("?Q?");
    // add on the encodeded data       
    _encodeQPWordData(data, out, specials);
    // the end of the encoding marker 
    out.append("?=");
  }

  /**
   * Perform RFC-2047 word encoding using Q-P data encoding.
   *
   * @param data     The source for the encoded data.
   * @param specials The set of special characters that we require to encoded.
   * @param out      The StringBuffer where the encoded data is to be written.
   */
  private static void _encodeQPWordData(
    byte[]       data, 
    StringBuffer out, 
    String       specials)
  {
    for (int i = 0; i < data.length; i++)
    {
      int ch = data[i] & 0xff;
      ;

      // spaces require special handling.  If the next character is a line terminator, then
      // the space needs to be encoded.
      if (ch == ' ')
      {
        // blanks get translated into underscores, 
        // because the encoded tokens can't have embedded blanks.
        out.append('_');
      }
      // non-ascii chars and the designated specials all get encoded.
      else if (ch < 32 || ch >= 127 || specials.indexOf(ch) != -1)
      {
        out.append('=');
        out.append((char) _ENCODING_TABLE[ch >> 4]);
        out.append((char) _ENCODING_TABLE[ch & 0x0F]);
      }
      else
      {
        // good character, just use unchanged.
        out.append((char) ch);
      }
    }
  }


  /**
   * Estimate the final encoded size of a segment of data. 
   * This is used to ensure that the encoded blocks do 
   * not get split across a unicode character boundary and 
   * that the encoding will fit within the bounds of 
   * a mail header line. 
   * 
   * @param data     The data we're anticipating encoding.
   * @param specials The set of special characters that we require to encoded.
   * @return         The size of the byte data in encoded form. 
   */
  private static int _estimateQPEncodedLength(byte[] data, String specials)
  {
    int count = 0;

    for (int i = 0; i < data.length; i++)
    {
      // make sure this is just a single byte value.
      int ch = data[i] & 0xff;

      // non-ascii chars and the designated specials all get encoded.
      if (ch < 32 || ch >= 127 || specials.indexOf(ch) != -1)
      {
        // Q encoding translates a single char into 3 characters 
        count += 3;
      }
      else
      {
        // non-encoded character 
        count++;
      }
    }
    return count;
  }
  
  private MimeUtility() {}
  
  private static final String _QP_WORD_SPECIALS = "=_?\"#$%&'(),.:;<>@[\\]^`{|}~";
  private static final byte[] _ENCODING_TABLE =
  { (byte) '0', (byte) '1', (byte) '2', (byte) '3', (byte) '4', (byte) '5', (byte) '6', (byte) '7', 
    (byte) '8', (byte) '9', (byte) 'A', (byte) 'B', (byte) 'C', (byte) 'D', (byte) 'E', 
    (byte) 'F' };
}
