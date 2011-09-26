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
package org.apache.myfaces.trinidadinternal.share.url;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.io.Writer;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;

import java.util.BitSet;

import org.apache.myfaces.trinidad.util.ThreadLocalUtils;

/**
 * Utility functions for encoding URLs for output.
 * This class contains three classes of encoding functions:
 * <ol>
 *   <li>Functions that encode entire URLs for final output.  The
 *       UIX rendering layer automatically uses these functions,
 *       so it is generally not necessary.
 *     <ul>
 *     <li>{@link #encodeURL encodeURL}
 *     <li>{@link #writeURL writeURL}
 *     </ul>
 *   <li>Functions that encode query parameters to be handed
 *       off to UIX Components (aka Marlin beans or UIX rendering).
 *     <ul>
 *     <li>{@link #encodeUIXQueryParameter encodeUIXQueryParameter}
 *     <li>{@link #appendUIXQueryParameter appendUIXQueryParameter}
 *     </ul>
 *   <li>Functions that encode query parameters for final output.
 *       These should be used only when the parameters are not
 *       being rendered with UIX (or UIX Components, aka Marlin Beans).
 *     <ul>
 *     <li>{@link #encodeString encodeString}
 *     </ul>
 * </ol>
 * <p>The major difference between these types of functions
 * is what characters they will encode.
 * <p>Category 1 (entire URLs)
 * will perform correct NLS encodings, but cannot encode
 * '%', '&' '=', '?', '#', and '+', as all of these characters
 * have very specific meanings in an URL.  In the case of '%' and '+'
 * note that this means UIX will never double-encode an URL.
 *
 * <p>Category 2 (parameters for UIX) will encode only
 * '%', '+', '&', and '#'.  Note that it does not perform
 * NLS encodings, or any disallowed URL characters (like the space
 * character), as it relies on the UIX rendering layer to
 * handle these where it can be performed more efficiently.
 * Also, note that '+' and '%' are encoded, so this function
 * should not be used on parameters that have already been URL encoded.
 * <p>
 * Finally, Category 3 (parameters for final output) encodes
 * all characters except for '-', '_', '.', and '/'.  There is
 * no need to call these functions if you'll be using UIX (or
 * Marlin beans) to render the URL.
 * <p>
 * EncoderUtils also includes support for manually decoding strings.
 * This is generally not necessary, as Servlets are generally responsible
 * for decoding parameters themselves, but developers working around
 * buggy servlet engines may find this useful, as will developers
 * writing their own HTTP code.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/url/EncoderUtils.java#0 $) $Date: 15-nov-2005.19:26:40 $
 */
public class EncoderUtils
{
  /**
   * Encodes a string into URL-encoded format.  For details
   * on exactly how this method works, please see the {@link #writeURL}
   * method.
   * <p>
   * @param text the unencoded (or partially encoded) String
   * @param queryEncoding the character set encoding for after the first
   *    question mark
   * @param useISOForBody if false, text before the first question
   *   mark will be encoded with UTF-8;  if true, encoded with ISO-8859-1.
   * @return the encoded string
   */
  static public String encodeURL(
    String      text,
    String      queryEncoding,
    boolean     useISOForBody) throws UnsupportedEncodingException
  {
    StringWriter sw = new StringWriter(text.length());
    try
    {
      writeURL(sw, text, queryEncoding, useISOForBody);
    }
    catch (UnsupportedEncodingException uee)
    {
      // Catch and rethrow - this exception can happen
      throw uee;
    }
    catch (IOException ioe)
    {
      // This should never happen - StringWriters don't throw anything!
      assert(false);
    }

    return sw.toString();
  }

  /**
   * Writes a string into URL-encoded format out to a Writer.
   * <p>
   * All characters before the start of the query string will be encoded
   * using either UTF-8 or ISO-8859-1.  The former is the ideal standard,
   * as it allows any Unicode character to be used in the path.  However,
   * many web servers assume ISO-8859-1 instead, hence the option.
   * <p>
   * Characters after the start of the query string will be encoded
   * using a client-defined encoding.  You'll need to use the encoding
   * that the server will expect.  (HTML forms will generate query
   * strings using the character encoding that the HTML itself was
   * generated in.)
   * <p>
   * All characters will be encoded as needed for URLs, with the exception
   * of the percent symbol ("%").  Because this is the character
   * itself used for escaping, attempting to escape this character
   * would cause this code to double-escape some strings.  It also may
   * be necessary to pre-escape some characters.  In particular, a
   * question mark ("?") is considered the start of the query string.
   * <p>
   * @param out a Writer for the output
   * @param text the unencoded (or partially encoded) String
   * @param queryEncoding the character set encoding for after the first
   *    question mark
   * @param useISOForBody if false, text before the first question
   *   mark will be encoded with UTF-8;  if true, encoded with ISO-8859-1.
   */
  static public void writeURL(
    Writer      out,
    String      text,
    String      queryEncoding,
    boolean     useISOForBody) throws IOException, UnsupportedEncodingException
  {
    _writeURL(out, text, queryEncoding, useISOForBody, 0, false, false);
  }

  /**
   * A clone of writeURL(), but applying rules knowing that it
   * is outputting HTML.
   */
  static public void writeURLForHTML(
    Writer      out,
    String      text,
    String      queryEncoding,
    boolean     useISOForBody) throws IOException, UnsupportedEncodingException
  {
    _writeURL(out, text, queryEncoding, useISOForBody, 0, false, true);
  }


  /**
   * Writes a query parameter.  Very few clients will
   * need to use this method - most will just call
   * writeURL or encodeURL.
   */
  static public void writeQueryParameters(
    Writer       out,
    String       text,
    String       encoding,
    int          start,
    boolean      forHtml)  throws IOException, UnsupportedEncodingException
  {
    _encodeString(out, text, encoding, start, _DONT_ENCODE_SET, true, forHtml);
  }
  
  /**
   * Writes a query parameter to the String Builder
  */
  static public void writeQueryParametersToStringBuilder(
    StringBuilder       sbout,
    char[]              cArray,
    String              encoding,
    int                 start,
    boolean             forHtml)  throws IOException, UnsupportedEncodingException
  {
    _encodeStringToStringBuilder(sbout, cArray, encoding, start, _DONT_ENCODE_SET, true, forHtml);
  }

  /**
   * Partially encodes a query parameter.  The characters '%',
   * '#', '&amp;',. and '+' will be encoded, but no other
   * characters will be.  This method should only be used
   * for encoding strings that will be passed to the UIX rendering
   * layer, because it does not perform the pieces of URL encoding
   * automatically handled in that layer.
   */
  static public String encodeUIXQueryParameter(String text)
  {
    int length = text.length();
    for (int i = 0; i < length; i++)
    {
      char ch = text.charAt(i);
      if (_isSpecialQueryParameterChar(ch))
      {
        // Append the entire string, then trim back to only
        // include the first "i" characters that don't need
        // to be escaped.  This optimization avoids the allocation
        // from calling substring().  Also, the built-in overallocation
        // of StringBuffer by 16 will obviate the need for reallocation
        // of the buffer as long as there's five or fewer "special" characters
        StringBuffer buffer = new StringBuffer(text);
        buffer.setLength(i);
        _appendUIXQueryParameter(buffer, text, i);
        return new String(buffer);
      }
    }

    return text;
  }

  public static String appendURLArguments(
    StringBuffer buffer,
    String baseURL,
    String[] keysAndValues
    )
  {

    // Bug 1814825: the anchor has to stay on the end.
    int anchorIndex = baseURL.indexOf('#');

    if (anchorIndex >= 0)
      buffer.append(baseURL.substring(0, anchorIndex));
    else
      buffer.append(baseURL);

    boolean queryAppended = (baseURL.indexOf('?') >= 0);

    for (int i = 0; i < keysAndValues.length; i += 2)
    {
      String value = keysAndValues[i+1];
      if (value != null)
      {
        // only append '?' at start if the URL doesn't already contain
        // arguments
        if (!queryAppended)
        {
          queryAppended = true;
          buffer.append('?');
        }
        else
          buffer.append('&');

        buffer.append(keysAndValues[i]);
        buffer.append('=');
        buffer.append(value);
      }
    }

    String beforeEncode = buffer.toString();
    return beforeEncode;
  }


  public static String appendURLArguments(
    String baseURL,
    String[] keysAndValues
    )
  {
    // buffer length = base + separators + keys + values
    int bufferLength = baseURL.length() + keysAndValues.length;
    for (int i = 0; i < keysAndValues.length; i +=2)
    {
      String value = keysAndValues[i + 1];
      if (value != null)
      {
        bufferLength += keysAndValues[i].length() + value.length();
      }
    }

    StringBuffer buffer = new StringBuffer(bufferLength);

    return appendURLArguments(buffer, baseURL, keysAndValues);
  }




  /**
   * Partially encodes a query parameter.  The characters '%',
   * '#', '&amp;',. and '+' will be encoded, but no other
   * characters will be.  This method should only be used
   * for encoding strings that will be passed to the UIX rendering
   * layer, because it does not perform the pieces of URL encoding
   * automatically handled in that layer.
   */
  static public void appendUIXQueryParameter(
    StringBuffer buffer, String text)
  {
    buffer.append(text);

    int length = text.length();
    for (int i = 0; i < length; i++)
    {
      char ch = text.charAt(i);
      if (_isSpecialQueryParameterChar(ch))
      {
        // Peel back all the characters that may need to be encoded
        // This is an optimization that assumes that the vast majority
        // of values won't need to encoded at all.
        buffer.setLength(buffer.length() - (length - i));
        _appendUIXQueryParameter(buffer, text, i);
        break;
      }
    }
  }

    
  static private void _writeURL(
    Writer      out,
    String      text,
    String      queryEncoding,
    boolean     useISOForBody,
    int         start,
    boolean     inAnchor,
    boolean     forHtml) throws IOException, UnsupportedEncodingException
  {
    int length = text.length();
    //Get the shared String Builder
    StringBuilder sbout = __getSharedStringBuilder(length * 3/2);
    //Convert to char array for faster access
    char[] cArray = text.toCharArray();
    //Write the URL to the StringBuilder   
    _writeURLtoStringBuilder(sbout, cArray, queryEncoding, useISOForBody, start, inAnchor, forHtml); 
    //Write to the writer
    out.write(sbout.toString());
  }
    
    
  static private void _writeURLtoStringBuilder(
    StringBuilder      sbout,
    char[]      cArray,
    String      queryEncoding,
    boolean     useISOForBody,
    int         start,
    boolean     inAnchor,
    boolean     forHtml) throws IOException, UnsupportedEncodingException
  {
    int length = cArray.length;
    
    for (int i = start; i < length; i++)
    {
      char ch = cArray[i]; 

      // Following code derived from Xalan 2.0 SerializerToHTML,
      // Apache Software Foundation, http://www.apache.org/
      if ((ch < 33) || (ch > 126))
      {
        if (useISOForBody)
        {
          // ISO-8859-1.  Blindly assume the character will be < 255.
          // Not much we can do if it isn't.
          _writeDoubleHex(sbout, ch);
        }
        else
        {
          // Following code derived from Xalan 2.0 SerializerToHTML,
          // Encode UTF16 to UTF8.
          // Reference is Unicode, A Primer, by Tony Graham.
          // Page 92.

          if (ch <= 0x7F)
          {
            _writeDoubleHex(sbout, ch);
          }
          else if (ch <= 0x7FF)
          {
            // Clear low 6 bits before rotate, put high 4 bits in low byte,
            // and set two high bits.
            int high = (ch >> 6) | 0xC0;
            int low = (ch & 0x3F) | 0x80; // First 6 bits, + high bit

            _writeDoubleHex(sbout, high);
            _writeDoubleHex(sbout, low);
          }
          // high surrogate
          else if ((ch & 0xFC00) == 0xD800)
          {
            // Clear high 6 bits.
            int highSurrogate = ch & 0x03FF;

            // Middle 4 bits (wwww) + 1
            // "Note that the value of wwww from the high surrogate bit
            // pattern is incremented to make the uuuuu bit pattern in
            // the scalar value so the surrogate pair don't address the BMP."
            int wwww = ((highSurrogate & 0x03C0) >> 6);
            int uuuuu = wwww+1;

            // next 4 bits
            int zzzz = (highSurrogate & 0x003C) >> 2;

            // low 2 bits
            int yyyyyy = ((highSurrogate & 0x0003) << 4) & 0x30;

            // Get low surrogate character.
            i++;
            if (i < length)
              ch = cArray[i];
            else
              // =-=AEW Shouldn't happen
              ch = 0;


            // Clear high 6 bits.
            int lowSurrogate = ch & 0x03FF;

            // put the middle 4 bits into the bottom of yyyyyy (byte 3)
            yyyyyy = yyyyyy | ((lowSurrogate & 0x03C0) >> 6);

            // bottom 6 bits.
            int xxxxxx = (lowSurrogate & 0x003F);

            int byte1 = 0xF0 | (uuuuu >> 2); // top 3 bits of uuuuu
            int byte2 = 0x80 | (((uuuuu & 0x03) << 4) & 0x30) | zzzz;
            int byte3 = 0x80 | yyyyyy;
            int byte4 = 0x80 | xxxxxx;

            _writeDoubleHex(sbout, byte1);
            _writeDoubleHex(sbout, byte2);
            _writeDoubleHex(sbout, byte3);
            _writeDoubleHex(sbout, byte4);
          }
          else
          {
            int high = (ch >> 12) | 0xE0; // top 4 bits
            int middle = ((ch & 0x0FC0) >> 6) | 0x80; // middle 6 bits
            int low = (ch & 0x3F) | 0x80; // First 6 bits, + high bit
            _writeDoubleHex(sbout, high);
            _writeDoubleHex(sbout, middle);
            _writeDoubleHex(sbout, low);
          }
        }
      }
      /* =-=AEW DO NOT encode '%'.  If you do, then for starters,
         we'll double-encode anything that's pre-encoded.
         And, what's worse, there becomes no way to use
         characters that must be encoded if you
         don't want them to be interpreted, like '?' or '&'.
      else if('%' == ch)
      {
      _writeDoubleHex(out, ch);
      }
      */
      else if (ch == '"')
      {
        sbout.append("%22");
      }
      else if (ch == '?')
      {
        if (inAnchor)
        {
          sbout.append("%3F");
        }
        else
        {
          // AEW: And - everything in the query parameters is interpreted
          // as if it were in the request's character set.  So use
          // the real encoding for those!
          sbout.append('?');
          writeQueryParametersToStringBuilder(sbout, cArray, queryEncoding, i + 1, forHtml);
          return;
        }
      }
      else if (ch == '&')
      {
        if (forHtml)
          sbout.append("&amp;");
        else
          sbout.append(ch);
      }
      else
      {
        sbout.append(ch);
      }
    }
  }

  static private boolean _isSpecialQueryParameterChar(char ch)
  {
    return ((ch == '%') || (ch == '+') || (ch == '&') || (ch == '#'));
  }

  static private void _appendUIXQueryParameter(
    StringBuffer buffer,
    String       text,
    int          from)
  {
    int length = text.length();
    for (int j = from; j < length; j++)
    {
      char c = text.charAt(j);
      switch (c)
      {
        case '#':
          buffer.append("%23");
          break;
        case '&':
          buffer.append("%26");
          break;
        case '%':
          buffer.append("%25");
          break;
        case '+':
          buffer.append("%2b");
          break;
        default:
          buffer.append(c);
          break;
      }
    }
  }


  /**
   * Fully encodes a string - all characters with
   * special meanings in an URL will be '%' encoded.  This
   * means that this function can double-encode strings
   * that are already encoded, and will cause serious
   * problems if used for an entire URL, since it'll
   * escape characters like '?' and '&'.  Generally,
   * this function should be used only for encoding
   * single query values.
   */
  static public String encodeString(
    String      text,
    String      encoding) throws UnsupportedEncodingException
  {
    StringWriter sw = new StringWriter(text.length());
    try
    {
      _encodeString(sw, text, encoding, 0, _DONT_ENCODE_SET_SMALL, false, false);
    }
    catch (UnsupportedEncodingException uee)
    {
      // Catch and rethrow - this exception can happen
      throw uee;
    }
    catch (IOException ioe)
    {
      // This should never happen - StringWriters don't throw anything!
      assert(false);
    }

    return sw.toString();
  }


  /**
   * URL-decodes a string.  This function assumes
   * that (for query parameters)
   * <code>CaboHttpUtils.decodeRequestParameter()</code> has already
   * been called, and this function is being passed the result
   * of that function.  That is: if the string does not
   * contain any '%' characters, the string will be returned unmodified.
   * <p>
   * @see org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils#decodeRequestParameter
   * @param text the original text
   * @param encoding the character encoding to assume
   */
  static public String decodeString(
    String      text,
    String      encoding) throws UnsupportedEncodingException
  {
    int firstPercent = text.indexOf('%');
    // No '%'s - just return.
    if (firstPercent < 0)
      return text;

    int length = text.length();
    StringBuffer buffer = new StringBuffer(length);

    // Slam in everything up to the first percent,
    // then trim back.  This is assumed to be faster than
    // calling substring().
    buffer.append(text);
    buffer.setLength(firstPercent);

    byte[] bytes = null;
    int    byteIndex = 0;
    for (int i = firstPercent; i < length; i++)
    {
      char ch = text.charAt(i);
      // If it's a normal character, just append
      if (ch != '%')
      {
        // But flush the byte buffer first.
        if (byteIndex > 0)
        {
          buffer.append(new String(bytes, 0, byteIndex, encoding));
          byteIndex = 0;
        }

        buffer.append(ch);
      }
      else
      {
        // We're after a percent;  add to the byte-buffer.
        if (i + 2 < length)
        {
          byte b = (byte) ((_fromHex(text.charAt(i + 1)) << 4) |
                            _fromHex(text.charAt(i + 2)));

          // No encoding - blindly assume ISO-8859-1 (the Servlet default)
          if (encoding == null)
          {
            buffer.append((char) b);
          }
          else
          {
            if (bytes == null)
            {
              bytes = new byte[10];
            }
            else if (bytes.length == byteIndex)
            {
              byte[] newBytes = new byte[byteIndex * 2];
              System.arraycopy(bytes, 0, newBytes, 0, byteIndex);
              bytes = newBytes;
            }

            bytes[byteIndex] = b;
            byteIndex++;
          }
        }
        else
        {
          //??? an error case.  No obvious correct results, so truncate.
          ;
        }

        i = i + 2;
      }
    }

    if (byteIndex > 0)
    {
      buffer.append(new String(bytes, 0, byteIndex, encoding));
    }

    return new String(buffer);
  }




  static private void _encodeStringToStringBuilder(
    StringBuilder       sbout,
    char[]       cArray,
    String       encoding,
    int          start,
    BitSet       dontEncodeSet,
    boolean      lookForAnchor,
    boolean      forHtml)
      throws IOException, UnsupportedEncodingException
  {
    ByteArrayOutputStream buf    = null;
    OutputStreamWriter writer    = null;
    char[]             charArray = null;

    int length = cArray.length;
    for (int i = start; i < length; i++)
    {
      char ch = cArray[i];

      // If we care about anchors and find one, abort here
      // and start writing out the rest of the text with _writeURL()
      if (lookForAnchor && ('#' == ch))
      {
        sbout.append(ch);
        _writeURLtoStringBuilder(sbout, cArray, encoding, false, i + 1, true, forHtml);
        return;
      }
      else if (dontEncodeSet.get(ch))
      {
        if (forHtml && (ch == '&'))
        {
          sbout.append("&amp;");
        }
        else
        {
          sbout.append(ch);
        }
      }
      else
      {
        if (buf == null)
        {
          buf = new ByteArrayOutputStream(_MAX_BYTES_PER_CHAR);

          if (encoding != null)
            writer = new OutputStreamWriter(buf, encoding);
          else
            writer = new OutputStreamWriter(buf);
          charArray = new char[1];
        }

        // convert to external encoding before hex conversion
        try
        {
          // An inspection of OutputStreamWriter reveals
          // that write(char) always allocates a one element
          // character array.  We can reuse our own.
          charArray[0] = ch;
          writer.write(charArray, 0, 1);
          writer.flush();
        }
        catch(IOException e)
        {
          buf.reset();
          continue;
        }

        byte[] ba = buf.toByteArray();
        for (int j = 0; j < ba.length; j++)
        {
          _writeDoubleHex(sbout, ba[j] + 256);
        }

        buf.reset();
      }
    }
  }

    static private void _encodeString(
      Writer       out,
      String       text,
      String       encoding,
      int          start,
      BitSet       dontEncodeSet,
      boolean      lookForAnchor,
      boolean      forHtml)
        throws IOException, UnsupportedEncodingException
    {
      ByteArrayOutputStream buf    = null;
      OutputStreamWriter writer    = null;
      char[]             charArray = null;

      int length = text.length();
      for (int i = start; i < length; i++)
      {
        char ch = text.charAt(i);

        // If we care about anchors and find one, abort here
        // and start writing out the rest of the text with _writeURL()
        if (lookForAnchor && ('#' == ch))
        {
          out.write(ch);
          _writeURL(out, text, encoding, false, i + 1, true, forHtml);
          return;
        }
        else if (dontEncodeSet.get(ch))
        {
          if (forHtml && (ch == '&'))
          {
            out.write("&amp;");
          }
          else
          {
            out.write(ch);
          }
        }
        else
        {
          if (buf == null)
          {
            buf = new ByteArrayOutputStream(_MAX_BYTES_PER_CHAR);

            if (encoding != null)
              writer = new OutputStreamWriter(buf, encoding);
            else
              writer = new OutputStreamWriter(buf);
            charArray = new char[1];
          }

          // convert to external encoding before hex conversion
          try
          {
            // An inspection of OutputStreamWriter reveals
            // that write(char) always allocates a one element
            // character array.  We can reuse our own.
            charArray[0] = ch;
            writer.write(charArray, 0, 1);
            writer.flush();
          }
          catch(IOException e)
          {
            buf.reset();
            continue;
          }

          byte[] ba = buf.toByteArray();
          for (int j = 0; j < ba.length; j++)
          {
            _writeDoubleHex(out, ba[j] + 256);
          }

          buf.reset();
        }
      }
    }
    
    
  static private void _writeDoubleHex(
    Writer  out,
    int     i) throws IOException
  {
    out.write('%');
    out.write(_getHex((i >> 4) % 0x10));
    out.write(_getHex(i % 0x10));
  }
  
  static private void _writeDoubleHex(
    StringBuilder  sb,
    int     i) throws IOException
  {
    sb.append('%');
    sb.append(_getHex((i >> 4) % 0x10));
    sb.append(_getHex(i % 0x10));
  }

  static private char _getHex(
    int i)
  {
    if (i < 10)
      return ((char) ('0' + i));
    else
      return ((char) ('A' + (i - 10)));
  }

  static private byte _fromHex(char ch)
  {
    if ((ch >= '0') && (ch <= '9'))
      return (byte) (ch - '0');

    if ((ch >= 'a') && (ch <= 'f'))
      return (byte) (10 + (ch - 'a'));

    return (byte) (10 + (ch - 'A'));
  }

  static StringBuilder __getSharedStringBuilder(int capacity)
  {
    StringBuilder sb = _STRING_BUILDER.get();

    if (sb == null)
    {
      sb = new StringBuilder(capacity);
      _STRING_BUILDER.set(sb);
    }
    else 
    {
      sb.ensureCapacity(capacity);   
    }
    // clear out the stringBuilder by setting the length to 0
    sb.setLength(0);

    return sb;
  }

  static private final int    _MAX_BYTES_PER_CHAR = 10;
  static private final BitSet _DONT_ENCODE_SET = new BitSet(256);

  // A smaller set of characters not to encode, used
  // for encodeString()
  static private final BitSet _DONT_ENCODE_SET_SMALL = new BitSet(256);


  // See "http://www.ietf.org/rfc/rfc2396.txt"
  // We're not fully along for that ride either, but we do encode
  // ' ' as '%20', and don't bother encoding '~' or '/'
  static
  {
    for (int i = 'a'; i <= 'z'; i++)
    {
      _DONT_ENCODE_SET.set(i);
      _DONT_ENCODE_SET_SMALL.set(i);
    }

    for (int i = 'A'; i <= 'Z'; i++)
    {
      _DONT_ENCODE_SET.set(i);
      _DONT_ENCODE_SET_SMALL.set(i);
    }

    for (int i = '0'; i <= '9'; i++)
    {
      _DONT_ENCODE_SET.set(i);
      _DONT_ENCODE_SET_SMALL.set(i);
    }

    // Don't encode '%' - we don't want to double encode anything.
    _DONT_ENCODE_SET.set('%');
    // Ditto for '+', which is an encoded space
    _DONT_ENCODE_SET.set('+');

    _DONT_ENCODE_SET.set('#');
    _DONT_ENCODE_SET.set('&');
    _DONT_ENCODE_SET.set('=');
    _DONT_ENCODE_SET.set('-');
    _DONT_ENCODE_SET_SMALL.set('-');
    _DONT_ENCODE_SET.set('_');
    _DONT_ENCODE_SET_SMALL.set('_');
    _DONT_ENCODE_SET.set('.');
    _DONT_ENCODE_SET_SMALL.set('.');
    _DONT_ENCODE_SET.set('*');
    _DONT_ENCODE_SET.set('~');
    _DONT_ENCODE_SET.set('/');
    _DONT_ENCODE_SET_SMALL.set('/');
    _DONT_ENCODE_SET.set('\'');
    _DONT_ENCODE_SET.set('!');
    _DONT_ENCODE_SET.set('(');
    _DONT_ENCODE_SET.set(')');
    _DONT_ENCODE_SET.set(';'); //ADFFACES-235 ";" are valid chars in querystrings
  }
  
  static private final ThreadLocal<StringBuilder> _STRING_BUILDER = ThreadLocalUtils.newRequestThreadLocal();
}
