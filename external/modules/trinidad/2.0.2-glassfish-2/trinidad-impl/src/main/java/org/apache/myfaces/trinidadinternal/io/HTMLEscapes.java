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
package org.apache.myfaces.trinidadinternal.io;

import java.io.IOException;
import java.io.Writer;

/**
 * Utility class for escaping HTML text.
 * <p>
 */
public class HTMLEscapes
{
  // =-=AEW Performance - look at whether text and attributes
  // should be stored as character arrays or strings (might be
  // different decision for each), and make this class conform.

  /**
   * Write char array text.  Note that this code is duplicated below
   * for Strings - change both places if you make any changes!!!
   */
  static public void writeText(
    Writer out,
    char[]      buff,
    char[]      text,
    int         start,
    int         length
    ) throws IOException
  {
    _writeText(out, XMLEscapes.__BODY_ENTITIES, buff, text, start, length, true);
  }
  
  /**
   * Write String text.
   */
  static public void writeText(
    Writer out,
    char[] buff,
    String text
    ) throws IOException
  {
    _writeText(out, XMLEscapes.__BODY_ENTITIES, buff, text, true);
  }

  /**
   * Write a string attribute
   */
  static public void writeAttribute(
    Writer out,
    char[] buff,
    String attributeValue
    )
    throws IOException
  {
    _writeText(out, _ATTRIBUTE_ENTITIES, buff, attributeValue, false);
  }

  /**
   * Write char array text.  Note that this code is duplicated below
   * for Strings - change both places if you make any changes!!!
   */
  static private void _writeText(
    final Writer      out,
    final String[]    entities,
    final char[]      buff,
    final char[]      text,
    final int         start,
    final int         length,
    final boolean     isBodyText
    ) throws IOException
  {
    int buffIndex = 0;

    final int end = start + length;

    for (int i = start; i < end; i++)
    {
      final char ch = text[i];

      if (ch < 0xA0)
      {
        // text is in the US7ASCII range        
        if (ch >= 0x3f)
        {
          // US7ASCII text at "?" and above never needs to be escaped
          buffIndex = _addToBuffer(out, buff, buffIndex, ch);
        }
        else
        {
          // speed up handling of common characters like the space character by splitting the
          // range with possible entities into two parts--the high part where a null entity means
          // write the chacter through straight and the low part where a null entity means that
          // we have a line feed or carriage return, which require special handling
          if (ch > 0xD)
          {
            // high part of entities. Null entry means write ch through to buffer
            String entity = entities[ch];
            
            if (entity == null)
            {
              buffIndex = _addToBuffer(out, buff, buffIndex, ch);
            }
            else
            {
              // special case handling of & in attributes to support macros in future versions
              // of HTML
              if (ch == '&' && !isBodyText && (i + 1 < length) && (text[i + 1] == '{'))
                buffIndex = _addToBuffer(out, buff, buffIndex, ch);
              else
              {
                // not weird & attribute case, so write the entity
                buffIndex = _addToBuffer(out, buff, buffIndex, entity);
              }
            }
          }
          else
          {            
            // low part of entities. Null entry means we have a carriage return or line feed
            String entity = entities[ch];
            
            if (entity != null)
            {
              buffIndex = _addToBuffer(out, buff, buffIndex, entity);
            }
            else
            {
              buffIndex = _flushBuffer(out, buff, buffIndex);
              // handle carriage return/line feed
              
              // write out a newline
              _println(out);
              
              // collapse combinations of carriage return/line feed or line feed/carriage return
              // together
              char checkChar = (char)((ch == 0xD) ? 0xA : 0xD);
  
              if ((i + 1 < length) && (text[i + 1] == checkChar))
                i++;
            }            
          }
        }
      }
      else if (ch <= 0xff)
      {
        // character is in the high ISO range, so use HTML entity
        buffIndex = _addToBuffer(out, buff, buffIndex, _sISO8859_1_Entities[ch - 0xA0]);
      }
      else if (ch < 0xfffe) // characters fffe and ffff are considered outside of unicode
      {
        // character is outside of the ISO range
        if (isBodyText)
        {
          // See above for what _UNICODE_LINE_BREAK means...
          if (ch == _UNICODE_LINE_BREAK)
            buffIndex = _addToBuffer(out, buff, buffIndex, "<br>");
          else if (ch == _UNICODE_HYPHENATION_POINT)
            buffIndex = _addToBuffer(out, buff, buffIndex, "<wbr>");
          else
            buffIndex = _writeDecRef(out, buff, buffIndex, ch);
        }
        else
        {
          buffIndex = _writeDecRef(out, buff, buffIndex, ch);         
        }
      }
    }

    // flush the buffer, since the caller doesn't try to maintain the buffer index betweeen calls
    _flushBuffer(out, buff, buffIndex);
  }
  
  /**
   * Write String text.  Note that this code is duplicated above for
   * character arrays - change both places if you make any changes!!!
   */
  static private void _writeText(
    final Writer      out,
    final String[]    entities,
    final char[]      buff,
    final String      text,
    final boolean     isBodyText
    ) throws IOException
  {
    int buffIndex = 0;

    final int length = text.length();

    for (int i = 0; i < length; i++)
    {
      final char ch = text.charAt(i);

      if (ch < 0xA0)
      {
        // text is in the US7ASCII range        
        if (ch >= 0x3f)
        {
          // US7ASCII text at "?" and above never needs to be escaped
          buffIndex = _addToBuffer(out, buff, buffIndex, ch);
        }
        else
        {
          // speed up handling of common characters like the space character by splitting the
          // range with possible entities into two parts--the high part where a null entity means
          // write the character through straight and the low part where a null entity means that
          // we have a line feed or carriage return, which require special handling
          if (ch > 0xD)
          {
            // high part of entities. Null entry means write ch through to buffer
            String entity = entities[ch];
            
            if (entity == null)
            {
              buffIndex = _addToBuffer(out, buff, buffIndex, ch);
            }
            else
            {
              // special case handling of & in attributes to support macros in future versions
              // of HTML
              if (ch == '&' && !isBodyText && (i + 1 < length) && (text.charAt(i + 1) == '{'))
                buffIndex = _addToBuffer(out, buff, buffIndex, ch);
              else
              {
                // not weird & attribute case, so write the entity
                buffIndex = _addToBuffer(out, buff, buffIndex, entity);
              }
            }
          }
          else
          {            
            // low part of entities. Null entry means we have a carriage return or line feed
            String entity = entities[ch];
            
            if (entity != null)
            {
              buffIndex = _addToBuffer(out, buff, buffIndex, entity);
            }
            else
            {
              buffIndex = _flushBuffer(out, buff, buffIndex);
              
              // handle carriage return/line feed
              
              // write out a newline
              _println(out);
              
              // collapse combinations of carriage return/line feed or line feed/carriage return
              // together
              char checkChar = (char)((ch == 0xD) ? 0xA : 0xD);
  
              if ((i + 1 < length) && (text.charAt(i + 1) == checkChar))
                i++;
            }            
          }
        }
      }
      else if (ch <= 0xff)
      {
        // character is in the high ISO range, so use HTML entity
        buffIndex = _addToBuffer(out, buff, buffIndex, _sISO8859_1_Entities[ch - 0xA0]);
      }
      else if (ch < 0xfffe) // characters fffe and ffff are considered outside of unicode
      {
        // character is outside of the ISO range
        if (isBodyText)
        {
          // See above for what _UNICODE_LINE_BREAK means...
          if (ch == _UNICODE_LINE_BREAK)
            buffIndex = _addToBuffer(out, buff, buffIndex, "<br>");
          else if (ch == _UNICODE_HYPHENATION_POINT)
            buffIndex = _addToBuffer(out, buff, buffIndex, "<wbr>");
          else
            buffIndex = _writeDecRef(out, buff, buffIndex, ch);
        }
        else
        {
          buffIndex = _writeDecRef(out, buff, buffIndex, ch);         
        }
      }
    }

    // flush the buffer, since the caller doesn't try to maintain the buffer index betweeen calls
    _flushBuffer(out, buff, buffIndex);
  }

  /**
   * Writes the output as a decimal escape.  This is the same size or smaller than the hex
   * equivalent and works on versions of Netscape before 4.74. See bug #1491321.
   * <p>
   */
  static private int _writeDecRef(
    final Writer out,
    final char[] buff,
    int          buffIndex,
    final char   ch
    ) throws IOException
  {
    // Formerly used String.valueOf().  This version tests out
    // about 40% faster (and on systems where GC is going gonzo,
    // it should be massively better)

    // two branches, one using the buffer and one not
    if (buffIndex + 8 > buff.length)
    {
      // not enough room for biggest possible numeric character entry,
      // so flush buffer before we write to the output stream directly
      buffIndex = _flushBuffer(out, buff, buffIndex);
      
      // use XML escaping code
      XMLEscapes.__writeDecRef(out, ch);     
    }
    else
    {
      int i = ch;

      // we have enough space for the biggest string, so use buffer
      buff[buffIndex++] = '&';
      buff[buffIndex++] = '#';
 
      if (i > 10000)
      {      
        buff[buffIndex++] = (char)('0' + (i / 10000));
        i = i % 10000;
        buff[buffIndex++] = (char)('0' + (i / 1000));
        i = i % 1000;
        buff[buffIndex++] = (char)('0' + (i / 100));
        i = i % 100;
        buff[buffIndex++] = (char)('0' + (i / 10));
        i = i % 10;
        buff[buffIndex++] = (char)('0' + i);
      }
      else if (i > 1000)
      {
        buff[buffIndex++] = (char)('0' + (i / 1000));
        i = i % 1000;
        buff[buffIndex++] = (char)('0' + (i / 100));
        i = i % 100;
        buff[buffIndex++] = (char)('0' + (i / 10));
        i = i % 10;
        buff[buffIndex++] = (char)('0' + i);
      }
      else
      {
        buff[buffIndex++] = (char)('0' + (i / 100));
        i = i % 100;
        buff[buffIndex++] = (char)('0' + (i / 10));
        i = i % 10;
        buff[buffIndex++] = (char)('0' + i);
      }

      buff[buffIndex++] = ';';
    }
    
    return buffIndex;
  }  

  /**
   * Add a character to the buffer, flushing the buffer if the buffer is full,
   * and returning the new buffer index
   */
  private static int _addToBuffer(
    Writer out,
    char[] buffer,
    int    bufferIndex,
    char   ch
    ) throws IOException
  {
    if (bufferIndex >= buffer.length)
    {
      out.write(buffer, 0, bufferIndex);
      bufferIndex = 0;
    }

    buffer[bufferIndex] = ch;

    return bufferIndex + 1;
  }

  /**
   * Add a String to the buffer, flushing the buffer if the buffer is full,
   * and returning the new buffer index
   */
  private static int _addToBuffer(
    Writer out,
    char[] buffer,
    int    bufferIndex,
    String outString
    ) throws IOException
  {
    int writeSize = outString.length();
    int outSize = bufferIndex + writeSize;
    
    if (outSize >= buffer.length)
    {
      // flush the buffer
      out.write(buffer, 0, bufferIndex);
      bufferIndex = 0;
      
      // we will be the first chars in the buffer
      outSize = writeSize;
    }

    // copy into the buffer
    outString.getChars(0, writeSize, buffer, bufferIndex);
    
    return outSize;
  }

  /**
   * Flush the contents of the buffer to the output stream
   * and return the reset buffer index
   */
  private static int _flushBuffer(
    Writer out,
    char[]  buffer,
    int     bufferIndex
    ) throws IOException
  {
    if (bufferIndex > 0)
    {
      out.write(buffer, 0, bufferIndex);
    }

    return 0;
  }

  private static void _println(Writer out) throws IOException
  {
    out.write('\n');
  }
  

  private HTMLEscapes()
  {
  }

  // array of entities that need to be output in attributes for code points 0 - 62
  private static final String[] _ATTRIBUTE_ENTITIES;

  static
  {
    // initialize the entities that need to be escaped for attributes
    
    // we also need to escape the quote in attributes, but not the less-than
    _ATTRIBUTE_ENTITIES = XMLEscapes.__BASE_ENTITIES.clone();
    _ATTRIBUTE_ENTITIES['"'] = "&quot;";
  }
  
  //
  // Entities from HTML 4.0, section 24.2.1; character codes 0xA0 to 0xFF
  // If content size is important, all entities longer than four characters should be replaced
  // by their numeric equivalents
  //
  static private String[] _sISO8859_1_Entities = new String[]
  {
    "&nbsp;",
    "&iexcl;",
    "&cent;",
    "&pound;",
    "&curren;",
    "&yen;",
    "&brvbar;",
    "&sect;",
    "&uml;",
    "&copy;",
    "&ordf;",
    "&laquo;",
    "&not;",
    "&shy;",
    "&reg;",
    "&macr;",
    "&deg;",
    "&plusmn;",
    "&sup2;",
    "&sup3;",
    "&acute;",
    "&micro;",
    "&para;",
    "&middot;",
    "&cedil;",
    "&sup1;",
    "&ordm;",
    "&raquo;",
    "&frac14;",
    "&frac12;",
    "&frac34;",
    "&iquest;",
    "&Agrave;",
    "&Aacute;",
    "&Acirc;",
    "&Atilde;",
    "&Auml;",
    "&Aring;",
    "&AElig;",
    "&Ccedil;",
    "&Egrave;",
    "&Eacute;",
    "&Ecirc;",
    "&Euml;",
    "&Igrave;",
    "&Iacute;",
    "&Icirc;",
    "&Iuml;",
    "&ETH;",
    "&Ntilde;",
    "&Ograve;",
    "&Oacute;",
    "&Ocirc;",
    "&Otilde;",
    "&Ouml;",
    "&times;",
    "&Oslash;",
    "&Ugrave;",
    "&Uacute;",
    "&Ucirc;",
    "&Uuml;",
    "&Yacute;",
    "&THORN;",
    "&szlig;",
    "&agrave;",
    "&aacute;",
    "&acirc;",
    "&atilde;",
    "&auml;",
    "&aring;",
    "&aelig;",
    "&ccedil;",
    "&egrave;",
    "&eacute;",
    "&ecirc;",
    "&euml;",
    "&igrave;",
    "&iacute;",
    "&icirc;",
    "&iuml;",
    "&eth;",
    "&ntilde;",
    "&ograve;",
    "&oacute;",
    "&ocirc;",
    "&otilde;",
    "&ouml;",
    "&divide;",
    "&oslash;",
    "&ugrave;",
    "&uacute;",
    "&ucirc;",
    "&uuml;",
    "&yacute;",
    "&thorn;",
    "&yuml;"
  };

  // =-=AEW Need entities from 24.3.1 and 24.4.1

  // Constant for the Unicode line break character
  static private final char _UNICODE_LINE_BREAK = 0x2028;
  
  // Constant for the Unicode hyphenation point character.
  // UIFunctions.hyphenate(..) uses this character to indicate where to insert
  // the <wbr> tag. This tag inserts a no-width-space so that the browser may
  // break the line at that point.
  static private final char _UNICODE_HYPHENATION_POINT = 0x2027;
}
