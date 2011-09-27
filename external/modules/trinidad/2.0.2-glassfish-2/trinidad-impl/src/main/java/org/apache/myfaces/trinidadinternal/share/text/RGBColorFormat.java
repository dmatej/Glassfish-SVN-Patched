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
package org.apache.myfaces.trinidadinternal.share.text;

import java.awt.Color;

import java.text.FieldPosition;
import java.text.ParsePosition;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Format for colors.
 * <p>
 * <strong>Time Format Syntax:</strong>
 * <p>
 * To specify the time format use a <em>color pattern</em> string.
 * In this pattern, all ASCII letters are reserved as pattern letters,
 * which are defined as the following:
 * <blockquote>
 * <pre>
 * Symbol   Meaning                 Presentation        Example
 * ------   -------                 ------------        -------
 * r        red component           (Number)            242
 * g        green component         (Number)            242
 * b        blue component          (Number)            242
 * a        alpha component         (Number)            255
 * R        red component           (Hex)               F2
 * G        green component         (Hex)               F2
 * B        blue component          (Hex)               F2
 * A        alpha component         (Hex)               FF
 * '        escape for text         (Delimiter)
 * ''       single quote            (Literal)           '
 * </pre>
 * </blockquote>
 * <p>
 * <strong>Examples:</strong>
 * <blockquote>
 * <pre>
 * Format Pattern                         Result
 * --------------                         -------
 * "#RRGGBB"                         ->>  #6609CC
 * "rrr,ggg,bbb"                     ->>  102,009,204
 * "t"                               ->>  Transparent (when alpha is zero)
 * </pre>
 * </blockquote>
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/text/RGBColorFormat.java#0 $) $Date: 10-nov-2005.19:00:00 $
 */
public class RGBColorFormat extends ColorFormat
{
  /**
   * Creates a RGBColorFormat with the specified pattern.
   * 
   * @param pattern  the color format pattern
   */
  public RGBColorFormat(
    String pattern)
  {
    if (pattern == null)
      throw new IllegalArgumentException();
      
    _pattern = pattern;
  }

  /**
   * Returns the value as a Color.
   */
  @Override
  public Object parseObject(
    String        text, 
    ParsePosition pos)
  {
    // do not attempt to parse null color string
    if (text == null)
      return null;
  
    int start = pos.getIndex();
    int oldStart = start;

    boolean inQuote = false; // inQuote set true when hits 1st single quote
    char prevCh = 0;
    int count = 0;
    int interQuoteCount = 1; // Number of chars between quotes
    int[] rgba = new int[_FIELD_COUNT];
    rgba[_ALPHA_FIELD] = 255; // default alpha to full (opaque)

    for (int i=0; i < _pattern.length(); i++)
    {
      char ch = _pattern.charAt(i);

      if (inQuote)
      {
        if (ch == '\'')
        {
          // ends with 2nd single quote
          inQuote = false;
          // two consecutive quotes outside a quote means we have
          // a quote literal we need to match.
          if (count == 0)
          {
            if (start >= text.length() || ch != text.charAt(start))
            {
              pos.setIndex(oldStart);
              pos.setErrorIndex(start);
              return null;
            }
            start++;
          }
          count = 0;
          interQuoteCount = 0;
        }
        else
        {
          // pattern uses text following from 1st single quote.
          if (start >= text.length() || ch != text.charAt(start)) 
          {
            // Check for cases like: 'at' in pattern vs "xt"
            // in time text, where 'a' doesn't match with 'x'.
            // If fail to match, return null.
            pos.setIndex(oldStart); // left unchanged
            pos.setErrorIndex(start);
            return null;
          }
          count++;
          start++;
        }
      }
      else    // !inQuote
      {
        if (ch == '\'')
        {
          inQuote = true;
          if (count > 0) // handle cases like: e'at'
          {
            int startOffset = start;
            start = _subParse(rgba, text, start, prevCh, count);
            if (start < 0) 
            {
              pos.setIndex(oldStart);
              pos.setErrorIndex(startOffset);
              return null;
            }
            count = 0;
          }

          if (interQuoteCount == 0)
          {
            // This indicates two consecutive quotes inside a quote,
            // for example, 'o''clock'.  We need to parse this as
            // representing a single quote within the quote.
            int startOffset = start;
            if (start >= text.length() ||  ch != text.charAt(start))
            {
              pos.setIndex(oldStart);
              pos.setErrorIndex(startOffset);
              return null;
            }
            start++;
            count = 1; // Make it look like we never left
          }
        }
        else if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z')
        {
          // ch is a color pattern
          if (ch != prevCh && count > 0) // e.g., rrr
          {
            int startOffset = start;
            start = _subParse(rgba, text, start, prevCh, count);
            if (start < 0) 
            {
              pos.setIndex(oldStart);
              pos.setErrorIndex(startOffset);
              return null;
            }
            prevCh = ch;
            count = 1;
          }
          else
          {
            if (ch != prevCh)
                prevCh = ch;
            count++;
          }
        }
        else if (count > 0)
        {
          // handle cases like: "rrr,ggg,bbb", "RR.GG.BB", "r g b", etc
          // where ch = ',', '.' or ' ', repectively.
          int startOffset = start;
          start = _subParse(rgba, text, start, prevCh, count);
          if ( start < 0 ) 
          {
            pos.setIndex(oldStart);
            pos.setErrorIndex(startOffset);
            return null;
          }
          if (start >= text.length() || ch != text.charAt(start)) 
          {
            // handle cases like: 'RR g' in pattern vs. "FFx20"
            // in color text, where ' ' doesn't match with 'x'.
            pos.setIndex(oldStart);
            pos.setErrorIndex(start);
            return null;
          }
          start++;
          count = 0;
          prevCh = 0;
        }
        else // any other unquoted characters
        {
          if (start >= text.length() || ch != text.charAt(start)) 
          {
            // handle cases like: 'RR   g' in pattern vs. "FF,,,20"
            // in color text, where "   " doesn't match with ",,,".
            pos.setIndex(oldStart);
            pos.setErrorIndex(start);
            return null;
          }
          start++;
        }

        interQuoteCount++;
      }
    }
    // Parse the last item in the pattern
    if (count > 0)
    {
      int startOffset = start;
      start = _subParse(rgba, text, start, prevCh, count);
      if (start < 0)
      {
        pos.setIndex(oldStart);
        pos.setErrorIndex(startOffset);
        return null;
      }
    }

    pos.setIndex(start);

    return new Color(rgba[_RED_FIELD], 
                     rgba[_GREEN_FIELD], 
                     rgba[_BLUE_FIELD], 
                     rgba[_ALPHA_FIELD]);
  }

  @Override
  public StringBuffer format(
    Color color,
    StringBuffer toAppendTo,
    FieldPosition pos)
  {
    // initialize
    pos.setBeginIndex(0);
    pos.setEndIndex(0);

    // do not attempt to format null color
    if (color == null)
      return toAppendTo;

    boolean inQuote = false; // true when between single quotes
    char prevCh = 0; // previous pattern character
    int count = 0;  // number of time prevCh repeated
    for (int i=0; i < _pattern.length(); ++i) 
    {
      char ch = _pattern.charAt(i);
      // Use subFormat() to format a repeated pattern character
      // when a different pattern or non-pattern character is seen
      if (ch != prevCh && count > 0) 
      {
        toAppendTo = _subFormat(color, prevCh, count, toAppendTo);
        count = 0;
      }
      if (ch == '\'') 
      {
        // Consecutive single quotes are a single quote literal,
        // either outside of quotes or between quotes
        if ((i+1) < _pattern.length() && 
            _pattern.charAt(i+1) == '\'') 
        {
          toAppendTo.append('\'');
          ++i;
        } 
        else 
        {
          inQuote = !inQuote;
        }
      } 
      else if (!inQuote && 
               (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z')) 
      {
        // ch is a date-time pattern character to be interpreted
        // by subFormat(); count the number of times it is repeated
        prevCh = ch;
        ++count;
      }
      else 
      {
        // Append quoted characters and unquoted non-pattern characters
        toAppendTo.append(ch);
      }
    }
    // Format the last item in the pattern, if any
    if (count > 0) 
    {
      toAppendTo = _subFormat(color, prevCh, count, toAppendTo);
    }
    
    return toAppendTo;
  }

  public int length()
  {
    int size = 0;
    boolean inQuote = false; // true when between single quotes
    char prevCh = 0; // previous pattern character
    int count = 0;  // number of time prevCh repeated
    for (int i=0; i < _pattern.length(); ++i) 
    {
      char ch = _pattern.charAt(i);
      // Use subFormat() to format a repeated pattern character
      // when a different pattern or non-pattern character is seen
      if (ch != prevCh && count > 0) 
      {
        size += _subLength(prevCh);
        count = 0;
      }
      if (ch == '\'') 
      {
        // Consecutive single quotes are a single quote literal,
        // either outside of quotes or between quotes
        if ((i+1) < _pattern.length() && 
            _pattern.charAt(i+1) == '\'') 
        {
          size++;
          ++i;
        } 
        else 
        {
          inQuote = !inQuote;
        }
      } 
      else if (!inQuote && 
               (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z')) 
      {
        // ch is a date-time pattern character to be interpreted
        // by subFormat(); count the number of times it is repeated
        prevCh = ch;
        ++count;
      }
      else 
      {
        // Append quoted characters and unquoted non-pattern characters
        size++;
      }
    }
    // Format the last item in the pattern, if any
    if (count > 0) 
    {
      size += _subLength(prevCh);
    }
    
    return size;
  }

  // Private member function that does the real length calculations.
  private int _subLength(
    char ch)
  {
    switch (ch)
    {
      case 'r':
      case 'g':
      case 'b':
      case 'a':
        return 3;
      case 'R':
      case 'G':
      case 'B':
      case 'A':
        return 2;
      default:
        throw new IllegalArgumentException(_LOG.getMessage(
          "ILLEGAL_PATTERN_CHARACTER", ch));
    }
  }
  
  // Private member function that does the real color parsing.
  private int _subParse(
    int[] rgba,
    String text, 
    int start, 
    char ch, 
    int count)
  {
    switch (ch)
    {
      case 'r':
        return _subParseDecimal(rgba, _RED_FIELD, text, start, count);
      case 'g':
        return _subParseDecimal(rgba, _GREEN_FIELD, text, start, count);
      case 'b':
        return _subParseDecimal(rgba, _BLUE_FIELD, text, start, count);
      case 'a':
        return _subParseDecimal(rgba, _ALPHA_FIELD, text, start, count);
      case 'R':
        return _subParseHex(rgba, _RED_FIELD, text, start, count);
      case 'G':
        return _subParseHex(rgba, _GREEN_FIELD, text, start, count);
      case 'B':
        return _subParseHex(rgba, _BLUE_FIELD, text, start, count);
      case 'A':
        return _subParseHex(rgba, _ALPHA_FIELD, text, start, count);
      default:
        throw new IllegalArgumentException(_LOG.getMessage(
          ">ILLEGAL_PATTERN_CHARACTER", ch));
    }
  }
  
  private int _subParseDecimal(
    int[] rgba,
    int field,
    String text, 
    int start,
    int count)
  {
    return _subParseBase(rgba, field, text, start, count, 3, 10);
  }
  
  private int _subParseHex(
    int[] rgba,
    int field,
    String text, 
    int start,
    int count)
  {
    return _subParseBase(rgba, field, text, start, count, 2, 16);
  }
  
  private int _subParseBase(
    int[] rgba,
    int field,
    String text, 
    int start,
    int minDigits,
    int maxDigits,
    int base)
  {
    int length = text.length();
    int atLeast = start + minDigits;

    // make sure the color string is long enough
    if (atLeast > length)
      return -start;
  
    int index;
    int end = Math.min(length, start + maxDigits);
    int value = 0;
    
    for(index=start; index < end; index++)
    {
      int digit = Character.digit(text.charAt(index), base);
      
      if (digit == -1)
      {
        // did not consume sufficient characters in color string
        if (index < atLeast)
          return -start;
        else
          break;
      }

      value *= base;
      value += digit;
    }
    
    rgba[field] = value;
    
    return index;
  }
  
  // Private member function that does the real color formatting.
  private StringBuffer _subFormat(
    Color color,
    char ch, 
    int count, 
    StringBuffer toAppendTo) throws IllegalArgumentException
  {
    switch (ch)
    {
      case 'r':
        return _subFormatDecimal(color.getRed(), count, toAppendTo);
      case 'g':
        return _subFormatDecimal(color.getGreen(), count, toAppendTo);
      case 'b':
        return _subFormatDecimal(color.getBlue(), count, toAppendTo);
      case 'a':
        return _subFormatDecimal(color.getAlpha(), count, toAppendTo);
      case 'R':
        return _subFormatHex(color.getRed(), count, toAppendTo);
      case 'G':
        return _subFormatHex(color.getGreen(), count, toAppendTo);
      case 'B':
        return _subFormatHex(color.getBlue(), count, toAppendTo);
      case 'A':
        return _subFormatHex(color.getAlpha(), count, toAppendTo);
      default:
        throw new IllegalArgumentException(_LOG.getMessage(
          ">ILLEGAL_PATTERN_CHARACTER", ch));
    }
  }

  private StringBuffer _subFormatDecimal(
    int value, 
    int minDigits,
    StringBuffer toAppendTo)
  {
    _prefixZeros(value, 10, minDigits, toAppendTo);
    toAppendTo.append(value);
    return toAppendTo;
  }

  private StringBuffer _subFormatHex(
    int value, 
    int minDigits,
    StringBuffer toAppendTo)
  {
    _prefixZeros(value, 16, minDigits, toAppendTo);
    toAppendTo.append(Integer.toHexString(value).toUpperCase());
    return toAppendTo;
  }

  private StringBuffer _prefixZeros(
    int value,
    int base,
    int minDigits,
    StringBuffer toAppendTo)
  {
    if (value >= 0)
    {
      int digits = (value > 0) 
                      ? (int)Math.ceil(Math.log(value) / Math.log(base))
                      : 1;
      while (minDigits > 0 && digits < minDigits)
      {
        toAppendTo.append('0');
        minDigits--;
      }
    }
    return toAppendTo;
  }
  
  private String  _pattern;

  private static final int _RED_FIELD   = 0;
  private static final int _GREEN_FIELD = 1;
  private static final int _BLUE_FIELD  = 2;
  private static final int _ALPHA_FIELD = 3;
  private static final int _FIELD_COUNT = 4;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RGBColorFormat.class);
  private static final long serialVersionUID = 1L;
}
