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
package org.apache.myfaces.trinidadinternal.util.nls;


/**
 * StringUtils provides functions for manipulating
 * mnemonics.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/nls/StringUtils.java#0 $) $Date: 10-nov-2005.18:49:14 $
 */
public class StringUtils
{
  public static final int MNEMONIC_INDEX_NONE = -1;

  /**
   * Truncates a string.
   * @param text the text value
   * @param truncateAt the number of characters at which the
   *   string should be truncated.  Strings will never be truncated
   *   to less than 13 characters.
   */
  static public String truncateString(
    String           text,
    int              truncateAt)
  {
    if (text == null)
      return null;

    if (truncateAt < _TRUNCATE_AT_MINIMUM)
      truncateAt = _TRUNCATE_AT_MINIMUM;

    if (text.length() > truncateAt)
    {
      text = text.substring(0, truncateAt - _ELLIPSIS_LENGTH) +
             _ELLIPSIS;
    }

    return text;
  }

  /**
   * Returns the index of the inline mnemonic in the specified string.
   * <p>
   * This method assumes that inline mnemonics are indicated via the ampersand
   * ('&') character.  It returns the index of the first ampersand character
   * followed by a non-ampersand character in the string.  If no inline
   * mnemonic character is found, MNEMONIC_INDEX_NONE is returned.
   * <p>
   * Strings which may contain mnemonics should be passed to
   * stripMnemonic() to strip the inline mnemonic indicator as
   * well as any escaped ampersands.
   * <p>
   * @see #stripMnemonic
   */
  public static int getMnemonicIndex(String string)
  {
    if (string == null)
    {
      return MNEMONIC_INDEX_NONE;
    }

    // Minus one, because a trailing ampersand doesn't count
    int lengthMinusOne = string.length() - 1;
    int i = 0;       // Index in the source sting
    int count = 0;

    while (i < lengthMinusOne)
    {
      int index = string.indexOf(_MNEMONIC_INDICATOR, i);
      // Are we at the end of the string?
      if ((index == -1) || (index >= lengthMinusOne))
        break;

      // if this isn't a double ampersand, return
      if (string.charAt(index + 1) != _MNEMONIC_INDICATOR)
        // This index has to be relative to the _stripped_ string,
        // so subtract off one character for every double ampersand found
        return (index - count);

      // Skip over the double ampersand
      i = index + 2;
      count++;
    }

    return MNEMONIC_INDEX_NONE;
  }


  /**
   * Removes non-displayable inline mnemonic characters.
   * <p>
   * In order to specify a mnemonic character in a translatable string
   * (eg. in a resource file), a special character is used to indicate which
   * character in the string should be treated as the mnemonic.  This method
   * assumes that an ampersand ('&') character is used as the mnemonic
   * indicator, and removes (single) ampersands from the input string.  A
   * double ampersand sequence is used to indicate that an ampersand should
   * actually appear in the output stream, in which case one of the ampersands
   * is removed.
   * <p>
   * Clients should call this method after calling
   * StringUtils.getMnemonicIndex() and before displaying the string.  The
   * returned string should be used in place of the input string when
   * displaying the string to the end user.  The returned string may be the
   * same as the input string if no mnemonic indicator characters are found,
   * but this is not guaranteed.
   * <p>
   * @see #getMnemonicIndex
   */
  public static String stripMnemonic(String string)
  {
    if (string == null)
    {
      return null;
    }

    int length = string.length();

    // Single character (or empty) strings can't have a mnemonic
    if (length <= 1)
      return string;

    StringBuffer buffer = null;
    int i = 0;

    while (i < length)
    {
      int index = string.indexOf(_MNEMONIC_INDICATOR, i);

      // We've reached the append.  Append the rest of the
      // string to the buffer, if one exists, then exit
      if ((index < 0) || (index >= length - 1))
      {
        if (buffer != null)
          buffer.append(string.substring(i));

        break;
      }

      if (buffer == null)
      {
        // If the string starts with an ampersand, but not a double
        // ampersand, then we just want to return
        // stripMnemonic(string.substring(1)).  This is basically
        // what we do here, only I've optimized the tail recursion away.
        if ((index == 0) && (string.charAt(1) != _MNEMONIC_INDICATOR))
        {
          string = string.substring(1);
          length--;
          continue;
        }
        else
        {
          // Allocate the buffer.  We can reserve only space
          // (length - 1), because, by now, we know there's at least
          // 1 ampersand
          buffer = new StringBuffer(length - 1);
        }
      }

      // Append the bits of the string before the ampersand
      buffer.append(string.substring(i, index));

      // And append the character after the ampersand
      buffer.append(string.charAt(index + 1));

      // And skip to after that character
      i = index + 2;
    }

    // If we never allocated a buffer, then there's no mnemonic
    // at all, and we can just return the whole string
    if (buffer == null)
      return string;

    return new String(buffer);
  }

  private static final char _MNEMONIC_INDICATOR = '&';
  static private final String _ELLIPSIS = "...";
  static private final int _ELLIPSIS_LENGTH = _ELLIPSIS.length();
  static private final int _TRUNCATE_AT_MINIMUM = 13;
}
