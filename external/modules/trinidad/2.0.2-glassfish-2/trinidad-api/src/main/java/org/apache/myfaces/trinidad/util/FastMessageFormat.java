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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * The FastMessageFormat class is a greatly reduced version
 * of the java.text.MessageFormat class.  It's also much faster
 * and much less expensive to create, which is especially
 * valuable when it is created and thrown away many times - 
 * a common use case in web applications.
 * <p>
 * The only syntax supported by this class is simple index-based
 * replacement, namely:
 * <pre>
 *     some{1}text{0}here{2}andthere
 * </pre>
 * Unlike MessageFormat, single quotes are NOT used for escaping.  
 * So, the following pattern could be used to include a left bracket:
 * <pre>
 *     some{text{0}
 * </pre>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/FastMessageFormat.java#0 $) $Date: 10-nov-2005.19:08:37 $
 */
public class FastMessageFormat
{
  /**
   * Creates a FastMessageFormat based on the given format string.
   */
  public FastMessageFormat(String formatString)
  {
    if (formatString == null)
      throw new NullPointerException();

    _formatText = formatString.toCharArray();
  }


  /**
   * This formatter will only replace patterns of the type "{[0-9]}"
   * for which there is an associated token.
   * Any other use of '{}' will be interpreted as literal text.
   * This aims to have the same behavior as TrFastMessageFormatUtils.format
   * on the client.
   * <p>
   * @param source an array of strings (tokens)
   */
  public String format(Object[] source)
  {
    int formatLength = _formatText.length;
    int length = 0;
    int tokenCount = source.length;
    for (int i = 0; i < tokenCount; i++)
    {
      Object sourceString = source[i];
      if (sourceString != null)
      {
        length += sourceString.toString().length();
      }
    }

    // The following buffer size is just an initial estimate. It is legal for
    // any given pattern, such as {0}, to occur more than once, in which case
    // the buffer size will expand automatically if need be.
    StringBuilder buffer = new StringBuilder(length + formatLength);

    int lastStart = 0;
    for (int i = 0; i < formatLength; i++)
    {
      char ch = _formatText[i];
      if (ch == '{')
      {
        // Only check for single digit patterns that have an associated token.
        if (i + 2 < formatLength && _formatText[i + 2] == '}')
        {
          int tokenIndex = _formatText[i + 1] - '0';
          if (tokenIndex >= 0 && tokenIndex < tokenCount)
          {
            buffer.append(_formatText, lastStart, i - lastStart);
            Object sourceString = source[tokenIndex];
            if (sourceString != null)
              buffer.append(sourceString.toString());

            i += 2;
            lastStart = i + 1;
          }
        }
      }
      // ELSE: Do nothing. The character will be added in later.
    }

    buffer.append(_formatText, lastStart, formatLength - lastStart);

    return new String(buffer);
  }

  private final char[] _formatText;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    FastMessageFormat.class);
}




