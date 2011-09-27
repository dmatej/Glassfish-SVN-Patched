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

/**
 * This class contains String utilities. The code was taken from
 * Apache Commons, LANG project.
 * 
 * Reason is to provide an alternate method to java.lang.String.replace().
 * That method uses java.util.regex.Pattern.matcher(), which has a PERF
 * issue. 
 *  
 */
public final class StringUtils
{
  private StringUtils()
  {
    // no-op
  }

  public static String replace(String text, String searchString, String replacement)
  {
    return replace(text, searchString, replacement, -1);
  }

  // making this public, doesn't hurt... but we don't need it, now
  private static boolean isEmpty(String str)
  {
    return str == null || str.length() == 0;
  }

  // making this public, doesn't hurt... but we don't need it, now
  private static String replace(String text, String searchString, String replacement, int max)
  {
    if (isEmpty(text) || isEmpty(searchString) || replacement == null || max == 0)
    {
      return text;
    }
    int start = 0;
    int end = text.indexOf(searchString, start);
    
    if (end == -1)
    {
      return text;
    }
    int replLength = searchString.length();
    int increase = replacement.length() - replLength;
    increase = (increase < 0 ? 0 : increase);
    increase *= (max < 0 ? 16 : (max > 64 ? 64 : max));
    StringBuffer buf = new StringBuffer(text.length() + increase);
    while (end != -1)
    {
      buf.append(text.substring(start, end)).append(replacement);
      start = end + replLength;

      if (--max == 0)
      {
        break;
      }
      end = text.indexOf(searchString, start);
    }
    buf.append(text.substring(start));
    return buf.toString();
  }
}