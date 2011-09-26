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

import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.StringTokenizer;


/**
 * Utilities for working with HTTP.
 * <P>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/util/CaboHttpUtils.java#0 $) $Date: 10-nov-2005.18:59:22 $
 */
public class CaboHttpUtils
{
  /**
   * Throws an UnsupportedEncodingException if the
   * provided encoding is not supported.
   * @param encoding the name of a character encoding
   */
  static public void validateEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    if (encoding != null)
    {
      // Try creating a string off of the default encoding
      new String(_sTestBytes, encoding);
    }
  }

  /**
   * Given a parameter string and the name of a character encoding,
   * fixes the string.  The Servlet API builds Strings for all submitted
   * data as if they were ISO-8859-1 encoded;  this function undoes
   * the damage.  Clients should pass in the same encoding that they
   * used for generating the page that the submission comes from.
   * <p>
   * @param string the string
   * @param encoding the name of a character encoding
   * @param buffer an optional byte buffer, for reuse;  pass null
   *    if you're not calling this function repeatedly
   */
  static public final String decodeRequestParameter(
    String string,
    String encoding,
    byte[] buffer) throws UnsupportedEncodingException
  {
    if (encoding == null)
      return string;
    if (string == null)
      return null;

    int stringLength = string.length();

    if ((buffer == null) || (stringLength > buffer.length))
      buffer = new byte[stringLength];

    // As documented, this function doesn't convert "properly"
    // from characters to bytes.  But it happens to do
    // exactly the conversion we want
    // -= Simon Lessard =- 
    // TODO: Wouldn't getBytes() do the trick?
    string.getBytes(0, stringLength, buffer, 0);

    return new String(buffer, 0, stringLength, encoding);
  }


  /**
   * Given a string that uses q-value encodings, returns
   * a sorted array of strings from most-preferred to least.
   * For example, given "foo;0.25,bar,baz;0.5", this
   * will return ["bar", "baz", "foo"].
   */
  static public String[] decodeQValueString(
    String string
    )
  {
    ArrayList<String> values = new ArrayList<String>();
    ArrayList<Float> qValues = new ArrayList<Float>();
    if (string != null)
    {
      StringTokenizer tokens = new StringTokenizer(string, ",");

      while (tokens.hasMoreTokens())
      {
        // get the current valueuage token
        String currToken = tokens.nextToken();

        //
        // separate the value from the q-value, if any
        //
        String currValue = currToken;

        int qSepIndex = currToken.indexOf(';');

        // If no qValue is specified, it always defaults to 1.0.
        float currQ = 1.0f;
        if (qSepIndex != -1)
        {
          currValue = currValue.substring(0, qSepIndex);

          int tokenLength = currToken.length();

          qSepIndex++;

          if (qSepIndex < tokenLength)
          {
            String qString = currToken.substring(qSepIndex, tokenLength);
            qString = qString.trim();

            if (qString.length() > 0)
            {
              try
              {
                currQ = Float.valueOf(qString).floatValue();
              }
              catch (NumberFormatException e)
              {
                // ignore
                ;
              }
            }
          }
        }

        int i = values.size() - 1;
        while (i >= 0)
        {
          float f = qValues.get(i).floatValue();
          if (f >= currQ)
            break;

          i--;
        }

        values.add(i + 1, currValue);
        qValues.add(i + 1, new Float(currQ));
      }
    }

    return values.toArray(new String[values.size()]);
  }


  static private final byte[] _sTestBytes = new byte[]{(byte) 65};

  private CaboHttpUtils()
  {
  }
}
