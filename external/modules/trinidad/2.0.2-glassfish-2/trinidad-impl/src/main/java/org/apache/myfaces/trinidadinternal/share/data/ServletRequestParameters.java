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
package org.apache.myfaces.trinidadinternal.share.data;

import java.util.Map;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Class wrapping up access to parameters.  This allows
 * the values and contents of these parameters to be modified from their
 * values in the HttpServletRequest
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/data/ServletRequestParameters.java#0 $) $Date: 10-nov-2005.19:00:16 $
 */
public class ServletRequestParameters extends RequestParameters
{
  /**
   * Flag parameter indicating that the request contains a
   * compound name that has to be parsed.
   */
  public static final String HAS_COMPOUND_NAME = "_compNm";


  /**
   * Returns a Map containing the modified parameters and
   * keys for Map of input request parameters.
   * <p>
   * If not modification is required the returned Dicitonary instance
   * will be the <cpde>requestParams</code> instance.
   */
  public static Map<String, Object> createRequestMap(
    Map<String, Object> requestParams
    )
  {
    if (requestParams == null)
      throw new IllegalArgumentException();

    return _processParameters(requestParams,
                              requestParams.size());
  }



  /**
   * Returns a request parameters object wrapping the parameters of
   * a ServletRequest.
   */
  public ServletRequestParameters(
    Map<String, Object> requestParams
    )
  {
    _params = createRequestMap(requestParams);

    // clone returned String arrays
    _cloneArrays = true;
  }


  /**
   * Decodes key value pairs from a single value, placing the decoded
   * values into the specified Map of String[]s.  This code
   * does not currently handle multiple values for the same key.
   * <p>
   * adapted from org.apache.myfaces.trinidadinternal.uix22.servlet.url.DefaultPageEncoder
   */
  public static void decodeCompoundKeyValues(
    String              compoundValue,
    int                 startIndex,
    Map<String, Object> keyValues
    )
  {
    if (compoundValue == null)
      return;

    int index    = startIndex - 1;
    int endIndex = compoundValue.length();

    // When submitting using an input type="image",
    // a .x or .y is appended to the name of the input
    // We want to remove this .x and .y before further processing
    int xIndex = compoundValue.lastIndexOf(_DOT_X);
    int yIndex = compoundValue.lastIndexOf(_DOT_Y);
    int xyIndex = Math.max(xIndex, yIndex);

    if ( xyIndex > -1)
    {
      // make sure this is not the .x of a user, which would have
      // a boundary character after it.
      if (compoundValue.substring(xyIndex + 1, endIndex).indexOf(_PROPERTY_BOUNDARY) < 0 )
        endIndex = xyIndex;
    }

    while (index >= -1)
    {
      boolean fixEscapes = false;

      // Find the start of the next property (or the end)
      int nextIndex = compoundValue.indexOf(_PROPERTY_BOUNDARY,
                                            index + 1);
      // Search for any especially escaped characters
      while ((nextIndex > 0) &&
             (nextIndex < endIndex - 1))
      {
        char nextChar = compoundValue.charAt(nextIndex + 1);

        if ((nextChar != _PROPERTY_BOUNDARY) && (nextChar != '='))
          break;

        fixEscapes = true;
        nextIndex = compoundValue.indexOf(_PROPERTY_BOUNDARY,
                                          nextIndex + 2);
      }

      if (nextIndex < 0)
        nextIndex = endIndex;

      // Find the '=' dividing the key and value
      int equalsIndex = compoundValue.indexOf('=', index + 1);

      while ((equalsIndex > 0) &&
             (equalsIndex < nextIndex) &&
             (compoundValue.charAt(equalsIndex - 1) == _PROPERTY_BOUNDARY))
      {
        equalsIndex = compoundValue.indexOf('=', equalsIndex + 1);
      }

      if ((equalsIndex >= nextIndex) || (equalsIndex < 0))
        equalsIndex = nextIndex;

      String key = compoundValue.substring(index + 1, equalsIndex);

      String value;

      if (equalsIndex == nextIndex)
        value = "";
      else
        value = compoundValue.substring(equalsIndex + 1, nextIndex);

      if (fixEscapes)
      {
        key   = _decodeEscapes(key);
        value = _decodeEscapes(value);
      }

      key = _decodeChars(key);
      value = _decodeChars(value);
      // add the key/value String[] pair to the Map
      keyValues.put(key, new String[]{value});

      if (nextIndex >= endIndex)
        break;

      index = nextIndex;
    }
  }


  /**
   * Encodes an Iterator key value pairs as a single value appended to
   * the baseName, if any;
   */
  public static String encodeCompoundKeyValues(
    String           namePrefix,
    Iterator<Object> keyValues,
    int              keyValueSize
    )
  {

    return encodeCompoundKeyValues(namePrefix, keyValues, keyValueSize,
                                   null, null, 0);
  }

  /**
   * Encodes an Iterator key value pairs as a single value appended to
   * the baseName, if any;
   */
  public static String encodeCompoundKeyValues(
    String           namePrefix,
    Iterator<Object> keyValues,
    int              keyValueSize,
    String           exchangePrefix,
    Iterator<Object> keyExchanges,
    int              keyExchangesSize
    )
  {


    int prefixSize = (namePrefix != null)
                       ? namePrefix.length() * 2
                       : 0;

    int exchangePrefixSize = (exchangePrefix != null)
                       ? exchangePrefix.length()
                       : 0;

    int totalSize = prefixSize + keyValueSize +
                    exchangePrefixSize + keyExchangesSize;

    StringBuffer compoundValue = new StringBuffer(totalSize);

    // append base name
    if (prefixSize > 0)
    {
      compoundValue.append(namePrefix);
    }

    if ((keyExchanges != null) && keyExchanges.hasNext())
    {
      _encodeIterator(keyExchanges, compoundValue, exchangePrefix);
      _encodeIterator(keyValues, compoundValue, namePrefix);
    }
    else
      _encodeIterator(keyValues, compoundValue, null);

    if (compoundValue.length() > prefixSize)
    {
      return compoundValue.toString();
    }
    else
    {
      // we didn't have any key value pairs to append
      return null;
    }
  }



  /**
   * Encodes an Iterator key value pairs as a single value appended to
   * the baseName, if any;
   */
  public static String encodeCompoundKeyValues(
    Iterator<Object> keyValues,
    int              keyValueSize
    )
  {
    return encodeCompoundKeyValues(_COMPOUND_NAME_PREFIX, keyValues, keyValueSize);
  }

  /**
   * Encodes an Iterator key value pairs as a single value appended to
   * the baseName, if any.
   */
  public static String encodeCompoundKeyValues(
    Iterator<Object> keyValues,
    int              keyValueSize,
    Iterator<Object> keyExchanges,
    int              keyExchangesSize
    )
  {
    return encodeCompoundKeyValues(_COMPOUND_NAME_PREFIX,
                                   keyValues, keyValueSize,
                                   _EXCHANGE_PARAM_NAME_PREFIX,
                                   keyExchanges, keyExchangesSize);
  }


  /**
   * Returns the value of a request parameter as a <code>String</code>,
   * or <code>null</code> if the parameter does not exist. Request parameters
   * are extra information sent with the request.  For HTTP servlets,
   * parameters are contained in the query string or posted form data.
   *
   * <p>You should only use this method when you are sure the
   * parameter has only one value. If the parameter might have
   * more than one value, use {@link #getParameterValues}.
   *
   * <p>If you use this method with a multivalued
   * parameter, the value returned is equal to the first value
   * in the array returned by <code>getParameterValues</code>.
   *
   *
   * @param name   a <code>String</code> specifying the
   *      name of the parameter
   *
   * @return    a <code>String</code> representing the
   *      single value of the parameter
   *
   * @see     #getParameterValues
   *
   */
  @Override
  public String getParameter(
    String name
    )
  {
    Object param = _params.get(name);

    if (param instanceof String[])
    {
      return ((String[])param)[0];
    }
    else
    {
      return (String)param;
    }
  }

  /**
   *
   * Returns an <code>Iterator</code> of <code>String</code>
   * objects containing the names of the parameters contained
   * in this request. If the request has
   * no parameters, the method returns an
   * empty <code>Iterator</code>.
   *
   * @return    an <code>Iterator</code> of <code>String</code>
   *      objects, each <code>String</code> containing
   *       the name of a request parameter; or an
   *      empty <code>Iterator</code> if the
   *      request has no parameters
   *
   */
  @Override
  public Iterator<String> getParameterNames()
  {
    return _params.keySet().iterator();
  }

  /**
   * Returns an array of <code>String</code> objects containing
   * all of the values the given request parameter has, or
   * <code>null</code> if the parameter does not exist.
   *
   * <p>If the parameter has a single value, the array has a length
   * of 1.
   *
   * @param name  a <code>String</code> containing the name of
   *      the parameter whose value is requested
   *
   * @return    an array of <code>String</code> objects
   *      containing the parameter's values
   *
   * @see    #getParameter
   *
   */
  @Override
  public String[] getParameterValues(
    String name
    )
  {
    Object param = _params.get(name);

    if (param instanceof String[])
    {
      // Give them a copy of the string array
      String[] paramArr = (String[])param;

      if (_cloneArrays)
      {
        return paramArr.clone();
      }
      else
      {
        return paramArr;
      }
    }
    else
    {
      return new String[]{(String)param};
    }
  }


  private static Map<String, Object> _processParameters(
    Map<String, Object> requestParams,
    int        dictionarySize
    )
  {

    boolean hasCompoundName = requestParams.get(HAS_COMPOUND_NAME) != null;

    if (hasCompoundName)
    {
      Hashtable<String, Object> params = 
        new Hashtable<String, Object>((int)(dictionarySize * 1.5));
      
      HashMap<String, String> keyExchangeMap = new HashMap<String, String>();

      Iterator<String> paramNames = requestParams.keySet().iterator();

      if (paramNames != null)
      {
        while (paramNames.hasNext())
        {
          String currName = paramNames.next();

          boolean putValue = true;

          if (hasCompoundName)
          {
            if (HAS_COMPOUND_NAME.equals(currName))
            {
              putValue = false;
            }
            else
            {

              // check to see if this is a compund element
              if (currName.startsWith(_COMPOUND_NAME_PREFIX) &&
                  (currName.indexOf('=') != -1))
              {

                int fromIndex =
                          currName.indexOf(_EXCHANGE_PARAM_NAME_PREFIX);

                if ( fromIndex < 0 )
                  fromIndex = _COMPOUND_NAME_PREFIX_LENGTH;
                else
                {
                  // =-=gc
                  // There are parameter names that will need to be exchanged
                  // after decode. This is needed for cases like lovInput
                  // on a pda without javascript where you want the param
                  // name to be searchText, but every textInput needs a unique
                  // name, so you want to replace [UNIQUE_NAME]=foo with
                  // searchText=foo

                  int toIndex = 0;

                  // the url should be of the form
                  //_uixcn_uixXlov1=searchText$_uixcnevent=lovFilter$
                  // so figure out where the 2nd compound name prefix is
                  int endIndex = currName.indexOf(_COMPOUND_NAME_PREFIX,
                                                  fromIndex);

                  // start search after exchange name prefix
                  fromIndex = fromIndex  +
                                        _EXCHANGE_PARAM_NAME_PREFIX.length();

                  while(fromIndex < endIndex)
                  {
                    toIndex = currName.indexOf('=', fromIndex);
                    // get the old param name
                    String oldKey = currName.substring(fromIndex, toIndex);
                    fromIndex = toIndex + 1;
                    toIndex = currName.indexOf(_PROPERTY_BOUNDARY, fromIndex);
                    // get the new param name
                    String newKey = currName.substring(fromIndex, toIndex);
                    fromIndex = toIndex + 1;
                    // add old and new keys to map
                    keyExchangeMap.put(oldKey, newKey);
                  }

                  // get the string starting after second compound name
                  fromIndex = endIndex + _COMPOUND_NAME_PREFIX_LENGTH;
                }

                //
                // decode the parameter into the Map
                //
                decodeCompoundKeyValues(currName,
                                        fromIndex,
                                        params);


                // don't put the undecoded name/value pair in
                putValue = false;


              }
            }
          }

          if (putValue)
          {
            params.put(currName, requestParams.get(currName));
          }
        }
      }

      // Exchange param names if needed
      Iterator<String> oldKeys = keyExchangeMap.keySet().iterator();
      while (oldKeys.hasNext())
      {
        String oldKey = oldKeys.next();
        Object value = params.get(oldKey);

        if ( value != null )
        {
          // Currently not removing old key/value pair
          String newKey = keyExchangeMap.get(oldKey);
          params.put(newKey, value);
        }
      }

      return params;
    }
    else
    {
      return requestParams;
    }
  }

  private static void _encodeIterator(
    Iterator<Object>  keyValues,
    StringBuffer      compoundValue,
    String            prefix
    )
  {

    // true if we need a property separator
    boolean needsPropSeparator = false;

    if ((keyValues != null) && keyValues.hasNext())
    {
      try
      {
        // append base name
        if (prefix != null)
        {
          compoundValue.append(prefix);
        }

        do
        {

          Object key = keyValues.next();

          if (key != null)
          {

            // only use a prop seperator if writing something,
            // which should only happen if key is not null
            if (needsPropSeparator)
            {
              compoundValue.append(_PROPERTY_BOUNDARY);
              needsPropSeparator = false;
            }


            _encodeChars(compoundValue, key.toString());
            compoundValue.append('=');

            Object value = keyValues.next();

            if (value != null)
            {
              _encodeChars( compoundValue, value.toString());
            }

            needsPropSeparator = true;
          }
          else
          {
            // The key was null so skip this value
            keyValues.next();
          }


        } while (keyValues.hasNext());
        // we add a property boundary at the end for the cases where
        // an input type="image" is used and a .x or .y is appended to the
        // name by the browser.
        // This way we won't confuse the .x or .y added by the browser
        // with one that may be in a value
        compoundValue.append(_PROPERTY_BOUNDARY);
      }
      catch (NoSuchElementException e)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "KEYS_AND_VALUES_MUST_MATCH"));
      }
    }
  }


  /**
   * Cabo page-property escape-decoding
   * copied from org.apache.myfaces.trinidadinternal.uix22.servlet.url.DefaultPageEncoder
   */
  private static String _decodeEscapes(String string)
  {
    int length = string.length();
    StringBuffer buffer = new StringBuffer(length);
    for (int i = 0; i < length; i++)
    {
      char c = string.charAt(i);
      if (c == '$')
      {
        i++;
        if (i < length)
          c = string.charAt(i);
        else
          break;
      }

      buffer.append(c);
    }

    return buffer.toString();
  }


  // mostly copied from org.apache.myfaces.trinidadinternal.uix22.servlet.url.UrlEncoder
  static private String _encodeChars(
    StringBuffer buffer,
    String       s)
  {

    if ( s == null )
      return null;

    int length = s.length();
    for (int i = 0; i < length; i++)
    {
      char c = s.charAt(i);

      if ((c == '%') ||
               //(c == '?') ||
               //(c == '#') ||
               //(c == ';') ||
               //(c == ':') ||
               //(c == '&') ||
          (c == '$') ||
          (c == '=') ||
          (c == ' ') )
      {
        buffer.append('%');
        buffer.append(_toHex((c >> 4) & 0xF));
        buffer.append(_toHex(c & 0xF));
      }
      else
        buffer.append(c);
    }

    return buffer.toString();
  }

  // copied from org.apache.myfaces.trinidadinternal.uix22.servlet.url.UrlEncoder
  static private char _toHex(int fourBits)
  {
    if (fourBits < 10)
      return (char) ('0' + fourBits);
    else
      return (char) ('A' + (fourBits - 10));
  }


  static private String _decodeChars(
    String       s)
  {

    if ( s == null )
      return null;
    else if( s.indexOf( '%' ) < 0 )
      return s;

    StringBuffer buffer = new StringBuffer( s.length());

    int length = s.length();
    int i = 0;
    while( i < length)
    {
      char c = s.charAt(i);

      if ( (c == '%') )
      {
        int digit = Integer.parseInt( s.substring( i + 1, i+3 ), 16);
        buffer.append( (char)digit );
        i = i + 2;
      }
      else
        buffer.append(c);

      i++;
    }

    return buffer.toString();
  }



  // separator used to separate compund key value pairs in a compund value
  // copied from org.apache.myfaces.trinidadinternal.uix22.servlet.url.DefaultPageEncoder
  private static final char _PROPERTY_BOUNDARY = '$';

  private static final String _DOT_X = _PROPERTY_BOUNDARY + ".x";
  private static final String _DOT_Y = _PROPERTY_BOUNDARY + ".y";

  // Prefix used to indicate that a name is a compound name
  // that should be interpreted as key/value pairs instead of
  // as a name
  //
  private static final String _COMPOUND_NAME_PREFIX = "_uixcn";
  private static final String _EXCHANGE_PARAM_NAME_PREFIX = "_uixX";

  private static final int _COMPOUND_NAME_PREFIX_LENGTH =
                                                _COMPOUND_NAME_PREFIX.length();

  // Dictioanry of parameter keys and values
  private Map<String, Object> _params;

  // whether the String arrays returned need to be cloned
  private boolean _cloneArrays;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ServletRequestParameters.class);
}
