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
/*
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.awt.Color;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;

/**
 * Utilities for writing out JSON notation strings inside XML attribute or text.
 */
public final class JsonUtils
{

  static public void writeObject(
      StringBuilder out,
      Object        attrValue,
      boolean       escapeXML) throws IOException
    {
      if (attrValue == null)
      {
        out.append("null");
      }
      else
      {
        if (attrValue instanceof CharSequence)
        {
          writeString(out, ((CharSequence)attrValue), escapeXML);
          return;
        }

        Class<?> valueClass = attrValue.getClass();
        if (Boolean.class == valueClass)
        {
          writeBoolean(out, ((Boolean)attrValue).booleanValue());
          return;
        }
        else if (Integer.class == valueClass)
        {
          writeInt(out, ((Integer)attrValue).intValue());
          return;
        }
        else if (Character.class == valueClass)
        {
          writeChar(out, ((Character)attrValue).charValue(), escapeXML);
          return;
        }
        else if (valueClass.isArray())
        {
          out.append('[');

          int length = Array.getLength(attrValue);

          for (int i = 0; i < length; i++)
          {
            writeObject(out, Array.get(attrValue, i), escapeXML);

            if ( i != (length - 1))
              out.append(',');
          }

          out.append(']');
          return;
        }
        
        GenericConverterFactory converter =
          GenericConverterFactory.getCurrentInstance();
        if (Byte.class == valueClass)
        {
          writeByte(out, ((Byte)attrValue).byteValue());
        }
        else if (converter.isConvertible(attrValue, Date.class))
        {
          Date date = (Date) converter.convert(attrValue, Date.class);
          writeDate(out, date);
        }
        else if (Double.class == valueClass)
        {
          writeDouble(out, ((Double)attrValue).doubleValue());
        }
        else if (Float.class == valueClass)
        {
          writeFloat(out, ((Float)attrValue).floatValue());
        }
        else if (Long.class == valueClass)
        {
          writeLong(out, ((Long)attrValue).longValue());
        }
        else if (Short.class == valueClass)
        {
          writeShort(out, ((Short)attrValue).shortValue());
        }
        else if (converter.isConvertible(attrValue, Number.class))
        {
          Number num = (Number) converter.convert(attrValue, Number.class);
          writeDouble(out, num.doubleValue());
        }
        else if (Map.class.isAssignableFrom(valueClass))
        {
          writeMap(out, ((Map)attrValue), escapeXML);
        }
        else if (Collection.class.isAssignableFrom(valueClass))
        {
          writeCollection(out, ((Collection)attrValue), escapeXML);
        }
        else if (Color.class == valueClass)
        {
          writeColor(out, ((Color)attrValue));
        }
      }
    }

    /**
     * Encodes a boolean in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the boolean value
     */
    static public void writeBoolean(
      StringBuilder out,
      boolean      value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a byte in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the byte value
     */
    static public void writeByte(
      StringBuilder out,
      byte         value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a char in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the char value
     */
    static public void writeChar(
      StringBuilder out,
      char         value,
      boolean      escapeXML) throws IOException
    {
      out.append('\'');
      _escapeChar(out, value, escapeXML);
      out.append('\'');
    }

    /**
     * Encodes a Collection in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the List value
     */
    static public void writeCollection(
      StringBuilder out,
      Collection<?> value,
      boolean       escapeXML) throws IOException
    {
      if (value == null)
      {
        out.append("null");
      }
      else if (value.isEmpty())
      {
        out.append("[]");
      }
      else
      {
        out.append("[");
        for (Iterator<?> iter = value.iterator();
             iter.hasNext();)
        {
          Object item = iter.next();
          writeObject(out, item, escapeXML);
          if (iter.hasNext())
            out.append(',');
        }
        out.append(']');
      }
    }

    static public void writeDate(StringBuilder out, Date value) throws IOException
    {
      out.append("new Date("+value.getTime()+")");
    }

    static public void writeColor(StringBuilder out, Color color) throws IOException
    {
      if(color.getAlpha() == 0)
      {
        out.append("null");
      }
      else
      {
        out.append("new TrColor(");
        out.append(color.getRed());
        out.append(",");
        out.append(color.getGreen());
        out.append(",");
        out.append(color.getBlue());
        out.append(")");
      }
    }

    /**
     * Encodes a double in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the double value
     */
    static public void writeDouble(
      StringBuilder out,
      double       value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a float in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the float value
     */
    static public void writeFloat(
      StringBuilder out,
      float        value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a long in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the long value
     */
    static public void writeLong(
      StringBuilder out,
      long         value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a int in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the int value
     */
    static public void writeInt(
      StringBuilder out,
      int          value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a short in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the short value
     */
    static public void writeShort(
      StringBuilder out,
      short        value) throws IOException
    {
      out.append(value);
    }

    /**
     * Encodes a String in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param value         the String value
     */
    static public void writeString(
      StringBuilder out,
      CharSequence  value,
      boolean       escapeXML) throws IOException
    {
      if (value == null)
      {
        out.append("null");
      }
      else
      {
        // escape chars as necessary

        out.append('\'');

        for (int i=0; i < value.length(); i++)
        {
          char ch = value.charAt(i);
          _escapeChar(out, ch, escapeXML);
        }

        out.append('\'');
      }
    }

    /**
     * Encodes a Map in JavaScript Object Notation.
     *
     * @param out           the output buffer
     * @param map           the Map value
     */
    static public void writeMap(
      StringBuilder out,
      Map<?, ?>     map,
      boolean       escapeXML) throws IOException
    {
      if (map == null)
      {
        out.append("null");
      }
      else if (map.isEmpty())
      {
        out.append("{}");
      }
      else
      {
        out.append('{');
        
        boolean needComma = false;
        for (Iterator<?> iter = map.entrySet().iterator(); iter.hasNext();)
        {
          Map.Entry<?, ?> entry = (Map.Entry<?, ?>)iter.next();
          Object rawKey = entry.getKey();
          if(rawKey == null)
            throw new IllegalArgumentException(_LOG.getMessage(
              "JAVASCRIPT_NOT_SUPPORT_NULL_KEYS"));
          String key = rawKey.toString(); 
          Object value = entry.getValue();
          if (value == null)
            continue;
          if (needComma)
            out.append(',');
          else
            needComma = true;

          writeString(out, key, escapeXML);
          out.append(':');
          writeObject(out, value, escapeXML);
        }
        out.append('}');
      }
    }

  

  /**
   * prevent object creation
   *
   */
  private JsonUtils()
  {
  }
  
  /**
   * Encodes a char in JavaScript Object Notation.
   *
   * @param out           the output buffer
   * @param value         the char value
   */
  static private void _escapeChar(
    StringBuilder out,
    char         value,
    boolean      escapeXML)
  {
    switch (value)
    {
      // Escapes needed for XML
      case '&':
        if (escapeXML)
          out.append("&amp;");
        else
          out.append('&');
        break;
      case '<':
        if (escapeXML)
          out.append("&lt;");
        else
          out.append('<');
        break;
      case '>':
        if (escapeXML)
          out.append("&gt;");
        else
          out.append('>');
        break;

      // Double quote
      case '\"':
        if (escapeXML)
          out.append("\\&quot;");
        else
          out.append("\\\"");
        break;
      // Apostrophe
      case '\'':
        out.append("\\\'");
        break;
      // Backslash
      case '\\':
        out.append("\\\\");
        break;
      case '\b':
        out.append("\\b");
        break;
      case '\f':
        out.append("\\f");
        break;
      case '\n':
        out.append("\\n");
        break;
      case '\r':
        out.append("\\r");
        break;
      case '\t':
        out.append("\\t");
        break;
      default:
        if (value >= 32 && value < 128)
        {
          // no escaping necessary
          out.append(value);
        }
        else
        {
          String hex = Integer.toHexString(value);

          if (value > 0x00FF)
          {
            // use unicode escaping
            out.append("\\u");
            if (value < 0x1000)
              out.append('0');
            out.append(hex);
          }
          else
          {
            // use hex escaping
            out.append("\\x");
            if (value < 0x10)
              out.append('0');
            out.append(hex);
          }
        }
        break;
    }
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    JsonUtils.class);
}