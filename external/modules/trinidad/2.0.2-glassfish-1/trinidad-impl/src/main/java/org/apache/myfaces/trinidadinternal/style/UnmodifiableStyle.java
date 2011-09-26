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
package org.apache.myfaces.trinidadinternal.style;

import java.util.Map;
import java.util.Collections;

import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidad.style.Style;

/**
 * Style implementation for CSS properties. Once created, you cannot mutate since there is no setProperties method.
 * The toInlineString prints the properties out in CSS format, e.g., color: blue; font-size: 12px;
 * To save memory, as the constructor parameter
 * use an ArrayMap instead of a HashMap or ConcurrentHashMap if you can.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/CSSStyle.java#0 $) $Date: 10-nov-2005.18:57:55 $
 */
public class UnmodifiableStyle extends Style
{
  /**
   * Creates a Style object with the specified properties. There is no no-argument constructor
   * because you must pass in all your properties when you create your object.
   *
   * @param properties The properties of this style, like  background-color, blue.  The
   *   values must be Strings. 
   * @throws NullPointerException if properties is null
   */
  public UnmodifiableStyle(Map<String, String> properties)
  {
    if (properties == null)
      throw new NullPointerException("properties must be non-null");

     _propertiesMap = Collections.unmodifiableMap(properties);
  }
     
  @Override
  public Map<String, String> getProperties()
  {
    return _propertiesMap;
  }
  
  /**
   * Converts the style to a String suitable for use as an inline style
   * attribute value.
   */
  @Override
  public String toInlineString()
  {
    String inline = _inline;

    if (inline != null)
      return inline;
    
    Map<String, String> properties = getProperties();
    StringBuffer buffer = new StringBuffer(_DEFAULT_BUFFER_SIZE);
    boolean first = true;   
    
    for (Map.Entry<String, String> entrySet : properties.entrySet())
    {

      if (first)
        first = false;
      else
        buffer.append(";");
      String name = entrySet.getKey();
      String value = entrySet.getValue();
      buffer.append(name);
      buffer.append(":");
      buffer.append(value);
    }

    inline = buffer.toString();
    
    _inline = inline;

    return inline;
  }

  @Override
  public String toString()
  {
    return "UnmodifiableStyle[css=" + toInlineString() + "]"; 
  }
  

  @Override
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof UnmodifiableStyle))
      return false;

    UnmodifiableStyle test = (UnmodifiableStyle)obj;
    return this._propertiesMap.equals(test._propertiesMap);
  }

  @Override
  public int hashCode()
  {
    return _propertiesMap.hashCode();
  }

  // The cached inline String value
  // Marking it volatile guarantees that a read of _inline 
  // always returns the most recent write by any thread.
  transient volatile private String _inline;
  final private Map<String, String> _propertiesMap;

  // Default length for inline string buffer
  private static final int _DEFAULT_BUFFER_SIZE = 100;
}
