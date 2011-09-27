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
package org.apache.myfaces.trinidadinternal.agent;

import java.util.HashMap;

public class CapabilityValue
{
  //AdfFacesAgent (the old Agent) uses Objects constants for 
  //capability values, and the code base uses identity comaprison
  //to check for a capability value.
 
  //With the new Agent, the values are in a file and are string objects.
  //So one way of making the identity comparison still work is to perform
  //an string.intern() on the values read from the file.
 
  //Also string values need to be retained, as need to support
  //clients accessing the Agent Api (RequestContext object constants 
  //are internal impl)
  
  //This is a poor man's quick substitute for string.intern().
  //Later, need to refine this properly.
  //Based on key, can know the value type object. Need metadata info on key
  public final static Object getCapabilityValue (CapabilityKey key,
                                                 Object value)
  {
    if (value instanceof String)
    {
      String stringValue = (String) value;
      if (stringValue.length() == 0)
        return null;
      if (Character.isDigit(stringValue.charAt(0)))
      {
        try
        {
          return Integer.valueOf(stringValue);
        }
        catch (NumberFormatException nfe)
        {
        }
      }
      
      if ("true".equals(value))
        return Boolean.TRUE;
      else if ("false".equals(value))
        return Boolean.FALSE;
    }
    
    return _getValue(value);
  }
  
  synchronized static private Object _getValue(Object value)
  {
    Object cValue =  _values.get(value);
    if (cValue != null)
      return cValue;

    _values.put(value, value);
    return value;
  }
  
  private static HashMap<Object, Object> _values = new HashMap<Object, Object>(32);
}
