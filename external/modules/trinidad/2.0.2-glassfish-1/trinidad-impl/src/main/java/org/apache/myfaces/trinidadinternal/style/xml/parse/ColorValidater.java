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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;

/**
 * PropertyValidater for color values
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/ColorValidater.java#0 $) $Date: 10-nov-2005.18:58:03 $
 */
class ColorValidater implements PropertyValidater
{
  public static ColorValidater getInstance()
  {
    if (_sInstance == null)
      _sInstance = new ColorValidater();

    return _sInstance;
  }

  /**
   * Tests whether the specified value is valid for the given property 
   * name.  If the property is not valid, an error message is returned.
   * Otherwise, null is returned to indicate that the value is valid.
   */
  public String validateValue(String name, String value)
  {
    if ((value != null) && (value.length() > 1))
    {
      if ((value.charAt(0) == '+') || (value.charAt(0) == '-'))
        value = value.substring(1);
    }

    try
    {
      CSSUtils.parseColor(value);
    }
    catch (PropertyParseException e)
    {
      return e.getMessage();
    }

    return null;
  }

  private ColorValidater() {}

  private static ColorValidater _sInstance;
}
