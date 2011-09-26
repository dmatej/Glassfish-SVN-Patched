/*
 * Copyright  2000-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * Private implementation of PropertyNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/PropertyNode.java#0 $) $Date: 10-nov-2005.18:58:09 $
 */
public class PropertyNode
{
  /**
     * Creates a PropertyNode with the specified name and value.
     * @param name name of the propertyNode. Examples are 'font-size', 'background-color'.
     * @param value value of the propertyNode. Examples are '12px', 'red', '0xeaeaea'
     * If name is null or the empty string, an IllegalArgumentException is thrown.
     */
    public PropertyNode(String name, String value)
    {

      if (name == null || "".equals(name))
        throw new IllegalArgumentException(_LOG.getMessage(
          "PROPERTYNODE_NAME_CANNOT_BE_NULL_OR_EMPTY", new Object[]{name, value}));

      // intern() name because many of the property names are the same,
      // like color, background-color, background-image, font-size, etc.
      // This will improve the memory used.
      _name = name.intern();
     
      if (value != null)
      {
        if (_INTERN_VALUES_FOR.contains(name))
        {
          value = value.intern();
        }
        else
        {
          String internedValue =  _INTERNED_VALUES.get(value);
         
          if (internedValue != null)
          {
            value = internedValue;
          }
        }
      }
     
      _value = value;
    }

  /**
   * Implementation of PropertyNode.getName().
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Implementation of PropertyNode.getValue().
   */
  public String getValue()
  {
    return _value;
  }

  @Override
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof PropertyNode))
      return false;

    // obj at this point must be an PropertyNode
    PropertyNode test = (PropertyNode)obj;

    return
      (_value == test._value || (_value != null && _value.equals(test._value))) &&
      (_name == test._name || (_name != null && _name.equals(test._name)));
  }

  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _name) ? 0 : _name.hashCode());
    hash = 37*hash + ((null == _value) ? 0 : _value.hashCode());
    return hash;
  }

  @Override
  public String toString()
  {
    return
      "[name="   + _name   + ", " +
      "value=" + _value + "]";
  }
  
  // map to the interned value for values that are common enough that we should intern them,
  // but we can't always intern based purely on the property name.  A good example are
  // proprties like vertical-align or cursor where there are many fixed values but the
  // option to use a percentage or URL is also there
  private static final Map<String, String> _INTERNED_VALUES = new HashMap<String, String>();

  static
  {
    // initialize values that we should share
    String[] internedValues = new String[]
    {
      "#000000",
      "#FFFFFF",
      "#ffffff",
      "0",
      "0%",
      "0px",
      "1",
      "10%",
      "100%",
      "1px",
      "1em",
      "2",
      "2px",
      "3px",
      "auto",
      "4px",
      "5px",
      "50%",
      "6px",
      "7px",
      "8px",
      "9px",
      "auto",
      "baseline",
      "black",
      "bottom",
      "center",
      "center center",
      "crosshair",
      "default",
      "e-resize",
      "gray",
      "help",
      "inherit",
      "left",
      "middle",
      "move",
      "n-resize",
      "ne-resize",
      "nw-resize",
      "none",
      "pointer",
      "progress",
      "right",
      "s-resize",
      "se-resize",
      "sub",
      "super",
      "sw-resize",
      "text",
      "text-bottom",
      "text-top",
      "top",
      "transparent",
      "w-resize",
      "wait",
      "white"
    };
   
    for (int i = 0; i < internedValues.length; i++)
    {
      String internedValue = internedValues[i];
     
      _INTERNED_VALUES.put(internedValue, internedValue);
    }
  }
 
  // property names that we should always intern the values for because they are either
  // enumerations or are repeated often (font-family)
  private static final Set<String> _INTERN_VALUES_FOR = new HashSet<String>(
    Arrays.asList("-moz-box-sizing",
                  "background-repeat",
                  "border-style",
                  "display",
                  "float",
                  "font-family",
                  "font-style",
                  "font-weight",
                  "list-style-position",
                  "list-style-type",
                  "overflow",
                  "overflow-x",
                  "overflow-y",
                  "position",
                  "text-align",
                  "text-decoration",
                  "transparent",
                  "visibility",
                  "white-space"));

  private final String _name;
  private final String _value;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    PropertyNode.class);
}
