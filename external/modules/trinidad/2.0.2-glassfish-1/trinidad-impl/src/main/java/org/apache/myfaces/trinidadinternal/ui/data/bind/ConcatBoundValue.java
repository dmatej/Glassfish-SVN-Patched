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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue implementation that will concatenate the string
 * results of a list of other BoundValues.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/ConcatBoundValue.java#0 $) $Date: 10-nov-2005.18:56:37 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ConcatBoundValue implements BoundValue
{
  /**
   * Create a BoundValue that concatenates the Strings of all
   * the contained BoundValues
   * <p>
   * @param values an array of BoundValues
   */
  public ConcatBoundValue(BoundValue[] values)
  {
    if (values == null)
      throw new IllegalArgumentException();

    _values = values;
  }
  
  
  /**
   * Called to retrieve a value based on the current rendering
   * context.
   * <p>
   * @param context the rendering context
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    BoundValue[] values = _values;
    int          length = values.length;

    // Append each string.  Optimize sizes zero-to-two most heavily
    switch (length)
    {
      case 0:
        return null;
      case 1:
        return _getValueSizeOne(context, values);
      case 2:
        return _getValueSizeTwo(context, values);
      default:
        break;
    }

    String[] evaluatedValues = new String[length];
    int      bufferLength    = 0;
    for (int i = 0; i < length; i++)
    {
      String s = _toString(_values[i].getValue(context));
      if (s != null)
      {
        evaluatedValues[i] = s;
        bufferLength       = bufferLength + s.length();
      }
    }

    StringBuffer buffer = new StringBuffer(bufferLength);
    for (int i = 0; i < length; i++)
    {
      String s = evaluatedValues[i];
      if (s != null)
        buffer.append(s);
    }

    return buffer.toString();
  }
  
  // Optimized version if there's only one element
  static private String _getValueSizeOne(
    UIXRenderingContext context,
    BoundValue[]     values)
  {
    return _toString(values[0].getValue(context));
  }


  // Optimized version if there's two elements
  static private String _getValueSizeTwo(
    UIXRenderingContext context,
    BoundValue[]     values)
  {
    String first = _toString(values[0].getValue(context));
    String second =_toString(values[1].getValue(context));
    
    if (first == null)
      return second;

    if (second == null)
      return first;

    StringBuffer buffer = new StringBuffer(first.length() + second.length());
    buffer.append(first);
    buffer.append(second);
    return buffer.toString();
  }

  static private String _toString(Object o)
  {
    if (o == null)
      return null;
    return o.toString();
  }

  private final BoundValue[] _values;
}
