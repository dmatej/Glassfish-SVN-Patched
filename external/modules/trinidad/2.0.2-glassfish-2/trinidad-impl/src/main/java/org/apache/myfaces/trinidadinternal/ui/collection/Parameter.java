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
package org.apache.myfaces.trinidadinternal.ui.collection;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * A single parameter of an URL or a form.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/Parameter.java#0 $) $Date: 10-nov-2005.18:57:35 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class Parameter implements Cloneable
{
  // =-=ags This code was copied from PageURLBoundValue$Parameter.
  //        We should change PageURLBoundValue to use this Parameter
  //        class once it moves out of UIXDEV.

  /**
   * Create a Parameter.
   */
  public Parameter()
  {
  }


  /**
   * Sets the key of the parameter.
   */
  public void setKey(String key)
  {
    _key = key;
  }


  /**
   * Gets the key of the parameter.
   */
  public String getKey()
  {
    return _key;
  }

  /**
   * Returns the unresolved BoundValue (a la getRawAttributeValue).
   */
  public BoundValue getRawValue()
  {
    return _valueBinding;
  }

  /**
   * Sets the value of the parameter to a fixed string.
   */
  public void setValue(String value)
  {
    _value = value;
  }


  /**
   * Returns a fixed value of the parameter.
   */
  public String getValue()
  {
    return _value;
  }

  /**
   * Sets the value of the parameter as a dynamically determined
   * string.
   */
  public void setValueBinding(BoundValue valueBinding)
  {
    _valueBinding = valueBinding;
  }


  /**
   * Makes a clone of the parameter.
   */
  @Override
  public Object clone()
  {
    try
    {
      return super.clone();
    }
    catch (CloneNotSupportedException cnse)
    {
      return null;
    }
  }

  /**
   * Gets the value, resolving any data binding.
   */
  public String getValue(UIXRenderingContext context)
  {
    return _getString(context, _valueBinding, _value);
  }

  // Convert a BoundValue into a string
  static private String _getString(
    UIXRenderingContext context,
    BoundValue value,
    String     s)
  {
    if (value != null)
    {
      Object o = value.getValue(context);
      // Note that we _do not_ do defaulting behavior (that is,
      // fall back on "s").  That's because UIBeanDef does defaulting
      // automatically.
      // =-=ags Does this code from PageURLBoundValue still apply?
      if (o == null)
        return null;
      return o.toString();
    }

    return s;
  }

  private String     _key;
  private String     _value;
  private BoundValue _valueBinding;
}
