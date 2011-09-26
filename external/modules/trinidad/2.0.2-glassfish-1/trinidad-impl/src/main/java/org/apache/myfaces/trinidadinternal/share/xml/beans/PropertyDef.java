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
package org.apache.myfaces.trinidadinternal.share.xml.beans;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * Class for defining a bean property to the parser engine.
 * <p>
 * =-=AEW Should more of these take a ParseContext?  Is
 *   there a way we can get around needing the ParseContext at all?
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/PropertyDef.java#0 $) $Date: 10-nov-2005.18:59:21 $
 */
public abstract class PropertyDef
{
  /**
   * Creates a PropertyDef.
   */
  public PropertyDef()
  {
  }

  abstract public String getName();

  /**
   * Retrieves the value of this property from the bean.
   * @return the value.  Will return null if this property can only
   * be set.
   */
  abstract public Object getValue(ParseContext context, Object bean);


  /**
   * Stores a value of this property on the bean.
   */
  abstract public void setValue(ParseContext context, Object bean, Object value);


  /**
   * Returns the property type to which all non-null values of this property
   * must be assignable.
   *
   * @return  the property type
   */
  abstract public Class<?> getPropertyType();

  /**
   * Parses a string into the correct Java type for this
   * class.  The returned object must be an instance of
   * the class returned by getPropertyType().
   *
   * @param context the current parsing context
   * @param text  the text of the property
   * 
   * @throws IllegalArgumentException if the text cannot
   *   be parsed.
   */
  abstract public Object parseText(
   ParseContext context,
   String text) throws IllegalArgumentException;

  /**
   * Parses a string into the correct Java type for this
   * class.  The returned object must be an instance of
   * the class returned by getPropertyType().
   *
   * @param context  the current parsing context
   * @param attrURI  the namespace of the attribute
   * @param attrText the text of the attribute
   * 
   * @throws IllegalArgumentException if the text cannot
   *   be parsed.
   */
  public Object parseText(
   ParseContext context,
   String attrURI,
   String attrText) throws IllegalArgumentException
  {
    return parseText(context, attrText);
  }
}
