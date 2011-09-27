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

import java.beans.PropertyDescriptor;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * PropertyDef that uses introspection - specifically,
 * a bean PropertyDescriptor - to define its behavior.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/IntrospectionPropertyDef.java#0 $) $Date: 10-nov-2005.18:59:20 $
 */
public class IntrospectionPropertyDef extends BasePropertyDef
{
  /**
   * Creates an IntrospectionPropertyDef.
   * @param descriptor the PropertyDescriptor defining how
   *   the property can be set and retrieved.
   */
  public IntrospectionPropertyDef(PropertyDescriptor descriptor)
  {
    if (descriptor == null)
      throw new NullPointerException();

    _descriptor = descriptor;
  }

  /**
   * Returns the name of the property definition.
   */
  @Override
  public String getName()
  {
    return _descriptor.getName();
  }

  @Override
  public Class<?> getPropertyType()
  {
    return _descriptor.getPropertyType();
  }

  @Override
  public Object getValue(ParseContext context, Object bean)
  {
    Method read = _descriptor.getReadMethod();
    if (read == null)
    {
      return null;
    }

    try
    {
      return read.invoke(bean);
    }
    catch (IllegalAccessException iae)
    {
      _LOG.severe(iae);
    }
    catch (InvocationTargetException ite)
    {
      _LOG.severe(ite);
    }

    return null;
  }

  @Override
  public void setValue(ParseContext context, Object bean, Object value)
  {
    Method write = _descriptor.getWriteMethod();
    if (write == null)
      return;

    try
    {
      write.invoke(bean, new Object[]{value});
    }
    catch (IllegalAccessException iae)
    {
      _LOG.severe(iae);
    }
    catch (InvocationTargetException ite)
    {
      _LOG.severe(ite);
    }
  }

  private PropertyDescriptor _descriptor;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(IntrospectionPropertyDef.class);
}

