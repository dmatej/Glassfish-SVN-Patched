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

import java.lang.reflect.UndeclaredThrowableException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.share.expl.Coercions;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * A BoundValue implementation that wraps another and converts
 * its results to the specified java type.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/ConvertBoundValue.java#0 $) $Date: 10-nov-2005.18:56:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ConvertBoundValue implements BoundValue
{
  /**
   * Creates a ConvertBoundValue.
   * <p>
   * @param wrapped  the BoundValue to wrap
   * @param javaType the Java type to convert to
   */
  public ConvertBoundValue(
    BoundValue wrapped,
    String     javaType)
  {
    if (wrapped == null)
      throw new IllegalArgumentException();

    _wrapped = wrapped;
    _javaType = getClassName(javaType);
  }

  /**
   * Creates a ConvertBoundValue.
   * <p>
   * @param wrapped  the BoundValue to wrap
   * @param javaType the Java type to convert to
   */
  public ConvertBoundValue(
    BoundValue wrapped,
    Class<?>   javaType)
  {
    if (wrapped == null)
      throw new IllegalArgumentException();

    _wrapped = wrapped;
    _javaType = javaType.getName();
    _class    = javaType;
  }

  public Object getValue(UIXRenderingContext context)
  {
    Object value = _wrapped.getValue(context);

    try
    {
      return Coercions.coerce(value, _getTargetType());
    }
    catch (NumberFormatException nfe)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_CONVERT", new Object[]{value, _getTargetType().getName()});
    }
    catch (IllegalArgumentException e)
    {
      _LOG.severe(e);
    }

    return null;
  }

  /**
   * Gets the class name for a particular javaType. javaType is usually a
   * Class name. However, in some cases it can be a simple string like: 'int',
   * 'string', etc..  This method returns the proper Class name for those
   * simple strings.
   * @param javaType a Class name, or a simple string like 'string', 'int'
   * @return if javaType is a Class name, then that Class name is returned. If
   * javaType is a simple string, then the proper Class name for that string
   * is returned.
   */
  public static String getClassName(String javaType)
  {
    if ("byte".equals(javaType))
      return "java.lang.Byte";
    else if ("short".equals(javaType))
      return "java.lang.Short";
    else if ("int".equals(javaType))
      return "java.lang.Integer";
    else if ("long".equals(javaType))
      return "java.lang.Long";
    else if ("float".equals(javaType))
      return "java.lang.Float";
    else if ("double".equals(javaType))
      return "java.lang.Double";
    else if ("char".equals(javaType))
      return "java.lang.Character";
    else if ("boolean".equals(javaType))
      return "java.lang.Boolean";
    else if ("string".equals(javaType))
      return "java.lang.String";

    return javaType;
  }

  private Class<?> _getTargetType()
  {
    if (_class == null)
    {
      try
      {
        _class = ClassLoaderUtils.loadClass(_javaType);
      }
      catch (ClassNotFoundException e)
      {
        throw new UndeclaredThrowableException(e);
      }
    }

    return _class;
  }

  private BoundValue _wrapped;
  private String     _javaType;
  private Class<?>   _class;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ConvertBoundValue.class);
}
