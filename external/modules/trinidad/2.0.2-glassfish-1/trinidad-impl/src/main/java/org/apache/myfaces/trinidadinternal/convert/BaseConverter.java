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
package org.apache.myfaces.trinidadinternal.convert;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

class BaseConverter extends GenericConverter
{
  public BaseConverter()
  {
  }

  @Override
  public Object convert(Object source, Class<?> targetType)
  {
    if (source instanceof Calendar)
    {
      Calendar cal = (Calendar) source;
      return cal.getTime();
    }
    if (source instanceof Date)
    {
      Date date = (Date) source;
      Calendar cal = Calendar.getInstance();
      cal.setTime(date);
      return cal;
    }
    // Source must be a Number
    Number num = (Number) source;
    
    // identity-equality is used since these are all final classes:
    if ((targetType == Integer.class) || (targetType == Integer.TYPE))
      return Integer.valueOf(num.intValue());
    if ((targetType == Byte.class) || (targetType == Byte.TYPE))
      return Byte.valueOf(num.byteValue());
    if ((targetType == Double.class) || (targetType == Double.TYPE))
      return Double.valueOf(num.doubleValue());
    if ((targetType == Float.class) || (targetType == Float.TYPE))
      return Float.valueOf(num.floatValue());
    if ((targetType == Long.class) || (targetType == Long.TYPE))
      return Long.valueOf(num.longValue());
    if ((targetType == Short.class) || (targetType == Short.TYPE))
      return Short.valueOf(num.shortValue());
    if (targetType == BigDecimal.class)
      return new BigDecimal(num.doubleValue());
    
    
    throw new IllegalArgumentException(_LOG.getMessage(
      "UNSUPPORTED_CONVERSION", new Object[]{source.getClass(), targetType}));
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<Class<?>> getTargetTypes(Class<?> sourceType)
  {
    ArrayList<Class<?>> list = new ArrayList<Class<?>>(1);
    if (Date.class.isAssignableFrom(sourceType))
    {
      list.add(Calendar.class);
    }
    else if (Calendar.class.isAssignableFrom(sourceType))
    {
      list.add(Date.class);
    }
    else if (Number.class.isAssignableFrom(sourceType))
    {
      list.ensureCapacity(13);
      list.add(Byte.class);
      list.add(Double.class);
      list.add(Float.class);
      list.add(Integer.class);
      list.add(Long.class);
      list.add(Short.class);
      list.add(BigDecimal.class);      
      list.add(Byte.TYPE);
      list.add(Double.TYPE);
      list.add(Float.TYPE); // bug 4891181
      list.add(Integer.TYPE);
      list.add(Long.TYPE);
      list.add(Short.TYPE);
    }
    
    return list;
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    BaseConverter.class);
}
