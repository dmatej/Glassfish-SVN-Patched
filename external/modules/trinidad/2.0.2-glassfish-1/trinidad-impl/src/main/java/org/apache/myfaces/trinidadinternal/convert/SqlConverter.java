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

import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * converter for the java.sql package
 */
class SqlConverter extends GenericConverter
{
  public SqlConverter()
  {
  }

  @Override
  public Object convert(Object source, Class<?> targetType)
  {
    java.util.Date jDate = (java.util.Date) source;
    if (targetType.isAssignableFrom(Date.class))
    {
      return new Date(jDate.getTime());
    }
    if (targetType.isAssignableFrom(Time.class))
    {
      return new Time(jDate.getTime());
    }
    if (targetType.isAssignableFrom(Timestamp.class))
    {
      return new Timestamp(jDate.getTime());
    }
    throw new TypeConversionException(source, targetType);
  }

  @Override
  public List<Class<?>> getTargetTypes(Class<?> sourceType)
  {
    if (java.util.Date.class.isAssignableFrom(sourceType))
    {
      List<Class<?>> list = new ArrayList<Class<?>>(3);
      list.add(Date.class);
      list.add(Time.class);
      list.add(Timestamp.class);
      return list;
    }
    return Collections.emptyList();
  }
}
