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
package org.apache.myfaces.trinidadinternal.facelets;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.TagAttribute;

import javax.faces.view.facelets.TagAttributeException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.text.DateFormat;

import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;


import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

class DatePropertyTagRule
  extends MetaRule
{
  static DatePropertyTagRule Instance = new DatePropertyTagRule();
  
  private static class LiteralPropertyMetadata extends Metadata
  {
    public LiteralPropertyMetadata(Method method, TagAttribute attribute,
                                   boolean adjustToEnd)
    {
      _method = method;
      _attribute = attribute;
      _adjustToEnd = adjustToEnd;
    }
    
    public void applyMetadata(FaceletContext ctx, Object instance)
    {
      if (_time == null)
      {
        Date date = _coerceToDate(_attribute.getValue(), _adjustToEnd);
        _time = (date == null) ? _UNKNOWN_TIME : date.getTime();
      }
      try
      {
        // TRINIDAD-2034 - create a new instance of Date every time to avoid issues
        // with sharing mutable objects
        Object params[] = new Object[]{(_time.longValue() == _UNKNOWN_TIME) ? null : new Date(_time)};
        _method.invoke(instance, params);
      }
      catch (InvocationTargetException e)
      {
        throw new TagAttributeException(_attribute, e.getCause());
      }
      catch (Exception e)
      {
        throw new TagAttributeException(_attribute, e);
      }
    }
    
    private final Method       _method;
    private final TagAttribute _attribute;
    private final boolean      _adjustToEnd;
    private       Long         _time;
    private static final long  _UNKNOWN_TIME = -1;
  }
  
  public Metadata applyRule(String name, TagAttribute attribute,
                            MetadataTarget meta)
  {
    if (meta.getPropertyType(name) == _DATE_TYPE && attribute.isLiteral())
    {
      Method m = meta.getWriteMethod(name);
      
      // if the property is writable
      if (m != null)
      {
        return new LiteralPropertyMetadata(m, attribute, _MAX_VALUE.equals(name));
      }
    }
    
    return null;
  }
  
  static private Date _coerceToDate(String str, boolean adjustToEnd)
  {
    if (str == null)
      return null;
    
    try
    {
      Date date = _getDateFormat().parse(str);
      
      // This code is taken from UIXComponentELTag
      // We should probably not do the ajustment here,
      // but instead be smarter when days are compared.
      if (adjustToEnd)
      {
        Calendar c = Calendar.getInstance();
        TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
        if (tz != null)
          c.setTimeZone(tz);
        c.setTime(date);
        // Original value had 00:00:00 for hours,mins, seconds now maximize those
        // to get the latest time value for the date supplied.
        c.set (Calendar.HOUR_OF_DAY, 23);
        c.set (Calendar.MINUTE, 59);
        c.set (Calendar.SECOND, 59);
        c.set (Calendar.MILLISECOND, 999);
      }
      
      return date;
    }
    catch (ParseException pe)
    {
      _LOG.info("CANNOT_PARSE_VALUE_INTO_DATE", str);
      return null;
    }
  }
  
  // We rely strictly on ISO 8601 formats
  private static DateFormat _getDateFormat()
  {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
    if (tz != null)
      sdf.setTimeZone(tz);
    return sdf;
  }
  
  static private final Class<? extends Date> _DATE_TYPE = Date.class;
  static private final String _MAX_VALUE = "maxValue";
  
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DatePropertyTagRule.class);
}
