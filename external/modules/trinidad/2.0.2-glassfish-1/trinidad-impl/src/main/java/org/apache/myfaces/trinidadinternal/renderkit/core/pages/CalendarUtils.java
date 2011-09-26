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
package org.apache.myfaces.trinidadinternal.renderkit.core.pages;

import java.util.Calendar;
import java.util.Date;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.input.CoreChooseDate;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;

/**
 * Private utility methods shared by CalendarDialogJSP/InlineCalendarJSP.
 * 
 * Code taken from CalendarDialogJSP.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/CalendarUtils.java#0 $) $Date: 10-nov-2005.19:03:32 $
 */
class CalendarUtils
{
  @SuppressWarnings("unchecked")
  public static CoreChooseDate createChooseDate(FacesContext context)
  {
    final String id = CalendarDialogJSP.__getParam(context,
                                                   XhtmlConstants.SOURCE_PARAM);
    
    CoreChooseDate ccd = new CoreChooseDate()
    {
      @Override
      public String getClientId(FacesContext context)
      {
        return id;
      }
    };

    String minTimeString = CalendarDialogJSP.__getParam(context,
                                             XhtmlConstants.MIN_VALUE_PARAM);
    if (minTimeString != null)
    {
      ccd.setMinValue(parseTime(minTimeString));
    }

    String maxTimeString = CalendarDialogJSP.__getParam(context,
                                             XhtmlConstants.MAX_VALUE_PARAM);
    if (maxTimeString != null)
    {
      ccd.setMaxValue(parseTime(maxTimeString));
    }

    String selectedTimeString = CalendarDialogJSP.__getParam(context,
                                             XhtmlConstants.VALUE_PARAM);
    if (selectedTimeString != null)
    {
      ccd.getAttributes().put("value",
                              parseTime(selectedTimeString));
    }

    String displayTimeString = CalendarDialogJSP.__getParam(context,
                                                            "scrolledValue");
    if (displayTimeString != null)
    {
      ccd.getAttributes().put("scrolledValue",
                              parseTime(displayTimeString));
    }

    return ccd;
  }

  // Parse a time string into a Long.  This should be a simple
  // call to Long.valueOf() - but for Mozilla bug 140852
  static private Date parseTime(String timeString)
  {
    if (timeString == null || timeString.equals(""))
      return null;

    // The aforementioned Mozilla bug leaves colons
    // in strings ostensibly representing numbers. Treat these
    // colons as if they represent "10" - replace the colon
    // with a zero, and increment the previous number.
    try
    {
      int colonIndex = timeString.indexOf(':');
      if (colonIndex > 0)
      {
        long start = Long.parseLong(timeString.substring(0, colonIndex));
        timeString = Long.toString(start + 1) + "0" +
        timeString.substring(colonIndex + 1);
      }
      
      return new Date(_adjustTimeZone(Long.parseLong(timeString)));
    }
    catch (NumberFormatException nfe)
    {
      return null;
    }
  }
  
  /**
   * Adjust the specified dateValue in milliseconds, which is in 
   * AdfRenderingContext's LocaleContext's timeZone to the server timeZone.
   */
  @SuppressWarnings("cast")
  private static long _adjustTimeZone(long dateValueInMs)
  {
    // Bug 4613506
    // adjust the date from AdfRenderingContext's LocaleContext's  
    // timeZone to server timeZone.
    Calendar clientCal = 
             Calendar.getInstance(RenderingContext.getCurrentInstance().
                                  getLocaleContext().getTimeZone());
    clientCal.setTimeInMillis(dateValueInMs);
    long tzOffset = clientCal.get(Calendar.ZONE_OFFSET) + 
                    clientCal.get(Calendar.DST_OFFSET);
    
    Calendar serverCal = Calendar.getInstance();
    serverCal.setTimeInMillis(dateValueInMs);

    tzOffset -= serverCal.get(Calendar.ZONE_OFFSET) + 
                serverCal.get(Calendar.DST_OFFSET);
    
    // make sure that adjusting to correct timeZone doesn't take the 
    // long value out of the range. Calendar too doesn't handle this 
    // properly ie. MIN_VALUE < (dateValueInMs + tzOffset) < MAX_VALUE. 
    // this is possible since we use Long.MAX_VALUE as the default 
    // maximum date (in ChooseDateRenderer).
    if (tzOffset < 0)
    {
      // Cast to (float) has a purpose
      tzOffset = (long)Math.max((float)tzOffset, 
                                (float)Long.MIN_VALUE - (float)dateValueInMs);
    }
    else
    {
      // Cast to (float) has a purpose
      tzOffset = (long)Math.min((float)tzOffset, 
                                (float)Long.MAX_VALUE - (float)dateValueInMs);
    }
    
    return dateValueInMs + tzOffset;
  }

  private CalendarUtils() {}
}
