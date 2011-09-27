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
package org.apache.myfaces.trinidad.convert;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.context.MockRequestContext;
import org.apache.myfaces.trinidad.convert.DateTimeConverter;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockFacesContext;
import org.jmock.Mock;

public class TrinidadDateTimeConverterTest extends DateTimeConverterTestCase
{
  public TrinidadDateTimeConverterTest(String name)
  {
    super(name);
  }
  
  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    _mafct = new MockRequestContext();
    _mafct.setTwoDigitYearStart(1950);
    _mafct.setTimeZone(DEFAULT_TIME_ZONE);
  }

  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
    _mafct.release();
    _mafct = null;
  }
  
  public static Test suite()
  {
    return new TestSuite(TrinidadDateTimeConverterTest.class);
  }

  public void testConveniencePatterns()
  {
    DateTimeConverter dtConv   = new DateTimeConverter();
    dtConv.setLocale(Locale.US);

    //this is what SimpleInputDateRenderer does
    if(dtConv.getTimeZone() == null)
    {
      TimeZone tz = null;
      tz = _mafct.getTimeZone();
      if(tz == null)
        tz = TimeZone.getDefault();
      dtConv.setTimeZone(tz);
    }

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    String[] inputValue = {"15/2/2002", "January 4, 2004", "4-JAnuARY-2004", "JANUARY/4/2007", "Jan 4, 2004",
        "01/4/2004", "01-4-2004", "01.4.2004", "1/4/2004", "1-4-2004", "1.4.2004", "Jan/4/2004", "Jan-4-2004",
        "Jan.4.2004", "04-jan-2004", "4-jan-04", "4-jan-2004", "04-jAn-04", "04-JAN-04", "04-JAN-2004",
        "4-JAN-04", "4-JAN-2004", "January 4, 2004", "Jan 4, 2004"};

    for(int i = 0; i < inputValue.length; i++)
    {
      dtConv.getAsObject(facesContext, component, inputValue[i]);
    }
  }

  public void testFormatedPatterns()
  {
    DateTimeConverter dtConv   = new DateTimeConverter();
    dtConv.setLocale(Locale.US);

    //this is what SimpleInputDateRenderer does
    if(dtConv.getTimeZone() == null)
    {
      TimeZone tz = null;
      tz = _mafct.getTimeZone();
      if(tz == null)
        tz = TimeZone.getDefault();
      dtConv.setTimeZone(tz);
    }

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    String[] inputValue = {"15/2/2002", "January 4, 2004", "4-JAnuARY-2004", "JANUARY/4/2007"};
    String[] formatedStrings = {"2/15/2002", "1/4/2004", "1/4/2004", "1/4/2007"};

    Date convertedDate = null;
    String returnedString = null;
    for(int i = 0; i < inputValue.length; i++)
    {
      convertedDate = (Date) dtConv.getAsObject(facesContext, component, inputValue[i]);
      returnedString = dtConv.getAsString(facesContext, component, convertedDate);
      assertEquals(returnedString, formatedStrings[i]);
    }
  }

  /**
   * @todo move this to the parent class once JSF fixes the bug
   */
  public void testEarlyExits()
  {
    checkNullComponent();
    checkNullContext();
  }

  public void testGermanDate()
  {
    DateTimeConverter dtConv   = new DateTimeConverter();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    String inputValue          = "30.06.09 12:11 Uhr ";

    dtConv.setType("both");
    dtConv.setTimeStyle("full");
    dtConv.setLocale(Locale.GERMAN);
    dtConv.setTimeZone(TimeZone.getTimeZone ("America/New_York"));
    dtConv.setPattern("dd.MM.yy HH:mm' Uhr '");

    Date dt = (Date) dtConv.getAsObject(facesContext, component, inputValue);
    assertNotNull(dt);
    mock.verify();
  }

  public void testShortishForDatePatern()
  {
    GregorianCalendar gcal = new GregorianCalendar();
    gcal.set(2999,Calendar.JUNE,4,0,0,0);

    gcal.setTimeZone(DEFAULT_TIME_ZONE);
    // Make use of this date for testing.
    Date date = gcal.getTime();

    DateTimeConverter dtConv   = new DateTimeConverter();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    String inputValue          = "6/4/2999";

    dtConv.setDateStyle("shortish");
    dtConv.setLocale(Locale.ENGLISH);

    Date dt = (Date) dtConv.getAsObject(facesContext, component, inputValue);
    assertEquals(true, isEqual(date, dt));

    String exptectedStr = dtConv.getAsString(facesContext, component, dt);
    assertEquals(inputValue, exptectedStr);
    mock.verify();
  }

  public void testShortishDateStyle()
  {
    doTestStyleValidity(_DATE_STYLE, new String[]{"shortish"});
  }

  public void testSecondaryPattern()
  {
    // Get as object should work fine - while getAsString is expected to fail
    GregorianCalendar gcal = new GregorianCalendar();
    gcal.set(1600,Calendar.JUNE,4,0,0,0);

    gcal.setTimeZone(DEFAULT_TIME_ZONE);
    // Make use of this date for testing.
    Date date = gcal.getTime();

    DateTimeConverter dtConv   = new DateTimeConverter();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    String inputValue          = "6/4/1600";
    String secondaryPattern    = "MM/d/yyyy";

    dtConv.setLocale(Locale.US);
    dtConv.setDateStyle("Let us unset it ");
    dtConv.setType("Let us un set it");
    dtConv.setSecondaryPattern(secondaryPattern);
    // This should work fine
    Date dt = (Date) dtConv.getAsObject(facesContext, component, inputValue);
    assertEquals(true, isEqual(date, dt));

    try
    {
      dtConv.getAsString(facesContext, component, dt);
      fail("Use of secondary pattern in the above fashion is expected to fail here");
    }
    catch (RuntimeException ce)
    {
      // Just proceed . This is the expected state
    }

    dtConv.setDateStyle("shortish");
    dtConv.setType("date");

    // now we set date and type so this is expected to work fine.

    String expectedOut = dtConv.getAsString(facesContext, component, date);
    assertEquals(inputValue, expectedOut);

    mock.verify();
  }

  public void testLeniencyOnPrimaryPattern()
  {
    String primaryPattern = "MMM/d/yyyy";
    String secondaryPattern = null;
    dotestLeniencyOnPattern(primaryPattern, secondaryPattern);
  }

  public void testLeniencyOnSecondaryPattern()
  {
    String primaryPattern = null;
    String secondaryPattern = "MMM/d/yyyy";
    dotestLeniencyOnPattern(primaryPattern, secondaryPattern);
  }

  protected void dotestLeniencyOnPattern(
    String primaryPattern,
    String secondaryPatttern
    )
  {
    // inputs such as 6/7/2004 is also valid. Avoiding it since - equality
    // of the output is also compared.

    // Each of these inputs ends up causing the MessageFactory to grab the
    // label from the component a different number of times, so the number of
    // iterations to set the component up for is difficult to figure. The
    // numbers after the input string are the number of iterations of
    // getAttribute that we need to set up for.
    String[] validInputs =
      {
        "Jun/4/2004" /* 0 */, "Jun-4-2004" /* 10 */, "Jun.4.2004" /* 8 */,
        "06/4/2004"  /* 4 */, "06-04-2004" /* 12 */, "06.04.2004" /* 2 */,
        "6/4/2004"   /* 4 */, "6-4-2004"   /* 12 */, "6.4.2004"   /* 2 */
      };

    int iterations = (0 + 10 + 8 + 4 + 12 + 2 + 4 + 12 + 2);
    GregorianCalendar cal = new GregorianCalendar(2004, Calendar.JUNE, 4);
    cal.setTimeZone(DEFAULT_TIME_ZONE);
    Date dt = cal.getTime();
    Mock mock = buildMockUIComponent(iterations);
    UIComponent component = (UIComponent) mock.proxy();
    
    for (int i = 0; i < validInputs.length; i++)
    {
      DateTimeConverter
        dtConv = (DateTimeConverter) getDateTimeConverter();
      dtConv.setLocale(Locale.ENGLISH);
      dtConv.setPattern(primaryPattern);
      dtConv.setSecondaryPattern(secondaryPatttern);
      dtConv.setTimeZone(DEFAULT_TIME_ZONE);
      dtConv.setType("INVALID"); // make this type invalid

      Date convDate = (Date) dtConv.getAsObject(facesContext, component,
                                                validInputs[i]);
      assertEquals(convDate, dt);
      mock.verify();
    }
  }

  public void testCompareDateTimeConverter()
  {
    Object[][] data = _getDataForPatterns();

    for (int i = 0; i < data.length ; i++)
    {
      DateTimeConverter dtConv = new DateTimeConverter();
      dtConv.setPattern((String)data[i][0]);
      dtConv.setLocale((Locale)data[i][2]);
      dtConv.setTimeZone((TimeZone)data[i][3]);
      String inputValue = (String)data[i][1];

      javax.faces.convert.DateTimeConverter fdtConv
        = new javax.faces.convert.DateTimeConverter();
      fdtConv.setPattern((String)data[i][0]);
      fdtConv.setLocale((Locale)data[i][2]);
      fdtConv.setTimeZone((TimeZone)data[i][3]);

      Mock mock = buildMockUIComponent();
      UIComponent component = (UIComponent) mock.proxy();
      Date dtConvDate  = (Date)dtConv.getAsObject(facesContext, component, inputValue);
      Date fdtConvDate = (Date)fdtConv.getAsObject(facesContext, component, inputValue);
      //      assertEquals(dtConvDate, fdtConvDate);

      dtConv.getAsString(facesContext, component, dtConvDate);
      fdtConv.getAsString(facesContext, component, fdtConvDate);
      //      assertEquals(dtConvPattern, fdtConvPattern);
    }
  }

  @Override
  protected javax.faces.convert.DateTimeConverter getDateTimeConverter()
  {
    return new DateTimeConverter();
  }

  @Override
  protected void setSecondaryPattern(
    javax.faces.convert.DateTimeConverter converter,
    String secondaryPattern
    )
  {
    ((DateTimeConverter)converter).setSecondaryPattern(secondaryPattern);
  }

  @Override
  protected void doTestStateHolderSaveRestore(
    Converter conv1,
    Converter conv2,
    MockFacesContext context,
    MockUIComponentWrapper wrapper
    )
  {
    super.doTestStateHolderSaveRestore(conv1, conv2, context, wrapper);
  }

  public void testCustomMessageIsSet()
  {
    //default is shortish - M/d/yyyy
    //default time is short hh:m A.M/P.M
    //default both
    //let us choose pattern as M/d/yyyy
    String[] failingValues = {"15/15/2002", "02;30 A.M,", "15/15/2002 22:22 A*M.", "M/d/yyyy"};
    String[] types         = {"date",   "time",  "both", "pattern"};
    String[] customMessage = {"date",   "time",  "both", "pattern"};

    for (int i = 0; i < failingValues.length ; i++)
    {
      Mock mock = buildMockUIComponent(3 * 4);
      UIComponent component = (UIComponent) mock.proxy();

      org.apache.myfaces.trinidad.convert.DateTimeConverter converter =
        new org.apache.myfaces.trinidad.convert.DateTimeConverter();

      for (int j = 0; j < 3; j++)
      {
        for (int k = 0; k < 4; k++)
          facesContext.getViewRoot().setLocale(Locale.US);
      }

      try
      {
        // Trinidad Converter is not lenient.
        converter.setMessageDetailConvertDate(customMessage[0]);
        converter.setMessageDetailConvertTime(customMessage[1]);
        converter.setMessageDetailConvertBoth(customMessage[2]);
        // pattern and date type is driven using the same message.


        if ("pattern".equals(types[i]))
        {
          converter.setPattern("M/d/yyyy");
          // There is no specific messaging scheme for pattern. So use the
          // dateMessageDetail itself for this.
          converter.setMessageDetailConvertDate(customMessage[3]);
        }
        else
        {
          converter.setType(types[i]);
        }

        converter.getAsObject(facesContext, component, failingValues[i]);
        fail("Expected converter exception");
      }
      catch (ConverterException ce)
      {
        // We expected a exception to occur
        String msg = ce.getFacesMessage().getDetail();
        assertEquals(msg, customMessage[i]);
      }
    }
  }

  private Object[][] _getDataForPatterns()
  {
    // pattern, inputvalue,locale,timezone
    Object[][] data =
    {
      {"yyyy.MM.dd G 'at' HH:mm:ss z", "2001.07.04 AD at 12:08:56 PDT", Locale.US, null },

      {"EEE, MMM d, ''yy","Wed, Jul 4, '01", Locale.ENGLISH, getTzone("GMT")},

      {"h:mm a","12:08 PM", Locale.GERMAN, getTzone("GMT+1")},

      {"hh 'o''clock' a, zzzz","12 o'clock PM, Pacific Standard Time", Locale.CANADA, getTzone("GMT-8")},

      {"K:mm a, z","0:08 PM, PST", Locale.US, getTzone("PST")},

      {"yyyyy.MMMMM.dd GGG hh:mm aaa","02001.July.04 AD 12:08 PM", Locale.US, null},
      {"EEE, d MMM yyyy HH:mm:ss Z","Wed, 4 Jul 2001 12:08:56 GMT",Locale.US, getTzone("GMT")},
      {"yyMMddHHmmss", "010704120856", Locale.ENGLISH, null, null},

    };
    return data;
  }

  private MockRequestContext _mafct;

  private static final TimeZone DEFAULT_TIME_ZONE = TimeZone.getDefault();
}
