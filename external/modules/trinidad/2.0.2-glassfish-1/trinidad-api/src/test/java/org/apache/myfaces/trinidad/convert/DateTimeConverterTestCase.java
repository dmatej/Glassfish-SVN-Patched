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
import java.util.HashMap;
import java.util.Locale;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.convert.ConverterTestCase;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.jmock.Mock;

/**
 * Test DateTimeConverter
 * @version $Name: $ ($version: $) $Date: 16-aug-2005.15:12:23 $
 */
public abstract class DateTimeConverterTestCase extends ConverterTestCase
{
  public DateTimeConverterTestCase(String testName)
  {
    super(testName);
  }
  
  /**
   * @todo once all test case work fine - we can move all in to one method
   */
  public void testCommonScenarios()
  {
    // FILED BUG ON JSF ID : 358594
    //checkNullComponent(); // FAILING - early checking not done
    //checkNullContext();   // FAILING - early checking not done

    // Failing since i have set locale to be null
    // look for it in the faces context using getCurrentInstance()
    // FAILING - same locale problem. :-( - temp fix for now
    checkStateHolderSaveRestore();

    checkEquals(); // FAILING - same locale problem. :-( - temp fix for now
    checkGetAsObject();
    checkConversionOnLongPatternWithTimeZone();
    checkDatesForPatternBasedChecks();
    checkDateStyleValidity();
    checkTimeStyleValidity();
    doTestBlankValue(getDateTimeConverter());
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ConverterException  when test fails
   */
  protected void checkNullInputValue() throws ConverterException
  {
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    javax.faces.convert.DateTimeConverter converter  = getDateTimeConverter();

    doTestNull(facesContext, wrapper, converter);
  }

  /**
   * Test when context is set to null
   */
  protected void checkNullContext()
  {
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    javax.faces.convert.DateTimeConverter converter = getDateTimeConverter();

    doTestNullContext(wrapper, converter);
  }

  protected void checkNullComponent()
  {
    javax.faces.convert.DateTimeConverter converter  = getDateTimeConverter();

    doTestNullComponent(facesContext, converter);
  }

  /**
   * Test for equality of converters
   */
  protected void checkEquals()
  {

    javax.faces.convert.DateTimeConverter converter      = getDateTimeConverter();
    javax.faces.convert.DateTimeConverter otherConverter = getDateTimeConverter();

    /**
     * @todo fix this up - Do not delete
     */
    // COMMENTING OF SINCE FACES CONVERTER LOOKS IN TO THE VIEW ROOT FOR
    // LOCALE WHEN THE LOCALE IS NULL IN ITS getLocale() method.
    // doTestEquals(converter, otherConverter, true);


    for (int i = 0; i < _DATE_STYLES.length; i++)
    {
      converter.setDateStyle(_DATE_STYLES[i]);
      otherConverter.setDateStyle(_DATE_STYLES[i]);

      /**
       * @todo fix this up - Do not delete
       */
      // COMMENTING OF SINCE FACES CONVERTER LOOKS IN TO THE VIEW ROOT FOR
      // LOCALE WHEN THE LOCALE IS NULL IN ITS getLocale() method.
      //doTestEquals(converter, otherConverter, true);

      converter.setLocale(_LOCALES[i]);
      otherConverter.setLocale(_LOCALES[i]);
      doTestEquals(converter, otherConverter, true);

      converter.setPattern(_PATTERNS[i]);
      otherConverter.setPattern(_PATTERNS[i]);
      doTestEquals(converter, otherConverter, true);

      converter.setTimeStyle(_TIME_STYLES[i]);
      otherConverter.setTimeStyle(_TIME_STYLES[i]);
      doTestEquals(converter, otherConverter, true);

      converter.setTimeZone(_TIME_ZONES[i]);
      otherConverter.setTimeZone(_TIME_ZONES[i]);
      doTestEquals(converter, otherConverter, true);

      converter.setType(_TYPES[i]);
      otherConverter.setType(_TYPES[i]);
      doTestEquals(converter, otherConverter, true);

      String secPattern = _SECONDARY_PATTERNS[i];
      setSecondaryPattern(converter, secPattern);
      setSecondaryPattern(otherConverter, secPattern);
      doTestEquals(converter, otherConverter, true);


      converter.setTransient(_TRANSIENT_VALUES[i]);
      otherConverter.setTransient(_TRANSIENT_VALUES[i]);
      doTestEquals(converter, otherConverter, true);
    }
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  protected void checkStateHolderSaveRestore()
  {

    javax.faces.convert.DateTimeConverter converter = getDateTimeConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    javax.faces.convert.DateTimeConverter restoreConverter = getDateTimeConverter();

    for (int i = 0; i < _DATE_STYLES.length; i++)
    {
      converter.setDateStyle(_DATE_STYLES[i]);
      restoreConverter.setDateStyle(_DATE_STYLES[i]);

       /**
       * @todo fix this up - Do not delete
       */
      // COMMENTING OF SINCE FACES CONVERTER LOOKS IN TO THE VIEW ROOT FOR
      // LOCALE WHEN THE LOCALE IS NULL IN ITS getLocale() method.
      // doTestStateHolderSaveRestore(converter, restoreConverter,
      //                           context, component);
      converter.setLocale(_LOCALES[i]);
      restoreConverter.setLocale(_LOCALES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);

      converter.setPattern(_PATTERNS[i]);
      restoreConverter.setPattern(_PATTERNS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);

      converter.setTimeStyle(_TIME_STYLES[i]);
      restoreConverter.setTimeStyle(_TIME_STYLES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);

      converter.setTimeZone(_TIME_ZONES[i]);
      restoreConverter.setTimeZone(_TIME_ZONES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);

      converter.setType(_TYPES[i]);
      restoreConverter.setType(_TYPES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);

      String secPattern = _SECONDARY_PATTERNS[i];
      setSecondaryPattern(converter, secPattern);
      setSecondaryPattern(restoreConverter, secPattern);

      doTestStateHolderSaveRestore(converter, restoreConverter,
                                   facesContext, wrapper);

      converter.setTransient(_TRANSIENT_VALUES[i]);
      restoreConverter.setTransient(_TRANSIENT_VALUES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter,
                                 facesContext, wrapper);
      mock.verify();
    }
  }

  protected void checkGetAsObject()
  {
    for (int i = 0; i < _SDATE_STYLES.length; ++i)
    {
      javax.faces.convert.DateTimeConverter dtConv = getDateTimeConverter();

      dtConv.setDateStyle(_SDATE_STYLES[i]);
      dtConv.setLocale(_SLOCALES[i]);
      dtConv.setPattern(_SPATTERNS[i]);
      setSecondaryPattern(dtConv, _SSECONDARY_PATTERNS[i]);
      dtConv.setTimeStyle(_STIME_STYLES[i]);
      dtConv.setTimeZone(_STIME_ZONES[i]);
      dtConv.setType(_STYPES[i]);

      Mock mock = mock(UIComponent.class);
      UIComponent component = (UIComponent) mock.proxy();
      
      setFacesContext(facesContext);
      try
      {
        Date obj = (Date)dtConv.getAsObject(facesContext, component, _SINPUT_VALUES[i]);
        assertEquals(true, isEqual(obj,_SEXPECTED_DATES[i]));
      }
      finally
      {
        setFacesContext(null);
      }

      mock.verify();
   }
  }

  protected void checkConversionOnLongPatternWithTimeZone()
  {
    String pattern    = "yyyy.MM.dd G 'at' HH:mm:ss z";
    String inputValue = "2001.07.04 AD at 12:08:56 " + "UTC";

    javax.faces.convert.DateTimeConverter dtConv = getDateTimeConverter();
    dtConv.setTimeZone(null);
    dtConv.setPattern(pattern);

    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();

    TimeZone.setDefault(getTzone("UTC"));

    GregorianCalendar gcal = new GregorianCalendar(getTzone("UTC"));
    gcal.set(2001,6,4,12,8,56);

    setFacesContext(facesContext);
    facesContext.getViewRoot().setLocale(Locale.ENGLISH);

    try
    {
      Date dt = (Date) dtConv.getAsObject(facesContext, component, inputValue);
      Date expectedDt = gcal.getTime();
      assertEquals(true, isEqual(dt, expectedDt));
    }
    finally
    {
      setFacesContext(null);
    }
 }

  protected void checkDatesForPatternBasedChecks()
  {
    // check for conversion of Date --> pattern and pattern --> Date
    // Check if TimeZone set on the converter is honoured.
    // Check if locale set on the converter is honoured
    // Check if locale set on the ViewRoot is honoured
    //2001.07.04 AD at 12:08:56 GMT

    // Prepare a date object to be used for testing.
    GregorianCalendar gcal = new GregorianCalendar();
    gcal.set(2001,Calendar.JULY,4,12,8,56);

    gcal.setTimeZone(getTzone("GMT"));
    // Make use of this date for testing.
    Date date = gcal.getTime();

    // set of values for testing
    Object[][] patternBasedValues
      = {
          // pattern, Locale, TimeZone, Expected Pattern
          {"yyyy.MM.dd G 'at' HH:mm:ss z",Locale.GERMAN, getTzone("GMT"),"2001.07.04 n. Chr. at 12:08:56 GMT"},
          {"yyyy.MM.dd G 'at' HH:mm:ss z",Locale.ENGLISH, getTzone("GMT"),"2001.07.04 AD at 12:08:56 GMT"},
          // Set the locale to null and see if it is picked up from the view root
          {"yyyy.MM.dd G 'at' HH:mm:ss z",null, getTzone("GMT"),"2001.07.04 n. Chr. at 12:08:56 GMT"},
        };

    for (int i = 0; i < patternBasedValues.length; i++)
    {

      Mock mock = mock(UIComponent.class);
      UIComponent component = (UIComponent) mock.proxy();

      javax.faces.convert.DateTimeConverter fdtConv
        = getDateTimeConverter();

      fdtConv.setPattern((String)patternBasedValues[i][0]);
      fdtConv.setTimeZone((TimeZone)patternBasedValues[i][2]);

      Locale loc = (Locale)patternBasedValues[i][1];
      fdtConv.setLocale((Locale)patternBasedValues[i][1]);

      setFacesContext(facesContext);
      // This mainly to set  up the locale on the view root and see if the
      // locale is indeed picked up from the view root
      if (loc == null)
      {
        facesContext.getViewRoot().setLocale(Locale.GERMAN);
      }

      try
      {
        String fobtPattern = fdtConv.getAsString(facesContext, component, date);
        
        assertEquals(patternBasedValues[i][3], fobtPattern);
        
        Date fdt = (Date)fdtConv.getAsObject(facesContext, component, fobtPattern);
        
        /**
         * @todo to find - why we get this millisecond difference
         */
        assertEquals(true, isEqual(date, fdt));
      }
      finally
      {
        setFacesContext(null);
      }
      mock.verify();
    }
  }

  protected void checkDateStyleValidity()
  {
    String[] dateStyles = {"short", "medium", "default", "long", "full",};
    doTestStyleValidity(_DATE_STYLE, dateStyles);
  }

  protected void checkTimeStyleValidity()
  {
    String[] timeStyles = {"short", "medium", "default", "long", "full",};
    doTestStyleValidity(_TIME_STYLE, timeStyles);
  }

  protected void doTestStyleValidity(int styleType, String[] styles) //OK
  {
    /* =-=FIXME AdamWiner having troubles getting this test to run now that 
       JSF 1.2 sometimes calls getAttributes(), getValueBinding(),
       and getClientId()
       
    Date dt = new Date(0);
    for (int i = 0; i < styles.length; i++)
    {
      // by default dateStyle is shortish and type is date

      javax.faces.convert.DateTimeConverter dtConv = getDateTimeConverter();
      Mock mock = mock(UIComponent.class);
      // Give getAttributes() a return value, getValueBinding(), and
      // getClientId() return values
      mock.expects(atLeastOnce()).method("getAttributes").withNoArguments().will(returnValue(new HashMap()));
      mock.expects(atLeastOnce()).method("getValueBinding").withAnyArguments().will(returnValue(null));
      mock.expects(atLeastOnce()).method("getClientId").withAnyArguments().will(returnValue("fooId"));

      UIComponent component = (UIComponent) mock.proxy();

      _setStyleOnConverter(dtConv, styleType, styles[i]);

      dtConv.setLocale(Locale.ENGLISH);
      setFacesContext(facesContext);
      try
      {
        String out =  dtConv.getAsString(facesContext, component, dt);
        assertEquals(true, (out != null));
      }
      finally
      {
        setFacesContext(null);
      }

      // =-=FIXME AdamWiner the invocations are not necessarily
      // asLeastOnce() - should we be using MockUIComponent from Shale here?
      //      mock.verify();

      Mock mok = mock(UIComponent.class);
      // Give getAttributes() a return value, getValueBinding(), and
      // getClientId() return values
      mok.expects(atLeastOnce()).method("getAttributes").withNoArguments().will(returnValue(new HashMap()));
      mok.expects(atLeastOnce()).method("getValueBinding").withAnyArguments().will(returnValue(null));
      mok.expects(atLeastOnce()).method("getClientId").withAnyArguments().will(returnValue("fooId"));
      UIComponent cmp = (UIComponent) mok.proxy();

      setFacesContext(facesContext);
      try
      {
        try
        {
          javax.faces.convert.DateTimeConverter extDtConv = getDateTimeConverter();
          extDtConv.setLocale(Locale.ENGLISH);
          _setStyleOnConverter(extDtConv, styleType, styles[i].toUpperCase());
          extDtConv.getAsString(facesContext, cmp, dt);
          fail("Upper case not accepted for styles");
        }
        catch (RuntimeException ce)
        {
          // Expected
        }
      }
      finally
      {
        setFacesContext(null);
      }

      // =-=FIXME AdamWiner the invocations are not necessarily
      // asLeastOnce() - should we be using MockUIComponent from Shale here?
      //      mok.verify();
    }
    */
  }

  protected abstract javax.faces.convert.DateTimeConverter getDateTimeConverter();

  protected abstract void setSecondaryPattern(
    javax.faces.convert.DateTimeConverter converter,
    String secondaryPattern );

  protected static TimeZone getTzone(String timeZoneId)
  {
    return TimeZone.getTimeZone(timeZoneId);
  }

  /**
   * @todo have to solve the milli second problem that which is there.!!
   * @param convDate
   * @param expectedDate
   * @return
   */
  @SuppressWarnings("deprecation")
  protected static boolean isEqual(Date convDate, Date expectedDate)
  {
    GregorianCalendar convGcal = new GregorianCalendar();
    convGcal.setTime(convDate);

    GregorianCalendar expGcal = new GregorianCalendar();
    expGcal.setTime(expectedDate);

    // -= Simon Lessard =-
    // FIXME: Switch to Calendar methods
    return
      (convDate.getSeconds() == expectedDate.getSeconds()) &&
      (convDate.getYear()    == expectedDate.getYear())    &&
      (convDate.getMonth()   == expectedDate.getMonth())   &&
      (convDate.getDate()    == expectedDate.getDate())    &&
      (convDate.getHours()   == expectedDate.getHours())   &&
      (convDate.getMinutes() == expectedDate.getMinutes()) ;
  }


  private static void _setStyleOnConverter(
    javax.faces.convert.DateTimeConverter dtConv,
    int styleType,
    String style
    )
  {
    if (_DATE_STYLE == styleType)
      {
        dtConv.setDateStyle(style);
      }
      else if (_TIME_STYLE == styleType)
      {
        dtConv.setTimeStyle(style);
        dtConv.setType("time");
      }
      else
      {
        throw new IllegalArgumentException("Invalid style id");
      }
  }

  private static Date[] _getDates(
    Locale[] locales,
    TimeZone[] timeZones,
    int[] day,
    int[] month,
    int[] year,
    int[] hours,
    int[] mins,
    int[] secs
    )
  {
    Date[] dates = new Date[timeZones.length];
    for (int i = 0; i < timeZones.length; ++i)
    {
      GregorianCalendar gcal = new GregorianCalendar();
      gcal.set(year[i],month[i],day[i],hours[i],mins[i],secs[i]);
      gcal.setTimeZone(timeZones[i]);
      dates[i] = gcal.getTime();
    }
    return dates;
  }

  protected static final int _TIME_STYLE = 0;

  protected static final int _DATE_STYLE = 1;


  // Values for positive test cases
  private static String[] _DATE_STYLES = {
                                           "short",
                                           "shortish",
                                           "default",
                                           "medium",
                                           "full",
                                           "long",
                                           "shortish",
                                           "long",
                                           null
                                        };

  private static Locale[] _LOCALES   = {
                                          Locale.US,
                                          Locale.CANADA,
                                          Locale.CANADA_FRENCH,
                                          Locale.TRADITIONAL_CHINESE,
                                          Locale.GERMAN,
                                          Locale.ENGLISH,  // set view root and check
                                          Locale.KOREAN,
                                          new Locale("en","","ORACLE10G"),
                                          new Locale("en","us","ORACLE10G")
                                        };

  private static String[] _PATTERNS = {
                                         null,
                                         "yyyy.MM.dd G 'at' HH:mm:ss z",
                                         "EEE, MMM d, ''yy",
                                         "h:mm a",
                                         "hh 'o''clock' a, zzzz",
                                         "K:mm a, z",
                                         "yyyyy.MMMMM.dd GGG hh:mm aaa",
                                         "EEE, d MMM yyyy HH:mm:ss Z",
                                         "yyMMddHHmmssZ",
                                      };

  private String[] _TIME_STYLES =  {
                                     "short",
                                     "default",
                                     "default",
                                     "medium",
                                     "fullx",     // wrong value set by purpose
                                     "long",
                                     null,
                                     "long",
                                     null
                                   };

  private static TimeZone[] _TIME_ZONES = {
                                            getTzone("GMT"), //null,  i want this to be null
                                            getTzone("GMT-8:00"),
                                            getTzone("GMT+8:00"),
                                            getTzone("GMT-23:59"),
                                            getTzone("GMT-23:00"),
                                            getTzone("GMT-9:00"),
                                            getTzone("GMT-5:30"),
                                            getTzone("GMT+23:59"),
                                            getTzone("GMT-01:05"),
                                          };

  private static String[] _TYPES = {
                                     "date",
                                     "both",
                                     "null",    // set by purpose
                                     null,
                                     "invalid", // set by purpose
                                     "time",
                                     "date",
                                     "both",
                                     "date"
                                   };

  private String[] _SECONDARY_PATTERNS = {
                                           null,
                                           "yyyy/MM/dd G 'at' HH:mm:ss z",
                                           "YYYY/EEE/dd HH:mm:ss Z",
                                           "yyyy-mm-dd G",
                                           "HH:mm:ss",
                                           "MMM/dd/yy",
                                           "EEE MMM dd YYYY",
                                           "dd-MMM-yy",
                                           null
                                        };

    private boolean[] _TRANSIENT_VALUES =  {
                                           true,
                                           false,
                                           false,
                                           true,
                                           true,
                                           false,
                                           true,
                                           true,
                                           true
                                         };


//   Values for test case
//   PREFIX _S stands for Success. These test data should result in success
  private static String[] _SDATE_STYLES = {
                                           "medium",       // Run based on primary pattern //0
                                           "long",         // Run based on long date style //2
                                           "long",         // Test on Date And Time
               /* M/d/yyyy */              "medium"       //  Run to check if medium works on date //6
                                         };

  private static Locale[] _SLOCALES =    {
                                           Locale.US,               //0
                                           Locale.ENGLISH,          //2
                                           Locale.ENGLISH,          //4
                                           Locale.ENGLISH,          //6
                                         };

  private static String[] _SPATTERNS =   {
                                           "d/M/yyyy",             //0
                                           null,                   //2
                                           null,                   //4
                                           null,                   //6
                                         };

  private static String[] _STIME_STYLES = {
                                           "long",                 //0
                                           null,                   //2
                                           "long",                 //4
                                           "ADD TEST !",           //6
                                         };

  private static TimeZone[] _STIME_ZONES = {
                                            getTzone("GMT-8:00"),    //0
                                            getTzone("GMT-4:00"),    //2
                                            getTzone("GMT"),         //4
                                            getTzone("GMT-4:00")     //6
                                          };

  private static String[] _STYPES =       {
                                            "date",                   //0
                                            "date",                   //2
                                            "both",                   //4
                                            "date"                    //6
                                          };

  private static String[] _SSECONDARY_PATTERNS = {
                                                  "dd/M/yy",           //0
                                                  null,                //2
                                                  "dd-M-yyyy",         //4
                                                  "ADD TEST !"         //6
                                                };


  private static String[] _SINPUT_VALUES    =    {
                                                  "9/1/2001",               //0
                                                  "February 12, 1952",      //2
                                                  "July 7, 2001 1:08:56 AM GMT",  //4
                                                  "Sep 06, 1972"            //6
                                                };

  private static  int[] _DAYS              = {
                                               9,         //0
                                               12,        //2
                                               7,         //4
                                               6,         //6
                                             };


  private static  int[] _MONTHS = {
                                    Calendar.JANUARY,    //0
                                    Calendar.FEBRUARY,   //2
                                    Calendar.JULY,       //4
                                    Calendar.SEPTEMBER,  //6
                                  };


  private static int[] _YEARS   = {
                                   2001,                //0
                                   1952,                //2
                                   2001,                //4
                                   1972,                //6
                                  };

  private static int[] _HOURS = {
                                  0, //0
                                  0, //2
                                  1, //4
                                  0, //6
                                  };

  private static int[] _MINS  =   {
                                    0,  //0
                                    0,  //2
                                    8,  //4
                                    0,  //6
                                  };

  private static int[] _SECS  =   {
                                    0,  //0
                                    0,  //2
                                    56, //4
                                    0,  //6
                                  };

  private static Date[]  _SEXPECTED_DATES
    = _getDates(_SLOCALES,_STIME_ZONES, _DAYS,_MONTHS, _YEARS, _HOURS, _MINS, _SECS);

}