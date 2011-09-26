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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.text.DateFormatSymbols;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreChooseDate;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.util.FastMessageFormat;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.pages.GenericEntry;
import org.apache.myfaces.trinidadinternal.share.url.EncoderUtils;


/**
 * Renders the calendar.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/ChooseDateRenderer.java#0 $) $Date: 15-nov-2005.19:27:02 $
 */
public class ChooseDateRenderer extends XhtmlRenderer
{
  // calendar, mobile dateField params
  public static final String MIN_VALUE_PARAM      = XhtmlConstants.MIN_VALUE_PARAM;
  public static final String MAX_VALUE_PARAM      = XhtmlConstants.MAX_VALUE_PARAM;
  public static final String LOC_PARAM            = XhtmlConstants.LOC_PARAM;
  public static final String SCROLLED_VALUE_PARAM = XhtmlConstants.SCROLLED_VALUE_PARAM;
  public static final String MONTH_PARAM          = XhtmlConstants.MONTH_PARAM;
  public static final String YEAR_PARAM           = XhtmlConstants.YEAR_PARAM;

  public ChooseDateRenderer()
  {
    this(CoreChooseDate.TYPE);
  }

  protected ChooseDateRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _maxValueKey = type.findKey("maxValue");
    _minValueKey = type.findKey("minValue");
    _valueKey = PropertyKey.createPropertyKey("value");
    _currTimeKey = PropertyKey.createPropertyKey("currTime");
    _scrolledValueKey = PropertyKey.createPropertyKey("scrolledValue");
    _destinationKey = PropertyKey.createPropertyKey("destination");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Currently, we require scripting to render anything
    if (!supportsScripting(rc))
      return;

    if (canSkipRendering(context, rc, component))
      return;

    // If we are running in inline mode, make sure that we are
    // in an environment that supports partial page rendering.
    // If not, render nothing - the user will need to use the
    // secondary window to select a date.
    boolean isInline = isInline(component, bean);
    if (isInline && !isInlineSupported(rc))
      return;

    // TRINIDAD-1349: The client converter assumes a fixed timezone offset
    // between the server and itself. It calculates that by passing the
    // server's timezone offset at the current date-time, as _uixLocaleTZ.
    // However, if we are rendering a month in which daylight savings occurs in
    // the application timezone, the offset value may be different. In that case
    // pass the new offset value for the client to use.
    TimeZone tz = rc.getLocaleContext().getTimeZone();

    // TRINIDAD-1419: chooseDate golden files should stay the same even if
    // the server runs in different timezones.
    long currTimeMillis = 0;
    Object currTimeValue =  bean.getProperty (_currTimeKey);
    if (currTimeValue != null)
      currTimeMillis = ((Date) currTimeValue).getTime();
    else
      currTimeMillis = System.currentTimeMillis();

    int baseTZOffsetMinutes = tz.getOffset(currTimeMillis/(1000*60));

    boolean isDesktop = isDesktop(rc);
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("table", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);
    if (isDesktop)
      OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);
    else
      OutputUtils.renderLayoutTableAttributes(context, rc, "0", "100%");


    // Get the styles that we'll use to render the calendar
    CalendarStyles styles = _getCalendarStyles(isInline);

    // get the calendar of the minimum displayable time
    long minTime = _getMinTime(rc, bean);

    // get the calendar of the maximum displayable time
    long maxTime = _getMaxTime(rc, bean);

    // get the currently selected Time
    long selectedTime = _getSelectedTime(rc, bean, minTime, maxTime);

    // get the id
    String id = getClientId(context, component);

    // get the destination for the date links
    String destString;
    if (isInline)
      destString = GenericEntry.getGenericEntryURL(
           context,
           GenericEntry.INLINE_DATE_PICKER_ENTRY);
    else
      destString = getDestination(component, bean);


     // get the calendar of the currently displayed time
    Calendar displayedCalendar = _getDisplayedCalendar(rc,
                                                       bean,
                                                       minTime,
                                                       maxTime,
                                                       selectedTime);

    int firstDOM = _getActualMinimumDayOfMonth(displayedCalendar);
    int lastDOM  = _getActualMaximumDayOfMonth(displayedCalendar);

    // determine the the starting times and ending times of the first and
    // last days of the month
    // Create a copy of the calendar so we don't hammer the current values
    Calendar calcCal = (Calendar) displayedCalendar.clone();
    // First is easy
    calcCal.set(Calendar.DAY_OF_MONTH, firstDOM);
    long firstDOMTime = calcCal.getTimeInMillis();

    // Last not just the last day of this month, it's the first day of next
    // month minus a millisecond.
    calcCal.set(Calendar.DAY_OF_MONTH, lastDOM);
    calcCal.add(Calendar.DATE, 1);
    long lastDOMTime = calcCal.getTimeInMillis() - 1;

    DateFormatSymbols dateSymbols = _getDateFormatSymbols(rc);

    int firstDOW = displayedCalendar.getMinimum(Calendar.DAY_OF_WEEK);
    int lastDOW = displayedCalendar.getMaximum(Calendar.DAY_OF_WEEK);
    int dowCount = lastDOW - firstDOW + 1;

    //
    // Write the month and year drop downs
    //
    // If we're running in inline mode, make sure we have
    // access to the necessary scripts
    if (isInline)
      XhtmlUtils.addLib(context, rc, "_calsd()");


    // make sure that the js lib is added
    XhtmlUtils.addLib(context, rc, "_updateCal()");

    String baseNavURL = _createNavURL(rc,
                                      destString,
                                      minTime,
                                      maxTime,
                                      selectedTime,
                                      id);

    writer.startElement("tr", null);

    // render the previous button
    _renderNextPrev(context,
                    rc,
                    component,
                    bean,
                    true,
                    minTime,
                    firstDOMTime,
                    baseNavURL,
                    isInline);

    writer.startElement("td", null);
    writer.writeAttribute("colspan", IntegerUtils.getString(dowCount - 2), null);
    renderStyleClass(context, rc, styles.TITLE_STYLE);

    // don't wrap the month and year controls
    writer.writeAttribute("nowrap", Boolean.TRUE, null);

    _renderMonthAndYear(context,
                        rc,
                        minTime,
                        maxTime,
                        displayedCalendar,
                        dateSymbols,
                        baseNavURL,
                        id,
                        isInline);

    writer.endElement("td");

    // render the next button
    _renderNextPrev(context,
                    rc,
                    component,
                    bean,
                    false,
                    maxTime,
                    lastDOMTime,
                    baseNavURL,
                    isInline);

    writer.endElement("tr");

    // Place the rest of the calendar content within its own
    // table, so that we can style the calendar's border
    writer.startElement("tr", null);
    writer.startElement("td", null);
    writer.writeAttribute("colspan", IntegerUtils.getString(dowCount), null);

    writer.startElement("table", null);
    //fix for bug 4410632: added summary attribute
    OutputUtils.renderDataTableAttributes(context,
                                            rc,
                                            "0", "0", "0", "100%",
                        rc.getTranslatedString("af_chooseDate.SUMMARY"));
    renderStyleClass(context, rc, styles.CONTENT_STYLE);

    //
    // Write the day of the week headers
    //
    writer.startElement("tr", null);
    renderStyleClass(context, rc, styles.HEADER_STYLE);

    String[] shortWeekdays;
    // Bug 2388968:  Java's "short" weekdays in Arabic are single
    // letters, which we're told are inadequate.  Output entire
    // names instead.
    if ("ar".equals(rc.getLocaleContext().getFormattingLocale().getLanguage()))
      shortWeekdays = dateSymbols.getWeekdays();
    else
      shortWeekdays = dateSymbols.getShortWeekdays();

    for (int i = firstDOW; i <= lastDOW; i++)
    {
      writer.startElement("th", null);
      writer.writeAttribute("scope", "col", null);
      writer.writeText(shortWeekdays[i], null);
      writer.endElement("th");
    }

    writer.endElement("tr");

    displayedCalendar.set(Calendar.DAY_OF_MONTH, firstDOM);

    int dow = displayedCalendar.get(Calendar.DAY_OF_WEEK);

    //
    // Output the days in the month
    //
    writer.startElement("tr", null);

    //
    // output the days from the previous month in the first week
    //
    int firstDOWInMonth = firstDOW - dow;

    if (firstDOWInMonth < 0)
    {
      // move to the previous month
      displayedCalendar.add(Calendar.MONTH, -1);

      // get the count of the last day of the the previous month
      int prevLastDOM = _getActualMaximumDayOfMonth(displayedCalendar) -
                        _getActualMinimumDayOfMonth(displayedCalendar) + 1;

      int firstPrevLastDOM = prevLastDOM + firstDOWInMonth + 1;

      for (int i = firstPrevLastDOM; i <= prevLastDOM; i++)
      {
        writer.startElement("td", null);

        // Hmm... the font for disabled days in inline calendars
        // is way too big - unless we render the disabled style class.
        if (isInline)
        {
          renderStyleClass(context, rc, styles.DISABLED_STYLE);
        }

        writer.writeText(String.valueOf(i), null);
        writer.endElement("td");
      }

      // move back to the current month
      displayedCalendar.add(Calendar.MONTH, 1);
    }

    int  currDOM    = firstDOM;
    long currTime   = firstDOMTime;
    displayedCalendar.add(Calendar.DAY_OF_MONTH, 1);
    long nextTime   = displayedCalendar.getTimeInMillis();
    int currLastDOW = firstDOWInMonth + dowCount;

    String[] keysAndValues = new String[]{
      XhtmlConstants.VALUE_PARAM,
      null, // placeholder
      XhtmlConstants.EVENT_PARAM,
      XhtmlConstants.DATE_EVENT,
      XhtmlConstants.TYPE_PARAM,
      XhtmlConstants.TYPE_POST,
      XhtmlConstants.SOURCE_PARAM,
      id};


    //
    // output the days in this month
    //
    do
    {
      for (; (currDOM <= currLastDOW) && (currDOM <= lastDOM); currDOM++)
      {
        // only days between the minimum and maximum times can be
        // selected
        boolean enabledDay  = (currTime >= minTime) && (currTime <= maxTime);

        writer.startElement("td", null);

        if (isInline && !enabledDay)
        {
         renderStyleClass(context, rc, styles.DISABLED_STYLE);
        }

        boolean selectedDay = false;

        if (enabledDay)
        {
          selectedDay = (selectedTime >= currTime ) &&
                        (selectedTime < nextTime);
        }



        if ( enabledDay)
        {
          //
          // even though the selected day doesn't show a link,
          // a link is generated to handle the case where the
          // user wants to select todays date, but hasn't supplied
          // a date in the date field. (see bug #1482511)
          //
          writer.startElement("a", null);
          renderSelectDayAttributes(rc,
                                    context,
                                    keysAndValues,
                                    id,
                                    currTime,
                                    baseTZOffsetMinutes,
                                    isInline,
                                    isDesktop,
                                    destString);
        }

        if (selectedDay)
        {
          writer.startElement("span", null);
          renderStyleClass(context, rc, styles.SELECTED_STYLE);
        }

        writer.writeText(String.valueOf(currDOM), null);

        if (selectedDay )
        {
          writer.endElement("span");
        }

        if (enabledDay)
        {
          writer.endElement("a");
        }

        writer.endElement("td");

        // move to the next day in time
        currTime = nextTime;
        displayedCalendar.add(Calendar.DAY_OF_MONTH, 1);
        nextTime = displayedCalendar.getTimeInMillis();
      }

      if (currDOM <= lastDOM)
      {
        // end the current week row
        writer.endElement("tr");

        // start next week's row
        writer.startElement("tr", null);

        currLastDOW += dowCount;
      }
      else
      {
        break;
      }
    } while (true);

    // Reset the calendar
    displayedCalendar.set(Calendar.DAY_OF_MONTH, firstDOM);

    //
    // output the days from the next month in the last week
    //
    int lastDOWInMonth = currLastDOW - currDOM + 1;

    if (lastDOWInMonth > 0)
    {
      // move to the next month
      displayedCalendar.add(Calendar.MONTH, 1);

      // get the count of the last day of the the previous month
      int nextFirstDOM = _getActualMinimumDayOfMonth(displayedCalendar);
      int nextLastDOM  = nextFirstDOM + lastDOWInMonth - 1;

      for (int i = nextFirstDOM; i <= nextLastDOM; i++)
      {
        writer.startElement("td", null);

        if (isInline)
        {
          renderStyleClass(context, rc, styles.DISABLED_STYLE);
        }

        writer.writeText(String.valueOf(i), null);
        writer.endElement("td");
      }
    }

    writer.endElement("tr");

    writer.endElement("table");
    writer.endElement("td");
    writer.endElement("tr");

    writer.endElement("table");
  }


  protected void renderSelectDayAttributes(
    RenderingContext rc,
    FacesContext     context,
    String[]         keysAndValues,
    String           id,
    long             currTime,
    int              baseTZOffsetMinutes,
    boolean          isInline,
    boolean          isDesktop,
    String           destString
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (isDesktop)
    {
      TimeZone tz = rc.getLocaleContext().getTimeZone();
      int tzOffsetMinutes = tz.getOffset(currTime)/(1000*60);

      StringBuilder clickRef = new StringBuilder(30);
      writer.writeURIAttribute("href", "#", null);

      if (isInline)
      {
        clickRef.append("return _calsd(");

        // First argument is the ID of the calendar
        if (id == null)
          clickRef.append("null");
        else
        {
          clickRef.append("'");
          clickRef.append(id);
          clickRef.append("'");
        }

        clickRef.append(',');
      }
      else
      {
        clickRef.append("return _selectDate(");
      }

      clickRef.append(currTime);
      if (tzOffsetMinutes != baseTZOffsetMinutes)
        clickRef.append (", " + tzOffsetMinutes);
      clickRef.append(')');
      writer.writeAttribute("onclick", clickRef, null);
    }
    else
    {
      keysAndValues[1] = IntegerUtils.getString( currTime);
      assert(destString != null);
      String url = EncoderUtils.appendURLArguments(destString,
                                                   keysAndValues);
      renderEncodedActionURI(context, "href", url);
    }
  }

  /**
   * Render the next and previous buttons of the calendar dialog.
   */
  protected void renderNextPrev(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          isPrev,
    boolean          isEnabled,
    String           halign,
    Object           altText,
    String           destination,
    String           onClick
    ) throws IOException
  {
    CalendarStyles styles = _getCalendarStyles(component, bean);

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("td", null);
    renderStyleClass(context, rc, styles.NAV_STYLE);

    writer.writeAttribute("align", halign, null);

    if (isEnabled)
    {
      writer.startElement("a", null);
      renderEncodedActionURI(context, "href", destination);
      writer.writeAttribute("onclick", onClick, null);
    }

    String iconName;
    if(isInline(component, bean))
    {
      if (isPrev)
      {
        iconName = (isEnabled)
                 ? SkinSelectors.AF_CHOOSE_DATE_PREV_ICON_NAME
                 : SkinSelectors.AF_CHOOSE_DATE_PREV_DISABLED_ICON_NAME;
      }
      else
      {
        iconName = (isEnabled)
                 ? SkinSelectors.AF_CHOOSE_DATE_NEXT_ICON_NAME
                 : SkinSelectors.AF_CHOOSE_DATE_NEXT_DISABLED_ICON_NAME;
      }
    }
    else
    {
      if (isPrev)
      {
        iconName = (isEnabled)
                 ? SkinSelectors.AF_SELECT_INPUT_DATE_PREV_ICON_NAME
                 : SkinSelectors.AF_SELECT_INPUT_DATE_PREV_DISABLED_ICON_NAME;
      }
      else
      {
        iconName = (isEnabled)
                 ? SkinSelectors.AF_SELECT_INPUT_DATE_NEXT_ICON_NAME
                 : SkinSelectors.AF_SELECT_INPUT_DATE_NEXT_DISABLED_ICON_NAME;
      }
    }

    Icon icon = rc.getIcon(iconName);

    // If we've got an Icon, render it
    if (icon != null)
    {
      OutputUtils.renderIcon(context,
                             rc,
                             icon,
                             altText,
                             null);
    }

    if (isEnabled)
    {
      writer.endElement("a");
    }

    writer.endElement("td");
  }

  protected String getDestination(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_destinationKey));
  }

  /**
   * Tests whether the calendar is running in "inline" mode.
   */
  protected boolean isInline(
    UIComponent component,
    FacesBean   bean)
  {
    // For now, we assume that a null destination means that
    // we are running in inline mode, since CalendarDialogJSP *always*
    // sets the destination.  Perhaps it would be safer if we
    // didn't make this assumption but instead used some explicit
    // attribute which indicates the mode.
    return (getDestination(component, bean) == null);
  }

  /**
   * Render the next and previous buttons of the calendar dialog.
   */
  private void _renderNextPrev(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          isPrev,
    long             compareTime,
    long             buttonTime,
    String           destination,
    boolean          isInline
    ) throws IOException
  {
    boolean isEnabled;
    String iconDesc;
    String halign;

    if (isPrev)
    {
      isEnabled = buttonTime >= compareTime;

      // move to the last day in the previous month
      buttonTime -= _MILLIS_IN_DAY;

      iconDesc = rc.getTranslatedString("af_chooseDate.PREVIOUS_MONTH_TIP");
      halign   = "left";
    }
    else
    {
      isEnabled = buttonTime <= compareTime;

      // move to the first day in the next month
      buttonTime += _MILLIS_IN_DAY;

      iconDesc = rc.getTranslatedString("af_chooseDate.NEXT_MONTH_TIP");
      halign   = "right";
    }

    if (isEnabled)
    {
      destination += "&scrolledValue=" +
                     buttonTime;
    }

    // If we are running in inline mode, convert the destination
    // to an onClick handler.
    String onClick = null;

    if (isInline)
    {
      onClick = AutoSubmitUtils.getPartialGetScript(destination);
      destination="#";
    }

    renderNextPrev( context,
                    rc,
                    component,
                    bean,
                    isPrev,
                    isEnabled,
                    halign,
                    iconDesc,
                    destination,
                    onClick);

  }

  /**
   * Creates the base navigation URL
   */
  private String _createNavURL(
    RenderingContext rc,
    String           destinationString,
    long             minTime,
    long             maxTime,
    long             selectedTime,
    String           id
    )
  {
    StringBuffer buffer = new StringBuffer();


    String[] params = _createNavURLParams( rc,
                                           minTime,
                                           maxTime,
                                           selectedTime,
                                           id);

    EncoderUtils.appendURLArguments(buffer, destinationString, params);

    return buffer.toString();
  }

  /**
   * Creates the params for navigation URL
   */
  private String[] _createNavURLParams(
    RenderingContext rc,
    long             minTime,
    long             maxTime,
    long             selectedTime,
    String           id
    )
  {
    return new String[] {
      XhtmlConstants.SOURCE_PARAM,
      id,
      MIN_VALUE_PARAM,
      String.valueOf(minTime),
      MAX_VALUE_PARAM,
      String.valueOf(maxTime),
      XhtmlConstants.VALUE_PARAM,
      String.valueOf(selectedTime),
      LOC_PARAM,
      rc.getLocaleContext().getFormattingIANALocaleString()
    };
  }

  /**
   * Returns the change handler to use for the choices
   */
  private String _getChangeHandler(
    String  baseNavURL,
    boolean isInline
    )
  {
    String prefix = "_updateCal(this,'";
    String suffix = ");";

    int length = prefix.length() + suffix.length() + baseNavURL.length() + 2;

    StringBuilder buffer = new StringBuilder(length);
    buffer.append(prefix);
    buffer.append(baseNavURL);
    buffer.append("',");
    buffer.append(isInline ? "1" : "0");
    buffer.append(suffix);

    return buffer.toString();
  }

  private void _renderMonthChoice(
    FacesContext     context,
    RenderingContext rc,
    String[]         months,
    Calendar         currentTime,
    int              visibleMonth,
    int              minimumMonth,
    int              maximumMonth,
    long             offset,
    String           onChange,
    String           baseId
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    String label = rc.getTranslatedString("af_chooseDate.MONTH_CHOICE_LABEL");
    String id = MONTH_PARAM;

    // If we've got a baseID, tack it on.  This is necessary
    // for inline mode, where we might have more than one
    // calendar (and thus more than one "month" choice) on
    // the same page.
    if (baseId != null)
      id = baseId + id;

    //
    // create the choice
    //
    writer.startElement("select", null);
    writer.writeAttribute("id", id, null);
    writer.writeAttribute("name", id, null);
    writer.writeAttribute("title", label, null);
    writer.writeAttribute("onchange", onChange, null);
    renderStyleClass(context,
                     rc,
                     SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS);

    for (int currMonth = minimumMonth; currMonth <= maximumMonth; currMonth++)
    {
      writer.startElement("option", null);

      if (currMonth == visibleMonth)
      {
        writer.writeAttribute("selected", Boolean.TRUE, null);
      }

      //
      // generate the new date as the value for each entry]
      //
      currentTime.set(Calendar.MONTH, currMonth);


      String value = String.valueOf(currentTime.getTimeInMillis() - offset);
      writer.writeAttribute("value", value, null );

      writer.writeText(months[currMonth], null);

      writer.endElement("option");
    }

    writer.endElement("select");

    HiddenLabelUtils.outputHiddenLabelIfNeeded(context,
                                               rc,
                                               id,
                                               label,
                                               null);
  }


  private void _renderYearChoice(
    FacesContext     context,
    RenderingContext rc,
    Calendar         currentTime,
    int              year,
    int              minimumYear,
    int              maximumYear,
    String           onChange,
    String           baseId
    ) throws IOException
  {
    String label = rc.getTranslatedString("af_chooseDate.YEAR_CHOICE_LABEL");
    String id = YEAR_PARAM;

    // If we've got a baseID, tack it on.  This is necessary
    // for inline mode, where we might have more than one
    // calendar (and thus more than one "year" choice) on
    // the same page.
    if (baseId != null)
      id = baseId + id;

    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("select", null);
    writer.writeAttribute("name", id, null);
    writer.writeAttribute("id", id, null);
    writer.writeAttribute("title", label, null);
    writer.writeAttribute("onchange", onChange, null);
    renderStyleClass(context,
                     rc,
                     SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS);

    boolean needsPrevItem = false;
    boolean needsNextItem = false;

    //
    // window the year, if necessary
    //
    if (maximumYear - minimumYear >= _MAX_CHOICE_ITEMS)
    {
      int minYearCount = year - minimumYear;
      int maxYearCount = maximumYear - year;

      boolean minSmaller   = true;
      int     smallerCount = minYearCount;

      if (maxYearCount < minYearCount)
      {
        smallerCount = maxYearCount;
        minSmaller = false;
      }

      //
      // test to see if only one side needs to be pegged
      //
      if (smallerCount <= _HALF_MAX_CHOICE_ITEMS)
      {
        // only one end needs to be windowed
        if (minSmaller)
        {
          needsNextItem = true;
          maximumYear = minimumYear + _MAX_CHOICE_ITEMS - 1;
        }
        else
        {
          needsPrevItem = true;
          minimumYear = maximumYear - _MAX_CHOICE_ITEMS + 1;
        }
      }
      else
      {
        // both ends need to be windowed
        needsPrevItem = true;
        needsNextItem = true;

        minimumYear = year - _HALF_MAX_CHOICE_ITEMS + 1;
        maximumYear = year + _HALF_MAX_CHOICE_ITEMS - 1;
      }
    }

    // write the previous page item, if necessary
    if (needsPrevItem)
    {
      _writeYearOption(writer,
                       currentTime,
                       minimumYear - 1,
                       year,
                       _getBeforeFormat(rc).format(
                          new String[]{String.valueOf(minimumYear)}));
    }

    //
    // write the years
    //
    for (int currYear = minimumYear; currYear <= maximumYear; currYear++)
    {
      _writeYearOption(writer,
                       currentTime,
                       currYear,
                       year,
                       String.valueOf(currYear));
    }

    // write the next page item, if necessary
    if (needsNextItem)
    {
      _writeYearOption(writer,
                       currentTime,
                       maximumYear + 1,
                       year,
                       _getAfterFormat(rc).format(
                              new String[]{String.valueOf(maximumYear)}));
    }

    writer.endElement("select");

    HiddenLabelUtils.outputHiddenLabelIfNeeded(context,
                                               rc,
                                               id,
                                               label,
                                               null);
  }



  /**
   * Writes the year option instance
   */
  private void _writeYearOption(
    ResponseWriter writer,
    Calendar       currentTime,
    int            year,
    int            selectedYear,
    String         text
    ) throws IOException
  {
    writer.startElement("option", null);

    if (selectedYear == year)
    {
      writer.writeAttribute("selected", Boolean.TRUE, null);
    }

    currentTime.set(Calendar.YEAR, year);

    writer.writeAttribute("value",
                          String.valueOf(currentTime.getTimeInMillis()),
              null);

    // output the label for the after item
    writer.writeText(text, null);

    writer.endElement("option");
  }

  /**
   * Renders the month and year portion of the Calendar
   */
  private void _renderMonthAndYear(
    FacesContext      context,
    RenderingContext  rc,
    long              minTime,
    long              maxTime,
    Calendar          displayedCalendar,
    DateFormatSymbols dateSymbols,
    String            baseNavURL,
    String            calendarId,
    boolean           isInline
    ) throws IOException
  {
    String jsNavURL = _escapeJSURL(context, baseNavURL);
    Calendar minCalendar = _getCalendar(rc, minTime);
    Calendar maxCalendar = _getCalendar(rc, maxTime);

    int minYear = minCalendar.get(Calendar.YEAR);
    int maxYear = maxCalendar.get(Calendar.YEAR);

    // =-= bts I suspect we will need some to deal
    //         with the case where min year and
    //         max year are in different eras
    ResponseWriter writer = context.getResponseWriter();

    int monthIndex = displayedCalendar.get(Calendar.MONTH);
    int year = displayedCalendar.get(Calendar.YEAR);

    String[] monthNames = dateSymbols.getMonths();

    // determine if only one year is allowed to be chosen
    boolean sameYear = minYear == maxYear;

    int minMonthIndex = minCalendar.get(Calendar.MONTH);
    int maxMonthIndex = maxCalendar.get(Calendar.MONTH);

    boolean sameMonth = sameYear && (minMonthIndex == maxMonthIndex);

    if (!sameMonth)
    {

      String onChange = _getChangeHandler(jsNavURL, isInline);

      //
      // Initialize the calendar for the current year so that we
      // can compute the month offsets
      //
      Calendar currentTime = _getCalendar(rc);

      _zeroOutTime(currentTime);

      currentTime.set(Calendar.YEAR, year);


      int minimumMonth = (year == minYear)
                         ? minMonthIndex
                         : displayedCalendar.getActualMinimum(Calendar.MONTH);

      int maximumMonth = (year == maxYear)
                         ? maxMonthIndex
                         : displayedCalendar.getActualMaximum(Calendar.MONTH);

      currentTime.set(Calendar.DAY_OF_MONTH, 15);

      _renderMonthChoice( context,
                          rc,
                          monthNames,
                          currentTime,
                          monthIndex,
                          minimumMonth,
                          maximumMonth,
                          0,
                          onChange,
                          calendarId);

    writer.writeText(XhtmlConstants.NBSP_STRING, null);

    if (sameYear)
    {
      // month and year are same, so just render the
      writer.writeText(String.valueOf(year), null);
    }
    else
    {
      _zeroOutTime(currentTime);

      currentTime.set(Calendar.MONTH, monthIndex);

      _renderYearChoice( context,
                         rc,
                         currentTime,
                         year,
                         minYear,
                         maxYear,
                         onChange,
                         calendarId);

    }
    }
    else
    {
      // format used for combining months and years
      FastMessageFormat titleFormat = _getTitleFormat(rc);

      String monthName = monthNames[monthIndex];
      String yearName = String.valueOf(year);

      String title = titleFormat.format(new String[]{monthName, yearName});

      writer.writeText(title, null);
    }
  }


  @SuppressWarnings({ "deprecation", "cast" })
  private static long _getTimeAttr(
    RenderingContext arc,
    FacesBean   bean,
    PropertyKey key,
    long        defaultTime
    )
  {
    Object value = bean.getProperty(key);
    if (value == null)
    {
      return defaultTime;
    }
    else
    {
      if (value instanceof String)
      {
        // although the Date that takes a String is deprecated in favor
        // of using DateFormat, we should not use DateFormat in this instance
        // as the use of this attribute is not to parse Date's in a locale
        // sensitive manner.  That said, specifying the attributes in
        // Date or Number form is still preferred over String form.
        //
        try
        {
          value = new Date((String)value);
        }
        catch (Exception e)
        {
          _LOG.warning("INVALID_STRING_ATTRIBUTE", value);
        }
      }
      else if (value instanceof Calendar)
      {
        value =  ((Calendar)value).getTime();
      }

      // adjust the date to use the time zone found in the
      // AdfRenderingContext's LocaleContext
      if (value instanceof Date)
      {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime((Date)value);
        long dateValueInMs = calendar.getTimeInMillis();
        long tzOffset = calendar.get(Calendar.ZONE_OFFSET) +
                        calendar.get(Calendar.DST_OFFSET);
        // get the timeZone specified in trinidad-config, if any or the
        // client timeZone.
        LocaleContext localeContext = arc.getLocaleContext();
        // find out the difference in timeZone
        tzOffset -= localeContext.getTimeZone().getOffset(dateValueInMs);

        // Bug 4570118
        // make sure that adjusting to correct timeZone doesn't take the
        // long value out of the range. Calendar too doesn't handle this
        // properly ie. MIN_VALUE < (longValue + tzOffset) < MAX_VALUE.
        // this is possible since we use Long.MAX_VALUE as the default
        // maximum date.
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

        // adjust the date in ms to the adjusted time zone.
        return (dateValueInMs + tzOffset);
      }
      else if (value instanceof Number)
      {
        return ((Number)value).longValue();
      }
      else
      {
        return defaultTime;
      }
    }
  }



  /**
   * Returns a Calendar, given Locale information
   */
  private static Calendar _getCalendar(
    RenderingContext arc
    )
  {
    LocaleContext localeContext = arc.getLocaleContext();

    Calendar calendar = Calendar.getInstance(localeContext.getTimeZone(),
                                localeContext.getFormattingLocale());
    if (calendar instanceof GregorianCalendar)
    {
      ((GregorianCalendar) calendar).setGregorianChange(
         new Date(Long.MIN_VALUE));
    }

    return calendar;
  }


  /**
   * Returns a Calendar, given a time and Locale information
   */
  private static Calendar _getCalendar(
    RenderingContext arc,
    long                time
    )
  {
    Calendar cal = _getCalendar(arc);

    cal.setTime(new Date(time));

    // reset to the first moment of this day
    _zeroOutTime(cal);

    return cal;
  }


  /**
   * Zeros out the Calendar's time, so that the Calendar records the
   * first moment of its day.
   */
  private static void _zeroOutTime(
    Calendar cal
    )
  {
    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MILLISECOND, 0);
  }


  /**
   * Returns the minimum pickable time
   */

  private long _getMinTime(RenderingContext arc, FacesBean bean)
  {
    return _getTimeAttr(arc, bean, _minValueKey, _MIN_TIME);
  }


  /**
   * Returns the maximum pickable time
   */
  private long _getMaxTime(RenderingContext arc, FacesBean bean)
  {
    return _getTimeAttr(arc, bean, _maxValueKey, _MAX_TIME);
  }


  /**
   * Returns the Calendar to display
   */
  private Calendar _getDisplayedCalendar(
    RenderingContext arc,
    FacesBean           bean,
    long                minTime,
    long                maxTime,
    long                selectedTime
    )
  {
    Calendar displayedCal = _getCalendar(
                             arc,
                            _getDisplayedTime(arc,
                                              bean,
                                              minTime,
                                              maxTime,
                                              selectedTime));

    displayedCal.set(Calendar.DAY_OF_MONTH, 1);

    return displayedCal;
  }


  /**
   * Returns the time of the displayed day.
   */
  private long _getDisplayedTime(
    RenderingContext arc,
    FacesBean        bean,
    long             minTime,
    long             maxTime,
    long             selectedTime
    )
  {
    long displayedTime = _getTimeAttr(arc,
                                      bean,
                                      _scrolledValueKey,
                                      selectedTime);

    return _getBoundedTime(displayedTime, minTime, maxTime);
  }

  /**
   * Returns the time of the selected day.
   */
  private long _getSelectedTime(
    RenderingContext arc,
    FacesBean bean,
    long      minTime,
    long      maxTime
    )
  {
    long selectedTime = _getTimeAttr(arc,
                                     bean,
                                     _valueKey,
                                     System.currentTimeMillis());

    return _getBoundedTime(selectedTime, minTime, maxTime);
  }

  /**
   * Bound the time between the mintime and the max time
   */
  private static long _getBoundedTime(
    long time,
    long minTime,
    long maxTime
    )
  {
    if (time < minTime)
    {
      time = minTime;
    }
    else if (time > maxTime)
    {
      time = maxTime;
    }

    return time;
  }


  private FastMessageFormat _getTitleFormat(
    RenderingContext arc
    )
  {
    return _getMessageFormat(arc,
                             "af_chooseDate.TITLE_FORMAT");
  }

  private FastMessageFormat _getBeforeFormat(
    RenderingContext arc
    )
  {
    return _getMessageFormat(arc,
                             "af_chooseDate.DIALOG_EARLIER");
  }

  private FastMessageFormat _getAfterFormat(
    RenderingContext arc
    )
  {
    return _getMessageFormat(arc,
                             "af_chooseDate.DIALOG_LATER");
  }

  private FastMessageFormat _getMessageFormat(
    RenderingContext arc,
    String              translationKey)
  {
    FastMessageFormat format =
        new FastMessageFormat(arc.getTranslatedString(translationKey));

    return format;
  }

  private static DateFormatSymbols _getDateFormatSymbols(
    RenderingContext arc
    )
  {
    DateFormatSymbols symbols = (DateFormatSymbols)
                           arc.getProperties().get(_DATE_SYMBOLS_KEY);

    if (symbols == null)
    {
      symbols = new DateFormatSymbols(arc.getLocaleContext().getFormattingLocale());

      arc.getProperties().put(_DATE_SYMBOLS_KEY, symbols);
    }

    return symbols;
  }


  /**
   * Get the first day of the month.
   */
  private static int _getActualMinimumDayOfMonth(
    Calendar calendar
    )
  {
    return calendar.getActualMinimum(Calendar.DAY_OF_MONTH);
  }


  /**
   * Get the last day of the month.
   */
  private static int _getActualMaximumDayOfMonth(
    Calendar calendar
    )
  {
    return calendar.getActualMaximum(Calendar.DAY_OF_MONTH);
  }

  // This method prepares an URL for output to a JavaScript script.
  // It does the following:
  // 1. Runs the URL through the URL encoder in case clients provide custom
  //    URL encoding.
  // 2. Runs the URL though EncoderUtils.encodeURL(), to perform the URL
  //    encoding of special characters
  // 3. Calls XhtmlUtils.escapeJS() to escape any backslash or quote
  //    characters
  private static String _escapeJSURL(
    FacesContext context,
    String           url
    )
  {
    // First, encode the URL
    url = context.getExternalContext().encodeActionURL(url);

    // Next, encode the URL and escape any special characters
    String encoding = OutputUtils.getOutputEncoding(context);

    try
    {
      url = EncoderUtils.encodeURL(url, encoding, false);
    }
    catch (Exception e)
    {
      if (_LOG.isWarning())
        _LOG.warning("UNABLE_ENCODE_URL", new Object[]{url, encoding});
        _LOG.warning(e);
    }

    // Finally, escape any characters that cause problems for JS
    return XhtmlUtils.escapeJS(url);
  }

  // Tests whether the current environment supports inline mode
  protected static boolean isInlineSupported(RenderingContext arc)
  {
    // Inline mode is only supported if partial page rendering is
    // supported and we are not running in screen reader mode.
    return (PartialPageUtils.supportsPartialRendering(arc) &&
            isDesktop(arc) &&
            !isScreenReaderMode(arc));
  }

  // Returns the CalendarStyles object to use when
  // rendering the specified calendar component
  private CalendarStyles _getCalendarStyles(
    UIComponent component,
    FacesBean   bean)
  {
    return _getCalendarStyles(isInline(component, bean));
  }

  // Gets the calendar styles for the specified mode
  private static CalendarStyles _getCalendarStyles(boolean isInline)
  {
    return _INLINE_STYLES;
  }

  // Just a little utility class which specifies the names of
  // the style classes to use when rendering the calendar.
  private static final class CalendarStyles
  {
    public final String NAV_STYLE;
    public final String TITLE_STYLE;
    public final String HEADER_STYLE;
    public final String DISABLED_STYLE;
    public final String ENABLED_STYLE;
    public final String SELECTED_STYLE;
    public final String CONTENT_STYLE;

    public CalendarStyles(
      String navStyle,
      String titleStyle,
      String headerStyle,
      String disabledStyle,
      String enabledStyle,
      String selectedStyle,
      String contentStyle
      )
    {
      NAV_STYLE = navStyle;
      TITLE_STYLE = titleStyle;
      HEADER_STYLE = headerStyle;
      DISABLED_STYLE = disabledStyle;
      ENABLED_STYLE = enabledStyle;
      SELECTED_STYLE = selectedStyle;
      CONTENT_STYLE = contentStyle;
    }
  }

  // Define inline calendar styles
  private static final CalendarStyles _INLINE_STYLES =
    new CalendarStyles(
      SkinSelectors.AF_CHOOSE_DATE_NAV_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_TITLE_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_HEADER_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_DISABLED_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_ENABLED_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_SELECTED_STYLE_CLASS,
      SkinSelectors.AF_CHOOSE_DATE_CONTENT_STYLE_CLASS
      );


  private static final int _MAX_CHOICE_ITEMS = 30;
  private static final int _HALF_MAX_CHOICE_ITEMS = _MAX_CHOICE_ITEMS / 2;


  private static final long _MILLIS_IN_DAY = 1000L * 60 * 60 * 24;

  private static final long _MIN_TIME;

  private static final long _MAX_TIME = Long.MAX_VALUE;

  //
  // Rendering Context cache keys
  //
  private static final Object _DATE_SYMBOLS_KEY = new Object();

  static
  {
    // =-= bts
    // default minimum date is 1AD so we don't have to deal with years in
    // different eras.  Hopefully this won't be a problem in other calendars
    // (but I think that it will)
    Calendar cal = Calendar.getInstance();
    cal.set(1, 1, 1, 0, 0);
    _MIN_TIME = cal.getTimeInMillis();
  }

  private PropertyKey _maxValueKey;
  private PropertyKey _minValueKey;
  private PropertyKey _valueKey;
  private PropertyKey _currTimeKey;
  private PropertyKey _scrolledValueKey;
  private PropertyKey _destinationKey;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ChooseDateRenderer.class);
}
