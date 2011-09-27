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

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.convert.DateTimeConverter;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreInputDate;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidad.validator.DateTimeRangeValidator;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.pages.GenericEntry;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.ConfigurationScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.DialogStyleScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;


/**
 */
public class SimpleInputDateRenderer
  extends SimpleInputListOfValuesRenderer
{
  public SimpleInputDateRenderer()
  {
    this(CoreInputDate.TYPE);
  }

  public SimpleInputDateRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _chooseIdKey = type.findKey("chooseId");
  }

  @Override
  protected void queueActionEvent(
    FacesContext context,
    UIComponent  component)
  {
    FacesBean bean = getFacesBean(component);
    // If there's a non-default action, then just launch away
    if (getActionExpression(component, bean) != null)
    {
      super.queueActionEvent(context, component);
    }
    // Otherwise, we'll fall back to launching the default dialog
    // (This should only happen on devices without support for
    // custom windows - everything else would have just launched
    // a calendar window with the _ldp JS function)
    else
    {
      Object submittedValue = getSubmittedValue(component, bean);
      Date date = null;
      try
      {
        Object converted = getConvertedValue(context,
                                             component,
                                             submittedValue);
        if (converted instanceof Date)
          date = (Date) converted;
        else
        {
          GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
          if (fac.isConvertible(converted, Date.class))
            date = (Date) fac.convert(converted, Date.class);
        }
      }
      // Not a big deal;  just means that an invalid value was entered,
      // so we'll launch the dialog showing nothing
      catch (ConverterException ce)
      {
        _LOG.fine(ce);
      }

      RequestContext afContext = RequestContext.getCurrentInstance();
      DateTimeRangeValidator dtrv = _findDateTimeRangeValidator(component, bean);

      if (date == null)
        date = new Date();

      Map<String, Object> parameters = new HashMap<String, Object>();
      parameters.put(XhtmlConstants.VALUE_PARAM, _getDateAsString(date));
      parameters.put(XhtmlConstants.MIN_VALUE_PARAM,
                     dtrv == null
                     ? null :  _getDateAsString(dtrv.getMinimum()));
      parameters.put(XhtmlConstants.MAX_VALUE_PARAM,
                     dtrv == null
                     ? null :  _getDateAsString(dtrv.getMaximum()));
      parameters.put(GenericEntry.getEntryKeyParam(),
                     GenericEntry.CALENDAR_DIALOG_ENTRY);

      afContext.launchDialog(GenericEntry.getGenericEntryViewRoot(context),
                             parameters,
                             component,
                             true,
                             null);
    }
  }

  /**
   * Give subclasses a chance to override the ReturnEvent.
   */
  @Override
  protected void queueReturnEvent(
    FacesContext context,
    UIComponent  component,
    ReturnEvent  event)
  {
    Object returnValue = event.getReturnValue();
    GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();

    // If we got passed a Date object, send it back to String
    // land (where it needs to be for submitted values).
    if ((returnValue instanceof Date) || fac.isConvertible(returnValue, Date.class))
    {
      FacesBean bean = getFacesBean(component);
      Converter converter = getConverter(component, bean);
      if (converter == null)
        converter = getDefaultConverter(context, component, bean);

      if (converter != null)
      {
        returnValue = converter.getAsString(context,
                                            component,
                                            returnValue);
      }
      else
      {
        returnValue = returnValue.toString();
      }

      event = new ReturnEvent(component,
                              returnValue,
                              event.getReturnParameters());
    }

    event.queue();
  }

  @Override
  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {

    if (!_dateScriptletsRegistered)
    {
      // Register our scriptlet
      DateTimeZoneOffsetScriptlet.sharedInstance().registerSelf();
      _dateScriptletsRegistered = true;
    }

    String chooseId = _computeChooseId(context, component, bean);
    rc.getProperties().put(_CACHED_CHOOSE_ID, chooseId);

    // Add the scriptlets required by the date field
    // =-=AEW What's this one?
    XhtmlUtils.addLib(context, rc, "_dfsv()");
    XhtmlUtils.addLib(context, rc, "_fixDFF()");
    XhtmlUtils.addLib(context, rc, _DATE_TIME_ZONE_OFFSET_KEY);
    super.encodeAllAsElement(context, rc, component, bean);

    if (!getDisabled(component, bean))
    {
      _checkIfActive(context, rc, component, _getChooseId(rc));
    }
  }

  @Override
  protected void renderIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Add the two scriptlets required by the icon
    XhtmlUtils.addLib(context,
                      rc,
                      ConfigurationScriptlet.sharedInstance().getScriptletKey());
//    XhtmlUtils.addLib(context,
//                      arc,
//                      _DATE_TIME_ZONE_OFFSET_KEY);

    // Add the dialog styles so the picker window is skinned correctly
    if (CoreRenderKit.usePopupForDialog(context, RequestContext.getCurrentInstance()))
    {
      XhtmlUtils.addLib(context,
          rc,
          DialogStyleScriptlet.sharedInstance().getScriptletKey());
    }

    super.renderIcon(context, rc, component, bean);
  }

  @Override
  protected void renderAfterTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // ADFFACES-317: don't bother rendering the icon if there's
    // an attached chooser
    if (_getChooseId(rc) == null)
      super.renderAfterTextField(context, rc, component, bean);
  }

  /**
   * @todo - should the default style be "short" or "default", which
   * may map to "medium"
   * @todo medium / default is  what it is defaulted to in faces
   */
  @Override
  protected Converter getDefaultConverter(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    Converter converter = context.getApplication().
                             createConverter(DateTimeConverter.CONVERTER_ID);

    // for convenience, we will set the time zone of the converter to that
    // specified by the context or, if that is not present, to the time zone
    // of the server
    if (converter instanceof DateTimeConverter)
    {
      DateTimeConverter dtc = (DateTimeConverter) converter;

      boolean trinidadDTC = _isTrinidadDateTimeConverter(converter);

      if (!trinidadDTC)
      {
        // if it is not the Trinidad DateTimeConverter, set the date style to
        // short
        dtc.setDateStyle("short");
      }

      // if it is not the Trinidad DateTimeConverter or (it is AND
      // no time zone is set) then we want to set the
      // time zone to the one in the faces context or use
      // the default server time zone on the converter
      if (!trinidadDTC || dtc.getTimeZone() == null)
      {
        TimeZone tz = null;

        RequestContext requestContext = RequestContext.getCurrentInstance();
        tz = requestContext.getTimeZone();
        if(tz == null)
        {
          tz = TimeZone.getDefault();
        }

        dtc.setTimeZone(tz);
      }
    }

    return converter;
  }

  @Override
  protected String getOnblur(
    UIComponent component,
    FacesBean   bean)
  {
    String onblur = super.getOnblur(component, bean);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    String chooseId = _getChooseId(arc);

    int length = _BLUR_PREFIX.length() + 4;
    if (chooseId != null)
      length += chooseId.length();

    StringBuilder buffer = new StringBuilder(length);
    buffer.append(_BLUR_PREFIX);

    if (chooseId != null)
    {
      buffer.append(",'");
      buffer.append(chooseId);
      buffer.append("'");
    }

    buffer.append(")");

    return XhtmlUtils.getChainedJS(buffer.toString(), onblur, false);
  }

  @Override
  protected String getOnfocus(
    UIComponent component,
    FacesBean   bean)
  {
    String onfocus = super.getOnfocus(component, bean);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    String chooseId = _getChooseId(arc);

    // The special _dff handler is only needed for date fields
    // connected to a chooser;  the blur handler is needed all the time.
    if (chooseId != null)
    {
      int length = _FOCUS_PREFIX.length() + 4;
      length += chooseId.length();

      StringBuilder buffer = new StringBuilder(length);
      buffer.append(_FOCUS_PREFIX);

      buffer.append(",'");
      buffer.append(chooseId);
      buffer.append("'");

      buffer.append(")");

      return XhtmlUtils.getChainedJS(buffer.toString(), onfocus, false);
    }
    else
    {
      return onfocus;
    }
  }

  @Override
  protected String getLaunchOnclick(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // If the field has an action, use the default behavior.  Or,
    // if the field doesn't support launching a window at all,
    // use the default behavior.
    if ((getActionExpression(component, bean) != null) ||
        !Boolean.TRUE.equals(
            rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_MULTIPLE_WINDOWS)))
      return super.getLaunchOnclick(context, rc, component, bean);

    String id = rc.getCurrentClientId();
    if ((id == null) || (rc.getFormData() == null))
      return null;

    // we want something big enough
    StringBuilder onClickBuffer = new StringBuilder(100);

    onClickBuffer.append("_ldp('");
    onClickBuffer.append(rc.getFormData().getName());
    onClickBuffer.append("','");

    onClickBuffer.append(id);
    onClickBuffer.append('\'');

    // Decide if we should display the picker in a popup or window
    onClickBuffer.append(',');
    onClickBuffer.append(CoreRenderKit.usePopupForDialog(context,
        RequestContext.getCurrentInstance()));

    DateTimeRangeValidator dtrv = _findDateTimeRangeValidator(component, bean);
    if (dtrv != null)
    {
      String minTime = _getDateAsString(dtrv.getMinimum());
      String maxTime = _getDateAsString(dtrv.getMaximum());


      if ((minTime != null) || (maxTime != null))
      {
        onClickBuffer.append(',');
        if (minTime != null)
        {
          onClickBuffer.append(minTime);
        }
        else
        {
          // placeholder for next parameters
          onClickBuffer.append("(void 0)");
        }
      }

      if (maxTime != null)
      {
        onClickBuffer.append(',');
        onClickBuffer.append(maxTime);
      }
    }

    onClickBuffer.append("); return false");

    return onClickBuffer.toString();
  }

  @Override
  protected Integer getDefaultColumns(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    Integer columnsInteger = null;

    Converter converter = getConverter(component, bean);

    // Ignoring the "default" converter code is intentional;  we'll just
    // fall through to _DEFAULT_COLUMNS here to save time
    if (converter instanceof
        org.apache.myfaces.trinidadinternal.convert.DateTimeConverter)
    {
      int columns = ((org.apache.myfaces.trinidadinternal.convert.DateTimeConverter)
              converter).getColumns();
      columnsInteger = (columns);
    }
    else
    {
      columnsInteger = _DEFAULT_COLUMNS;
    }

    return columnsInteger;
  }

  @Override
  protected String getButtonIconName()
  {
    // Currently, date picker is not working for any PDA devices. An issue
    // (TRINIDAD-1203) was created to fix this problem, so until it is fixed
    // let's skip date picker rendering for PDA.
    if (isPDA(RenderingContext.getCurrentInstance()))
      return null;

    return SkinSelectors.AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME;
  }

  protected String getChooseId(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_chooseIdKey));
  }

  @Override
  protected String getSearchDesc(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    if (isInaccessibleMode(arc))
      return null;

    return arc.getTranslatedString(_LAUNCH_PICKER_TIP_KEY);
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputDate";
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputDate::content";
  }

  private String _getChooseId(
    RenderingContext rc)
  {
    return (String) rc.getProperties().get(_CACHED_CHOOSE_ID);
  }

  private String _computeChooseId(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return RenderUtils.getRelativeId(context,
                                     component,
                                     getChooseId(component, bean));

  }

  // Checks to see whether the current dateField should
  // be active, and if so, renders a script that will activate
  // it.
  @SuppressWarnings("unchecked")
  private void _checkIfActive(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    String           chooseId
    ) throws IOException
  {
    if (chooseId == null)
      return;

    String id = getClientId(context, component);

    Map<String, Boolean> activeDateFields = (Map<String, Boolean>)
      rc.getProperties().get(_ACTIVE_DATE_FIELDS_KEY);

    if (activeDateFields == null)
    {
      activeDateFields = new HashMap<String, Boolean>();
      rc.getProperties().put(_ACTIVE_DATE_FIELDS_KEY, activeDateFields);
    }

    // The first dateField that is rendered for each inlineDatePicker
    // is the "active" dateField.  Check to see if we already
    // have an active dateField for this inlineDatePicker.
    if (activeDateFields.get(chooseId) == null)
    {
      // We don't already have an active dateField, so
      // this one is it.  Render the script to activate
      // the dateField.
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", component);
      renderScriptTypeAttribute(context, rc);
      renderScriptDeferAttribute(context, rc);

      writer.writeText("_dfa('", null);
      writer.writeText(id, null);
      writer.writeText("','", null);
      writer.writeText(chooseId, null);
      writer.writeText("');", null);
      writer.endElement("script");

      // Mark the inlineDatePicker as having an active dateField
      activeDateFields.put(chooseId, Boolean.TRUE);
    }
  }

  //
  // Find a DateTimeRangeValidator for use in setting up a
  // minimum and maximum value
  //
  private DateTimeRangeValidator _findDateTimeRangeValidator(
    UIComponent component,
    FacesBean   bean)
  {
    Iterator<Validator> validators = getValidators(component, bean);
    while (validators.hasNext())
    {
      Object validator = validators.next();
      if (validator instanceof DateTimeRangeValidator)
        return (DateTimeRangeValidator) validator;
    }

    return null;
  }

  private static boolean _isTrinidadDateTimeConverter(
    Converter converter)
  {
    return (converter instanceof
            org.apache.myfaces.trinidad.convert.DateTimeConverter);
  }

  /**
   * Stringify the date into canonical form;  we currently
   * use the long integer date.getTime().
   */
  private static String _getDateAsString(
    Date date)
  {
    if (date == null)
      return null;

    return String.valueOf(_adjustTimeZone(date));
  }

  /**
   * Adjust the specified date, which is in server timeZone to the timeZone
   * found in RequestContext and return the new date long value.
   */
  @SuppressWarnings("cast")
  private static long _adjustTimeZone(
    Date date)
  {
    // get the current date of the server
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    long dateValueInMs = calendar.getTimeInMillis();

    // adjust the date; first, get this into GMT
    long tzOffset = calendar.get(Calendar.ZONE_OFFSET) +
                   calendar.get(Calendar.DST_OFFSET);
    // get the timeZone specified in trinidad-config, if any or the
    // client timeZone and find out the difference in timeZone
    TimeZone timeZone = RequestContext.getCurrentInstance().getTimeZone();
    if(timeZone == null)
    {
        timeZone = TimeZone.getDefault();
    }

    // then, adjust for the "local" time zone (either the client's, as
    // specified in RequestContext, or the server's if it wasn't specified
    // in RequestContext)
    tzOffset -= timeZone.getOffset(dateValueInMs);

    // make sure that adjusting to correct timeZone doesn't take the
    // long value out of the range. Calendar too doesn't handle this
    // properly ie. MIN_VALUE < (longValue + tzOffset) < MAX_VALUE.
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
    long adjusted = dateValueInMs + tzOffset;
    return adjusted;
  }

  // this scriptlet is to pass in the time zone raw offset from the locale
  // context in javascript. It will be used in the date picker when
  // we format the date field with time values.
  private static class DateTimeZoneOffsetScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    private DateTimeZoneOffsetScriptlet()
    {
    }

    @Override
    public Object getScriptletKey()
    {
      return _DATE_TIME_ZONE_OFFSET_KEY;
    }

    @Override
    protected void outputScriptletContent(
      FacesContext context,
      RenderingContext arc)
      throws IOException
    {
      // get the tzOffset for the current date. I will compare this with
      // the tzOffset for the current data in javascript, and use it to
      // manipulate the formatted datefield's value so that it shows the

      // localeContext's timeZone, and not the time zone on the browser.
      TimeZone tz = arc.getLocaleContext().getTimeZone();
      int tzOffsetMinutes = tz.getOffset(System.currentTimeMillis())/(1000*60);
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("var _uixLocaleTZ=", null);
      writer.writeText(String.valueOf(tzOffsetMinutes), null);
      writer.writeText(";", null);
    }

    private static final Scriptlet _sInstance =
      new DateTimeZoneOffsetScriptlet();
  }

  private PropertyKey _chooseIdKey;

  private static final Integer _DEFAULT_COLUMNS = Integer.valueOf(10);
  private static final String _BLUR_PREFIX = "_dfb(this";
  private static final String _FOCUS_PREFIX = "_dff(this";

  // RenderingContext property key for the Map which tracks the
  // active date field for each inlineDatePicker
  private static final Object _ACTIVE_DATE_FIELDS_KEY = new Object();

  private static final String _LAUNCH_PICKER_TIP_KEY =
    "af_inputDate.LAUNCH_PICKER_TIP";

  // Key for remembering the cached chooseId
  private static final Object _CACHED_CHOOSE_ID = new Object();

  // name for our scriptlet
  private static final String _DATE_TIME_ZONE_OFFSET_KEY = "dateTimeZoneOffset";
  private static boolean _dateScriptletsRegistered = false;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SimpleInputDateRenderer.class);
}