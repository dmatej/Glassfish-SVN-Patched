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

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXPoll;
import org.apache.myfaces.trinidad.component.core.CorePoll;
import org.apache.myfaces.trinidad.component.core.nav.CoreGoButton;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.PollEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PollRenderer.java#0 $) $Date: 10-nov-2005.18:55:33 $
 */
public class PollRenderer extends XhtmlRenderer
{
  public PollRenderer()
  {
    super(CorePoll.TYPE);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();

    Object event = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.POLL_EVENT.equals(event))
    {
      Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
      String id = clientId == null ? component.getClientId(facesContext) : clientId;

      if (id.equals(source))
      {
        // This component always uses PPR (unless not supported at all)
        PartialPageUtils.forcePartialRendering(facesContext);

        // And forcibly re-render ourselves - because that's how
        // we get the poll re-started
        RequestContext.getCurrentInstance().addPartialTarget(component);

        UIXPoll poll = (UIXPoll) component;
        (new PollEvent(component)).queue();
        if (poll.isImmediate())
          facesContext.renderResponse();
      }
    }
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _intervalKey = type.findKey("interval");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    XhtmlUtils.addLib(context, rc, "PollManager()");
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", component);
    renderId(context, component);
    // A page should not auto-refresh while in screen-reader mode.
    // So only render the setTimeout script if we are in auto-refresh mode.
    if (_isAutoRefreshMode(rc))
    {
      _renderPollingScript(context, rc, component, bean);
    }
    else
    {
      _renderManualRefresh(context, rc, component, bean);
    }
    rw.endElement("span");
  }


  /**
   * A page should not auto-refresh while in screen-reader mode.
   * In this case, we create a button which when clicked will send the
   * "poll" event. This is instead of the setTimeout javascript function,
   * which we render when not in screen-reader mode.
   */
  private void _renderManualRefresh(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String elementID = getClientId(context, component);
    boolean isPartial = PartialPageUtils.supportsPartialRendering(rc);
    String onclick = _getScriptContents(rc, component, bean, elementID, isPartial, false);

    CoreGoButton goButton = new CoreGoButton();
    goButton.setOnclick(onclick);
    goButton.setText(rc.getTranslatedString("af_poll.MANUAL"));

    goButton.encodeBegin(context);
    goButton.encodeEnd(context);
  }

  /**
   * Renders a script which sends a 'poll' event after a timeout.
   */
  private void _renderPollingScript(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String elementID = getClientId(context, component);
    boolean isPartial = PartialPageUtils.supportsPartialRendering(rc);
    String buffer =
      _getScriptContents(rc, component, bean, elementID, isPartial, true);

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("script", component);
    renderScriptTypeAttribute(context, rc);
    renderScriptDeferAttribute(context, rc);
    rw.writeText(buffer, null);
    rw.endElement("script");
  }

  protected int getInterval(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_intervalKey);
    if (o == null)
    {
      o = _intervalKey.getDefault();
    }

    if (o instanceof Number)
      return ((Number) o).intValue();

    return _POLL_INTERVAL_DEFAULT;
  }

  /**
  * returns a String which is the entire script to be rendered.
  * extract the attributes of the poll element and build
  * a script.
  * the javascript function we build depends on whether the page is refreshed
  * full or partially, and whether or not it is in a form and whether or
  * not it should be auto-refreshed. For auto-refresh, then
  * the script is a setTimeout script. For manual-refresh
  * this will be the same script we render,
  * but minus the setTimeout call. In manual-refresh mode
  * the user will need to click on a button
  * to send the 'poll' event
  */
  private String _getScriptContents(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           elementID,
    boolean          isPartial,
    boolean          isAutoRefreshMode
    )
  {
    // We will build a script like one of the following:
    // (these examples assume isAutoRefreshMode is false,
    // if isAutoRefreshMode is true, these commands are registered in the
    // _TrPollManager javascript object, which will manage executing this commands
    // at the specified pollInterval.)
    //
    // Partial page refresh when the poll component is inside of a form:
    // "_submitPartialChange('formName',0,{event:'poll',source:'pollingWidgetId',
    //    partialTargets:'pollingWidgetId', partial='true'})")
    // Full page refresh when the poll component is inside of a form:
    // "submitForm('formName',0,{event:'poll', source:'pollingWidgetId'}),
    //    timeoutMS)
    /* -------------------------------------------------------- */

    // get variables needed to create the javascript function
    int pollInterval = 0;
    if (isAutoRefreshMode)
    {
      pollInterval = getInterval(component, bean);
    }

    if (rc.getFormData() == null)
    {
      _LOG.warning("POLL_COMPONENT_MUST_INSIDE_FORM", elementID);
      return null;
    }

    String startScript = _getStartScript(isPartial,
                                   isAutoRefreshMode,
                                   elementID);
    String argumentString = _getArgumentString(elementID,
                                               rc.getFormData().getName(),
                                               isPartial);
    int length = _getScriptBufferLength(startScript,
                                        argumentString,
                                        pollInterval,
                                        isAutoRefreshMode);

    // build the script:
    StringBuilder builder = new StringBuilder(length);

    builder.append(startScript);

    //pu: In manual-refresh mode, submit is through onclick of button, not an
    //  auto-scheduled command string, hence no escaping "'" in arguments.
    if (_isAutoRefreshMode(rc))
    {
      builder.append(XhtmlUtils.escapeJS(argumentString));
    }
    else
    {
      builder.append(argumentString);
    }

    // auto-refresh is not allowed in accessible mode.
    // So render the pollInterval portion only if in auto-refresh mode.
    if (isAutoRefreshMode)
    {
      builder.append(_MIDDLE_SCRIPT_AUTO_REFRESH);
      builder.append(pollInterval);
    }

    builder.append(_END_SCRIPT);

    return builder.toString();
  }


  /**
  * Computes the length of the buffer that is needed to build up the script
  */
  private static int _getScriptBufferLength(
    String  startScript,
    String  argumentString,
    Integer pollInterval,
    boolean isAutoRefreshMode
    )
  {
    if ((startScript == null) || (argumentString == null))
    {
      return 0;
    }

    int length =  startScript.length() +
                  argumentString.length() +
                  _MIDDLE_SCRIPT_AUTO_REFRESH.length() +
                  _END_SCRIPT.length();

    if (isAutoRefreshMode && (pollInterval != null))
    {
     length +=  pollInterval.toString().length() + 3;
    }

    return length;
  }

  /**
  * Returns the start script. The start script depends on whether the poll
  * element is within a form and whether the page is in PPR mode and whether
  * the page is in auto-refresh mode.
  */
  private static String _getStartScript(
    boolean isPartial,
    boolean isAutoRefreshMode,
    String elementID
    )
  {
    String startScript = "";
    if (isAutoRefreshMode)
    {
      startScript = _PRE_START_SCRIPT_AUTO_REFRESH;
      startScript += elementID;
      startScript += "\", \"";
    }

    if (isPartial)
    {
      startScript += _START_SCRIPT_PARTIAL_FORM;
    }
    else
    {
      startScript += _START_SCRIPT_FULL_FORM;
    }

    return startScript;
  }

  /**
   * Returns a String which will be used as the JavaScript arguments: either
   * a URL with event information or the needed arguments to
   * submitForm/_submitPartialChange.
   * e.g., if within a form and PPR, the argument string is:
   * 'myform',0,{event:'poll',source:'polling-widget'}",
  */
  private static String _getArgumentString(
    String  elementID,
    String  formName,
    boolean isPartial
    )
  {
    StringBuilder buffer = new StringBuilder(60);
    buffer.append("'");
    buffer.append(formName);
    // 0 means do not validate
    buffer.append("',0,");
    buffer.append("{" +
                  XhtmlConstants.EVENT_PARAM + ":\'" +
                  XhtmlConstants.POLL_EVENT + "\'," +
                  XhtmlConstants.SOURCE_PARAM + ":\'");
    buffer.append(elementID);
    buffer.append("\'}");
    return buffer.toString();
  }

  /**
   * return true if this element should poll automatically
   * rather than manually.
   */
  private boolean _isAutoRefreshMode(
    RenderingContext rc
    )
  {
    // auto poll when NOT in screen-reader mode.
    return (!isScreenReaderMode(rc));
  }

  private PropertyKey _intervalKey;

  // script constants
  // Script for initializing the auto mode for the command
  private static final String _PRE_START_SCRIPT_AUTO_REFRESH =
  " if (!self._trPollManager) _trPollManager = new _TrPollManager(); _trPollManager.addAndActivate(\"";

  private static final String _START_SCRIPT_PARTIAL_FORM =
    "_submitPartialChange(";
  private static final String _START_SCRIPT_FULL_FORM =
    "submitForm(";
  private static final String _END_SCRIPT     = ");";
  private static final String _MIDDLE_SCRIPT_AUTO_REFRESH  = ")\", ";

  private static final int _POLL_INTERVAL_DEFAULT = 5000;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PollRenderer.class);
}
