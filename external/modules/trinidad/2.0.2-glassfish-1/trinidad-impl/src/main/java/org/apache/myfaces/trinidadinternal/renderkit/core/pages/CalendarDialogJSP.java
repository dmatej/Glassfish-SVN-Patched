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

import java.io.IOException;
import java.util.Map;
import java.util.Date;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.component.core.CoreImportScript;
import org.apache.myfaces.trinidad.component.core.input.CoreChooseDate;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroupLayout;
import org.apache.myfaces.trinidad.component.core.nav.CoreGoButton;
import org.apache.myfaces.trinidad.component.core.output.CoreSpacer;
import org.apache.myfaces.trinidad.component.html.HtmlRowLayout;
import org.apache.myfaces.trinidad.component.html.HtmlTableLayout;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.url.EncoderUtils;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;


/**
 * Entry point for the "calendarDialog" JSP.
 * <p>
 * Parameters:
 * <ul>
 * <li>Java locale to use
 * <li>minValue, maxValue: min and max dates
 * <li>value: current value
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/CalendarDialogJSP.java#0 $) $Date: 10-nov-2005.19:03:32 $
 */
class CalendarDialogJSP
{

  /**
   * @todo Integrate the time zone with RequestContext (somehow)
   */
  @SuppressWarnings("unchecked")
  static public void service(FacesContext context)
    throws IOException
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    CoreDocument doc = new CoreDocument();
    context.getViewRoot().getChildren().add(doc);
    doc.setTitle(arc.getTranslatedString(_DIALOG_TITLE_KEY));

    CoreForm form = new CoreForm();
    doc.getChildren().add(form);
    form.setId("a");

    boolean isDesktop = (arc.getAgent().getType().equals(Agent.TYPE_DESKTOP));

    //
    // Create the calendar row
    //
    UIComponent calendarRow = _createRow(isDesktop,
                                         XhtmlConstants.H_ALIGN_END);
    CoreChooseDate ccd = CalendarUtils.createChooseDate(context);
    calendarRow.getChildren().add(ccd);

    // Get a destination that points back to this dialog, including
    // any viewIDs
    String destination =
      GenericEntry.getGenericEntryURL(context,
                                      GenericEntry.CALENDAR_DIALOG_ENTRY);
    ccd.getAttributes().put("destination", destination);

    //
    // Create a spacer
    //
    UIComponent spacerRow = _createRow(isDesktop, null);
    CoreSpacer cos = new CoreSpacer();
    spacerRow.getChildren().add(cos);
    cos.setHeight("8");

    //
    // Create the button row
    //
    UIComponent buttonRow = _createRow(isDesktop, XhtmlConstants.H_ALIGN_END);

    CoreGoButton cancelButton =
       JspUtils.createGoButton(arc, _CANCEL_LABEL_KEY);
    buttonRow.getChildren().add(cancelButton);

    Object cap = arc.getAgent().getCapabilities().get(
                     TrinidadAgent.CAP_MULTIPLE_WINDOWS);
    boolean multWindowsSupported = Boolean.TRUE.equals( cap );
    if (multWindowsSupported )
    {
      cancelButton.setOnclick("return _doCancel()");
    }
    else
    {
      // create the destination of the cancel button
      StringBuffer cancelDest = new StringBuffer();
      EncoderUtils.appendURLArguments(cancelDest,
                                      destination,
                                      new String[]{
                                        XhtmlConstants.EVENT_PARAM,
                                        XhtmlConstants.CANCEL_EVENT
                                      });

      // Prepend a slash, because this destination already
      // includes the context root
      cancelButton.setDestination("/" + cancelDest.toString());

      String value = __getParam(context, XhtmlConstants.VALUE_PARAM);
      if (value != null)
      {
        long lg = Long.parseLong(value);
        ccd.getAttributes().put("value", new Date(lg));
      }

      // get the scrolled value
      String month = __getParam(context,XhtmlConstants.MONTH_PARAM);
      String year = __getParam(context,XhtmlConstants.YEAR_PARAM);

      // try to create scrolledValue
      if ( month != null && year != null )
      {
        try
        {
          long scrolledValue = Long.parseLong(month) + Long.parseLong(year);


          ccd.getAttributes().put("scrolledValue", new Date(scrolledValue));
        }
        catch ( NumberFormatException nfe){;}
      }
    }

    if (multWindowsSupported )
    {
      CoreImportScript cis = new CoreImportScript();
      cis.setNames(new String[]{"_doCancel()", "_selectDate()"});
      doc.getFacets().put(CoreDocument.META_CONTAINER_FACET, cis);

      HtmlTableLayout htl = new HtmlTableLayout();
      form.getChildren().add(htl);

      //
      // Add the rows to the stacker
      //
      htl.getChildren().add(calendarRow);
      htl.getChildren().add(spacerRow);
      htl.getChildren().add(buttonRow);
    }
    else
    {
      CorePanelGroupLayout cpg = new CorePanelGroupLayout();
      form.getChildren().add(cpg);

      cpg.setLayout(CorePanelGroupLayout.LAYOUT_VERTICAL);
      cpg.getChildren().add(calendarRow);
      cpg.getChildren().add(spacerRow);
      cpg.getChildren().add(buttonRow);
    }
  }


  @SuppressWarnings("unchecked")
  static public boolean processReturnDialog(
    FacesContext context
    )
  {
    Map<String, String> requestParameters = 
      context.getExternalContext().getRequestParameterMap();
    
    String event = requestParameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.DATE_EVENT.equals(event))
    {
      String value = requestParameters.get(XhtmlConstants.VALUE_PARAM);
      Date date;
      try
      {
        long millis = Long.parseLong(value);
        date = new Date( millis );
}
      catch (NumberFormatException nfe)
      {
        date = new Date();
      }

      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.returnFromDialog(date, null);
      return true;
    }
    else if (XhtmlConstants.CANCEL_EVENT.equals(event))
    {
      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.returnFromDialog(null, null);
      return true;
    }
    return false;
  }


  @SuppressWarnings("unchecked")
  static String __getParam(
    FacesContext context,
    String name
    )
  {
    Map<String, String> requestParams = 
      context.getExternalContext().getRequestParameterMap();
    
    String value = requestParams.get(name);
    if (value == null || value.equals(""))
    {
      RequestContext afContext = RequestContext.getCurrentInstance();
      Object o = afContext.getPageFlowScope().get(name);
      if (o != null)
        value = o.toString();
    }

    return value;
  }

  static private UIComponent _createRow(
    boolean isDesktop,
    String  halign)
  {
    if (!isDesktop)
      return new CorePanelGroupLayout();

    HtmlRowLayout hrl = new HtmlRowLayout();
    if (halign != null)
      hrl.setHalign(halign);
    return hrl;
  }

  static private final String _CANCEL_LABEL_KEY =
    "af_chooseDate.CANCEL";
  static private final String _DIALOG_TITLE_KEY =
    "af_chooseDate.DIALOG_TITLE";
}
