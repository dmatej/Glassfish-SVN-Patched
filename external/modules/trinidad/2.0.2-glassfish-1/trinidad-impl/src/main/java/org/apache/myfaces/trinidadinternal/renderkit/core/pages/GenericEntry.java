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
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.render.InternalView;
import org.apache.myfaces.trinidad.render.RenderUtils;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * Generic entry point for all UIX JSPs.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/GenericEntry.java#0 $) $Date: 05-jan-2006.13:18:09 $
 */
public class GenericEntry extends InternalView
{
  static public String getViewId()
  {
    return _GENERIC_ENTRY_VIEW_ID;
  }

  /**
   * Key identifying the color picker entry.
   */
  static public final String COLOR_PICKER_ENTRY    = "cp";


  /**
   * Key identifying the calendar dialog entry.
   */
  static public final String CALENDAR_DIALOG_ENTRY    = "cd";

  /**
   * Key identifying the inline date picker entry.
   */
  static public final String INLINE_DATE_PICKER_ENTRY    = "idp";

  /**
   * Key identifying the "new" frame redirect entry.
   */
  static public final String NEW_FRAME_REDIRECT_ENTRY = "fred";


  public GenericEntry()
  {
  }

  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    return null;
  }

  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    return null;
  }

  @Override
  public void renderView(
    FacesContext context,
    UIViewRoot   viewToRender) throws IOException, FacesException
  {
    String entryKey = (String) context.getExternalContext().getRequestParameterMap().get(__ENTRY_KEY_PARAM);
    if (entryKey == null)
    {
      RequestContext requestContext = RequestContext.getCurrentInstance();
      if (requestContext != null)
      {
        Object o = requestContext.getPageFlowScope().get(__ENTRY_KEY_PARAM);
        if (o != null)
          entryKey = o.toString();
      }
    }

    service(context,entryKey);
  }


  /*
  public static boolean isGenericEntryPage(String viewId)
  {
    if (viewId == null)
      return false;

    // In theory, this should be just "equals()", not starts-with;  but
    // extension-mapped views don't have the view ID you'd expect - it
    // includes the extension
    return viewId.startsWith(_GENERIC_ENTRY_VIEW_ID);
  }


  public static boolean isGenericEntryPage(FacesContext context)
  {
    if (context.getViewRoot() == null)
      return false;

    return isGenericEntryPage(context.getViewRoot().getViewId());
  }
  */

  public static String getEntryKeyParam()
  {
    return __ENTRY_KEY_PARAM;
  }

  public static UIViewRoot getGenericEntryViewRoot(FacesContext context)
  {
    ViewHandler viewHandler = context.getApplication().getViewHandler();
    UIViewRoot uvr = viewHandler.createView(context, _GENERIC_ENTRY_VIEW_ID);
    return uvr;
  }

  public static String getGenericEntryURL(
    FacesContext  context,
    String        jspName)
  {
    String name = getGenericEntryPath(context);
    StringBuffer sb = new StringBuffer(name.length() +
                                       1 +
                                       __ENTRY_KEY_PARAM.length() +
                                       1 +
                                       jspName.length());
    sb.append(name);
    if (name.indexOf('?') < 0)
      sb.append('?');
    else
      sb.append('&');

    sb.append(__ENTRY_KEY_PARAM);
    sb.append('=');
    sb.append(jspName);

    return sb.toString();
  }

  static public String getGenericEntryPath(FacesContext context)
  {
    String url = context.getApplication().getViewHandler().
      getActionURL(context, _GENERIC_ENTRY_VIEW_ID);

    return url;
  }


  @SuppressWarnings("unchecked")
  static private void service(FacesContext context, String name)
    throws IOException
  {
    if (_processReturnDialog(context,name))
    {
      return;
    }

    HttpServletResponse response =
      (HttpServletResponse) context.getExternalContext().getResponse();

    if (!_isValidEntry(name))
    {
      response.sendError(HttpServletResponse.SC_BAD_REQUEST);
      return;
    }

    // Only use "text/html" for now, even if the browser thinks
    // it supports "text/html".
    String contentType = "text/html";
    String encoding = JspUtils.getEncoding(context, "UTF-8");

    if (encoding == null)
      response.setContentType(contentType);
    else
      response.setContentType(contentType +
                              "; charset=" + encoding);

    // =-=AEW Getting the Writer straight off of the ServletResponse
    // means losing buffered and unsynchronized goodness
    ResponseWriter responseWriter = context.getRenderKit().createResponseWriter(
      response.getWriter(),
      contentType,
      encoding);
    context.setResponseWriter(responseWriter);

    Map<String, String> requestParams = 
      context.getExternalContext().getRequestParameterMap();
    
    String localeName = requestParams.get("loc");
    if (localeName != null)
    {
      Locale locale =
        LocaleUtils.getLocaleForIANAString(localeName);
      // Push the locale into JSF land
      context.getViewRoot().setLocale(locale);
    }

    _service(context, name);
  }


  static private void _service(
    FacesContext context,
    String jspName) throws IOException
  {
    // Prepare the view tree
    if (NEW_FRAME_REDIRECT_ENTRY.equals(jspName))
      FredJSP.service(context);
    else if (CALENDAR_DIALOG_ENTRY.equals(jspName))
      CalendarDialogJSP.service(context);
    else if (INLINE_DATE_PICKER_ENTRY.equals(jspName))
      InlineDatePickerJSP.service(context);
    else if (COLOR_PICKER_ENTRY.equals(jspName))
      ColorPickerJSP.service(context);
    else
    {
      assert(false);
    }

    // And render
    ResponseWriter rw = context.getResponseWriter();
    rw.startDocument();
    RenderUtils.encodeRecursive(context, context.getViewRoot());
    rw.endDocument();
  }

  static private boolean _processReturnDialog(
    FacesContext context,
    String jspName)
  {
    if (CALENDAR_DIALOG_ENTRY.equals(jspName))
      return CalendarDialogJSP.processReturnDialog(context);
    else
      return false;
  }


  static private boolean _isValidEntry(String name)
  {
    return _VALID_ENTRIES.contains(name);
  }




  //
  // Parameter that is used to store the key for the JSP name.
  //
  static final String __ENTRY_KEY_PARAM = "_t";
  static private final Set<String> _VALID_ENTRIES;

  static
  {
    _VALID_ENTRIES = new HashSet<String>();
    _VALID_ENTRIES.add(NEW_FRAME_REDIRECT_ENTRY);
    _VALID_ENTRIES.add(CALENDAR_DIALOG_ENTRY);
    _VALID_ENTRIES.add(INLINE_DATE_PICKER_ENTRY);
    _VALID_ENTRIES.add(COLOR_PICKER_ENTRY);
  }

  static private final String _GENERIC_ENTRY_VIEW_ID = "/__ADFv__";
}
