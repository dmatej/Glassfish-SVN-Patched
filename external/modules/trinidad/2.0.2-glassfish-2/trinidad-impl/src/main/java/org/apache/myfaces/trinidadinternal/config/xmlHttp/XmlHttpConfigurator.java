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
package org.apache.myfaces.trinidadinternal.config.xmlHttp;

import java.io.IOException;
import java.io.PrintWriter;

import java.lang.reflect.InvocationTargetException;

import java.lang.reflect.Proxy;

import javax.faces.FacesException;
import javax.faces.context.ExternalContext;

import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import javax.servlet.jsp.JspException;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestType;
import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.XmlResponseWriter;

public class XmlHttpConfigurator extends Configurator
{
  public XmlHttpConfigurator()
  {
  }

  public static ServletRequest getAjaxServletRequest(ServletRequest request)
  {
    return new XmlHttpServletRequest(request);
  }
  
  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    if(CoreRenderKit.isPartialRequest(externalContext))
    {    
      StateManagerImpl.reuseRequestTokenForResponse(externalContext);
      
      RequestType type = ExternalContextUtils.getRequestType(externalContext);
      
      switch(type)
      {
        case SERVLET:
          if(ExternalContextUtils.isHttpServletRequest(externalContext))
          {
            externalContext.setResponse(new XmlHttpServletResponse(externalContext));
          }
          break;
        case RESOURCE:
          externalContext = new XmlHttpPortletExternalContext(externalContext);
          externalContext.setResponse(new XmlHttpResourceResponse(externalContext));
      }
    }
    
    return externalContext;
  }
  
  
  /**
   * Sends a <redirect> element to the server
   */
  public static void sendXmlRedirect(final PrintWriter writer, final String url)
    throws IOException
  {
    XmlResponseWriter rw = new XmlResponseWriter(writer, "UTF-8");
    rw.startDocument();
    rw.startElement("partial-response", null);
    rw.startElement("redirect", null);
    rw.writeAttribute("url", url, null);
    rw.endElement("redirect");
    rw.endElement("partial-response");
    rw.endDocument();
    rw.close();
  }

  /**
   * Handle a server-side error by reporting it back to the client.
   * TODO: add configuration to hide this in a production
   * environment.
   */
  public static void handleError(ExternalContext ec, 
                                 Throwable t) throws IOException
  {
    String error = _getErrorString();
    _LOG.severe(error, t);

    ServletResponse response = (ServletResponse)ec.getResponse();
    PrintWriter writer = response.getWriter();
    XmlResponseWriter rw = new XmlResponseWriter(writer, "UTF-8");
    rw.startDocument();
    rw.startElement("partial-response", null);
    rw.startElement("error", null);
    rw.startElement("error-name", null);
    rw.writeText(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, null);
    rw.endElement("error-name");
    rw.startElement("error-message", null);
    rw.writeText(_getExceptionString(t) + _PLEASE_SEE_ERROR_LOG + error, null);
    rw.endElement("error-message");
    rw.endElement("error");
    rw.endElement("partial-response");
    rw.endDocument();
    rw.close();
  }


  static private String _getExceptionString(Throwable t)
  {
    // Unwrap any uninteresting exceptions
    while (_isUninterestingThrowable(t))
    {
      Throwable cause = t.getCause();
      if (cause == null)
        break;
      t = cause;
    }

    String message = t.getMessage();
    if ((message == null) || "".equals(message))
      message = t.getClass().getName();

    return message + "\n\n";
  }

  /**
   * Unwrap a bunch of "uninteresting" throwables
   */
  static private boolean _isUninterestingThrowable(Throwable t)
  {
    // FIXME: add ELException in EE5
    return ((t instanceof ServletException) ||
            (t instanceof JspException) ||
            (t instanceof FacesException) ||
            (t instanceof InvocationTargetException));
  }

  static private String _getErrorString()
  {
    return _PPR_ERROR_PREFIX + _getErrorCount();
  }

  static private synchronized int _getErrorCount()
  {
    return (++_ERROR_COUNT);
  }

  static private final String _PPR_ERROR_PREFIX = "Server Exception during PPR, #";
  // TODO Get this from a resource bundle?
  static private final String _PLEASE_SEE_ERROR_LOG =
    "For more information, please see the server's error log for\n" +
    "an entry beginning with: ";

  static private int _ERROR_COUNT = 0;

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(XmlHttpConfigurator.class);
}
