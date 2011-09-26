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

import javax.faces.context.ExternalContext;

import javax.servlet.ServletOutputStream;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.XmlResponseWriter;

@SuppressWarnings("deprecation")
final class XmlHttpServletResponse extends HttpServletResponseWrapper
{
  XmlHttpServletResponse(ExternalContext ec)
  {
    super((HttpServletResponse)ec.getResponse());
    
    _contentType = "text/xml;charset=utf-8";
    
    // must set contentType here since
    // setContentType is ignored when inside an included page (bug 5591124)    
    this.setContentType(_contentType);
  }
  
  @Override
  public void sendRedirect(
    final String url) throws IOException
  {
    XmlHttpConfigurator.sendXmlRedirect(getWriter(), url);
  }
  
  @Override
  public ServletOutputStream getOutputStream()
    throws IOException
  {
    ServletOutputStream base = super.getOutputStream();
    return new XmlOutput(base).getOutputStream();
  }

  @Override
  public PrintWriter getWriter() throws IOException
  {
    PrintWriter base = super.getWriter();
    return new XmlOutput(base).getWriter();
  }

  @Override
  public void setContentType(final String type) 
  {
    // the reason we're using XmlHttpServletResponse is because
    // we're producing a ppr xml response, so ignore any 
    // attempts to set the contentType, since the contentType
    // must be text/xml:
    _LOG.finer("ignoring setContentType:{0}", type);
    super.setContentType(_contentType);
  }
  
  @Override
  public void sendError(final int sc) throws IOException
  {
    sendError(sc, null);
  }

  @Override
  public void sendError(final int sc, final String string) throws IOException
  {
    PrintWriter writer = getWriter();
    XmlResponseWriter rw = new XmlResponseWriter(writer, "UTF-8");
    rw.startDocument();
    rw.startElement("error", null);
    rw.writeAttribute("status", sc, null);
    rw.writeText(string, null);
    rw.endElement("error");
    rw.endDocument();
    rw.close();    
  }
  
  private String _contentType = null;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(XmlHttpServletResponse.class); 
}
