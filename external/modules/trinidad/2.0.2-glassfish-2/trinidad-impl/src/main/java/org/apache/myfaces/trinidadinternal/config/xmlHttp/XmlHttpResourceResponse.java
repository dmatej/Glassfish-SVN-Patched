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
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.faces.context.ExternalContext;

import javax.portlet.ResourceResponse;
import javax.portlet.filter.ResourceResponseWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

public class XmlHttpResourceResponse
  extends ResourceResponseWrapper
{
  public XmlHttpResourceResponse(ExternalContext ec)
  {
    super((ResourceResponse) ec.getResponse());

    _contentType = "text/xml;charset=utf-8";

    // must set contentType here since
    // setContentType is ignored when inside an included page (bug 5591124)
    super.setContentType(_contentType);
  }

  @Override
  public OutputStream getPortletOutputStream()
    throws IOException
  {
    OutputStream base = super.getPortletOutputStream();
    return new XmlOutput(base).getOutputStream();
  }

  @Override
  public PrintWriter getWriter()
    throws IOException
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

  private String _contentType = null;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(XmlHttpServletResponse.class);
}
