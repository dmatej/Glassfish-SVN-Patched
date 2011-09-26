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
package org.apache.myfaces.trinidadinternal.config.dispatch;

import javax.faces.context.ExternalContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

@SuppressWarnings("deprecation")
public class DispatchServletResponse extends HttpServletResponseWrapper
{
  public DispatchServletResponse(ExternalContext ec)
  {
    super((HttpServletResponse)ec.getResponse());
    _request = (HttpServletRequest)ec.getRequest();
  }

  @Override
  public void setContentType(
    String contentTypeAndCharset)
  {
    ContentTypeAndCharacterSet ct = new ContentTypeAndCharacterSet(contentTypeAndCharset);    

    // Ignore all calls to setContentType() if they come in the scope
    // of a Servlet include (generally a jsp:include).  The JSP
    // engine will ignore them, and in a .jspx, the default contentType
    // is text/xml!  So as a result, the absence of a contentType
    // in included jspx files was sometimes leading us to turn on
    // XHTML!
    if((_request.getAttribute("javax.servlet.include.request_uri")) == null &&  (ct.getContentType() != null))
    {
      _request.setAttribute(DispatchResponseConfiguratorImpl.__CONTENT_TYPE_KEY, ct.getContentType());
    }
    
    super.setContentType(ct.toString());
  }

   private final HttpServletRequest _request;
}
