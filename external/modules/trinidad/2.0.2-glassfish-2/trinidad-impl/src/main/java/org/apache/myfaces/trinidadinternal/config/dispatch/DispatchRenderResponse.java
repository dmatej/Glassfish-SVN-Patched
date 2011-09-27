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
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;
import javax.portlet.filter.RenderResponseWrapper;

/**
 * This class is for Portlet 2.0 compatibility and it uses the real RenderResponseWrapper
 */
@SuppressWarnings("deprecation")
class DispatchRenderResponse extends RenderResponseWrapper
{
  public DispatchRenderResponse(ExternalContext ec)
  {
    super((RenderResponse)ec.getResponse());
     _request = (RenderRequest)ec.getRequest();
  }
  
  @Override
  public void setContentType(String contentTypeAndCharset)
  {
    ContentTypeAndCharacterSet ct = new ContentTypeAndCharacterSet(contentTypeAndCharset);
    
    if(ct.getContentType() != null)
    {
      _request.setAttribute(DispatchResponseConfiguratorImpl.__CONTENT_TYPE_KEY, ct.getContentType());
    }
    super.setContentType(ct.toString());
  }

  private final RenderRequest _request;
}
