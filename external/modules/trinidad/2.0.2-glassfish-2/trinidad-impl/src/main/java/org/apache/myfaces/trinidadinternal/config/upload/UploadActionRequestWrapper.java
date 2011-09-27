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
package org.apache.myfaces.trinidadinternal.config.upload;

import java.io.UnsupportedEncodingException;

import java.util.Enumeration;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.ActionRequest;
import javax.portlet.ActionResponse;

import javax.portlet.filter.ActionRequestWrapper;

public class UploadActionRequestWrapper
  extends ActionRequestWrapper
{
  public UploadActionRequestWrapper(
      ExternalContext ec,
      Map<String, String[]> params)
  {
    super((ActionRequest)ec.getRequest());
    _response = (ActionResponse)ec.getResponse();
    _manager = new UploadRequestManager(ec, params);
  }

  /**
   * Hide the content type so that no one tries to re-download the
   * uploaded files.
   */
  @Override
  public String getContentType()
  {
    return _manager.getContentType();
  }

  @Override
  public String getCharacterEncoding()
  {
    return _manager.getCharacterEncoding();
  }

  /**
   * Trap calls to setCharacterEncoding() to decode parameters correctly
   */
  @Override
  public void setCharacterEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    if(getCharacterEncoding().equals(encoding))
    {
      return;
    }
    
    _manager.setCharacterEncoding(encoding);
    _response.setRenderParameters(_manager.getParameterMap());

    // Let the UploadedFiles know, so it can fix up filenames
    UploadedFiles.setCharacterEncoding(this, encoding);
  }

  @Override
  public String getParameter(String param)
  {
    return _manager.getParameter(param);
  }

  @Override
  public Map<String, String[]> getParameterMap()
  {
    return _manager.getParameterMap();
  }

  @Override
  public Enumeration<String> getParameterNames()
  {
    return _manager.getParameterNames();
  }

  @Override
  public String[] getParameterValues(String param)
  {
    return _manager.getParameterValues(param);
  }

  private UploadRequestManager _manager;
  private ActionResponse _response;
}
