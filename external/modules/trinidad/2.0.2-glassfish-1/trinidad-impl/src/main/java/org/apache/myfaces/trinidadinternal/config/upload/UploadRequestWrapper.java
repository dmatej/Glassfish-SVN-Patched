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
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.ActionResponse;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;

/**
 * Request wrapper class that hooks in parameters identified in
 * the servlet request.
 *
 */
// TODO Stop going String -> bytes -> String;  change MultipartFormHandler
//    to simply extract byte arrays, and do all the type conversion here.
@SuppressWarnings("deprecation")
public class UploadRequestWrapper extends HttpServletRequestWrapper
{
  public UploadRequestWrapper(ExternalContext ec, Map<String, String[]> params)
  {
    this((HttpServletRequest) ec.getRequest(), params);
    
  }
  
  public UploadRequestWrapper(HttpServletRequest req, Map<String, String[]> params)
  {
    super(req);
    _manager = new UploadRequestManager(req, params);
    
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
    if (getCharacterEncoding().equals(encoding))
    {
      return;
    }
    
    // It is illegal to set the character encoding after parameters
    // have been retrieved.  This is an annoying restriction,
    // but we shouldn't break it
    if (_manager.isParameterRetrieved())
    {
      _LOG.warning("UNABLE_SET_REQUEST_CHARACTER", encoding);
      return;
    }
    
    _manager.setCharacterEncoding(encoding);

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
  private static final TrinidadLogger _LOG =
     TrinidadLogger.createTrinidadLogger(UploadRequestWrapper.class);
}
