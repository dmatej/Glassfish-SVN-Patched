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

import java.io.UnsupportedEncodingException;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Wrapper used to force AJAX requests to be treated as UTF-8,
 * since they all are UTF-8.
 */
@SuppressWarnings("deprecation")
final class XmlHttpServletRequest extends HttpServletRequestWrapper
{
  XmlHttpServletRequest(ServletRequest request)
  {
    super((HttpServletRequest)request);
    
    try
    {
      super.setCharacterEncoding("UTF-8");
    }
    catch (UnsupportedEncodingException uee)
    {
      // Should NEVER happen, since UTF-8 is supported everywhere
      _LOG.severe(uee);
    }
  }
  
  @Override
  public void setCharacterEncoding(String encoding)
  {
    _LOG.fine("Ignoring attempt to set encoding to {0} in an AJAX request",
              encoding);
  }
  
  @Override
  public String getCharacterEncoding()
  {
    return "UTF-8";
  }
  
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(XmlHttpServletRequest.class); 
}
