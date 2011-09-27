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
package org.apache.myfaces.trinidadinternal.webapp;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

@SuppressWarnings("deprecation")
public class ReplaceParametersRequestWrapper extends HttpServletRequestWrapper
{
  public ReplaceParametersRequestWrapper(
    HttpServletRequest    request,
    Map<String, String[]> parameters)
  {
    super(request);
    _parameters = parameters;
  }

  @Override
  public void setCharacterEncoding(String encoding)
  {
    // Do nothing
  }

  @Override
  public String getParameter(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value[0];
  }

  @Override
  public Map<String, String[]> getParameterMap()
  {
    return Collections.unmodifiableMap(_parameters);
  }

  @Override
  public Enumeration<String> getParameterNames()
  {
    return Collections.enumeration(_parameters.keySet());
  }

  @Override
  public String[] getParameterValues(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value.clone();
  }

  private String[] _getParameterValues(String param)
  {
    return _parameters.get(param);
  }

  private Map<String, String[]> _parameters;
}
