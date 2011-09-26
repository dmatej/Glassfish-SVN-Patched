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
package org.apache.myfaces.trinidadinternal.context.external;

import java.util.Enumeration;

import javax.servlet.ServletRequest;


/**
 * ServletRequest parameters as Map.
 *
 * @version $Revision: 167257 $ $Date: 2004-10-13 05:51:02 -0600 (Wed, 13 Oct 2004) $
 */
public class ServletRequestParameterMap extends AbstractAttributeMap<String, String>
{
  public ServletRequestParameterMap(final ServletRequest servletRequest)
  {
    _servletRequest = servletRequest;
  }

  @Override
  protected String getAttribute(final Object key)
  {
    if (key.toString().equals(key))
    {
      return _servletRequest.getParameter(key.toString());
    }
    return null;
  }

  @Override
  protected Enumeration<String> getAttributeNames()
  {
    @SuppressWarnings("unchecked")
    Enumeration<String> parameterNames = _servletRequest.getParameterNames();
    return parameterNames;
  }

  private final ServletRequest _servletRequest;
}
