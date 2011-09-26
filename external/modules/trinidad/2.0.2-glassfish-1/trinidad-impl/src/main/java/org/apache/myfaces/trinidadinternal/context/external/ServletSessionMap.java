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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;


import java.util.Enumeration;

/**
 * HttpSession attibutes as Map.
 *
 * @version $Revision: 382015 $ $Date: 2006-03-01 06:47:11 -0700 (Wed, 01 Mar 2006) $
 */
class ServletSessionMap extends ModifiableAbstractAttributeMap<String, Object>
{
  ServletSessionMap(final HttpServletRequest httpRequest)
  {
    _httpRequest = httpRequest;
  }

  @Override
  protected Object getAttribute(final Object key)
  {
    if (key.toString().equals(key))
    {
      final HttpSession httpSession = _getSession();
      return httpSession == null ? null : httpSession.getAttribute(key.toString());
    }

    return null;
  }

  @Override
  @SuppressWarnings("unchecked")
  protected Enumeration<String> getAttributeNames()
  {
    final HttpSession httpSession = _getSession();
    return httpSession == null ? NullEnumeration.instance() : httpSession.getAttributeNames();
  }

  @Override
  protected void removeAttribute(final Object key)
  {
    if (key.toString().equals(key))
    {
      final HttpSession httpSession = _getSession();
      if (httpSession != null)
      {
        httpSession.removeAttribute(key.toString());
      }
    }
  }

  @Override
  protected void setAttribute(final String key, final Object value)
  {
    _httpRequest.getSession(true).setAttribute(key, value);
  }

  private HttpSession _getSession()
  {
    return _httpRequest.getSession(false);
  }

  private final HttpServletRequest _httpRequest;
}
