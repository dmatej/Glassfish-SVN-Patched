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

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;


/**
 * HttpServletRequest header values (multi-value headers) as Map of String[].
 *
 * @version $Revision: 167257 $ $Date: 2004-10-13 05:51:02 -0600 (Wed, 13 Oct 2004) $
 */
public class ServletRequestHeaderValuesMap extends AbstractAttributeMap<String, String[]>
{
  public ServletRequestHeaderValuesMap(final HttpServletRequest httpServletRequest)
  {
    _httpServletRequest = httpServletRequest;
  }

  @Override
  protected String[] getAttribute(final Object key)
  {
    if (key.toString().equals(key))
    {
      final String k = key.toString();
      String[] ret = _valueCache.get(k);
      if (ret == null)
      {
        @SuppressWarnings("unchecked")
        final Enumeration<String> headers = _httpServletRequest.getHeaders(k);
        ret = _toArray(headers);
        _valueCache.put(k, ret);
      }

      return ret;
    }
    return null;
  }

  @Override
  protected Enumeration<String> getAttributeNames()
  {
    @SuppressWarnings("unchecked")
    Enumeration<String> headerNames = _httpServletRequest.getHeaderNames();
    return headerNames;
  }

  private String[] _toArray(final Enumeration<String> e)
  {
    final List<String> ret = new ArrayList<String>();

    while (e.hasMoreElements())
    {
      ret.add(e.nextElement());
    }

    return ret.toArray(new String[ret.size()]);
  }

  private final HttpServletRequest    _httpServletRequest;
  private final Map<String, String[]> _valueCache = new HashMap<String, String[]>();
}
