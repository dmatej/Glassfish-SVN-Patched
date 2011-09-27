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
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;


/**
 * HttpServletRequest Cookies as Map.
 *
 * @version $Revision: 278654 $ $Date: 2005-09-04 18:32:35 -0600 (Sun, 04 Sep 2005) $
 */
public class ServletCookieMap extends AbstractAttributeMap<String, Object>
{
  public ServletCookieMap(final HttpServletRequest httpServletRequest)
  {
    _httpServletRequest = httpServletRequest;
  }

  @Override
  public boolean containsKey(final Object key)
  {
    final Cookie[] cookies = _httpServletRequest.getCookies();
    if (cookies == null)
    {
      return false;
    }
    for (Cookie element : cookies)
    {
      if (element.getName().equals(key))
      {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean containsValue(final Object findValue)
  {
    if (findValue == null)
    {
      return false;
    }

    final Cookie[] cookies = _httpServletRequest.getCookies();
    if (cookies == null)
    {
      return false;
    }
    for (Cookie element : cookies)
    {
      if (findValue.equals(element))
      {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean isEmpty()
  {
    final Cookie[] cookies = _httpServletRequest.getCookies();
    return cookies == null || cookies.length == 0;
  }

  @Override
  public int size()
  {
    final Cookie[] cookies = _httpServletRequest.getCookies();
    return cookies == null ? 0 : cookies.length;
  }

  @Override
  protected Object getAttribute(final Object key)
  {
    final Cookie[] cookies = _httpServletRequest.getCookies();
    if (cookies == null)
    {
      return null;
    }
    for (Cookie element : cookies)
    {
      if (element.getName().equals(key))
      {
        return element;
      }
    }

    return null;
  }

  @Override
  protected Enumeration<String> getAttributeNames()
  {
    final Cookie[] cookies = _httpServletRequest.getCookies();
    if (cookies == null)
    {
      return new CookieNameEnumeration(EMPTY_ARRAY);
    }
    else
    {
      return new CookieNameEnumeration(cookies);
    }
  }

  final HttpServletRequest      _httpServletRequest;
  private static final Cookie[] EMPTY_ARRAY = new Cookie[0];

  private static class CookieNameEnumeration implements Enumeration<String>
  {
    public CookieNameEnumeration(final Cookie[] cookies)
    {
      _cookies = cookies;
      _length = cookies.length;
    }

    public boolean hasMoreElements()
    {
      return _index < _length;
    }

    public String nextElement()
    {
      return _cookies[_index++].getName();
    }

    private final Cookie[] _cookies;
    private int            _index;
    private final int      _length;
  }
}
