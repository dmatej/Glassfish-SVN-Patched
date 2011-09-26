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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;

class ContentTypeAndCharacterSet
{
  ContentTypeAndCharacterSet(String contentTypeAndCharset)
  {
    if(contentTypeAndCharset != null)
    {
      Matcher matcher = _CONTENT_TYPE_PATTERN.matcher(contentTypeAndCharset);
      if (matcher.matches())
      {
        _contentType = matcher.group(1);
        _characterSet = (matcher.groupCount() > 1) ? matcher.group(2) : null;

        if ("application/xhtml+xml".equals(_contentType))
        {
          //TODO: Is this still needed in IE7??
          Agent agent = RenderingContext.getCurrentInstance().getAgent();
          if (agent != null && Agent.AGENT_IE.equals(agent.getAgentName()))
          {
            // IE must serve XHTML as text/html
            contentTypeAndCharset = "text/html";

            if (_characterSet != null)
              contentTypeAndCharset += ";charset=" + _characterSet;
          }
        }
      }
    }
    _contentTypeAndCharset = contentTypeAndCharset;
  }

  public String getContentType()
  {
    return _contentType;
  }

  public String getCharacterSet()
  {
    return _characterSet;
  }

  @Override
  public String toString()
  {
    return _contentTypeAndCharset;
  }

  private String _contentType;
  private String _characterSet;
  private String _contentTypeAndCharset;

  static private final Pattern _CONTENT_TYPE_PATTERN =
                                  Pattern.compile("([^;]+)(?:;charset=(.*))?");

}
