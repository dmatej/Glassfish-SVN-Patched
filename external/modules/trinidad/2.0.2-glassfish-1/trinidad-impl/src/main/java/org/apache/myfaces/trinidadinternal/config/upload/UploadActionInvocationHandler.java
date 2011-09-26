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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.RenderRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

class UploadActionInvocationHandler
  implements InvocationHandler
{
  private Object _request;
  private Object _response;
  private UploadRequestManager _manager;
  private static final TrinidadLogger _LOG =
     TrinidadLogger.createTrinidadLogger(UploadActionInvocationHandler.class);
  
  public UploadActionInvocationHandler(ExternalContext ec, Map<String, String[]> params)
  {
    _request = ec.getRequest();
    _response = ec.getResponse();
    _manager = new UploadRequestManager(ec, params);
  }

  public Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable
  {
    String methodName = method.getName();
    int paramCount    = method.getParameterTypes().length;
    
    switch(paramCount)
    {
      case 0:
        if("getContentType".equals(methodName))
        {
          return _manager.getContentType();
        }
        else if ("getCharacterEncoding".equals(methodName))
        {
          return _manager.getCharacterEncoding();
        }
        else if("getParameterMap".equals(methodName))
        {
          return _manager.getParameterMap();
        }

        break;
      
      case 1:
        if("setCharacterEncoding".equals(methodName))
        {
          String encoding = (String)args[0];
          //Don't do anything if we are trying to set the character encoding to what is already set
          if (encoding.equals(_manager.getCharacterEncoding()))
          {
            return null;
          }
          
          // It is illegal to set the character encoding after parameters
          // have been retrieved.  This is an annoying restriction,
          // but we shouldn't break it
          if (_manager.isParameterRetrieved())
          {
            _LOG.warning("UNABLE_SET_REQUEST_CHARACTER", encoding);
            return null;
          }
          
          _manager.setCharacterEncoding(encoding);
          
          Method m = _response.getClass().getMethod("setRenderParameters", Map.class);
          m.invoke(_response, _manager.getParameterMap());
          
          // Let the UploadedFiles know, so it can fix up filenames
          UploadedFiles.setCharacterEncoding((RenderRequest)_request, encoding);
          return null;
        }
        else if("getParameterValues".equals(methodName))
        {
          return _manager.getParameterValues((String)args[0]);
        }
        else if("getParameter".equals(methodName))
        {
          return _manager.getParameter((String)args[0]);
        }
    }
    
    return method.invoke(_request, args);
  }
}
