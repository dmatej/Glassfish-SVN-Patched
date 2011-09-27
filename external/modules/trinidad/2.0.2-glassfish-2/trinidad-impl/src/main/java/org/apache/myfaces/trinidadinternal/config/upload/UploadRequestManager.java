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

import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;

public class UploadRequestManager
{
  public UploadRequestManager(ExternalContext ec, Map<String, String[]> params)
  {
    _params = params;
    setRequest(ec);
  }

  public UploadRequestManager(HttpServletRequest req, Map<String, String[]> params)
  {
    _params = params;
    setRequest(req);
  }

  /**
   * Hide the content type so that no one tries to re-download the
   * uploaded files.
   */
  public String getContentType()
  {
    return _WWW_FORM_URLENCODED_TYPE;
  }

  public String getCharacterEncoding()
  {
    return _encoding;
  }
  
  /**
   * Trap calls to setCharacterEncoding() to decode parameters correctly
   */
  public void setCharacterEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    // If the encoding is already right, we can bail
    if (encoding.equals(_encoding))
      return;
    
    // Don't call super.setCharacterEncoding() - it's too late
    // and we'll get a warning
    _encoding = encoding;
    
    if (_LOG.isFine())
    {
      _LOG.fine("Switching encoding of wrapper to " + encoding);
    }
    
    Map<String, String[]> extractedParams = _getExtractedParams();

    Map<String, String[]>decodedParams = new HashMap<String, String[]>(extractedParams.size());

    byte[] buffer = new byte[256];

    for(Map.Entry<String, String[]> entry : extractedParams.entrySet())
    {
      String key = entry.getKey();
      key = CaboHttpUtils.decodeRequestParameter(key, encoding, buffer);

      String[] oldValue = entry.getValue();
      int length = oldValue.length;
      String[] newValue = new String[length];
      for (int i = 0; i < length; i++)
      {
        newValue[i] = CaboHttpUtils.decodeRequestParameter(oldValue[i],
                                                           encoding,
                                                           buffer);
        if (_LOG.isFinest())
          _LOG.finest("Parameter " + key + ":" + newValue[i]);
      }

      decodedParams.put(key, newValue);
    }
    _extractedAndDecodedParams = Collections.unmodifiableMap(decodedParams);
  }
  
  public String getParameter(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value[0];
  }

  public Map<String, String[]> getParameterMap()
  {
    // Mark that parameters have been retrieved so we 
    // can log a proper warning
    _parametersRetrieved = true;

    if (_extractedAndDecodedParams != null)
      return _extractedAndDecodedParams;

    return _getExtractedParams();
  }

  public Enumeration<String> getParameterNames()
  {
    return Collections.enumeration(getParameterMap().keySet());
  }

  public String[] getParameterValues(String param)
  {
    String[] value = _getParameterValues(param);
    
    if (value == null)
      return null;

    return value.clone();
  }
  
  public boolean isParameterRetrieved()
  {
    return _parametersRetrieved;
  }
  
  public void setRequest(ExternalContext ec)
  {
    _clearMap();
    _requestParams = ec.getRequestParameterValuesMap();
    _encoding = ec.getRequestCharacterEncoding();    
  }
  
  @SuppressWarnings("unchecked")
  public void setRequest(HttpServletRequest req)
  {
    _clearMap();
    _requestParams = req.getParameterMap();
    _encoding = req.getCharacterEncoding();
  }
  
  private Map<String, String[]> _getExtractedParams()
  {
    if(_extractedParams == null)
    {
      Map<String, String[]> m = new HashMap<String, String[]>(_requestParams);
      m.putAll(_params);
      _extractedParams = Collections.unmodifiableMap(m);
    }
    
    return _extractedParams;
  }

  private String[] _getParameterValues(String param)
  {
    return getParameterMap().get(param);
  }
  
  private void _clearMap()
  {
    _parametersRetrieved = false;
    _extractedAndDecodedParams = null;
    _extractedParams = null;
  }
  
  private boolean _parametersRetrieved = false;
  private Map<String, String[]> _extractedAndDecodedParams;
  private Map<String, String[]> _extractedParams;
  private Map<String, String[]> _params;
  private Map<String, String[]> _requestParams;
  private String _encoding;
  
  private static final String _WWW_FORM_URLENCODED_TYPE =
    "application/x-www-form-urlencoded";

  private static final TrinidadLogger _LOG =
     TrinidadLogger.createTrinidadLogger(UploadRequestManager.class);
}
