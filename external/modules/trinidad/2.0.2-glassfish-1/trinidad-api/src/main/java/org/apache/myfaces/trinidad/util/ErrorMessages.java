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
package org.apache.myfaces.trinidad.util;

import java.io.Serializable;

// CLASS  for way of sending back the translated string for 
// given key. There are two messages for the key. One for the given 
// key and other of key_detail.
class ErrorMessages implements Serializable
{
  ErrorMessages(String message, String detailMessage)
  {
    _message = message;
    _detailMessage = detailMessage;    
  }
  
  public String getMessage()
  {
    return _message;
  }
  
  public String getDetailMessage()
  {
    return _detailMessage;
  }
  
  private String _message;
  private String _detailMessage;
  private static final long serialVersionUID = 1L;
}
