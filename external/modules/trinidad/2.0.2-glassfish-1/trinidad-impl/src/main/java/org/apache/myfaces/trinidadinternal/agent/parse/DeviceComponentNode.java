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
package org.apache.myfaces.trinidadinternal.agent.parse;


/**
 * Object that holds information about the device component node in capabilities file
 */
class DeviceComponentNode
{

  /**
   * The User Agent component of device
   */
  public static final Object TYPE_BROWSER = "browser";
  /**
   * The software/os platform of the device
   */
  public static final Object TYPE_PLATFORM = "platform";
  /**
   * The hardware platform of the device
   */
  public static final Object TYPE_HARDWARE = "hardware";
  /**
   * The push component of the device
   */
  public static final Object TYPE_PUSH = "push";
  /**
   * the mms component of the device
   */
  public static final Object TYPE_MMS = "mms";
  /**
   * unknown component
   */
  private static final Object TYPE_UNKNOWN = "unknown";

  public DeviceComponentNode(String type,
                             IncludeNode[] nodesWithRef,
                             IncludeNode[] nodesWithSrc)
  {
    _type = _getType(type);
    _nodesWithRef = nodesWithRef;
    _nodesWithSrc = nodesWithSrc;
  }

  IncludeNode[] __getIncludesByUri()
  {
    return _nodesWithSrc;
  }

  IncludeNode[] __getIncludesByRef()
  {
    return _nodesWithRef;
  }

  Object __getType()
  {
    return _type;
  }


  private Object _getType(String type)
  {
    if (type == null)
      return TYPE_UNKNOWN;

    type = type.intern();
    if ((type == TYPE_BROWSER) ||
        (TYPE_BROWSER.equals(type)))
    {
      return TYPE_BROWSER;
    }
    else if ((type == TYPE_PLATFORM) ||
             (TYPE_PLATFORM.equals(type)))
    {
      return TYPE_PLATFORM;
    }
    else if ((type == TYPE_HARDWARE) ||
             (TYPE_HARDWARE.equals(type)))
    {
      return TYPE_HARDWARE;
    }
    else if ((type == TYPE_MMS) ||
             (TYPE_MMS.equals(type)))
    {
      return TYPE_MMS;
    }
    else if ((type == TYPE_PUSH) ||
             (TYPE_PUSH.equals(type)))
    {
      return TYPE_PUSH;
    }

    return TYPE_UNKNOWN;
  }


  private Object _type;
  private IncludeNode[] _nodesWithRef;
  private IncludeNode[] _nodesWithSrc;
}
