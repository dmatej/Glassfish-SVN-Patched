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
package org.apache.myfaces.trinidadinternal.agent;

import org.apache.myfaces.trinidad.context.Agent;

import java.util.Map;


/**
 * Default base class for Agent
 */
public class DefaultAgent implements Agent
{
  /**
   *
   * @return return the Type of Agent. Returns <code>TYPE_UNKNOWN</code> if not available.
   * <br>E.g. desktop, pda, phone
   *
   */
  public Object getType() {
     return TYPE_UNKNOWN;
  }
  /**
   *
   * @return return the canonical name of the Agent. Returns <code>null</code> if not available.
   * <br>E.g. gecko, ie, opera, pocketie
   *
   */
  public String getAgentName()
  {
    return null;
  }

  /**
   *
   * @return return the version number of the Agent. Return <code>null</code> if not available.
   *
   */
  public String getAgentVersion()
  {
    return null;
  }

  /**
   *
   * @return return the canonical name for the platform. Returns <code>null</code> if not available.
   *  <br>E.g ppc, series60, windows, mac, linux, solaris
   */
  public String getPlatformName()
  {
    return null;
  }


  /**
   *
   * @return return the version number for the platform. Returns <code>null</code> if not available.
   */
  public String getPlatformVersion()
  {
    return null;
  }


  /**
   *
   * @return return a canonical name for the Hardware make and Model. Returns <code>null</code> if not available.
   * <br>E.g nokia6600, sonyericssonP900, nokai3650i
   */
  public String getHardwareMakeModel()
  {
    return null;
  }


  /**
   *
   * @return Map of capability name and value for the current Agent.
   */
  public Map<Object, Object> getCapabilities ()
  {
    return null;
  }  

}
