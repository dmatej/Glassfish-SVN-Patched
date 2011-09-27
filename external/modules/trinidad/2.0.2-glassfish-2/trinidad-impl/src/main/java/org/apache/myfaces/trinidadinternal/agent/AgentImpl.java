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

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * The implementation of agent interface
 * <p></p>
 * This implementation supports agents recognized by all uix22
 * This class returns name strings (instead of int's)
 * Certain agents/platforms have been renamed
 * - using "webkit" (instead of safari), on recommendation from uix team
 * - using "gecko" for all gecko based browsers
 * - using "ppc" (instead of windows) for platform
 * <p></p>
 *
 */

public class AgentImpl extends DefaultAgent
{

  public AgentImpl()
  {
    this(false);
  }

  public AgentImpl(boolean nullAgentEntry)
  {
    if (nullAgentEntry)
    {
      _LOG.warning("UNKNOWN_AGENT_TYPE_CREATE_WITH_NULL");
      _entry = _NULL_AGENT_ENTRY;
    }
    else
    {
      _entry = new AgentEntry();
    }
  }

/*  public AgentImpl(String userAgent, String accept)
  {
    _entry = _getAgentEntry(userAgent, accept);
  }
  */
  @Override
  public Object getType()
  {
    return _entry._type;
  }

  @Override
  public String getAgentName()
  {
    return _entry._agent;
  }

  @Override
  public String getAgentVersion()
  {
    return _entry._agentVersion;
  }

  @Override
  public String getPlatformName()
  {
    return _entry._platform;
  }

  @Override
  public String getPlatformVersion()
  {
    return _entry._platformVersion;
  }

  @Override
  public String getHardwareMakeModel()
  {
    return _entry._makeModel;
  }

  @Override
  public Map<Object, Object> getCapabilities()
  {
    return _requestCapabilities;
  }

  //setter methods for AgentImpl
  public void setType(Object type)
  {
    _entry._type = type;
  }

  public void setAgent(String agent)
  {
    _entry._agent = agent;
  }

  public void setAgentVersion(String version)
  {
    _entry._agentVersion = version;
  }

  public void setPlatform(String platform)
  {
    _entry._platform = platform;
  }

  public void setPlatformVersion(String version)
  {
    _entry._platformVersion = version;
  }

  public void setMakeModel(String makemodel)
  {
    _entry._makeModel = makemodel;
  }

  public void setAgentEntryToNULL()
  {
    _entry = _NULL_AGENT_ENTRY;
  }

  //Private entry structure to
  //store the Agent attributes
  private static class AgentEntry
  {
    Object _type = TYPE_UNKNOWN;
    String _agent = AGENT_UNKNOWN;
    String _agentVersion = AGENT_VERSION_UNKNOWN;
    String _platform = PLATFORM_UNKNOWN;
    String _platformVersion = PLATFORM_VERSION_UNKNOWN;
    String _makeModel = MAKE_MODEL_UNKNOWN;
  }

  void __addRequestCapability(CapabilityKey key,Object value)
  {
     if (_requestCapabilities == null)
     {
       _requestCapabilities = new HashMap<Object, Object>();
     }
    _requestCapabilities.put(key,value);
  }

  private HashMap<Object, Object> _requestCapabilities;
  private AgentEntry _entry;
  static private final AgentEntry _NULL_AGENT_ENTRY = new AgentEntry();
  static final private TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(AgentImpl.class);

}
