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

import org.apache.myfaces.trinidad.context.Agent;

/**
 * Object for Capabilities node
 */
class CapabilitiesNode
{

  public CapabilitiesNode(String id,
                          boolean isDefault,
                          NameVersion agent,
                          NameVersion platform,
                          IncludeNode[] nodesWithRef,
                          IncludeNode[] nodesWithSrc)
  {
    _id = id;
    _isDefault = isDefault;
    _agent = agent;
    _platform = platform;
    _nodesWithRef = nodesWithRef;
    _nodesWithSrc = nodesWithSrc;
  }

  String __getId()
  {
    return _id;
  }

  boolean __isDefault()
  {
    return _isDefault;
  }

  void setNodesWithRef(IncludeNode[] nodesWithRef)
  {
    _nodesWithRef = nodesWithRef;
  }

  void __setNodesWithSrc(IncludeNode[] nodesWithSrc)
  {
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

  double __matches(Agent agent)
  {

    double agentScore = 0;  //TODO: use the NO_MATCH constant
    if (_agent != null) {
      agentScore = _getAgentMatch(agent);
      if (agentScore <= 0)
        return 0;
    }

    double platformScore = 0; //TODO: use the NO_MATCH constant
    if (_platform != null) {
      platformScore = _getPlatformMatch(agent);

      if (platformScore <= 0)
        return 0;
    }

    //provide heavy weightage for agent
    return (agentScore * 10000 + platformScore);
  }

  private double _getPlatformMatch(Agent agent)
  {
    String platName = agent.getPlatformName();
    String version = agent.getPlatformVersion();
    VersionId platVersionId = null;
    if (version != null)
      platVersionId = new VersionId(version);

    return _platform.match(platName, platVersionId);
  }

  private double _getAgentMatch(Agent agent)
  {
    String agentName = agent.getAgentName();
    String version = agent.getAgentVersion();
    VersionId agentVersionId = null;
    if (version != null)
      agentVersionId = new VersionId(version);

    return _agent.match(agentName, agentVersionId);
  }


  NameVersion _agent;
  NameVersion _platform;
  private String _id;
  private boolean _isDefault = false;
  private IncludeNode[] _nodesWithRef;
  private IncludeNode[] _nodesWithSrc;


/*
  private static String _AGENT_VERSIONID_KEY =
          "org.apache.myfaces.trinidadinternal.agent.parse.AGENT_VERSIONID";
  private static String _PLATFORM_VERSIONID_KEY =
          "org.apache.myfaces.trinidadinternal.agent.parse.PLATFORM_VERSIONID";
*/
}
