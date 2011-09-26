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

import java.util.Map;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RequestContext;

import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * agent util class
 */
public class AgentUtil
{
  /**
   * Get a default agent, that defines no characteristics.
   *
   * @return
   */
  public static TrinidadAgent getUnknownAgent()
  {
    DefaultAgent adfAgent = new DefaultAgent();
    return new TrinidadAgentImpl(adfAgent);
  }

  /**
   * Get agent for the current faces context
   *
   * @param context
   * @return
   */
  public static TrinidadAgent getAgent(FacesContext context)
  {
    Agent agent = RequestContext.getCurrentInstance().getAgent();
    if (agent instanceof TrinidadAgent)
      return (TrinidadAgent) agent;

    return new TrinidadAgentImpl(context, agent);
  }


  /**
   *
   * Merge capabilities provided with the Agent capabilities
   *
   * @param agent  Agent to merge the capabilities with
   * @param capabilities  List (array of {name, value}) capabilities that should be merged
   * @return An Agent with the capabilities merged with the provided agent
   */
   //@TODO: Check this: Why is this called from an LookAndFeel.
   //@Ideally the an Agent's display mode (called facet in uix22), should be
   //@built into the capabilities repository, hence is should not be
   //@look and feel sepecific
  //=-=AEW: I don't believe this is a repository thing:  the concept
  //of output mode is entirely renderkit specific, and the tweaks
  //that are made to the capabilities are also renderkit specific.
  public static TrinidadAgent mergeCapabilities(
      TrinidadAgent agent, 
      Map<Object, Object> capabilities)
  {
    if (!(agent instanceof TrinidadAgentImpl))
      throw new IllegalArgumentException(_LOG.getMessage(
        "MERGECAPABILITIES_ONLY_USED_WITH_AGENTS_CREATED_BY_THIS_CLASS"));
    // Make a copy of the agent first
    agent = (TrinidadAgent) agent.clone();

    // Then merge in the capabilities
    // =-=AEW This codepath ends up creating copies of the
    // capability map twice, once in clone(), once in __mergeCapabilities()
    ((TrinidadAgentImpl)agent).__mergeCapabilities(capabilities);

    return agent;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    AgentUtil.class);
}
