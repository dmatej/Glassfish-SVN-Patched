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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.Agent;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.net.URL;


/**
 * implementation of Capabilities document
 * only the capabilities provider needs to know about the document.
 */
public class CapabilitiesDocument
{

  CapabilitiesDocument(CapabilitiesNode[] agents, DeviceNode[] devices)
  {
    _agents = agents;
    _devices = devices;
    _includeNodeBySrcCaps = new HashMap<URL, Object>();
    _defaultAgentCapabilities = _getDefaultAgentCapabilities(_agents);
  }


  private CapabilitiesDocument()
  {
    this(null, null);
  }

  /**
   * @param agent
   * @return An array of Capabilities {name, value, name, value}.
   * The array may have duplicates and is ordered in ascending order (priority)
   */
  public Object[][] getCapabilities(Agent agent)
  {
    Object[][] agentCaps = null;
    //TODO: Right now we go by best match. But May be we should
    //get all that nodes that matched (sort by match score) and merge
    //This way the xml file would be simpler (--or more cryptic??)
    CapabilitiesNode agentMatchedNode = _getAgentCapabiltiesNode(agent);
    if (agentMatchedNode != null)
      agentCaps = _getCapabilities(agent, _agents, null, agentMatchedNode);



    //Now get the capabilities of the device, if it is known
    // - Match the correct device node
    // - Get the components in the device
    // - resolve the capabilities of the components
    // - If agent name is aavailable and is not "UNKNOWN" then
    //   use the capabilities of the agent (instead of the capabilities of the
    //   browser component). We need to do this as request could be from a agent
    //   like opera, but the device default browser may be something else.
    //TODO: Need to support a "extends" mechanism for devices, so that one device
    //can extend capabilities from a similar device.
    DeviceNode dNode = _getDeviceNode(agent);
    if (dNode != null)
    {
      DeviceComponentNode[] cNodes = dNode.__getComponents();
      if (cNodes == null)
        return agentCaps;

      Object[][][] deviceCaps = new Object[cNodes.length][][];
      for (int i = 0; i < cNodes.length; i++)
      {
        if ((cNodes[i].__getType() == DeviceComponentNode.TYPE_BROWSER) &&
            (agent.getAgentName() != null))
        {
          deviceCaps[i] = agentCaps;
        }
        else
        {
          deviceCaps[i] = _getCapabilities(agent, _agents, cNodes[i]);
        }
      }

      return _mergeCaps(deviceCaps, null);
    }

    //if device is null return agentCaps
    //if agentName is also null then return
    //default agentCaps
    if (agentCaps != null)
      return agentCaps;

    return _defaultAgentCapabilities;
  }


  private Object[][] _getDefaultAgentCapabilities(CapabilitiesNode[] agents)
  {
    if (_agents == null)
      return null;

    for (int i = 0; i < _agents.length; i++)
    {
      if (agents[i].__isDefault())
      {
        return _getCapabilities(null, _agents, null, _agents[i]);
      }
    }

    return null;
  }

  /**
   * @param agent
   * @return the capabilities node that best matches for current Agent
   */
  private CapabilitiesNode _getAgentCapabiltiesNode(Agent agent)
  {
    if (_agents == null)
      return null;
    //get the best match node
    double maxScore = 0;
    CapabilitiesNode agentMatchedNode = null;
    for (int i = 0; i < _agents.length; i++)
    {
      double score = _agents[i].__matches(agent);
      if ((score > 0) && (score > maxScore))
      {
        maxScore = score;
        agentMatchedNode = _agents[i];
      }
    }
    return agentMatchedNode;
  }


  /**
   * @param agent
   * @return device node that best matches the current Agent
   */
  private DeviceNode _getDeviceNode(Agent agent)
  {
    if (_devices == null)
       return null;

    double maxScore = 0;
    DeviceNode deviceNode = null;
    for (int i = 0; i < _devices.length; i++)
    {
      double score = _devices[i].__matches(agent);
      if ((score > 0) && (score > maxScore))
      {
        maxScore = score;
        deviceNode = _devices[i];
      }
    }
    return deviceNode;
  }


  /**
   * Get/resolve the capabilities for for a give capabilitiesNode
   */
  private Object[][] _getCapabilities(Agent agent,
                                      CapabilitiesNode[] capNodes,
                                      List<String> includedByRefs,
                                      CapabilitiesNode matchNode)
  {
    assert (matchNode != null);

    if (capNodes == null)
      return new Object[0][0];

    //--do we need to bother about caching (a resolved state)
    //for each node? Not until this becomes a problem

    //check for circular dependecy
    if (includedByRefs == null)
      includedByRefs = new ArrayList<String>();

    if (matchNode.__getId() != null)
    {
      if (includedByRefs.contains(matchNode.__getId()))
      {
        _LOG.warning("INVALID_DEPENDENCY");
        return new Object[0][0];
      }

      includedByRefs.add(matchNode.__getId());
    }

    //process node that includes by reference
    IncludeNode[] refIncludes = matchNode.__getIncludesByRef();
    Object[][][] refCaps = null;
    if (refIncludes != null)
    {
      refCaps = new Object[refIncludes.length][][];
      for (int i = 0; i < refIncludes.length; i++)
      {
        assert (refIncludes[i].__getRefId() != null);
        refCaps[i] = _getCapabilities(agent, capNodes,
                                      includedByRefs, refIncludes[i].__getRefId());
      }
    }

    //process nodes the includes a external file
    IncludeNode[] uriIncludes = matchNode.__getIncludesByUri();
    Object[][] uriCaps = null;
    if (uriIncludes != null)
    {
      uriCaps = new Object[uriIncludes.length][];
      for (int i = 0; i < uriIncludes.length; i++)
      {
        assert (uriIncludes[i].__getSrcUrl() != null);
        uriCaps[i] = _getCapabilities(uriIncludes[i].__getSrcUrl());
        assert (uriCaps[i] != null);
      }
    }

    return _mergeCaps(refCaps, uriCaps);
  }


  private Object[][] _getCapabilities(Agent agent,
                                      CapabilitiesNode[] capNodes,
                                      DeviceComponentNode dcNode)
  {

    assert (capNodes != null);
    assert (dcNode != null);

    ArrayList<String> includedByRefs = new ArrayList<String>();

    //process node that includes by reference
    IncludeNode[] refIncludes = dcNode.__getIncludesByRef();
    Object[][][] refCaps = null;
    if (refIncludes != null)
    {
      refCaps = new Object[refIncludes.length][][];
      for (int i = 0; i < refIncludes.length; i++)
      {
        assert (refIncludes[i].__getRefId() != null);
        refCaps[i] = _getCapabilities(agent, capNodes,
                                      includedByRefs, refIncludes[i].__getRefId());
      }
    }

    //process nodes the includes a external file
    IncludeNode[] uriIncludes = dcNode.__getIncludesByUri();
    Object[][] uriCaps = null;
    if (uriIncludes != null)
    {
      uriCaps = new Object[uriIncludes.length][];
      for (int i = 0; i < uriIncludes.length; i++)
      {
        assert (uriIncludes[i].__getSrcUrl() != null);
        uriCaps[i] = _getCapabilities(uriIncludes[i].__getSrcUrl());
        assert (uriCaps[i] != null);
      }
    }

    return _mergeCaps(refCaps, uriCaps);
  }


  /**
   * get capabilties of node using a node refid
   */
  private Object[][] _getCapabilities(Agent agent,
                                      CapabilitiesNode[] capNodes,
                                      List<String> includedByRefs,
                                      String refId)
  {
    assert (capNodes != null);
    assert (refId != null);
    assert (includedByRefs != null);

    for (int i = 0; i < capNodes.length; i++)
    {
      if (refId.equals(capNodes[i].__getId()))
      {
        Object[][] caps = _getCapabilities(agent, capNodes, includedByRefs, capNodes[i]);
        return caps;
      }
    }

    _LOG.warning("REFERENCE_ID_NOT_FOUND", refId);
    return new Object[0][0];
  }


  /**
   * capabilities from parsed and cached capability data documents
   */
  private Object[] _getCapabilities(URL srcUrl)
  {
    Object o = _includeNodeBySrcCaps.get(srcUrl);
    if (o != null)
      return (Object[]) o;

    Object[] caps = null;
    synchronized (_includeNodeBySrcCaps)
    {
      caps = CapabilityDataDocumentParser.parse(srcUrl);
      _includeNodeBySrcCaps.put(srcUrl, caps);
    }

    return caps;
  }

  //merge capabilities array
  private Object[][] _mergeCaps(Object[][][] caps1, Object[][] caps2)
  {
    if (caps1 == null)
      return caps2;

    int len = 0;
    for (int i = 0; i < caps1.length; i++)
    {
      len += caps1[i].length;
    }

    Object[][] merged = new Object[len + (caps2 == null ? 0 : caps2.length)][];
    len = 0;
    for (int i = 0; i < caps1.length; i++)
    {
      System.arraycopy(caps1[i], 0, merged, len, caps1[i].length);
      len += caps1[i].length;
    }

    if (caps2 != null)
      System.arraycopy(caps2, 0, merged, len, caps2.length);

    return merged;
  }


  private CapabilitiesNode[] _agents;
  private DeviceNode[] _devices;
  private HashMap<URL, Object> _includeNodeBySrcCaps;
  private Object[][] _defaultAgentCapabilities;

  static final CapabilitiesDocument EMPTY_DOCUMENT = new CapabilitiesDocument();

  private static TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CapabilitiesDocument.class);

}
