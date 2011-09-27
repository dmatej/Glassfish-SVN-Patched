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
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.parse.CapabilitiesDocument;
import org.apache.myfaces.trinidadinternal.agent.parse.CapabilitiesDocumentParser;

import java.util.Map;
import java.util.HashMap;
import java.net.URL;

/**
 * An implementation of Capabilities Provider. The Capabilities provider
 * provides capabilities (CapabilityMap) for the context Agent from
 * a configured capabilities store (currently XML files)
 *
 */
public class CapabilitiesProvider
{

  /**
   * @param capUrl URI to the capabilities document defining the
   * capabilities for various clients/agents
   * //TODO: Right now all capabilities get an Key. Do we really need to that?
   * //--why should we create a key for capabilities we don't care?
   */
  private CapabilitiesProvider(URL capUrl)
  {
    _capUrl = capUrl;
  }

  /**
   * @param capUrl URI to the capabilities document defining the
   * capabilities for various clients/agents
   * @return A provider of Capabilities based on the context Agent
   */
  static public CapabilitiesProvider getCapabilitiesProvider(URL capUrl)
  {
    //getCapabilitiesProvider
    //--Are we really going to support multiple files??
    CapabilitiesProvider provider = null;
    Map<URL, CapabilitiesProvider> providerMap = _getProviderMap();
    synchronized (providerMap)
    {
      provider = providerMap.get(capUrl);
      if (provider == null)
      {
        provider = new CapabilitiesProvider(capUrl);
        providerMap.put(capUrl, provider);
      }
    }
    return provider;
  }

  /**
   * @param agent Agent for which the capabilities are to be evaluated
   * @return capability map that has merged capabilities from the Agent provided and
   *         capabilities defined in the configured provider
   */
  public CapabilityMap getCapabilities(Agent agent)
  {
    if (agent == null)
      return _EMPTY_MAP;

    //get out of cache
    CapabilityMap caps = null;
    CacheKey key = new CacheKey(agent);
    synchronized (this) {
      try
      {
        caps = _sCache.get(key);

        //if not in cache, acquire
        if (caps == null)
        {
          caps = _getCapabilities(agent);
          _sCache.put(key, caps);
        }
      }
      catch (RuntimeException re)
      {
        _LOG.severe("CANNOT_GET_CAPABILITIES", re);
      }
    }

    //finally merge the capabilities of the current agent
    //and the capabilities we acquired
    caps = caps.merge(agent.getCapabilities());
    return (caps);
  }


  private CapabilityMap _getCapabilities(Agent agent)
  {
    CapabilitiesDocument doc = _getCapabilityDocument();
    Object[][] capArray = doc.getCapabilities(agent);
    if (capArray != null)
      return new CapabilityMap(capArray);

    return _EMPTY_MAP;
  }


  private CapabilitiesDocument _getCapabilityDocument()
  {
    if (_document != null)
      return _document;

    synchronized (this)
    {
      if (_document == null)
      {
        CapabilitiesDocument doc =
                CapabilitiesDocumentParser.createInstance(_capUrl);
        _document = doc;
      }
    }

    return _document;
  }

  static private Map<URL, CapabilitiesProvider> _getProviderMap()
  {
    return _providerMap;
  }

  /**
   * Cachekey for the Agent
   */
  static private class CacheKey
  {
    CacheKey(Agent ag)
    {
      _agentName = ag.getAgentName();
      if (_agentName == null)
        _agentName = _EMPTY;

      _agentVersion = ag.getAgentVersion();
      if (_agentVersion == null)
        _agentVersion = _EMPTY;

      _platformName = ag.getPlatformName();
      if (_platformName == null)
        _platformName = _EMPTY;

      _platformVersion = ag.getPlatformVersion();
      if (_platformVersion == null)
        _platformVersion = _EMPTY;

      _hardwareMakeModel = null;
      if (_hardwareMakeModel == null)
        _hardwareMakeModel = _EMPTY;

      //pre compute the hash code
      _hashCode = _agentName.hashCode() ^
                  _agentVersion.hashCode() ^
                  _platformName.hashCode() ^
                  _platformVersion.hashCode() ^
                  _hardwareMakeModel.hashCode();

    }

    @Override
    public int hashCode()
    {
      return _hashCode;
    }

    @Override
    public boolean equals(Object obj)
    {
      if (obj == this)
        return true;
      if (!(obj instanceof CacheKey))
        return false;
      CacheKey key = (CacheKey) obj;

      if ((_agentName.equals(key._agentName)) &&
          _agentVersion.equals(key._agentVersion) &&
          _platformName.equals(key._platformName) &&
          _platformVersion.equals(key._platformVersion) &&
          _hardwareMakeModel.equals(key._hardwareMakeModel))
        return true;

      return false;
    }

    private Object _agentName;
    private Object _agentVersion;
    private Object _platformName;
    private Object _platformVersion;
    private Object _hardwareMakeModel;
    private int _hashCode;

    static final private Object _EMPTY = new Object();
  }


  private URL _capUrl;
  private volatile CapabilitiesDocument _document;

  static private CapabilityMap _EMPTY_MAP = new CapabilityMap(new Object[0][0]);

  //@todo: The caches are global. Should we store in SC?
  //@todo: Use an LRU, to limit number of entries
  static private HashMap<CacheKey, CapabilityMap> _sCache = 
    new HashMap<CacheKey, CapabilityMap>(64);


  static private Map<URL, CapabilitiesProvider> _providerMap = 
    new HashMap<URL, CapabilitiesProvider>(4);
  static final private TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CapabilitiesProvider.class);

}
