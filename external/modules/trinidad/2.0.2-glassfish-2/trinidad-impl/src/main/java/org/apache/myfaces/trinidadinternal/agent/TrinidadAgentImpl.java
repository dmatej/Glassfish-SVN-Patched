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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.Agent;

import java.net.URL;

import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * implementation that supports the AdfFacesAgent
 */
public class TrinidadAgentImpl extends TrinidadAgent
{

  public TrinidadAgentImpl(FacesContext context, Agent agent)
  {
    _delegate = agent;

    _type = AgentNameUtil.getAgentType(getType());
    _application = Application.fromAgentName(getAgentName());
    _os = AgentNameUtil.getPlatform(getPlatformName());

    String versionString = getAgentVersion();
    
    if (Application.GECKO == _application)
      _major = 1;
    else
      _major = _getMajorVersion(versionString);
    
    _version = new Version(versionString);
    
    _skinFamilyType = _getSkinFamilyType(_os, _application);
    _capMap = _getCapabilityMap(context);
    
    Map<Object, Object> requestCaps = _delegate.getCapabilities();
    
    if (requestCaps != null)
    {
      _capMap = _capMap.merge(requestCaps);
    }
  }

  public TrinidadAgentImpl(Agent agent)
  {
    this (null, agent);
  }

  /**
   * @return return the Type of Agent.
   *         <br>E.g. desktop, pda, phone, voice
   */
  public Object getType()
  {
    return _delegate.getType();
  }

  /**
   * Returns the type of agent to which we're rendering.  Currently,
   * only web browsers are understood.
   */
  public int getAgentType()
  {
    return _type;
  }

  /**
   * @return return the canonical name of the Agent. Return <code>null</code> if not available.
   *         <br>E.g. gecko, ie, opera
   */
  public String getAgentName()
  {
    return _delegate.getAgentName();
  }

  /**
   * Returns the specific application to which we're rendering.
   * Returns Application.UNKNOWN is the application couldn't
   * be identified.
   */
  public Application getAgentApplication()
  {
    return _application;
  }

  /**
   * @return return the version number of the Agent. Return <code>null</code> if not available.
   */
  public String getAgentVersion()
  {
    return _delegate.getAgentVersion();
  }

  /**
   * Returns the Version identifier to use for comparing the Version of this Agent to other
   * Versions.
   * @return This Agent's Version
   */
  public Version getVersion()
  {
    return _version;
  }

  /**
   * Returns the major version number of the application, or 0
   * if a version number couldn't be identified.
   */
  public int getAgentMajorVersion()
  {
    return _major;
  }

  /**
   * @return return the canonical name for the platform. Return <code>null</code> if not available.
   *         <br>E.g ppc, series60
   */
  public String getPlatformName()
  {
    return _delegate.getPlatformName();
  }

  /**
   * @return return the version number for the platform. Return <code>null</code> if not available.
   */
  public String getPlatformVersion()
  {
    return _delegate.getPlatformVersion();
  }

  /**
   * @return Map of capability name and value for the current Agent.
   */
  public Map<Object, Object> getCapabilities()
  {
    return _capMap;
  }

  /**
   * Returns the client operating system.  Returns OS_UNKNOWN if the
   * operating system can't be identified.
   */
  public int getAgentOS()
  {
    return _os;
  }

  /**
   * @return return a canonical name for the Hardware make. Return <code>null</code> if not available.
   *         <br>E.g nokia6600, nokia3650, sonyericssonP900 etc
   */
  public String getHardwareMakeModel()
  {
    return _delegate.getHardwareMakeModel();
  }

  /**
   * @param key
   * @return Agent's capability value for the key
   */
  public Object getCapability(CapabilityKey key)
  {
    return _capMap.getCapability(key);
  }
  
  /*
   * @return <code>String</code> object that represents an agent's
   * skin-family
   */
  public String getSkinFamilyType()
  {
    return _skinFamilyType;
  }


  @Override
  public Object clone()
  {
    TrinidadAgentImpl that = (TrinidadAgentImpl)cloneTrinidadAgent();
    that._capMap = (CapabilityMap) _capMap.clone();
    return that;
  }


  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer("Agent[");
    buffer.append(getType());
    buffer.append(',');

    String name = getAgentName();

    if (name != null)
    {
      buffer.append(name);
    }
    else
    {
      buffer.append("unknown");
    }

    String version = getAgentVersion();

    if (version != null)
    {
      buffer.append(' ');
      buffer.append(version);
    }

    buffer.append(',');
    name = getPlatformName();

    if (name != null)
    {
      buffer.append(name);
    }
    else
    {
      buffer.append("unknown");
    }

    buffer.append(']');
    version = getPlatformVersion();

    if (version != null)
    {
      buffer.append(' ');
      buffer.append(version);
    }

    return new String(buffer);
  }

  /**
   * intialization must have happened beforre calling this methodb
   */
  private CapabilityMap _getCapabilityMap(FacesContext context)
  {
    CapabilityMap capMap = null;
    DeviceRepository repository = _getDeviceRepository();

    if (repository != null)
    {
      capMap = repository.getCapabilityMap(context, this);
    }

    if (capMap == null)
    {
      URL url = _getCapabilitiesFile();
      if (url == null)
          _LOG.severe ("CANNOT_LOCATE_CAPABILITIES_DOCUMENT");

      CapabilitiesProvider capProvider = 
         CapabilitiesProvider.getCapabilitiesProvider(url);

      capMap = capProvider.getCapabilities(this);
    }

    return capMap;
  }


  private synchronized static DeviceRepository
     _getDeviceRepository()
  {
    if (!_deviceRepositoryLoaded)
    {
      {
        List<DeviceRepository> list = ClassLoaderUtils.getServices(_DEVICE_REPOSITORY_URL);
        _deviceRepository = list.isEmpty() ? null : list.get(0);
        _deviceRepositoryLoaded = true;
      }
    }
    
    return _deviceRepository;
  }

  private static URL _getCapabilitiesFile()
  {
    if (_capUrl != null) {
      return _capUrl;
    }

    URL path = null;
    synchronized (TrinidadAgentImpl.class)
    {
      if (_capUrl != null)
        return _capUrl;

      //then try the class loader
      if (path == null)
      {
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        if (loader == null)
          loader = TrinidadAgentImpl.class.getClassLoader();
        path =  loader.getResource(_CAPABILITIES_FILE);
      }
      
      if (path == null)
      {
        _LOG.severe("CANNOT_RESOLVE_CAPABILITIES_FILE");
      }
      
      _capUrl = path;
      return _capUrl;
    }
  }
  
  //Original UIX impl.
  //E.g a version string of  0.9.7 returns 0.9 and 1.9.7 returns 1.9
  static private int _getMajorVersion(String source)
  {
    double version = 0;

    if (source != null)
    {
      boolean hasDecimal = false;
      double divisor = 10.0;

      int sourceLength = source.length();
      int currIndex = 0;

      while (currIndex < sourceLength)
      {
        char currChar = source.charAt(currIndex);

        if ((currChar >= '0') && (currChar <= '9'))
        {
          double addValue = (currChar - '0');

          if (hasDecimal)
          {
            // handle digits to right of decimal
            addValue /= divisor;
            divisor = divisor * 10.0;
          }
          else
          {
            // handle digits to left of decimal
            version *= 10.0;
          }

          version += addValue;
        }
        else
        {
          if (!hasDecimal && (currChar == '.'))
          {
            // found decimal place
            hasDecimal = true;
          }
          else
          {
            break;
          }
        }

        // read next char
        currIndex++;
      }
    }

    return (int) version;
  }

  /*
   * This method determines a mobile agent's skin-family, which is based upon      
   * the agent's CSS-support. Agents with the same name can have different 
   * skin families if the agents' versions or platforms are different.
   */
  private String _getSkinFamilyType(int os, Application application)
  {
    if (os == TrinidadAgent.OS_PPC)
    {
      return TrinidadAgent.SKIN_WINDOWS_MOBILE;
    }
    else if (application == TrinidadAgent.Application.NOKIA_S60)
    {
      return TrinidadAgent.SKIN_WEBKIT_NOKIA;
    }
    else if (application == TrinidadAgent.Application.SAFARI)
    {
      if (os == TrinidadAgent.OS_IPHONE)
      {
        return TrinidadAgent.SKIN_WEBKIT_IPHONE;
      }
      else if (os == TrinidadAgent.OS_LINUX)
      {
        return TrinidadAgent.SKIN_WEBKIT_LINUX;
      }
      else if (os == TrinidadAgent.OS_MACOS)
      {
        return TrinidadAgent.SKIN_WEBKIT_MAC;
      }
      else if (os == TrinidadAgent.OS_WINDOWS)
      {
        return TrinidadAgent.SKIN_WEBKIT_WINDOWS;
      }
      else
      {
        return TrinidadAgent.SKIN_WEBKIT_DEFAULT;  
      }
    }
    else if (application == TrinidadAgent.Application.GENERICPDA)
    {
      return TrinidadAgent.SKIN_GENERIC_PDA;
    }
    else if (application == TrinidadAgent.Application.BLACKBERRY)
    {
      // BlackBerry browsers' versions are in the format x.y.z.[...]
      // (x,y and z are numerals). For determining the skin-family, truncate
      // the version after the first decimal-point's immediate digit.
      // Example, versions 4.6.0.80  and 4.1.2 will be  truncated to 4.6  
      // and 4.1 respectively.
      String version = getAgentVersion();
      version = version.substring(0, version.indexOf(".")+2);
            
      if (Float.parseFloat(version) > 4.5)
      {
        return TrinidadAgent.SKIN_BLACKBERRY;
      }
      else
      {
        return TrinidadAgent.SKIN_BLACKBERRY_MINIMAL;
      }
    }
    
    return null;
  }
  
  void __mergeCapabilities(Map<Object, Object> capabilities)
  {
    _capMap = _capMap.merge(capabilities);
  }

  private static volatile URL _capUrl;

  //@todo: Get this from the Configuration Object
  static final private String _CAPABILITIES_FILE = "META-INF/agent/capabilities.xml";

  static private final String _DEVICE_REPOSITORY_URL =
    "org.apache.myfaces.trinidadinternal.agent.DeviceRepository";


  static final private TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrinidadAgentImpl.class);

  private static boolean _deviceRepositoryLoaded;
  private static DeviceRepository _deviceRepository;

  private final Agent _delegate;
  private CapabilityMap _capMap;
  private final int _type;
  private final Application _application;
  private final int _os;
  private final int _major;
  private final String _skinFamilyType;
  private final Version _version;
}
