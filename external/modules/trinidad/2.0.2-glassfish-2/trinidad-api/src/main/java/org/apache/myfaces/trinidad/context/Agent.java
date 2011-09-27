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
package org.apache.myfaces.trinidad.context;

import java.util.Map;

/**
 * The Agent interface describes the client that is making the request that will display
 * the rendered output.
 * <p>
 * Implementations that provide the set of capabilities must clearly define the names
 * of these capabilities and their values.
 * </p>
 * <p>
 * Capability names that are implementation private must be defined so using appropriate
 * naming schemes. Trinidad private capability names are prefixed using "-adfinternal-xxx",
 * and such capability names (and their values) may change at anytime
 * (and not guaranteed to be supported in future releases).
 * <p>
 */
public interface Agent
{
  /**
   * Constant for Unknown device type
   */
  public static final Object TYPE_UNKNOWN = "unknown";

  /**
   * Constant for telnet device type
   */
  public static final Object TYPE_TELNET = "telnet";

  /**
   * Constant for desktop devices
   */
  public static final Object TYPE_DESKTOP = "desktop";

  /**
   * Constant for handheld sized devices (Pocket-PC, Palm)
   */
  public static final Object TYPE_PDA = "pda";

  /**
   * Constant for Phone sized devices
   */
  public static final Object TYPE_PHONE = "phone";
  
  /**
   * Constant for Web Crawlers
   */
  public static final Object TYPE_WEBCRAWLER = "webcrawler";

  /**
   * Constant for unknown platform
   */
  public static final String PLATFORM_UNKNOWN = "unknown";
  
  /**
   * Constant for windows platform
   */
  public static final String  PLATFORM_WINDOWS = "windows";

  /**
   * Constant for linux platform
   */
  public static final String  PLATFORM_LINUX = "linux";

  /**
   * Constant for MacOS platform
   */
  public static final String  PLATFORM_MACOS = "mac";

  /**
   * Constant for Mac platform
   * @deprecated
   */
  @Deprecated
  public static final String  PLATFORM_MAC = PLATFORM_MACOS;

  /**
   * Constant for the iOS (iPhone/iPod touch/iPad) platform
   */
  public static final String  PLATFORM_IPHONE = "iphone";

  /**
   * Constant for plam platform
   */
  public static final String  PLATFORM_PALM = "palm";

  /**
   * Constant for solaris platform
   */
  public static final String  PLATFORM_SOLARIS = "solaris";

  /**
   * Constant for pocket pc platform
   */
  public static final String PLATFORM_PPC = "ppc";

  /**
   * Constant for blackberry platform
   */
  public static final String PLATFORM_BLACKBERRY = "blackberry";

  /**
  /**
   * Constant for Nokia S60 platform
   */
  public static final String PLATFORM_NOKIA_S60 = "nokia_s60";

  /**
   * Constant for generic PDA device browser
   */
  public static final String PLATFORM_GENERICPDA = "genericpda";
  
  /**
   * Constant for android device browsers
   */
  public static final String PLATFORM_ANDROID = "android";

  /**
   * Constant for unknown platform version
   */
  public static final String PLATFORM_VERSION_UNKNOWN = "unknown";

  /**
   * Constant for when the agent is not supported or not recognized
   */
  public static final String AGENT_UNKNOWN = "unknown";

  /**
   * Constant for Konqueror agent
   */
  public static final String AGENT_KONQUEROR = "konqueror";

  /**
   * Constant for Internet Explorer agent
   */
  public static final String AGENT_IE = "ie";

  /**
   * Constant for Gecko agent. Used for all Gecko based agents like Mozilla, Netscape 6+
   */
  public static final String AGENT_GECKO = "gecko";

  /**
   * Constant for Opera agent.
   */
  public static final String AGENT_OPERA = "opera";

  /**
   * Constant for email agent. Used for all email agents like Outlook 2007
   * and Thunderbird
   */
  public static final String AGENT_EMAIL = "email";
  
  /**
   * Constant for Apple Webkit agent. Used for all Webkit based agent like Safari
   */
  public static final String AGENT_WEBKIT = "webkit";

  /**
   * Constant for BlackBerry Browser agent.  (Note the distinction from the
   * BlackBerry platform.  The BlackBerry Browser agent runs on the
   * BlackBerry platform.  It is possible for other agents to run on the
   * BlackBerry platform.)
   */
  public static final String AGENT_BLACKBERRY = "blackberry";

  /**
   * Constant for Symbian Nokia S60 agent. Used for Nokia Series 60
   * 3rd Edition or later
   */
  public static final String AGENT_NOKIA_S60 = "nokia_s60";

  /**
   * Constant for basic HTML (without JavaScript) Browser agent.
   */
  public static final String AGENT_GENERICPDA = "genericpda";
  
  /**
   * Constant for MSN web crawler (currently used by Bing and Yahoo)
   */
  public static final String AGENT_MSNBOT = "msnbot";
  
  /**
   * Constant for Google web crawler
   */
  public static final String AGENT_GOOGLEBOT = "googlebot";
  
  /**
   * Constant for Oracle SES web crawler
   */
  public static final String AGENT_ORACLE_SES = "oracle_ses";

  /**
   * Constant for unknown Agent version
   */
  public static final String AGENT_VERSION_UNKNOWN = "unknown";

  /**
   * Constant for unknown make model version
   */
  public static final String MAKE_MODEL_UNKNOWN = "unknown";

  /**
   *
   * @return return the Type of Agent. Returns <code>TYPE_UNKNOWN</code> if not available.
   * <br>E.g. desktop, pda, phone
   *
   */
  public Object getType();

  /**
   *
   * @return return the canonical name of the agent (browser application).
   * Returns <code>null</code> if not available.
   * <br>E.g. gecko, ie, opera, pocketie
   */
  public String getAgentName();

  /**
   *
   * @return return the version number of the agent (browser application).
   * Return <code>null</code> if not available.
   */
  public String getAgentVersion();

  /**
   *
   * @return return the canonical name for the platform. Returns <code>null</code> if not available.
   *  <br>E.g ppc, series60, windows, mac, linux, solaris
   */
  public String getPlatformName();


  /**
   *
   * @return return the version number for the platform.
   * Returns <code>null</code> if not available.
   */
  public String getPlatformVersion();


  /**
   *
   * @return return a canonical name for the Hardware make and Model. Re
   * turns <code>null</code> if not available.
   * <br>E.g nokia6600, sonyericssonP900, nokai3650i
   */
  public String getHardwareMakeModel();


  /**
   * @return Map of capability names and their values for the current client request.
   * <br>Some of the available capability names are:
   * <br><i>height</i>- provides the screen height in pixels of the Agent as an Integer.
   * <br><i>width</i>- provides the screen width in pixels of the Agent as an Integer.
   * <br><i>dom</i>- provides the DOM API support of the agent as a String.
   *   Possible values are: <i>level2</i>, <i>level1</i>, <i>form</i>, and <i>none</i>.
   * <br><i>frames</i>- returns a Boolean value signifying whether or not the Agent
   * supports frames.
   * <br><i>accessKeys</i>- returns a Boolean value signifying whether or not the Agent
   * supports accessKeys.
   */
  // See CapabilityMap for why this takes Object as a key instead
  // of String, at least for now
  public Map<Object, Object> getCapabilities();
}
