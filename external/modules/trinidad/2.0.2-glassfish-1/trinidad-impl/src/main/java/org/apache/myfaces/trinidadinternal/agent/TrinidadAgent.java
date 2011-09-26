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

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.Version;


/**
 * Extension of public Agent interface. Defines constants/method for AdInternal use.
 * Keeping this Interace as is (from before), but extends the public
 * the current (internal) code base uses this heavily.
 *
 */
public abstract class TrinidadAgent implements Agent, Cloneable
{
  static public final CapabilityKey CAP_DOM =
          CapabilityKey.getCapabilityKey("dom", true);

  static public final CapabilityKey CAP_ONCLICK_IMG_INPUT =
          CapabilityKey.getCapabilityKey("-adfinternal-onclickOnImgInput", true);

  static public final CapabilityKey CAP_XMLDOM =
          CapabilityKey.getCapabilityKey("-adfinternal-xmldom", true);

  static public final CapabilityKey CAP_ID =
          CapabilityKey.getCapabilityKey("-adfinternal-id", true);

  static public final CapabilityKey CAP_ACCESS_KEYS =
          CapabilityKey.getCapabilityKey("accessKeys", true);

  static public final CapabilityKey CAP_PARTIAL_RENDERING =
          CapabilityKey.getCapabilityKey("partialRendering", true);

  static public final CapabilityKey CAP_DISABLED_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-disabledFormElements", true);

  static public final CapabilityKey CAP_READONLY_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-readonlyFormElements", true);

  static public final CapabilityKey CAP_AUTO_COMPLETE_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-autoCompleteFormElements", true);

  static public final CapabilityKey CAP_ADVANCED_BUTTONS =
          CapabilityKey.getCapabilityKey("-adfinternal-advancedButtons", true);

  static public final CapabilityKey CAP_WIDTH =
          CapabilityKey.getCapabilityKey("width", true);

  static public final CapabilityKey CAP_HEIGHT =
          CapabilityKey.getCapabilityKey("height", true);

  /**
   * Touch-screen capability, indicating the agent supports TouchEvent and GestureEvent
   * events.
   */
  static public final CapabilityKey CAP_TOUCH_SCREEN =
          CapabilityKey.getCapabilityKey("touchScreen", true);
  
  /**
   * Session History Management (HTML5 History) Capability indicating support for 
   * APIs like history.pushState, history.replaceState, etc.
   */
  static public final CapabilityKey CAP_HISTORY_MANAGEMENT = 
          CapabilityKey.getCapabilityKey("historyManagement", true);

  //
  // XHTML Modularization
  //
  static public final CapabilityKey CAP_TEXT_PRESENTATION =
          CapabilityKey.getCapabilityKey("textPresentation", true);

  static public final CapabilityKey CAP_ADVANCED_FORMS =
          CapabilityKey.getCapabilityKey("-adfinternal-advancedForms", true);

  static public final CapabilityKey CAP_TABLES =
          CapabilityKey.getCapabilityKey("tables", true);

  static public final CapabilityKey CAP_FRAMES =
          CapabilityKey.getCapabilityKey("frames", true);

  static public final CapabilityKey CAP_TARGET =
          CapabilityKey.getCapabilityKey("-adfinternal-target", true);

  static public final CapabilityKey CAP_IFRAMES =
          CapabilityKey.getCapabilityKey("iframes", true);

  static public final CapabilityKey CAP_INTRINSIC_EVENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-intrinsicEvents", true);

  static public final CapabilityKey CAP_STYLE_ATTRIBUTES =
          CapabilityKey.getCapabilityKey("-adfinternal-styleAttributes", true);

  static public final CapabilityKey CAP_NAME_IDENTIFICATION =
          CapabilityKey.getCapabilityKey("-adfinternal-nameIdentification", true);

  static public final CapabilityKey CAP_FIELDSET =
          CapabilityKey.getCapabilityKey("-adfinternal-fieldset", true);

  /**
   * capability describing level of support for css selectors
   */
  static public final CapabilityKey CAP_CSS_SELECTORS =
          CapabilityKey.getCapabilityKey("-adfinternal-cssSelectors", true);

  /**
   * true if supports disabling wrapping
   */
  static public final CapabilityKey CAP_NOWRAP =
          CapabilityKey.getCapabilityKey("-adfinternal-nowrap", true);

  /**
   * true if supports vertical alignment
   */
  static public final CapabilityKey CAP_VALIGN =
          CapabilityKey.getCapabilityKey("-adfinternal-valign", true);

  /**
   * true if the alt key renders a tooltip for an image *
   */
  static public final CapabilityKey CAP_ALT_RENDERS_TOOLTIP_ON_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-altRendersTooltipOnImage", true);

  static public final CapabilityKey CAP_SCRIPTING_SPEED =
          CapabilityKey.getCapabilityKey("scriptingSpeed", true);


  /**
   * true if multiple windows can be opened
   */
  static public final CapabilityKey CAP_MULTIPLE_WINDOWS =
          CapabilityKey.getCapabilityKey("-adfinternal-multipleWindows", true);


  // True if agent supports page navigation
  static public final CapabilityKey CAP_NAVIGATION =
          CapabilityKey.getCapabilityKey("-adfinternal-navigation", true);

  // True if agent supports editing
  static public final CapabilityKey CAP_EDITING =
          CapabilityKey.getCapabilityKey("-adfinternal-editing", true);

  // True if agent supports image stretching, ie. setting the img
  // width/height to a percentage.
  static public final CapabilityKey CAP_IMAGE_STRETCH =
          CapabilityKey.getCapabilityKey("-adfinternal-imageStretch", true);

  static public final CapabilityKey CAP_GIF_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-gifImage", true);

  static public final CapabilityKey CAP_JPEG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-jpegImage", true);

  static public final CapabilityKey CAP_PNG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-pngImage", true);

  static public final CapabilityKey CAP_TRANSPARENT_PNG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-transparentPngImage", true);

  static public final CapabilityKey CAP_BMP_TYPE_IMAGE =
               CapabilityKey.getCapabilityKey("-adfinternal-bmpImage", true);

  static public final CapabilityKey CAP_SUPPORTS_DISABLED_OPTIONS = CapabilityKey
    .getCapabilityKey("-adfinternal-supportsDisabledOptions", true);


  static public final CapabilityKey CAP_IS_JDEV_VE = CapabilityKey
    .getCapabilityKey("-adfinternal-isJDevVE", true);

  static public final CapabilityKey CAP_IS_JDEV_JAVASCRIPT_VE = CapabilityKey
    .getCapabilityKey("-adfinternal-isJDevJavascriptVE", true);

  // If this capability flag is true, it means that the request is from an agent
  // that is running in a narrow-screen PDA. Trinidad optimizes its rendering
  // for narrow-screen PDAs to reduce the overall width of the page rendered.
  static public final CapabilityKey CAP_NARROW_SCREEN = CapabilityKey
    .getCapabilityKey("narrowScreen", true);

  //
  // Values for CAP_DOM
  //
  static public final Object DOM_CAP_NONE    =
          CapabilityValue.getCapabilityValue (CAP_DOM, "none");
  static public final Object DOM_CAP_FORM    =
          CapabilityValue.getCapabilityValue (CAP_DOM, "form");
  static public final Object DOM_CAP_LEVEL_1 =
          CapabilityValue.getCapabilityValue (CAP_DOM, "level1");
  static public final Object DOM_CAP_LEVEL_2 =
          CapabilityValue.getCapabilityValue (CAP_DOM, "level2");

  //
  // Values for CAP_SCRIPTING_SPEED
  //
  static public final Object SCRIPTING_SPEED_CAP_NONE =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED, "none");
  static public final Object SCRIPTING_SPEED_CAP_SLOW =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED, "slow");
  static public final Object SCRIPTING_SPEED_CAP_FAST =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED,"fast");

  //
  // Values for CAP_TABLES
  //
  static public final Object TABLES_CAP_BASIC          =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "basic");
  static public final Object TABLES_CAP_ADVANCED_ATTRS =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "advanced_attrs");
  static public final Object TABLES_CAP_ADVANCED       =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "advanced");

  //
  // Values for CAP_STYLE_ATTRIBUTES
  //
  // no styling is supported
  static public final Object STYLES_NONE               =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "none");
  // only the 'style' attribute is supported. The 'class' attribute is not
  // supported.
  static public final Object STYLES_STYLE_ONLY              =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "style_only");
  // internal styles only; this includes the style attribute, or the class attribute
  // as long as the style selectors are in the page with the <style> tag.
  static public final Object STYLES_INTERNAL           =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "internal");
  // external css files are supported.
  static public final Object STYLES_EXTERNAL           =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "external");

  // Values for CAP_CSS_SELECTORS
  static public final Object SELECTORS_NONE            =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "none");
  static public final Object SELECTORS_SINGLE          =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "single");
  static public final Object SELECTORS_MULTIPLE        =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "multiple");

  // Values for CAP_TOUCH_SCREEN
  /**
   * Agent does not have a touch screen
   */
  static public final Object TOUCH_SCREEN_NONE =
          CapabilityValue.getCapabilityValue (CAP_TOUCH_SCREEN, "none");

  /**
   * Agent only supports single finger touch events
   */
  static public final Object TOUCH_SCREEN_SINGLE =
          CapabilityValue.getCapabilityValue (CAP_TOUCH_SCREEN, "single");
  /**
   * Agent supports multiple finger touch events and gestures
   */
  static public final Object TOUCH_SCREEN_MULTIPLE =
          CapabilityValue.getCapabilityValue (CAP_TOUCH_SCREEN, "multiple");
  
  // Values for CAP_HISTORY_MANAGEMENT
  /**
   * Agent fully supports Session History Management (HTML5 History) APIs
   */
  static public final Object HISTORY_MANAGEMENT_FULL =
          CapabilityValue.getCapabilityValue (CAP_HISTORY_MANAGEMENT, "full");
  

  /**
   * Application constant for Desktop devices
   */
  static public final int TYPE_DESKTOP = 0;

  /**
   * Application constant for Phone-sized devices
   */
  static public final int TYPE_PHONE = 1;

  /**
   * Application constant for Palm-sized devices.  Pocket-PC,
   * Palm
   */
  static public final int TYPE_PDA = 2;

  /**
   * Application constant for voice
   */
  static public final int TYPE_VOICE = 3;

  /**
   * Application constant for web crawlers
   */
  static public final int TYPE_WEBCRAWLER = 4;

  /**
   * Enumeration representing an Application
   */
  public static enum Application
  {
    /**
     * Application enum when the user agent isn't known.
     */
    UNKNOWN("0", AGENT_UNKNOWN),
    /**
     * Application enum for the Netscape Navigator browser.
     */
    NETSCAPE("netscape", AGENT_NETSCAPE),
    /**
     * Application enum for the Microsoft Internet Explorer browser.
     */
    IEXPLORER("ie", AGENT_IE),
    /**
     * Application enum  for browsers based on the Gecko Layout Engine,
     * eg: Mozilla, Netscape 7.0+
     */
    GECKO("gecko", AGENT_GECKO, "mozilla"),
    /**
     * Application enum for Palm Web Clippings
     */
    WEB_CLIPPING("web-clipping", AGENT_ELAINE),
    /**
     * Application enum for the ICE Browser
     */
    ICE("ice", AGENT_ICE_BROWSER, "ice"),
    /**
     * Application enum for the Pixo Microbrowser
     */
    PIXO("pixo", AGENT_PIXO),
    /**
     * Application enum for the WML Microbrowser
     */
    WML("wml", "wml"),
    /**
     * Application enum for SimpleResult intermediate Form
     */
    SIMPLE_RESULT("simple-result", "simple-result"),
    /**
     * Application enum for iAS wireless (PTG) client
     */
    PTG("ptg", AGENT_PTG),
    /**
     * Application enum for the NetFront browser.
     */
    NET_FRONT("netfront", AGENT_NETFRONT),
    /**
     * Application enum for the Safari browser.
     */
    SAFARI("safari", AGENT_WEBKIT),
    /**
     * Application enum for the BlackBerry browser.
     */
    BLACKBERRY("blackberry", AGENT_BLACKBERRY),
    /**
     * Application enum for the Nokia S60 browser.
     */
    NOKIA_S60("nokia_s60", AGENT_NOKIA_S60),
    /**
     * Application enum for the basic HTMLbrowser.
     */
    GENERICPDA("genericpda", AGENT_GENERICPDA),
    /**
     * Application enum for Konqueror.
     */
    KONQUEROR("konqueror", AGENT_KONQUEROR),
    /**
     * Application enum for email.
     */
    EMAIL("email", AGENT_EMAIL),
    /**
     * Application enum for opera.
     */
    OPERA("opera", AGENT_OPERA),
    /**
     * Application enum for Google web crawler.
     */
    GOOGLEBOT("googlebot", AGENT_GOOGLEBOT),
    /**
     * Application enum for Bing web crawler.
     */
    MSNBOT("msnbot", AGENT_MSNBOT),
    /**
     * Application enum for Oracle SES.
     */
    ORACLE_SES("oracle_ses", AGENT_ORACLE_SES);

    /**
     * Return the appropriate Application instance given the name of an Application
     * @param applicationName Name of application to return the instance of
     * @return Then Application instance for this name, if any
     */
    public static Application fromApplicationName(String applicationName)
    {
      return StateHolder.NAME_TO_APPLICATION.get(applicationName);
    }

    /**
     * Return the appropriate Application instance given the name of an Agent.  For
     * backwards compatibility reasons, alias names for some Agents are also supported.
     * @param agentName Name of Agent to return the instance of
     * @return Then Application instance for this name, if any
     */
    public static Application fromAgentName(String agentName)
    {
      return StateHolder.AGENT_TO_APPLICATION.get(agentName);
    }

    /**
     * @param applicationName application name for this Application
     * @param agentName       agent name for this Application
     */
    private Application(String applicationName, String agentName)
    {
      this(applicationName, agentName, null);
    }

    /**
     * @param applicationName application name for this Application
     * @param agentName       agent name for this Application
     * @param agentAlias      additional agent name, if any, to support for look ups
     */
    private Application(String applicationName, String agentName, String agentAlias)
    {
      _applicationName = applicationName;
      _agentName = agentName;

      StateHolder.NAME_TO_APPLICATION.put(applicationName, this);

      Map<String, Application> agentToApplication = StateHolder.AGENT_TO_APPLICATION;
      agentToApplication.put(agentName, this);

      // if we have an alias for this agent name, register that as well
      if (agentAlias != null)
        agentToApplication.put(agentAlias, this);
    }

    /**
     * Returns the application name that this Application is registered under
     * @return application name that this Application is registered under
     */
    public String getApplicationName()
    {
      return _applicationName;
    }

    /**
     * Returns the agent name that this Application is registered under
     * @return agent name that this Application is registered under
     */
    public String getAgentName()
    {
      return _agentName;
    }

    private final String _applicationName;
    private final String _agentName;

    /**
     * Holds the maps of agent name and application name to Application object.  By
     * using a separate class, we avoid restrictions on Enums refrerencing static
     * variables from their constructors.
     */
    private static class StateHolder
    {
      static public final Map<String, Application> NAME_TO_APPLICATION = new HashMap<String, Application>();
      static public final Map<String, Application> AGENT_TO_APPLICATION = new HashMap<String, Application>();
    }
  }


  /**
   * Application constant for an entirely unknown application.
   */
  static public final int APPLICATION_UNKNOWN   = Application.UNKNOWN.ordinal();

  /**
   * Application constant for the Netscape Navigator browser.
   * Note that Netscape 6 is considered as Mozilla, since
   * its rendering engine is that of the Mozilla project.
   */
  static public final int APPLICATION_NETSCAPE  = Application.NETSCAPE.ordinal();

  /**
   * Application constant for the Microsoft Internet Explorer
   * browser.
   */
  static public final int APPLICATION_IEXPLORER = Application.IEXPLORER.ordinal();

  /**
   * Application constant for browsers based on the Gecko Layout Engine,
   * eg: Mozilla, Netscape 7.0+
   */
  static public final int APPLICATION_GECKO   = Application.GECKO.ordinal();

  /**
   * Application constant for the Mozilla browser, or browsers
   * based on it (like Netscape 6).
   * @deprecated since 2.2.0. Use {@link #APPLICATION_GECKO}.
   */
  @Deprecated
  static public final int APPLICATION_MOZILLA   = APPLICATION_GECKO;

  /**
   * Application constant for Palm Web Clippings
   */
  static public final int APPLICATION_WEB_CLIPPING = Application.WEB_CLIPPING.ordinal();

  /**
   * Application constant for the ICE Browser
   */
  static public final int APPLICATION_ICE = Application.ICE.ordinal();

  /**
   * Application constant for the Pixo Microbrowser
   */
  static public final int APPLICATION_PIXO = Application.PIXO.ordinal();

  /**
   * Application constant for a WML Microbrowser
   */
  static public final int APPLICATION_WML = Application.WML.ordinal();

  /**
   * Application constant for SimpleResult intermediate Form
   */
  static public final int APPLICATION_SIMPLE_RESULT = Application.SIMPLE_RESULT.ordinal();

  /**
   * Application constant for iAS wireless (PTG) client
   */
  static public final int APPLICATION_PTG = Application.PTG.ordinal();

  /**
   * Application constant for the NetFront browser.
   */
  static public final int APPLICATION_NET_FRONT = Application.NET_FRONT.ordinal();

  /**
   * Application constant for the Safari browser.
   */
  static public final int APPLICATION_SAFARI = Application.SAFARI.ordinal();

  /**
   * Application constant for the BlackBerry browser.
   */
  static public final int APPLICATION_BLACKBERRY = Application.BLACKBERRY.ordinal();

  /**
   * Application constant for the Nokia S60 browser.
   */
  static public final int APPLICATION_NOKIA_S60 = Application.NOKIA_S60.ordinal();

  /**
   * Application constant for the basic HTMLbrowser.
   */
  static public final int APPLICATION_GENERICPDA = Application.GENERICPDA.ordinal();

  /**
   * Application constant for Konqueror.
   */
  static public final int APPLICATION_KONQUEROR = Application.KONQUEROR.ordinal();

  /**
   * Application constant for email.
   */
  static public final int APPLICATION_EMAIL = Application.EMAIL.ordinal();

  /**
   * Application constant for opera.
   */
  static public final int APPLICATION_OPERA = Application.OPERA.ordinal();

  /**
   * Application constant for Google web crawler.
   */
  static public final int APPLICATION_GOOGLEBOT = Application.GOOGLEBOT.ordinal();

  /**
   * Application constant for Bing web crawler.
   */
  static public final int APPLICATION_MSNBOT = Application.MSNBOT.ordinal();

  /**
   * Application constant for Oracle SES.
   */
  static public final int APPLICATION_ORACLE_SES = Application.ORACLE_SES.ordinal();

  /**
   * OS constant for an unknown operating system.
   */
  static public final int OS_UNKNOWN = 0;

  /**
   * OS constant for any Microsoft Windows version.
   */
  static public final int OS_WINDOWS = 1;

  /**
   * OS constant for Apple MacOS.
   */
  static public final int OS_MACOS   = 2;

  /**
   * OS constant for any Linux version.
   */
  static public final int OS_LINUX   = 3;

  /**
   * OS constant for any Solaris version.
   */
  static public final int OS_SOLARIS = 4;

  /**
   * OS constant for any Palm version.
   */
  static public final int OS_PALM = 5;

  /**
   * OS constant for any Windows Pocket PC
   */
  static public final int OS_PPC = 6;

  /**
   * OS constant for any BlackBerry device
   */
  static public final int OS_BLACKBERRY = 7;

  /**
   * OS constant for iPhone
   */
  static public final int OS_IPHONE   = 8;

  /**
   * OS constant for Symbian
   */
  static public final int OS_NOKIA_S60   = 9;

  /**
   * OS constant for generic PDA
   */
  static public final int OS_GENERICPDA   = 10;

  /**
   * OS constant for Android
   */
  static public final int OS_ANDROID  = 11;

  /**
   * Name Constant for Netfront agent
   */
  public static final String AGENT_NETFRONT = "netfront";

  /**
   * Name Constant for Netscape agent. Used only for Netscape versions that are not Gecko
   * based such as Netscape 4.7
   */
  public static final String AGENT_NETSCAPE = "netscape";

  /**
   * Name Constant for Palm Webpro agent
   * //@TODO: Check: Isn't webpro same as netfront access
   */
  public static final String AGENT_WEBPRO = "webpro";

  /**
   * Name constant for ICE browser agent
   */
  public static final String AGENT_ICE_BROWSER = "icebrowser";

  /**
   * Name Constant for Pixo agent
   * //@TODO: Check: Are we still supporting Pixo??
   */
  public static final String AGENT_PIXO = "pixo";

  /**
   * Name Constant for OracleAS Wireless.
   * //@TODO: Check: Do we still have to support this??
   */
  public static final String AGENT_PTG = "ptg";

  /**
   * Name Constant for Blazer agent
   */
  public static final String AGENT_BLAZER = "blazer";

  /**
   * Name Constant for Xiino agent
   */
  public static final String AGENT_XIINO = "xiino";

  /**
   * Name Constant for Palm Web clipping (Elaine) agent
   */
  public static final String AGENT_ELAINE = "elaine";

  /*
   * Skin family for BlackBerry browsers whose version > 4.5
   */
  public static final String SKIN_BLACKBERRY = "blackberry";

  /*
   * Skin family for BlackBerry browsers whose version <=4.5
   */
  public static final String SKIN_BLACKBERRY_MINIMAL = "blackberryminimal";

  /*
   * Skin family for all the default mobile browsers
   */
  public static final String SKIN_GENERIC_PDA = "genericpda";

  /*
   * Skin family for all Safari browsers running in the iOS platform (iPhones/iPod/iPad)
   */
  public static final String SKIN_WEBKIT_IPHONE = "iPhonewebkit";

  /*
   * Skin family for all Safari browsers running in linux platform
   */
  public static final String SKIN_WEBKIT_LINUX = "linuxwebkit";

  /*
   * Skin family for all Safari browsers running in Mac platform
   */
  public static final String SKIN_WEBKIT_MAC = "macwebkit";

  /*
   * Skin family for all Safari browsers running in Nokia S60
   */
  public static final String SKIN_WEBKIT_NOKIA = "nokiawebkit";

  /*
   * Skin family for all Safari browsers running in windows platforms
   */
  public static final String SKIN_WEBKIT_WINDOWS = "windowswebkit";

  /*
   * Skin family for all Safari browsers running in unknown platforms
   */
  public static final String SKIN_WEBKIT_DEFAULT = "defaultwebkit";

  /*
   * Skin family for Windows Mobile browsers
   */
  public static final String SKIN_WINDOWS_MOBILE = "windowsmobile";

  /**
   * Returns the type of agent to which we're rendering.  Currently,
   * only web browsers are understood.
   */
  public abstract int getAgentType();


  /**
   * Returns the specific application to which we're rendering.
   * Returns Application.UNKNOWN is the application couldn't
   * be identified.
   */
  public abstract Application getAgentApplication();


  /**
   * Returns the major version number of the application, or 0
   * if a version number couldn't be identified.
   */
  public abstract int getAgentMajorVersion();

  /**
   * Returns the Version identifier to use for comparing the Version of this Agent to other
   * Versions.
   */
  public abstract Version getVersion();

  /**
   * Returns the client operating system.  Returns OS_UNKNOWN if the
   * operating system can't be identified.
   */
  public abstract int getAgentOS();

  /**
   * Returns a capability of a TrinidadAgent
   */
  public abstract Object getCapability(CapabilityKey key);

  public abstract Object clone();

  /*
   * @return <code>String</code> object that represents an agent's
   * skin-family, which is based upon the agent's CSS-support.
   * Agents with the same name can have different skin families
   * if agents' versions or platforms are different.
   */
  public abstract String getSkinFamilyType();

  /**
   * Convenienc method for cloning by subclases.
   */
  protected final TrinidadAgent cloneTrinidadAgent()
  {
    try
    {
      return (TrinidadAgent) super.clone();
    }
    catch (CloneNotSupportedException cnse)
    {
      assert false;
      return null;
    }
  }
}
