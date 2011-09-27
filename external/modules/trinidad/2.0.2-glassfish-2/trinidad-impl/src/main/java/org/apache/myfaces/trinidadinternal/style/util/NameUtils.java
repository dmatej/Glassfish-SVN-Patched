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
package org.apache.myfaces.trinidadinternal.style.util;

import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.Vector;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * Utilities for converting between variant names and ids
 * 
 * @version $Name: $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/NameUtils.java#0 $) $Date: 10-nov-2005.18:58:52 $
 */
public class NameUtils
{
  private NameUtils() {}

  /**
   * Returns the id of the browser with the specified name
   */
  public static TrinidadAgent.Application getAgentApplication(String agentName)
  {
    if (agentName == null)
      return TrinidadAgent.Application.UNKNOWN;
    
    TrinidadAgent.Application application = TrinidadAgent.Application.fromAgentName(agentName);
                                          
    if (application == null)
    {
      // Either a new or an invalid browser
      assert false:"Invalid browser name: " + agentName;
      
      application = TrinidadAgent.Application.UNKNOWN;
    }
                                         
    return application;
  }

  /**
   * Returns the direction value for the specified name.
   */
  public static int getDirection(String directionName)
  {
    int direction = LocaleUtils.DIRECTION_DEFAULT;

    if (_DIRECTION_LTR.equals(directionName))
      direction = LocaleUtils.DIRECTION_LEFTTORIGHT;
    else if (_DIRECTION_RTL.equals(directionName))
      direction = LocaleUtils.DIRECTION_RIGHTTOLEFT;
    else if (directionName != null)
    {
      // Either a new or an invalid direction
      assert false:"Invalid direction name: " + directionName;
    }

    return direction;
  }

  public static int getMode(String modeName)
  {
    int mode = ModeUtils.MODE_DEFAULT;
    if (ModeUtils.STANDARD_MODE.equals(modeName))
      mode = ModeUtils.MODE_STANDARDS;
    else if (ModeUtils.QUIRKS_MODE.equals(modeName))
      mode = ModeUtils.MODE_QUIRKS;
    else if (modeName != null)
      assert false : "Invalid mode name" + modeName;
    return mode;
  }

  /**
   * Returns the name of the specified direction
   */
  public static String getDirectionName(int direction)
  {
    String name = null;

    if (direction == LocaleUtils.DIRECTION_LEFTTORIGHT)
      name = _DIRECTION_LTR;
    else if (direction == LocaleUtils.DIRECTION_RIGHTTOLEFT)
      name = _DIRECTION_RTL;
    else if (direction != LocaleUtils.DIRECTION_DEFAULT)
    {
      // Either a new or an invalid direction
      assert false:"Invalid direction id: " + direction;
    }

    return name;
  }

  public static String getModeName(int mode)
  {
    String modeName = null;
    if (mode == ModeUtils.MODE_STANDARDS)
      modeName = ModeUtils.STANDARD_MODE;
    else if (mode == ModeUtils.MODE_QUIRKS)
      modeName = ModeUtils.QUIRKS_MODE;
    else if (mode != ModeUtils.MODE_DEFAULT)
      assert false : "invalid mode id" + mode;
    return modeName;
  }

  /**
   * Returns the platform with the specified name
   */
  public static int getPlatform(String platformName)
  {
    if (platformName == null)
      return TrinidadAgent.OS_UNKNOWN;

    int platform = TrinidadAgent.OS_UNKNOWN;

    if (_PLATFORM_WINDOWS.equals(platformName))
      platform = TrinidadAgent.OS_WINDOWS;
    else if (_PLATFORM_MACOS.equals(platformName))
      platform = TrinidadAgent.OS_MACOS;
    else if (_PLATFORM_IPHONE.equals(platformName))
      platform = TrinidadAgent.OS_IPHONE;
    else if (_PLATFORM_LINUX.equals(platformName))
      platform = TrinidadAgent.OS_LINUX;
    else if (_PLATFORM_SOLARIS.equals(platformName))
      platform = TrinidadAgent.OS_SOLARIS;
    else if (_PLATFORM_PPC.equals(platformName))
      platform = TrinidadAgent.OS_PPC;
    else if (_PLATFORM_BLACKBERRY.equals(platformName))
      platform = TrinidadAgent.OS_BLACKBERRY;
    else if (_PLATFORM_NOKIA_S60.equals(platformName))
      platform = TrinidadAgent.OS_NOKIA_S60;
    else if (_PLATFORM_GENERICPDA.equals(platformName))
      platform = TrinidadAgent.OS_GENERICPDA;
    else if (_PLATFORM_ANDROID.equals(platformName))
      platform = TrinidadAgent.OS_ANDROID;


    else if (!_PLATFORM_UNIX.equals(platformName))
    {
      // Either a new or an invalid platform
      assert false:"Invalid platform name: " + platformName;
    }

    return platform;
  }

  /**
   * Returns the name of the specified platform
   */
  public static String getPlatformName(int platform)
  {
    String name = null;

    switch (platform)
    {
    case TrinidadAgent.OS_WINDOWS:
      name = _PLATFORM_WINDOWS;
      break;
    case TrinidadAgent.OS_MACOS:
      name = _PLATFORM_MACOS;
      break;
    case TrinidadAgent.OS_IPHONE:
      name = _PLATFORM_IPHONE;
      break;
    case TrinidadAgent.OS_LINUX:
      name = _PLATFORM_LINUX;
      break;
    case TrinidadAgent.OS_SOLARIS:
      name = _PLATFORM_SOLARIS;
      break;
    case TrinidadAgent.OS_PPC:
      name = _PLATFORM_PPC;
      break;
    case TrinidadAgent.OS_BLACKBERRY:
      name = _PLATFORM_BLACKBERRY;
      break;
    case TrinidadAgent.OS_NOKIA_S60:
      name = _PLATFORM_NOKIA_S60;
      break;
    case TrinidadAgent.OS_GENERICPDA:
      name = _PLATFORM_GENERICPDA;
      break;
    case TrinidadAgent.OS_ANDROID:
      name = _PLATFORM_ANDROID;
      break;
    case TrinidadAgent.OS_UNKNOWN:
      // This case is only here to avoid the default assertion
      break;
    default:
      // New or invalid os id
      assert false:"Invalid platform id: " + platform;
    }

    return name;
  }

  /**
   * Returns name for the specified context, suitable for inclusion
   * in a file name.
   * <p>
   * This utility method generates the context-specific portion of
   * a style sheet file name. It combines that locale, direction,
   * agent and color scheme information provided by the context,
   * and returns a name which can be embedded in a file name
   * - eg. "en_US-ltr-ie-4-windows-red".
   * 
   * @param context The context for which a name is generated.
   */
  public static String getContextName(StyleContext context)
  {
    StringBuffer buffer = new StringBuffer();
    buffer.append(_getLocaleString(context));
    buffer.append(_VARIANT_SEPARATOR);
    buffer.append(_getDirectionString(context));
    buffer.append(_VARIANT_SEPARATOR);
    buffer.append(_getBrowserString(context));
    buffer.append(_VARIANT_SEPARATOR);
    buffer.append(_getVersionString(context));
    buffer.append(_VARIANT_SEPARATOR);
    buffer.append(_getPlatformString(context));
    buffer.append(_getModeString(context));
    buffer.append(_VARIANT_SEPARATOR);

    return buffer.toString();
  }

  /**
   * Returns name for the specified context, based on the actual
   * matching style sheets in the document.
   * <p>
   * Like getContextName(StyleContext), this utility method generates
   * the context-specific portion of a style sheet file name. However,
   * getContextName(StyleContext) always returns a fully specified
   * name. This version of getContextName() returns a partially
   * specified name - only those variants which match the underlying
   * style sheets are included in the returned name.
   * <p>
   * So, where getContextName(StyleContext) might return a name
   * with placeholders for unknown variants, eg:
   * <P>
   * styles-en_US-ie-0-0-0-default.css
   * <p>
   * This method would return the partially specified name:
   * <p>
   * styles-en_US-ie-default.css
   * 
   * @param context The context for which a name is generated.
   */
  public static String getContextName(StyleContext context, StyleSheetDocument document)
  {
    // Copy the matching style sheets into an array
    Iterator<StyleSheetNode> e = document.getStyleSheets(context);
    // -= Simon Lessard =-
    // TODO: Check if synchronization is truly required.
    Vector<StyleSheetNode> v = new Vector<StyleSheetNode>();
    while (e.hasNext())
      v.addElement(e.next());
    StyleSheetNode[] styleSheets = new StyleSheetNode[v.size()];
    v.copyInto(styleSheets);

    // Determine which variants actually match the variants specified
    // in the matching style sheets
    int localeMatch = _isLocaleMatch(context, styleSheets);
    boolean directionMatch = _isDirectionMatch(styleSheets);
    boolean modeMatch  = _isModeMatch(styleSheets);
    boolean[] browserAndVersionMatch = _isBrowserAndVersionMatch(context, styleSheets);
    boolean browserMatch = browserAndVersionMatch[0];
    boolean versionMatch = browserAndVersionMatch[1];
    boolean platformMatch = _isPlatformMatch(context, styleSheets);
    boolean highContrastMatch = _isHighContrastMatch(context, styleSheets);
    boolean largeFontsMatch = _isLargeFontsMatch(context, styleSheets);
    
    boolean needSeparator = false;

    StringBuffer buffer = new StringBuffer();

    // Start with the document id

    String documentId = _getDocumentId(context, document);
    if (documentId != null)
    {
      buffer.append(documentId);
      needSeparator = true;
    }

    if (localeMatch != _LOCALE_MATCH_NONE)
    {
      if (needSeparator)
        buffer.append(_VARIANT_SEPARATOR);

      if (localeMatch == _LOCALE_MATCH_LANGUAGE)
        buffer.append(_getLanguageString(context));
      else
        buffer.append(_getLocaleString(context));

      needSeparator = true;
    }

    if (directionMatch)
    {
      if (needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getDirectionString(context));

      needSeparator = true;
    }

    if (browserMatch)
    {
      if (needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getBrowserString(context));

      needSeparator = true;
    }

    if (versionMatch)
    {
      if (needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getVersionString(context));

      needSeparator = true;
    }

    if (platformMatch)
    {
      if (needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getPlatformString(context));

      needSeparator = true;
    }
    if(modeMatch)
    {
      if(needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getModeString(context));
      needSeparator = true;
    }

    if(highContrastMatch)
    {
      if(needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getHighContrastString(context));
      needSeparator = true;
    }

    if(largeFontsMatch)
    {
      if(needSeparator)
        buffer.append(_VARIANT_SEPARATOR);
      buffer.append(_getLargeFontsString(context));
      needSeparator = true;
    }

    return buffer.toString();
  }
  
  /**
   * Tests whether the specified name is a valid accessibility profile
   * property name (eg. "high-contrast" or "large-fonts").
   */
  public static boolean isAccessibilityPropertyName(String name)
  {
    return (XMLConstants.ACC_HIGH_CONTRAST.equals(name) ||
            XMLConstants.ACC_LARGE_FONTS.equals(name));
  }

  // Get the locale as a String
  private static String _getLocaleString(StyleContext context)
  {
    Locale locale = context.getLocaleContext().getTranslationLocale();

    if (locale == null)
      locale = Locale.getDefault();

    return locale.toString();
  }

  // Get the language only (no country) as a String
  private static String _getLanguageString(StyleContext context)
  {
    Locale locale = context.getLocaleContext().getTranslationLocale();

    if (locale == null)
      locale = Locale.getDefault();

    return locale.getLanguage();
  }

  // Get the direction as a String
  private static String _getDirectionString(StyleContext context)
  {
    int direction = LocaleUtils.getReadingDirection(context.getLocaleContext());

    String name = getDirectionName(direction);

    if (name == null)
      return _UNKNOWN_NAME;

    return name;
  }
  private static String _getModeString(StyleContext context)
  {
     String longModeString = ModeUtils.getCurrentMode(context);
     if(longModeString.equals(ModeUtils.QUIRKS_MODE))
       return ModeUtils.MODE_QUIRKS_NAME;
     else
       return ModeUtils.MODE_STANDARD_NAME;
  }

  // Get the browser as a String
  private static String _getBrowserString(StyleContext context)
  {
    TrinidadAgent.Application application = context.getAgent().getAgentApplication();
  
    return application.getApplicationName();
  }

  // get the StyleSheetDocument's id.
  private static String _getDocumentId(StyleContext context, StyleSheetDocument document)
  {
    return document.getDocumentId(context);
  }

  // Get the version as a String
  private static String _getVersionString(StyleContext context)
  {
    TrinidadAgent agent = context.getAgent();
    String version = agent.getAgentVersion();

    if (version == null)
      return _UNKNOWN_NAME;

    return version;
  }

  // Get the platform as a String
  private static String _getPlatformString(StyleContext context)
  {
    int platform = context.getAgent().getAgentOS();
    String name = getPlatformName(platform);

    if (name == null)
      return _UNKNOWN_NAME;

    return name;
  }

  // Get the platform as a String
  private static String _getHighContrastString(StyleContext context)
  {
    if (context.getAccessibilityProfile().isHighContrast())
      return _ACC_HIGH_CONTRAST;
    
    return _UNKNOWN_NAME;
  }

  // Get the platform as a String
  private static String _getLargeFontsString(StyleContext context)
  {
    if (context.getAccessibilityProfile().isLargeFonts())
      return _ACC_LARGE_FONTS;
    
    return _UNKNOWN_NAME;
  }

  // Tests whether the locale specified in the context match the
  // locale variant of any matching style sheet. Returns an integer
  // indicating whether the language matches, the language and
  // country matches, or no match
  private static int _isLocaleMatch(
      StyleContext context,
      StyleSheetNode[] styleSheets
      )
  {
    Locale locale = context.getLocaleContext().getTranslationLocale();
    if (locale == null)
      locale = Locale.getDefault();

    String language = locale.getLanguage();
    String country = locale.getCountry();
    if (country.length() == 0)
      country = null;

    boolean languageMatch = false;

    for (int i = 0; i < styleSheets.length; i++)
    {
      Iterable<Locale> localeList = styleSheets[i].getLocales();
      for (Locale tmpLocale : localeList)
      {
        if (language.equals(tmpLocale.getLanguage()))
        {
          languageMatch = true;

          // If the country is not specified, we're done.
          // Otherwise, we need to see if the country matches too.
          if (country == null)
            return _LOCALE_MATCH_LANGUAGE;

          if (country.equals(tmpLocale.getCountry()))
          {
            // If the country matches, we're done.
            // Otherwise, we need to keep looking to see if there
            // is a full match in one of the other style sheets.
            return _LOCALE_MATCH_FULL;
          }
        }
      }
    }

    if (languageMatch)
      return _LOCALE_MATCH_LANGUAGE;

    return _LOCALE_MATCH_NONE;
  }

  // Tests whether the direction specified in the context matches
  // the direction variant of any matching style sheet
  private static boolean _isDirectionMatch(StyleSheetNode[] styleSheets)
  {
    // If any style sheet has a non-null direction variant, we must
    // have a direction match.
    for (int i = 0; i < styleSheets.length; i++)
    {
      StyleSheetNode styleSheet = styleSheets[i];
      if (styleSheet.getReadingDirection() != LocaleUtils.DIRECTION_DEFAULT)
        return true;
    }

    return false;
  }
  
  private static boolean _isModeMatch(StyleSheetNode[]styleSheets)
  {
    for (int i = 0; i < styleSheets.length; i++)
    {
      StyleSheetNode styleSheet = styleSheets[i];
      if(styleSheet.getMode()!=ModeUtils.MODE_DEFAULT)
        return true;
    }
    return false;
  }

  /**
   * Tests whether the browser specified in the context matches
   * the browser variant of any matching style sheet
   * @return a boolean array with the first value being the browser match
   *          and the second value being the version match
   */
  private static boolean[] _isBrowserAndVersionMatch(StyleContext context,
      StyleSheetNode[] styleSheets)
  {
    TrinidadAgent agent = context.getAgent();
    TrinidadAgent.Application browser = agent.getAgentApplication();
    if (browser == TrinidadAgent.Application.UNKNOWN)
    {
      return new boolean[] { false, false };
    }

    boolean browserMatched = false;
    
    // If any style sheet has a non-null browser variant, we must
    // have a browser match.
    for (int i = 0; i < styleSheets.length; i++)
    {
      AgentAtRuleMatcher agentMatcher = styleSheets[i].getAgentMatcher();
      
      if (agentMatcher != null)
      {
        Set<AgentAtRuleMatcher.Match> matches = agentMatcher.match(agent);
        
        if (matches.contains(AgentAtRuleMatcher.Match.APPLICATION))
        {
          // latch the browser matched
          browserMatched = true;
          
          // we can't match better than application and version, so return immediately
          if (matches.contains(AgentAtRuleMatcher.Match.VERSION))
            return new boolean[] { true, true };          
        }
      }
      else
      {
        // no agent matcher, so all agents match
        browserMatched = true;
      }
    }

    return new boolean[] { browserMatched, false };
  }

  // Tests whether the platform specified in the context matches
  // the platform variant of any matching style sheet
  private static boolean _isPlatformMatch(StyleContext context,
      StyleSheetNode[] styleSheets)
  {
    int platform = context.getAgent().getAgentOS();
    if (platform == TrinidadAgent.OS_UNKNOWN)
      return false;

    // If any style sheet has a non-null platform variant, we must
    // have a platform match.
    for (int i = 0; i < styleSheets.length; i++)
    {
      if (!(styleSheets[i].getPlatforms().isEmpty()))
        return true;
    }

    return false;
  }

  // Tests whether the high contrast value specified in the context matches
  // the high contrast value of any matching style sheet
  private static boolean _isHighContrastMatch(
      StyleContext context,
      StyleSheetNode[] styleSheets)
  {
    if (!context.getAccessibilityProfile().isHighContrast())
      return false;

    // If the high-contrast accessibility profile property is specified
    // on any style sheet, we have a match.
    for (int i = 0; i < styleSheets.length; i++)
    {
      if (styleSheets[i].getAccessibilityProperties().contains(
            XMLConstants.ACC_HIGH_CONTRAST))
        return true;
    }

    return false;
  }

  // Tests whether the high contrast value specified in the context matches
  // the high contrast value of any matching style sheet
  private static boolean _isLargeFontsMatch(
      StyleContext context,
      StyleSheetNode[] styleSheets)
  {
    if (!context.getAccessibilityProfile().isLargeFonts())
      return false;

    // If the large-fonts accessibility profile property is specified
    // on any style sheet, we have a match.
    for (int i = 0; i < styleSheets.length; i++)
    {
      if (styleSheets[i].getAccessibilityProperties().contains(
            XMLConstants.ACC_LARGE_FONTS))
        return true;
    }

    return false;
  }

  // Direction constants
  private static final String _DIRECTION_RTL = "rtl";

  private static final String _DIRECTION_LTR = "ltr";
  
  // Platform constants
  private static final String _PLATFORM_WINDOWS = "windows";

  private static final String _PLATFORM_MACOS = "macos";

  private static final String _PLATFORM_IPHONE = "iphone";

  private static final String _PLATFORM_LINUX = "linux";

  private static final String _PLATFORM_SOLARIS = "solaris";

  private static final String _PLATFORM_UNIX = "unix";

  private static final String _PLATFORM_PPC = "ppc";

  private static final String _PLATFORM_BLACKBERRY = "blackberry";

  private static final String _PLATFORM_NOKIA_S60 = "nokia_s60";

  private static final String _PLATFORM_GENERICPDA = "genericpda";
  
  private static final String _PLATFORM_ANDROID = "android";

  // Accessibility constants
  private static final String _ACC_HIGH_CONTRAST = "hc";

  private static final String _ACC_LARGE_FONTS = "lf";

  // Name for unknown values (ie. browser, platforms, etc...)
  private static final String _UNKNOWN_NAME = "0";

  // Separator for variants in file names
  private static final char _VARIANT_SEPARATOR = '-';

  // Locale matches
  private static final int _LOCALE_MATCH_NONE = 0;

  private static final int _LOCALE_MATCH_LANGUAGE = 1;

  private static final int _LOCALE_MATCH_FULL = 2;
}
