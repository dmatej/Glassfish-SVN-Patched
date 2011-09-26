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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;
import org.apache.myfaces.trinidadinternal.style.util.ModeUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * Private implementation of StyleSheetNode. A StyleSheetNode has StyleNodes for particular
 * browsers, direction, versions, platforms and mode.  In addition, the StyleSheetNode
 * provides access to IconNodes representing the icons which were defined within
 * the context of this style sheet. StyleSheetNodes are contained in StyleSheetDocuments.
 * And a StyleSheetNode is created for .css files.
 * .css skin files create StyleSheetNodes via SkinStyleSheetParserUtils
 * @see org.apache.myfaces.trinidadinternal.skin.SkinStyleSheetParserUtils
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetNode.java#0 $) $Date: 10-nov-2005.18:58:46 $
 */
public class StyleSheetNode
{

  /**
   * Creates a StyleSheetNode with the specified attributes.
   */
  public StyleSheetNode(
    StyleNode[] styles,
    Collection<IconNode> icons,
    Set<Locale> locales,
    int direction,
    AgentAtRuleMatcher agentMatcher,
    int[] platforms,
    int mode,
    Set<String> accessibilityProperties
    )
  {
    // StyleNodes order might matter so this is a List
    if (styles != null)
      _styles = Collections.unmodifiableList(Arrays.asList(styles));
    else
      _styles = Collections.emptyList();

    if (icons != null)
      _icons = Collections.unmodifiableList(new ArrayList<IconNode>(icons));
    else
      _icons = Collections.emptyList();

    // locales, browsers, versions, platforms order does not matter, so these are Sets.
    if (locales != null)
    {
      _locales = Collections.unmodifiableSet(locales);
    }
    else
      _locales = Collections.emptySet();

    _agentMatcher = agentMatcher;

    if (platforms != null)
    {
      Set<Integer> platformsSet = _copyIntArrayToSet(platforms);
      _platforms = Collections.unmodifiableSet(platformsSet);
    }
    else
      _platforms = Collections.emptySet();

    if (accessibilityProperties != null)
    {
      Set<String> accPropsSet = _copyAccessibilityProperties(accessibilityProperties);
      _accProps = Collections.unmodifiableSet(accPropsSet);
    }
    else
      _accProps = Collections.emptySet();
      
    _mode = mode;
    _direction = direction;
    _id = _computeStyleSheetId();
  }

  /**
   * Implementation of StyleSheetNode.getStyles().
   */
  public Collection<StyleNode> getStyles()
  {
    return _styles;
  }

  /**
   * Returns icons contained by this StyleSheetNode.
   */
  public Collection<IconNode> getIcons()
  {
    return _icons;
  }
 
  /**
   * Implementation of StyleSheetNode.getReadingDirection();
   */
  public int getReadingDirection()
  {
    return _direction;
  }
  
  public int getMode()
  {
    return _mode;
  }

  /**
   * Implementation of StyleSheetNode.getLocales().
   */
  public Collection<Locale> getLocales()
  {
    return _locales;
  }

  /**
   * @return AgentAtRuleMatcher for matching @agent selectors
   */
  public AgentAtRuleMatcher getAgentMatcher()
  {
    return _agentMatcher;
  }

  /**
   * Implementation of StyleSheetNode.getPlatforms().
   */
  public Collection<Integer> getPlatforms()
  {
    return _platforms;
  }

  /**
   * Returns the accessibility properties for this StyleSheetNode.
   */
  public Collection<String> getAccessibilityProperties()
  {
    return _accProps;
  }


  /**
   * Tests whether this StyleSheet matches the specified variants.
   * Returns a number indicating the specificity of the match.
   * Zero means there is no match.  Larger numbers indicate
   * better matches.  The value returned by compareVariants is used to
   * sort style sheets according to precedence.
   * @param mode
   */
  public int compareVariants(
    Locale               locale,
    int                  direction,
    TrinidadAgent        agent,
    int                  mode,
    AccessibilityProfile accessibilityProfile)
  {
    int localeMatch = _compareLocale(locale);
    if (localeMatch == 0)
      return 0;

    int directionMatch = _compareDirection(direction);
    if (directionMatch == 0)
      return 0;
    
    int browserAndVersionMatch = _compareBrowserAndVersion(agent);
    if (browserAndVersionMatch == 0)
      return 0;
    int modeMatch = _compareMode(mode);
    if(modeMatch == 0)
      return 0;

    int osMatch = _compareOS(agent.getAgentOS());
    if (osMatch == 0)
      return 0;

    int accessibilityMatch = _compareAccessibility(accessibilityProfile);
    if (accessibilityMatch == 0)
      return 0;

    return (localeMatch | browserAndVersionMatch | osMatch | accessibilityMatch);
  }

  @Override
  public final boolean equals(Object obj)
  {
    // A single StyleSheetNode is created for each "style sheet"
    // section in a StyleSheetDocument.  As such, we have no need
    // for a logical equality test.  We always want to perform
    // identity comparisons when determining the equality of
    // two StyleSheetNodes.  We use the default Object.equals()
    // implementation to achieve this.
    
    // Note that technically speaking this override is not needed.
    // However, to avoid confusion about whether we should be performing
    // logical equality vs. identity comparisons, we explicitly define
    // this final override, if only to serve as documentation of our
    // intentions.

  
    return super.equals(obj);
  }

  @Override
  public final int hashCode()
  {
    // Since unique StyleSheetNode instance is created for each "style sheet"
    // section in a StyleSheetDocument, and since we have no need for
    // logical equality comparisions, the default hashCode() implementation
    // is sufficient for our needs.
    
    // Like the override of equals(), this override is not strictly
    // necessary, but is here to make it clear that we always want to
    // use the default Object.hashCode() implementation.

    return super.hashCode();
  }
  
  @Override
  public String toString()
  {
    return getClass().getName() + "[" +
      "locales="   + (_locales != null ? _locales.toString() : "")    + ", " +
      "direction=" + _getDirectionString() + ", " +
      "agentVersions="  + (_agentMatcher != null ? _agentMatcher.toString() : "")  + ", " +
      "platforms=" + (_platforms != null ? _platforms.toString() : "") + ", " +
      "styles="    + (_styles != null ? _styles.toString() : "") + ", " +
      "icons="     + (_icons != null ? _icons.toString() :"")     + ", " +
      "accessibility-profile=" + (_accProps != null ? _accProps.toString() : "") + "]";

  }

  /**
   * Returns an int-based id which can be used by StyleSheetDocument.getDocumentId()
   * to (semi-)uniquely identify this StyleSheetNode.  This id is very similar
   * to a hash code, though is not exactly the same, as some StyleSheetNode
   * properties (such as icons) are intentionally excluded from the style sheet
   * id.
   * 
   * @return An integer value suitable for use by StyleSheetDocument.getDocumentId().
   */
  public int getStyleSheetId()
  {
    return _id;
  }

  // Compute the style sheet id and return it.
  private int _computeStyleSheetId()
  {
    // This is very similar to computing a hash code.  However, we
    // explicitly exclude properties which have no impact on the
    // generated style sheet, such as icons.  The reason for this
    // is that the generated style sheet version produced by
    // StyleSheetDocument.getDocumentId() should not change unless
    // the generated style sheet itself changes.  Since icons do not
    // impact the generated style sheet, these are excluded form the
    // style sheet id.

    int hash = 17;
    hash = 37*hash + _mode;
    hash = 37*hash + _direction;
    hash = 37*hash + _locales.hashCode();
    hash = 37*hash + ((_agentMatcher != null) ? _agentMatcher.hashCode() : 0);
    hash = 37*hash + _platforms.hashCode();
    hash = 37*hash + _styles.hashCode();
    hash = 37*hash + _accProps.hashCode();
    
    return hash;
  }

  // Compares the specified locale against the supported variants
  private int _compareLocale(Locale locale)
  {
    // If we don't have any locales specified, anything matches
    if (_locales.isEmpty())
      return _LOCALE_UNKNOWN_MATCH;

    // On the other hand, if the client-locale is not specified,
    // but we do have a locale specified, there is no match.
    if (locale == null)
      return 0;

    int match = 0;

    if (_locales.contains(locale))
        return _LOCALE_EXACT_MATCH;

    for (Locale tmpLocale : _locales)
    {
      if (tmpLocale.getLanguage().equals(locale.getLanguage()))
      {
        if (tmpLocale.getCountry().equals(locale.getCountry()))
        {
          match = _LOCALE_EXACT_MATCH;
          break;
        }

        // If we've got a partial match, keep looking - we may could find
        // an exact match
        match = _LOCALE_PARTIAL_MATCH;
      }
    }

    return match;
  }

  // Compares the specified direction against the supported direction
  private int _compareDirection(int direction)
  {
    // If we don't have a locale specified, we match anything
    if (_direction == LocaleUtils.DIRECTION_DEFAULT)
      return _DIRECTION_UNKNOWN_MATCH;

    // This comparison will return 0 if the client-direction is
    // not specified (ie. if direction == DIRECTION_DEFAULT).
    if (direction == _direction)
      return _DIRECTION_EXACT_MATCH;

    return 0;
  }
  
  private int _compareMode(int mode)
  {
    if (_mode == ModeUtils.MODE_DEFAULT)
      return _MODE_UNKNOWN_MATCH;
    
    if(mode == _mode)
      return _MODE_EXACT_MATCH;
    
    return 0;
  }

  // Compares the browser and its version against the supported variants
  // This uses the AgentAtRuleMatcher object _agentMatcher, which stores the agent  
  // information for the StyleSheetNode and has a built-in matcher.
  private int _compareBrowserAndVersion(TrinidadAgent agent)
  {
    // If we don't have a browser specified, we match anything
    if (_agentMatcher == null)
      return _BROWSER_UNKNOWN_MATCH;

    TrinidadAgent.Application application = agent.getAgentApplication();

    // On the other hand, if we do have a browser specified, but
    // the client browser is not known, we don't have a match
    if (application == TrinidadAgent.Application.UNKNOWN)
      return _NO_MATCH;
    
    Set<AgentAtRuleMatcher.Match> matches = _agentMatcher.match(agent);
    
    int matchResult = _NO_MATCH;
    
    // first check that we have a match against the browser
    if (matches.contains(AgentAtRuleMatcher.Match.APPLICATION))
    {
      matchResult |= _BROWSER_EXACT_MATCH;
      
      // check if the browser also specified a version match that we matched
      int versionMatch = (matches.contains(AgentAtRuleMatcher.Match.VERSION))
                           ? _VERSION_EXACT_MATCH
                           : _VERSION_UNKNOWN_MATCH;
      matchResult |= versionMatch;

      int capTouchMatch = (matches.contains(AgentAtRuleMatcher.Match.CAP_TOUCH_SCREEN))
                           ? _CAP_TOUCH_SCREEN_EXACT_MATCH
                           : _CAP_TOUCH_SCREEN_UNKNOWN_MATCH; 
      matchResult |= capTouchMatch;
    }

    return matchResult;
  }


  // Compares the specified OS against the supported variants
  private int _compareOS(int os)
  {
    // If we don't have a platform specified, we match anything
    if (_platforms.isEmpty())
      return _OS_UNKNOWN_MATCH;

    // On the other hand, if we do have a platform specified, but
    // the client platform is unknown, we don't have a match.
    if (os == TrinidadAgent.OS_UNKNOWN)
      return 0;
      
    if (_platforms.contains(Integer.valueOf(os)))
      return _OS_EXACT_MATCH;

    if (_isUnixPlatform(os) && (_platforms.contains(Integer.valueOf(__OS_UNIX))))
      return _OS_PARTIAL_MATCH;

    return 0;
  }

  // Compares accessibilty profile against supported variants.
  private int _compareAccessibility(AccessibilityProfile accProfile)
  {
    // If we don't have any accessibility properties, we match anything.
    if (_accProps.isEmpty())
      return _ACC_UNKNOWN_MATCH;

    // If we match any property, the style sheet is a match
    for (String accProp : _accProps)
    {
      if (_isCompoundAccessibilityProperty(accProp))
      {
        if (_matchCompoundAccessibilityProperty(accProp, accProfile))
          return _ACC_EXACT_MATCH;
      }
      else if (_matchAccessibilityProperty(accProp, accProfile))
        return _ACC_EXACT_MATCH;
    }

    return 0;
  }

  // Tests whether the specified property is a compound accessibility property.
  private boolean _isCompoundAccessibilityProperty(String propertyName)
  {
    // Compund acc properties use "&" to separate the individual properties
    return (propertyName.contains("&"));
  }
  
  // Tests whether the we have a match for a compound accessibility property.
  private boolean _matchCompoundAccessibilityProperty(
    String               propertyName,
    AccessibilityProfile accProfile
    )
  {
    StringTokenizer tokens = new StringTokenizer(propertyName, "&");

    while (tokens.hasMoreTokens())
    {
      // If any piece of the compound property fails to match, the
      // compound property fails to match
      if (!_matchAccessibilityProperty(tokens.nextToken(), accProfile))
        return false;
    }
    
    // Everything matched - the compound property matches.
    return true;
  }

  // Tests whether we have a match for a particular accessibility
  // property.
  private boolean _matchAccessibilityProperty(
    String               propertyName,
    AccessibilityProfile accProfile
    )
  {
    if (XMLConstants.ACC_HIGH_CONTRAST.equals(propertyName))
      return accProfile.isHighContrast();
    if (XMLConstants.ACC_LARGE_FONTS.equals(propertyName))
      return accProfile.isLargeFonts();

    // Should never reach here (did we add a new property?)
    assert(false);
    
    return false;
  }

  // Get a String representing the direction
  private String _getDirectionString()
  {
    if (_direction == LocaleUtils.DIRECTION_DEFAULT)
      return _EMPTY_STRING;
    return NameUtils.getDirectionName(_direction);
  }

  // Tests whether the int n is contained within the int array
  private static boolean _containsInt(int n, int[] array)
  {
    if (array == null)
      return false;

    for (int i = 0; i < array.length; i++)
    {
      if (array[i] == n)
        return true;
    }

    return false;
  }
  
  // Returns a copy of the int array into a Set<Integer>
  private static Set<Integer> _copyIntArrayToSet(int[] array)
  {
   int arrayCount = (array != null)
                      ? array.length
                      : 0;

   Set<Integer> set = new HashSet<Integer>(arrayCount);

   for (int i=0; i < arrayCount ; i++)
     set.add(array[i]);

   return set;
  }
  
  // Returns a copy of the Locale array into a Set<Locale>
  private static Set<Locale> _copyLocaleArrayToSet(Locale[] array)
  {
   int arrayCount = (array != null)
                      ? array.length
                      : 0;

   Set<Locale> set = new HashSet<Locale>(arrayCount);

   for (int i=0; i < arrayCount ; i++)
     set.add(array[i]);

   return set;
  }

  // Copies accessibility properties from an array into a set.
  private static Set<String> _copyAccessibilityProperties(Set<String> accProps)
  {
    return new HashSet<String>(accProps);
  }

  // Tests whether the specified Agent.OS value is a Unix platform
  private static boolean _isUnixPlatform(int os)
  {
    return (_containsInt(os, _UNIX_PLATFORMS));
  }

  private final List<StyleNode> _styles;     // The styles contained within this node
  private final List<IconNode>  _icons;      // The icons contained within this node
  // Order does not matter for locales, browsers, versions, platforms
  private final Set<Locale>     _locales;    // The locale variants
  private final int             _direction;  // The reading direction
  
  // matches Agent at-rules
  private final AgentAtRuleMatcher _agentMatcher;
  private final Set<Integer>    _platforms;  // The platform variants
  private final int             _mode;       // The mode
  private final Set<String>     _accProps;   // Accessibility profile properties
  private final int             _id;         // The cached style sheet id

  // Constants for accessibility matches - 0x0f000000 bits
  private static final int _NO_MATCH                = 0;

  private static final int _ACC_EXACT_MATCH         = 0x02000000;
  private static final int _ACC_UNKNOWN_MATCH       = 0x01000000;

  // Constants for mode matches - 0x00f00000 bits
  private static final int _MODE_EXACT_MATCH        = 0x00200000;
  private static final int _MODE_UNKNOWN_MATCH      = 0x00100000;

  // Constants for locale matches - 0x000f0000 bits
  private static final int _LOCALE_EXACT_MATCH      = 0x00040000;
  private static final int _LOCALE_PARTIAL_MATCH    = 0x00020000;
  private static final int _LOCALE_UNKNOWN_MATCH    = 0x00010000;

  // Constants for direction matches - 0x0000f000 bits
  private static final int _DIRECTION_EXACT_MATCH   = 0x00002000;
  private static final int _DIRECTION_UNKNOWN_MATCH = 0x00001000;
  
  // Constants for browser matches - 0x00000f00 bits
  private static final int _BROWSER_EXACT_MATCH     = 0x00000200;
  private static final int _BROWSER_UNKNOWN_MATCH   = 0x00000100;

  // Constants for version matches - 0x000000f0 bits
  private static final int _VERSION_EXACT_MATCH     = 0x00000020;
  private static final int _VERSION_UNKNOWN_MATCH   = 0x00000010;

  // Constants for capability touchScreen matches - 0x000000f0 bits
  private static final int _CAP_TOUCH_SCREEN_EXACT_MATCH     = 0x00000040;
  private static final int _CAP_TOUCH_SCREEN_UNKNOWN_MATCH   = 0x00000030;

  // Constants for os matches - 0x0000000f bits
  private static final int _OS_EXACT_MATCH          = 0x00000004;
  private static final int _OS_PARTIAL_MATCH        = 0x00000002;
  private static final int _OS_UNKNOWN_MATCH        = 0x00000001;

  private static final String _EMPTY_STRING = "\"\"";

  // List of all known Unix platforms.   This array must be updated any
  // time a new Unix OS constant is added Agent.
  private static final int[] _UNIX_PLATFORMS =
  {
    TrinidadAgent.OS_LINUX,
    TrinidadAgent.OS_SOLARIS
  };

  // This special platform constant is used to indicate that the style sheet
  // is Unix-specific, but not specific to a particular Unix OS.  It is
  // package private. Agent.OS constants start from 0.
  // We use Integer.MAX_VALUE to avoid collisions
  static final int __OS_UNIX = Integer.MAX_VALUE;
}
