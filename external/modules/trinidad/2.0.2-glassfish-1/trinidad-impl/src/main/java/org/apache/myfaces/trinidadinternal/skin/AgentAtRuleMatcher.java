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
package org.apache.myfaces.trinidadinternal.skin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;

import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;

/**
 * Threadsafe immutable class that stores the @agent rule for a particular @agent query string
 * from the skinning css file (e.g., @agent ie and (version:5), ie and (version:6), gecko {}).
 * This class is used to match the current agent against the @agent rule.
 * @see StyleSheetNode
 * @see NameUtils
 */
public final class AgentAtRuleMatcher
{

  /**
   * Enumeration representing the result of a call to <code>match</code>.
   * @see #match
   */
  public enum Match
  {
    /** The Match matched the agent applications */
    APPLICATION,
    /** The Match matched the agent version */
    VERSION,
    /** the Match matched capability touchScreen */
    CAP_TOUCH_SCREEN
  };

  /**
   * Creates an AgentAtRuleMatcher from an @agent query string.  if the @agent rule were
   * <code>@agent ie and (version:8), gecko and (version:1.9) { .foo {background-color:red}}</code>
   * the query string would be <code>"ie and (version:8), gecko and (version:1.9)"</code>
   * @param queryString String containing the query portion of the @agent at-rule
   */
  public AgentAtRuleMatcher(String queryString)
  {
    // split each of the comma-separated 'or' pieces into separate strings from processing
    this(queryString.split(","));
  }


  /**
   * Creates an AgentAtRuleMatcher from a decomposed array of @agent query selectors. With one
   * array entry for each comma-separated selector.
   * if the @agent rule were
   * <code>@agent ie and (version:8), gecko and (version:1.9) { .foo {background-color:red}}</code>
   * the decomposed selectors would be:
   * <code>selector[0] = "ie and (version:8)"; selector[1] = "gecko and (version:1.9)";</code>
   * @param selectors Array of query selectors with one selector for every or'ed rule
   */
  public AgentAtRuleMatcher(String[] selectors)
  {
    Map<TrinidadAgent.Application, Set<AgentMatcher>> selectorAgents = Collections.emptyMap();
    Set<AgentMatcher> capTouchMatchers = new HashSet<AgentMatcher>();

    // process each of the selectors to build the Map of the AgentMatchers to run for any
    // particular agent in this array of rules.
    for (int selectorIndex = 0; selectorIndex < selectors.length; selectorIndex++)
    {
      // parse the agent versions. Examples:
      // @agent ie and (version:6)
      // @agent ie and (version:6.*)
      // @agent ie and (version:5.0.*)
      // @agent ie and (min-version:5.*) and (max-version:6)
      // @agent ie and (version:6), ie and (version:7), gecko and (version:1.1)
      // @agent (touchScreen)
      // @agent (touchScreen:none)
      // @agent (touchScreen:none), ie and (version:6)
      // @agent webkit and (version:6), (touchScreen)
      // @agent webkit and (touchScreen:single)
      // @agent webkit and (version:9) and (touchScreen:multiple)

      String currSelector = selectors[selectorIndex].trim();
      if (currSelector.startsWith(TOUCH_SCREEN_RULE_STR))
      {
  	    capTouchMatchers.add(_getTouchScreenMatcher(currSelector));
        continue;
      }

      // split each of the sections between the "and"s.  For example
      // gecko and (min-version:1.5) and (max-version:1.9) would result in
      // {"gecko", "(min-version:1.5)", "(max-version:1.9)"}
      String[] sections = _AND_SPLITTER.split(currSelector);

      // the agent is always the first section and is required.
      // We trim because we are often passed strings with trailing whitespace
      String agentName = sections[0].trim();

      // convert the name of the agent to its TrinidadAgent.Application enum
      TrinidadAgent.Application browser = NameUtils.getAgentApplication(agentName);

      // turn the array of sections into the Set of AgentMatchers
      //
      if (browser != TrinidadAgent.Application.UNKNOWN)
      {
        int sectionCount = sections.length;

        Set<AgentMatcher> agentMatchers;

        if (sectionCount == 1)
        {
          // we only have the agent identifier, so no matchers
          agentMatchers = Collections.emptySet();
        }
        else
        {
          AgentMatcher newAgentMatcher;

          if (sectionCount > 2)
          {
            // we have multiple ands, so we need to build up the list of sections to pass to
            // to the AndMatcher, which will AND the results of each of the matches together
            List<AgentMatcher> andedMatchers = new ArrayList<AgentMatcher>(sectionCount - 1);

            // create agent matchers for each section
            // this could be version matcher or touchScreen matcher
            for (int sectionIndex = 1; sectionIndex < sectionCount; sectionIndex++)
            {
              andedMatchers.add(_getAgentMatcher(sections[sectionIndex]));
            }

            newAgentMatcher = new AndMatcher(andedMatchers);
          }
          else
          {
            // only a single and with a agent match, so return that matcher directly
            // this could be version matcher or touchScreen matcher
            newAgentMatcher = _getAgentMatcher(sections[1]);
          }

          // if you have selectors that repeat the browser, then get the agentMatchers that
          // you have so far for that browser so you can append to it.
          // @agent ie and (version: 6), ie and (version: 7), gecko and (version: 1.9)
          // Th comma ORs the rules together.
          agentMatchers = selectorAgents.get(browser);

          // create the Version Set if it doesn't already exist, or add the Version to the
          // current Version Set if it isn't already in there.  Since most Version entries are
          // a single value, we optimize for that case
          if ((agentMatchers == null) || agentMatchers.isEmpty())
            agentMatchers = Collections.singleton(newAgentMatcher);
          else if (!agentMatchers.contains(newAgentMatcher))
          {
            // we didn't already have an entry for this matcher.  If the old size is 1, then
            // the current set is going to be a Collections.singleton(), which is immutable,
            // so we need to copy the the singleton Set into a HashSet, which IS mutable
            if (agentMatchers.size() == 1)
              agentMatchers = new HashSet<AgentMatcher>(agentMatchers);

            agentMatchers.add(newAgentMatcher);
          }
        }

        // optimize the size of the SelectorAgents Map when we add the new Versions
        if (selectorAgents.isEmpty())
          selectorAgents = Collections.singletonMap(browser, agentMatchers);
        else if (!agentMatchers.equals(selectorAgents.get(browser)))
        {
          if (selectorAgents.size() == 1)
          {
            // similar issue as with the Set case above.  If the Map currently has only one
            // entry, then we have an immutable singleton Map and we need to copy the single item
            // out into a new mutable Map.  Chances are, this map isn't going to be very big, so
            // a size of 3 seems a good start.
            Map<TrinidadAgent.Application, Set<AgentMatcher>> newSelectorAgents =
                                   new HashMap<TrinidadAgent.Application, Set<AgentMatcher>>(3);

            // get the single entry out of here so we can add it into the new mutable map
            Map.Entry<TrinidadAgent.Application, Set<AgentMatcher>> singleEntry =
                                                       selectorAgents.entrySet().iterator().next();

            newSelectorAgents.put(singleEntry.getKey(), singleEntry.getValue());

            selectorAgents = newSelectorAgents;
          }

          // add the new mapping
          selectorAgents.put(browser, agentMatchers);
        }
      }
    }

    _selectorAgents = selectorAgents;
    _capTouchMatchers = capTouchMatchers;
    _hashCode = _calculateStableHashCode(_selectorAgents, _capTouchMatchers);
  }

  /**
   * Parses touchScreen rule and creates appropriate AgentMatcher
   * @param currSelector
   */
  private TouchScreenCapabilityMatcher _getTouchScreenMatcher(String currSelector) {
    // split out the separate <capability> and <value> parts of the property selector.  The
    // <capability> will be in the first group, the <value> in the second.
    Matcher m = _PROPERTY_SPLITTER.matcher(currSelector);
    String[] capTouchArray = new String[]{};

    if (m.find())
    {
      String propName = m.group(1);
      String propValue = m.group(2);

      // Needless to check if propName is touchScreen or not. It is already done in the caller side.
      if (propValue != null)
      {
        String capValue = propValue.trim();
        if (_ALL_TOUCH_CAPABILITIES.contains(capValue))
          capTouchArray = new String[] {capValue};
        else
        {
          _LOG.warning("INVALID_AGENT_PROPERTY", new Object[]{propName, capValue});
        }
      }
    }
    else
    {
      // when (touchScreen) is specified _PROPERTY_SPLITTER fails to match
      // in this case add single and multiple
      capTouchArray = _AFFIRMATIVE_TOUCH_CAPABILITIES;
    }

    if (capTouchArray.length > 0) {
      Set<String> capTouchValues = Collections.unmodifiableSet(new HashSet<String>(Arrays.<String>asList(capTouchArray)));
      TouchScreenCapabilityMatcher matcher = new TouchScreenCapabilityMatcher(capTouchValues);
      return matcher;
    }

    // If touchScreen syntax was not parsed and no AgentMatcher was created.
    throw new IllegalArgumentException("Invalid @agent rule specified: " + currSelector);
  }



  /**
   * Because Enums don't have stable hash codes, we can't use their hash code directly.  Instead
   * we want to use the has code of the enum's name, which should be stable.  Here we essentially
   * duplicate the hash code calculation of Map, using the stable hash code instead
   * @return stable hash code
   */
  private static int _calculateStableHashCode(final Map<TrinidadAgent.Application, Set<AgentMatcher>> selectorAgents,
                                              final Set<AgentMatcher> capTouchMatchers)
  {
    int hashCode = 0;

    // Map hash code is defined as the additive hash code of the entries
    for (Map.Entry<TrinidadAgent.Application, Set<AgentMatcher>> entry : selectorAgents.entrySet())
    {
      // use the enum's name to have a stable hash code
      int stableKeyHashCode = entry.getKey().name().hashCode();

      // entry hash code is defined as the XOR of the key and value.
      int entryHashCode = stableKeyHashCode ^ entry.getValue().hashCode();

      hashCode += entryHashCode;
    }

    hashCode += capTouchMatchers.hashCode();

    return hashCode;
  }

  private AgentMatcher _getAgentMatcher(String propertySelector)
  {
    propertySelector = propertySelector.trim();
    if (propertySelector.startsWith(TOUCH_SCREEN_RULE_STR))
    {
      return _getTouchScreenMatcher(propertySelector);
    } else
    {
      return _getVersionMatcher(propertySelector);
    }

  }

  /**
   * Given a property selector of the form
   * (<opt whitespace><identifier><opt whitespace>:<opt whitespace><version><opt whitespace>)
   * where "identifier" can be "min-version", "max-version" or "version" and "value" represents
   * a browser or HTML layout engine version number, return a VersionMatcher that will
   * perform the correct comparison against the current Agent.
   * @param propertySelector String containing the type of version comparison and version value
   * to match
   * @return VersionMatcher for the property selector
   */
  private VersionMatcher _getVersionMatcher(String propertySelector)
  {
    // split out the separate <identifier> and <version> parts of the property selector.  The
    // <identifier> will be in the first group, the <version> in the second.
    Matcher m = _PROPERTY_SPLITTER.matcher(propertySelector);

    if (m.find())
    {
      String propName = m.group(1);
      String version = m.group(2);

      // turn the property name--either "min-version", "max-version" or "version" into the
      // type of comparison to perform
      Comparison comparison = _COMPARISON_PARSER.get(propName);

      if (comparison == null)
      {
        throw new IllegalArgumentException("Invalid @agent property name: " + propName);
      }

      // create the new Version, padding the version number out with wildcards
      Version newVersion = new Version(version, "*");

      return new VersionMatcher(newVersion, comparison);
    }
    else
    {
      throw new IllegalArgumentException("Invalid @agent property selector: " + propertySelector);
    }
  }

  /**
   * <p>
   * Called to actually determine if this AgentAtRuleMatcher applies to the current agent.  The
   * result is a Set of Match constants indicating how precise the match is.  While all anded
   * rules must match for the match to succeed, some callers need information regarding whether
   * the Version information was also necessary in order for the match to succeed.  For example,
   * when generating CSS files, the version information is only part of the CSS name if the
   * generated CSS file for the agent contained version-dependent content.
   * </p>
   * <p>
   * There are three possible return values
   * <ol>
   * <li>An empty Set, indicating no match</li>
   * <li>A set containing <code>Match.APPLICATION</code>, indicating a match that only needed to
   * match against the agent Application</li>
   * <li>A set containing both <code>Match.APPLICATION</code> and <code>Match.VERSION</code>,
   * indicating that both the agent Application and its version needed to be matched against</li>
   * </ol>
   * @param agent Agent to test for compatibility with
   * @return the Set of successful matches, if any
   */
  public Set<Match> match(TrinidadAgent agent)
  {
    Set<Match> matches = new HashSet<Match>();
    // If we have browser exact match, compare versions
    TrinidadAgent.Application browser = agent.getAgentApplication();

    if (_selectorAgents.containsKey(browser))
    {
      matches.add(Match.APPLICATION);
      Set<AgentMatcher> agentMatchers = _selectorAgents.get(browser);
      if (!agentMatchers.isEmpty())
      {
        for (AgentMatcher currMatcher : agentMatchers)
        {
          if (currMatcher.match(agent))
          {
            if (currMatcher instanceof VersionMatcher)
              matches.add(Match.VERSION);
            else if (currMatcher instanceof TouchScreenCapabilityMatcher)
              matches.add(Match.CAP_TOUCH_SCREEN);
            else if (currMatcher instanceof AndMatcher)
            {
               AndMatcher andMatcher = (AndMatcher) currMatcher;
                if (andMatcher.hasVersionMatcher())
                  matches.add(Match.VERSION);
                if (andMatcher.hasTouchScreenCapabilityMatcher())
                  matches.add(Match.CAP_TOUCH_SCREEN);
            }
          }
        }

        // Expect at least one match from the matchers
        if (!matches.contains(Match.CAP_TOUCH_SCREEN) && !matches.contains(Match.VERSION))
        {
          // There were one or many matchers but none matched, so remove all matchers.
          matches.clear();
        }
      }
    }

    // If touchScreen is already matched then no need to check further.
    if (!matches.contains(Match.CAP_TOUCH_SCREEN) && !_capTouchMatchers.isEmpty())
    {
      for (AgentMatcher currMatcher : _capTouchMatchers)
      {
        if (currMatcher.match(agent))
        {
          // If there is a capability matching then APPLICATION match should be by default.
          matches.add(Match.APPLICATION);
          matches.add(Match.CAP_TOUCH_SCREEN);
          break;
        }
      }
    }

    return Collections.unmodifiableSet(matches);
  }

  @Override
  public boolean equals(Object other)
  {
    if (this == other)
      return true;
    else
    {
      if (other instanceof AgentAtRuleMatcher)
      {
        AgentAtRuleMatcher otherAgentMatcher = (AgentAtRuleMatcher) other;
        return (_selectorAgents.equals(otherAgentMatcher._selectorAgents))
                && (_capTouchMatchers.equals(otherAgentMatcher._capTouchMatchers));
      }
      else
      {
        return false;
      }
    }
  }

  @Override
  public final int hashCode()
  {
    return _hashCode;
  }

  @Override
  public String toString()
  {
    return super.toString() + "agents=" + _selectorAgents.toString() + "touchScreenCap=" + _capTouchMatchers.toString();
  }

  // the type of comparison to perform on the version
  private static enum Comparison {
    EQUALS,  // Compared object must be equal
    MIN,     // Compared object must be less than or equal to
    MAX};    // Compared object must be greater than or equal to

  /**
   * <p>
   * Abstract class that all matchers against Agents must implement.
   * </p>
   * <p>
   * <code>match(TrinidadAgent)</code> is called to determine if the match succeeds.
   * </p>
   * <p>
   * Since the AgentMatchers
   * are added to Maps (or sets that use Maps in the implementation), they are required to
   * implement <code>equals</code> and <code>hashCode</code>
   * </p>
   * <p>
   * While used to hide the differences between VersionMatchers and AndMatchers,
   * this class should be abstract enough to support any new types of Agent matchers that we might
   * want to add in the future.
   * </p>
   * @see #match
   */
  private static abstract class AgentMatcher
  {
    /**
     * Returns <code>true</code> if the AgentMatcher matches the Agent
     * @param agent Agent to check against
     * @return <code>true</code> if the match succeeds
     */
    public abstract boolean match(TrinidadAgent agent);

    @Override
    public abstract int hashCode();

    @Override
    public abstract boolean equals(Object o);
  }

  /**
   * Immutable and thread-safe AgentMatcher that matches the supplied Version against the
   * version of a TrinidadAgent using the supplied, MAX, MIN, or EQUALS Comparison
   */
  private static final class VersionMatcher extends AgentMatcher
  {
    /**
     * Creates a VersionMatcher
     * @param version Version to compare with
     * @param comparison Comparison to perform
     */
    public VersionMatcher(Version version, Comparison comparison)
    {
      _version = version;
      _comparison = comparison;

      // cache the hash code.  Because enums don't have stable hash codes,
      // we use the hash code of the name of the enum, which is stable
      _hashCode = _version.hashCode() * 37 + _comparison.name().hashCode();
    }

    /**
     * Matches the Version of this VersionMatcher against the Version of the supplied Agent,
     * using the VersionMatcher's comparison
     * @param agent Agent to check the Version of
     * @return <code>true</code> if the comparison is successful
     */
    @Override
    public boolean match(TrinidadAgent agent)
    {
      // use the Version's comparator
      int result = _version.compareTo(agent.getVersion());

      // since MIN means less than or equal to and MAX means greater than or equal to, a result
      // of equality always means success
      if (result == 0)
        return true;
      else
      {
        switch (_comparison)
        {
          case MIN:
            return result < 0; // min version has to be less than current version

          case MAX:
            return result > 0; // max version has to be bigger than current version

          case EQUALS:
            return false; // if the result were equal, we wouldn't have gotten this far

          default:
            assert false : "Unknown comparison type " + _comparison;
            return false; // this should never happen, but the compiler doesn't know that
        }
      }
    }

    @Override
    public final int hashCode()
    {
      return _hashCode;
    }

    @Override
    public boolean equals(Object other)
    {
      if (this == other)
        return true;
      else
      {
        if (other instanceof VersionMatcher)
        {
          VersionMatcher otherVersionMatcher = (VersionMatcher)other;

          return _version.equals(otherVersionMatcher._version) &&
                 _comparison.equals(otherVersionMatcher._comparison);
        }
        else
        {
          return false;
        }
      }
    }

    @Override
    public String toString()
    {
      return super.toString() + ", version=" + _version + ", comparison=" + _comparison;
    }

    private final Version _version;
    private final Comparison _comparison;
    private final int _hashCode;
  }

  /**
   * AgentMatcher that ANDs the results of all calling match() on its AgentMatchers together,
   * short-circuiting on the first AgentMatcher.match() that returns false.
   */
  private static class AndMatcher extends AgentMatcher
  {
    /**
     * Creates an AndMatcher
     * @param matchers List of AgentMatchers to AND together
     */
    public AndMatcher(List<AgentMatcher> matchers)
    {
      // =-= btsulliv should we do anything if matchers contains fewer than two items?  The code
      //              will still work, it just suggests a lack of optimization in the caller
      _matchers = matchers;

      // cache the hashcode so we don't traverse the list asking all of the other matchers for
      // their hash codes each time our hash code is called
      _hashCode = matchers.hashCode();

      for (AgentMatcher matcher : matchers)
      {
        if (matcher instanceof VersionMatcher)
          hasVersionMatcher = true;
        if (matcher instanceof TouchScreenCapabilityMatcher)
          hasTouchScreenCapabilityMatcher = true;
      }
    }

    protected boolean hasTouchScreenCapabilityMatcher()
    {
      return hasTouchScreenCapabilityMatcher;
    }

    protected boolean hasVersionMatcher()
    {
      return hasVersionMatcher;
    }

    /**
     * @param agent Agent to match againt
     * @return <code>true</code> if all of the matchers matched the Agent
     */
    @Override
    public boolean match(TrinidadAgent agent)
    {
      int matcherCount = _matchers.size();

      for (int i = 0; i < matcherCount; i++)
      {
        // a matcher failed, so no sense in trying to match any more.  Let's pick up our
        // ball and go home
        if (!_matchers.get(i).match(agent))
          return false;
      }

      // all of the matchers matched.  Yay!
      return true;
    }

    @Override
    public int hashCode()
    {
      return _hashCode;
    }

    @Override
    public boolean equals(Object other)
    {
      if (this == other)
        return true;
      else
      {
        if (other instanceof AndMatcher)
        {
          return _matchers.equals(((AndMatcher)other)._matchers);
        }
        else
        {
          return false;
        }
      }
    }

    @Override
    public String toString()
    {
      return super.toString() + ", matchers=" + _matchers;
    }

    private final List<AgentMatcher> _matchers;
    private final int _hashCode;
    private boolean hasVersionMatcher;
    private boolean hasTouchScreenCapabilityMatcher;

  }

  /**
   * Immutable and thread-safe AgentMatcher that matches the supplied touchScreen capability against the
   * that of the agent's.
   */
  private static final class TouchScreenCapabilityMatcher extends AgentMatcher
  {
    /**
     * Creates a TouchScreenCapabilityMatcher
     * @param touchCapabilities
     */
    public TouchScreenCapabilityMatcher(Set<String> touchCapabilities)
    {
      if (touchCapabilities == null)
        throw new NullPointerException("touchCapabilities must be non-null");

      _touchCapabilities = touchCapabilities;
      _hashCode = _touchCapabilities.hashCode();
    }

    /**
     * Matches the  TouchScreenCapabilityMatcher against the touchScreen capability of the supplied Agent
     * @param agent Agent to check the touchScreen capability of
     * @return <code>true</code> if the comparison is successful
     */
    @Override
    public boolean match(TrinidadAgent agent)
    {
      String capTouchScreen = (String) agent.getCapabilities().get(TrinidadAgent.CAP_TOUCH_SCREEN);
      for (String touchCap : _touchCapabilities)
      {
        if (capTouchScreen.equals(touchCap))
        {
          return true;
        }
      }

      return false;
    }

    @Override
    public final int hashCode()
    {
      return _hashCode;
    }

    @Override
    public boolean equals(Object other)
    {
      if (this == other)
        return true;
      else
      {
        if (other instanceof TouchScreenCapabilityMatcher)
        {
          TouchScreenCapabilityMatcher otherMatcher = (TouchScreenCapabilityMatcher)other;
          return _touchCapabilities.equals(otherMatcher._touchCapabilities);
        }
        else
        {
          return false;
        }
      }
    }

    @Override
    public String toString()
    {
      return super.toString() + ", touchCapabilities=" + _touchCapabilities;
    }

    private final Set<String> _touchCapabilities;
    private final int _hashCode;
  }

  // RegExp Pattern used to breaking apart the property selectors into a separate property name
  // and value.  The property selectors are in the form:
  // (<opt whitespace><prop name><opt whitespace>:<opt whitespace><prop value><opt whitespace>)
  // This isn't as complicated as it seems at first.  Taking it from outside-in
  // 1) The double backslashes are because Java Strings already use backslashes for escaping, so
  //    a double backslash is necessary in order to generate the single backslash needed for RegExp,
  //    so this is really the RegExp pattern
  // \(\s*([A-Za-z0-9_-]+)\s*:\s*(\S+)\s*\)
  // 2) We are using parentheses for both exact character matching (the parentheses at the ends of
  // the RegExp), and for RegExp grouping (so that we can extract the property name and value.
  // Notice that since we need to exactly match the parentheses at the ends, they are escaped.
  // Removing these gives us:
  // \s*([A-Za-z0-9_-]+)\s*:\s*(\S+)\s*
  // 3) The \s* represent the option white space, removing these gives us the grouping expressions
  // for the property name:([A-Za-z0-9_-]+) and property value (\S+)
  // The property name is allowed to contain one or more word characters, plush the character '-'
  // (needed for min-version and max-version), while the property value can contain any
  // non-whitespace character
  private static final Pattern _PROPERTY_SPLITTER =
    Pattern.compile("\\(\\s*([A-Za-z0-9_-]+)\\s*:\\s*(\\S+)\\s*\\)");

  // used for splitting up the agenet selectors between the "and"s.  Just in case a "sand" operator
  // is added later, we require the "and" to be surrounded with whitespace.
  private static final Pattern _AND_SPLITTER = Pattern.compile("\\s+and\\s+");

  private static final String[] _AFFIRMATIVE_TOUCH_CAPABILITIES =
    new String[]
    { TrinidadAgent.TOUCH_SCREEN_MULTIPLE.toString(),
      TrinidadAgent.TOUCH_SCREEN_SINGLE.toString()
    };

  private static final Set<String> _ALL_TOUCH_CAPABILITIES =
    new HashSet<String>(Arrays.<String>asList(new String[]
        { TrinidadAgent.TOUCH_SCREEN_MULTIPLE.toString(),
          TrinidadAgent.TOUCH_SCREEN_SINGLE.toString(),
          TrinidadAgent.TOUCH_SCREEN_NONE.toString()
        }));

  // map of property names to the types of comparison to use for each name
  private static final Map<String, Comparison> _COMPARISON_PARSER;

  static
  {
    // initialize property-name to Comparison type Map.  Thread-safe because the Map is not
    // modified after initialization
    _COMPARISON_PARSER = new HashMap<String, Comparison>(4);

    _COMPARISON_PARSER.put("version", Comparison.EQUALS);
    _COMPARISON_PARSER.put("max-version", Comparison.MAX);
    _COMPARISON_PARSER.put("min-version", Comparison.MIN);
  }

  // As we need to be able to have multiple version matching rules for the same agent:
  // @agent ie and (version:5), ie and (version:6), gecko.
  // We store a map of agents and their version sets
  private final Map<TrinidadAgent.Application, Set<AgentMatcher>> _selectorAgents;
  private final Set<AgentMatcher> _capTouchMatchers;

  // cached hash code
  private final int _hashCode;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(AgentAtRuleMatcher.class);
  private static final String TOUCH_SCREEN_RULE_STR = "(" + TrinidadAgent.CAP_TOUCH_SCREEN.getCapabilityName();
}
