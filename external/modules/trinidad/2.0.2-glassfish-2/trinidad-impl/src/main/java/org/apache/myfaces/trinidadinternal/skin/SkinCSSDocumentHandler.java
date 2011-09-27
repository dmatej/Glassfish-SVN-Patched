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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;

import java.text.ParseException;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Set;
import java.util.HashSet;
import java.util.HashMap;
import java.util.LinkedList;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.io.CachingInputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParseErrorUtils;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.style.util.ModeUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/** As the Skin css file is parsed, methods in this class are called to
 * build up a SkinStyleSheetNode.
 * TODO figure out if this is thread-safe
 *
 */
public class SkinCSSDocumentHandler
{
  /* ParseContext is useful for parsing CSS files only because we use it to set/get properties,
   * like the current NameResolver and InputStreamProvider.
   */
   public SkinCSSDocumentHandler(ParseContext pContext)  
   {
    _parseContext = pContext;     
   }

  /**
   * Return the List of SkinStyleSheetNodes that was created
   *  at the end of parsing the skin css file (endDocument).
   */
  public List <SkinStyleSheetNode> getSkinStyleSheetNodes()
  {
    // We now have a list of CompleteSelectorNodes.
    // We need to group this list into stylesheet nodes by matching
    // the additional information, like direction.
    // Then we create a list of SkinStyleSheetNodes.
    List <SkinStyleSheetNode> skinStyleSheetNodes = 
      _createSkinStyleSheetNodes(_completeSelectorNodeList, _namespaceMap);
     
    List<SkinStyleSheetNode> allSkinStyleSheetNodes = new ArrayList<SkinStyleSheetNode>();

    if (_imports != null && !_imports.isEmpty())
    {
      // _imports is a List<List<SkinStyleSheetNode>>();;
      for (List <SkinStyleSheetNode> nodeList : _imports)
      {
        for (SkinStyleSheetNode node : nodeList)
        {
          allSkinStyleSheetNodes.add(node);
        }
      }

    }
    
    allSkinStyleSheetNodes.addAll(skinStyleSheetNodes);

    return
      allSkinStyleSheetNodes;
  }
  
   
  /**
  * Call this at the start of parsing the skin css file.
  */
  public void startDocument()
  {
    // do nothing
  }
   
  /**
  * Call this at the end of parsing the skin css file.
  */
  public void endDocument()
  {
    // do nothing
  }

  public void comment(String text)
  {
     // ignore comments
  }
   
  /**
  * Call this at the beginning of parsing one set of selectors/properties.
  * e.g., .AFDefaultFont, af|breadCrumbs::font
  * {font-family:Arial,Helvetica; font-size:small}
  */
  public void startSelector()
  {
    _inStyleRule = true;
    // CSS spec says to ignore all @import rules after any other rules are processed.
    _ignoreImports = true;
    _propertyNodeList = new ArrayList<PropertyNode>();
  }
   
  /**
   * Call this at the end of parsing one set of selectors/properties.
   * @param selectors A List of Strings, each String is a selector.
   * e.g., given the selectors/properties:
   * .AFDefaultFont, af|breadCrumbs::font
   * {font-family:Arial,Helvetica; font-size:small}
   * The selectors in the List are
   * ".AFDefaultFont" and "af|breadCrumbs::font"
  */
  public void endSelector(List<String> selectors)
  {
    if (selectors == null)
      return;
      
    int selectorNum = selectors.size();
    
    for (int i = 0; i < selectorNum; i++)
    {
       String selector = selectors.get(i);
       CompleteSelectorNode node =
         _createCompleteSelectorNode(selector,
                                     _propertyNodeList,
                                     _locales,
                                     _agentAtRuleMatcher,
                                     _selectorPlatforms,
                                     _getSelectorAccProperties(),
                                     _mode);
       _completeSelectorNodeList.add(node);
    }
    // reset flags
    _inStyleRule = false;
    _propertyNodeList = null;
  }

   /**
    * Call this when a property name/value is found.
    * e.g., given the selectors/properties:
    * .AFDefaultFont, af|breadCrumbs::font
    * {font-family:Arial,Helvetica; font-size:small}
    * One property name/value pair is "font-family"/"Arial,Helvetica"
    * If the name and value are both non-null and we are in a style rule,
    * then a PropertyNode will be created and added to the _propertyNodeList.
    * @param name
    * @param value
    * 
    */
  public void property(String name, String value)
  {

    if (_inStyleRule && (_propertyNodeList != null))
    {
      if (name == null || "".equals(name))
      {
        _LOG.severe("ERR_PARSING_SKIN_CSS_FILE", new Object[] {name, value});
      }
      else
        _propertyNodeList.add(new PropertyNode(name, value));
    }

  }

 /**
  * Call when you have an atRule. This will do further processing.
  * @param atRule The @rule string
  * e.g., @namespace af url(http:\\www.xxx.com);
  * e.g., @agent gecko { .foo {color:red}}
  */
  public void atRule(String atRule)
  {
    // parse the atRule further here.
   boolean importRule = atRule.startsWith(_AT_IMPORT);
   boolean charsetRule = atRule.startsWith(_AT_CHARSET);
   
    if (atRule != null)
    {
      if (importRule)
      {
        if (_ignoreImports)
        {
          // according to the css spec, @imports must come before all other rules (except @charset).
          if (_LOG.isWarning())
             _LOG.warning("AT_IMPORT_NOT_FIRST", atRule);
        }
        else
          _parseImport(atRule);
      }
      else if (atRule.startsWith(_AT_NAMESPACE))
      {
        _parseNamespace(_namespaceMap, atRule);
      }
      else if (atRule.startsWith(_AT_AGENT))
      {
        _parseCustomAtRule(_AT_AGENT, atRule);
      }
      else if (atRule.startsWith(_AT_PLATFORM))
      {
        _parseCustomAtRule(_AT_PLATFORM, atRule);
      }
      else if (atRule.startsWith(_AT_LOCALE))
      {
        _parseCustomAtRule(_AT_LOCALE, atRule);
      }
      else if (atRule.startsWith(_AT_ACC_PROFILE))
      {
        _parseCustomAtRule(_AT_ACC_PROFILE, atRule);
      }
      else if (atRule.startsWith(_AT_MODE))
      {
        _parseCustomAtRule(_AT_MODE, atRule);
      }
      // for now, ignore other atRules in a skinning css file
      
      // CSS spec says you ignore all @import rules after any other rules are processed
      // (except for @charset).
      if(!importRule && !charsetRule)
      {
        _ignoreImports = true;
      }
      
    }
  }

  private void _parseNamespace(Map<String, String> namespaceMap, String atRule)
  {
   // TODO deal with default namespaces that don't have prefixes??
   String[] namespaceParts = atRule.split("\\s+");
   if (namespaceParts.length > 2)
   {
     String url = _extractUrl(namespaceParts[2]);
     namespaceMap.put(namespaceParts[1], url);
   }
  }

  private String _extractUrl(String urlString)
  {

    urlString = urlString.trim();
     
    // first, strip off the url( and );
    if (urlString.startsWith("url("))
      urlString = urlString.substring(4);
    if (urlString.endsWith(");"))
      urlString = urlString.substring(0, urlString.length() - 2);
    else if (urlString.endsWith(";"))
      urlString = urlString.substring(0, urlString.length() - 1);
       
    // second, strip off the starting/ending quotes if there are any
    urlString = SkinStyleSheetParserUtils.trimQuotes(urlString);
    return urlString;
  }
  
  
  private void _parseImport(String type)
  {

    // parse any of these
    //@import "mystyle.css";
    //@import url("mystyle.css");
    //@import url(mystyle.css);

    // strip out @import any spaces, then get the url
    String styleSheetName = _extractUrl(type.substring(8));

    try
    {
      if (_imports == null)
        _imports = new ArrayList<List<SkinStyleSheetNode>>();
      _imports.add(_parseImportStyleSheetFile(_parseContext, styleSheetName, List.class));
    }
    catch (IOException e)
    {
      if (_LOG.isSevere())
        _LOG.severe("CANNOT_LOAD_STYLESHEET", styleSheetName);
        _LOG.severe(e);
    }
    catch (ParseException e)
    {
      _LOG.severe(e);
    }

  }
  
  private List<SkinStyleSheetNode> _parseImportStyleSheetFile(
    ParseContext  context,
    String        sourceName,
    Class<?>      expectedType)
    throws IOException, ParseException
  {
    // Step 1. Find the name resolver  
    NameResolver resolver = XMLUtils.getResolver(context);
    if (resolver == null)
    {
      if (_LOG.isWarning())
         _LOG.warning("Internal error: couldn't find NameResolver");
      
      return Collections.emptyList();
    }
    
    // Step 2. Find an InputStreamProvider. Mark a dependency on the base provider (if necessary)
    InputStreamProvider importProvider = resolver.getProvider(sourceName);
    InputStreamProvider baseProvider = XMLUtils.getInputStreamProvider(context);
    if (baseProvider instanceof CachingInputStreamProvider)
    {
      // important: hasSourceChanged takes into account this dependency
      ((CachingInputStreamProvider)baseProvider).addCacheDependency(importProvider);
    }
    
    // Step 3. Detect if this will be a circular include
    ArrayList<Object> list = (ArrayList<Object>) context.getProperty(_SHARE_NAMESPACE, 
                                                                     _INCLUDE_STACK);
    Object identifier = importProvider.getIdentifier();
    
    if ((list != null) && (list.contains(identifier)))
    {
      // Just logging an error isn't really enough - the include
      // will fail, but parsing continues and you'll get a stack overflow.  So, instead, we throw
      // an exception...
     throw new ParseException(_LOG.getMessage(
      "CIRCULAR_INCLUDE_DETECTED", sourceName), 0);
    }

    // Step 4. Try to get a cached version
    // =-=jmw  I don't know when the cached returns non-null other than when
    // the same import is included twice. This step (and Step 7) might not be worth it.
    // comment out caching code
    //Object cached = importProvider.getCachedResult();
    //if ((cached != null) && expectedType.isInstance(cached))
    //{
     //return (List<List<SkinStyleSheetNode>>)cached;
    //}
    
    // Step 5. Set up the new context; first, clone the original
    ParseContext newContext = (ParseContext)context.clone();
    
    // Add the current identifier to the stack (used for detecting circular includes) 
    // placed on the ParseContext
    // cloning ParseContext does a shallow copy. It doesn't copy this list.
    if (list == null)
      list = new ArrayList<Object>();
    else
      list = new ArrayList<Object>(list);
    list.add(identifier);
    newContext.setProperty(_SHARE_NAMESPACE, _INCLUDE_STACK, list);
    
    InputStream stream = importProvider.openInputStream();
    try
    {

      // Store a resolver relative to the file we're about to parse. This will be used for imports.
      // Store the inputStreamProvider on the context;
      // this will be used to get the document's timestamp later on
      XMLUtils.setResolver(newContext, resolver.getResolver(sourceName));
      XMLUtils.setInputStreamProvider(newContext, importProvider);

      // PARSE!
      // create a SkinStyleSheetNode
      // (contains a namespaceMap and a List of SkinSelectorPropertiesNodes
      // and additional information like direction, locale, etc.)
      // (selectorName + a css propertyList))
      BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
      SkinCSSParser parser = new SkinCSSParser();
      // Send over the ParseContext so that we can get the resolver from it in case we encounter 
      // an @import in the CSS file.
      SkinCSSDocumentHandler documentHandler = new SkinCSSDocumentHandler(newContext);
      parser.parseCSSDocument(reader, documentHandler);
      // If the imported css has an import of its own, this list will contain all the
      // imported nodes.
      List <SkinStyleSheetNode> importSkinSSNodeList = documentHandler.getSkinStyleSheetNodes();      

      reader.close();
      
      // Step 7. Store the cached result (if successful)
      //if (!_imports.isEmpty())
      //{
      //  importProvider.setCachedResult(_imports);
      //}
      return importSkinSSNodeList;

    }
    finally
    {
      stream.close();
    }
  }
  
  /** Get the atRule, and send its contents through the SkinCSSParser
   * again, using the current DocumentHandler object. The start/end
   * callbacks will be called again, but in the context of the atRule.
   */
  private void _parseCustomAtRule(String type, String atRule)
  {
    // get the @agent agents, @platform platforms or the @locale locales 
    // they are deliminated by commas parse out the content
    // save the atRule type, so the document handler code can get to it.
    // run this through parser again
    String content = _getAtRuleContent(atRule);
    _initAtRuleTargetTypes(type, atRule);
    
    // use this current DocumentHandler. This way we can add to the
    // CompleteSelectorNode list with agent information.
    SkinCSSParser parser = new SkinCSSParser();
    parser.parseCSSDocument(new StringReader(content), this);
    
    // reset
    _resetAtRuleTargetTypes(type);

  }
  
  private void _resetAtRuleTargetTypes(
    String type)
  {
    if (_AT_AGENT.equals(type))
      _agentAtRuleMatcher = null;
    else if (_AT_PLATFORM.equals(type))
      _selectorPlatforms = null;
    else if (_AT_LOCALE.equals(type))
      _locales = null;
    else if (_AT_ACC_PROFILE.equals(type))
    {
      assert(!_selectorAccPropertiesStack.isEmpty());
      
      if (!_selectorAccPropertiesStack.isEmpty())
        _selectorAccPropertiesStack.removeLast();
    }
    else if (_AT_MODE.equals(type))
    {
      _mode = ModeUtils.MODE_DEFAULT;
    }
  }
  
   // create a CompleteSelectorNode (this is the selector, properties, and
   // additional info, like 'rtl' direction
  private CompleteSelectorNode _createCompleteSelectorNode(
    String                     selector,
    List<PropertyNode>         propertyNodeList,
    Set<Locale>                locales,
    AgentAtRuleMatcher         agentMatcher,
    int[]                      selectorPlatforms,
    Set<String>                selectorAccProperties,
    int                        mode)
  {
    // parse the selector to see if there is a :rtl or :ltr ending.
    // if so, then set the reading direction.
    int direction = LocaleUtils.DIRECTION_DEFAULT;
    if (selector.endsWith(StyleUtils.RTL_CSS_SUFFIX))
    {
      int length = StyleUtils.RTL_CSS_SUFFIX.length();
      // strip off the SUFFIX
      selector = selector.substring(0, selector.length()-length);
      direction = LocaleUtils.DIRECTION_RIGHTTOLEFT;
    }
    else if (selector.endsWith(StyleUtils.LTR_CSS_SUFFIX))
    {
       int length = StyleUtils.LTR_CSS_SUFFIX.length();
       // strip off the SUFFIX
       selector = selector.substring(0, selector.length()-length);
       direction = LocaleUtils.DIRECTION_LEFTTORIGHT;
    }

    return
      new CompleteSelectorNode(
        selector,
        propertyNodeList,
        locales,
        direction,
        agentMatcher,
        selectorPlatforms,
        selectorAccProperties,
        mode);
  }

  /**
    * Given a List of CompleteSelectorNodes (selector nodes with
    * infor about selectors, properties, direction, agent), we create a List of
    * SkinStyleSheetNodes.
    * @param selectorList a list of CompleteSelectorNodes.
    * @param namespaceMap the namespace map
    * @return a List of SkinStyleSheetNodes
    */
  private List <SkinStyleSheetNode> _createSkinStyleSheetNodes(
    List<CompleteSelectorNode> selectorList,
    Map<String, String>        namespaceMap)
  {
    List<SkinStyleSheetNode> skinStyleSheetNodes =
      new ArrayList<SkinStyleSheetNode>();
     
    for (CompleteSelectorNode completeSelectorNode : selectorList)
    {
      // we add to the ssNodeList in this method.
      int direction = completeSelectorNode.getDirection();
      AgentAtRuleMatcher agentMatcher = completeSelectorNode.getAgentMatcher();
      int[] platforms = completeSelectorNode.getPlatforms();
      Set<Locale> locales = completeSelectorNode.getLocales();
      Set<String> accProperties = completeSelectorNode.getAccessibilityProperties();
      int mode = completeSelectorNode.getMode();

      // loop through the skinStyleSheetNodeList to find a match
      // of direction, agents, platforms, etc.
      boolean match = false;
         
      // iterate backwards, because the last node is most likely the
      // matching stylesheetnode.
      for (int i = skinStyleSheetNodes.size() - 1; i >= 0 && !match; --i)
      {
        SkinStyleSheetNode ssNode = skinStyleSheetNodes.get(i);
        match = ssNode.matches(direction, agentMatcher, platforms, locales, accProperties, mode);

        if (match)
          ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
      }

      if (!match)
      {
        // no matching stylesheet node found, so create a new one
        SkinStyleSheetNode ssNode =
         new SkinStyleSheetNode(namespaceMap, direction, locales, agentMatcher, platforms, accProperties, mode);
        ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
        skinStyleSheetNodes.add(ssNode);
      }
    }
    return skinStyleSheetNodes;
  }

  /**
   * Initialized at rule target types.
   * 
   * @param type type of the at rule. _AT_AGENT, _AT_PLATFORM, _AT_ACC_PROFILE or _AT_LOCALE
   * @param atRule - the atRule string
   */
  private void _initAtRuleTargetTypes(
    String type,
    String atRule)
  {
    // given the atRule string, get the target types --
    // @agent ie, gecko {...} => target types are the
    // AdfFacesAgent constants for ie and gecko.
    int firstSpace = atRule.indexOf(' ');
    int openBrace = atRule.indexOf('{');
    if (firstSpace != -1 && openBrace != -1)
    {
      String types = atRule.substring(firstSpace, openBrace);
      String[] typeArray = types.split(",");
      
      if (_AT_AGENT.equals(type))
      {
        _agentAtRuleMatcher = new AgentAtRuleMatcher(typeArray);
      }
      else if (_AT_PLATFORM.equals(type))
      {
        List<Integer> list = new ArrayList<Integer>();

        for (int i=0; i < typeArray.length; i++)
        {
          int platformInt = NameUtils.getPlatform(typeArray[i].trim());

          if (platformInt != TrinidadAgent.OS_UNKNOWN)
            list.add(platformInt);
        }
        
        _selectorPlatforms = _getIntArray(list);
      }
      else if (_AT_LOCALE.equals(type))
      {
        _locales = new HashSet<Locale>();
        for (int i = 0; i < typeArray.length; i++)
        {
          Locale locale = LocaleUtils.getLocaleForIANAString(typeArray[i].replace('_', '-').trim());
          _locales.add(locale);
        }
      }
      else if (_AT_ACC_PROFILE.equals(type))
      {
        // The number of profile properties is always going to be
        // very small, so we need to specify some initial capacity -
        // the default is too large.  Make the hash set twice as
        // large as the number of properties so that there is some
        // room to avoid collisions.  This probably isn't especially
        // effective, but probably doesn't matter much given the
        // small size of our sets.
        Set<String> set = new HashSet<String>(typeArray.length * 2);

        for (int i=0; i < typeArray.length; i++)
        {
          String accProp = typeArray[i].trim();

          if (NameUtils.isAccessibilityPropertyName(accProp))
          {
            set.add(accProp);
          }
          else
          {
            _LOG.warning("INVALID_ACC_PROFILE", new Object[]{accProp});
          }
        }
        
        if (!_selectorAccPropertiesStack.isEmpty())
          set = _mergeAccProperties(_selectorAccPropertiesStack.getLast(), set);

        _selectorAccPropertiesStack.add(set);
      }
      else if (_AT_MODE.equals(type))
      {
        _mode = NameUtils.getMode(typeArray[0].trim());
      }
    }
  }
 
  // Copies Integers from a List of Integers into an int array
  private int[] _getIntArray(List <Integer> integerList)
  {
    int count = integerList.size();
  
    if (count == 0)
      return null;
  
    int[] array = new int[count];
  
    for (int i = 0; i < count; i++)
      array[i] = integerList.get(i).intValue();
  
    return array;
  }
   
   /**
    * 
    * @param atRule - the entire @rule's definition, including content.
    * e.g., @agent ie, gecko { af|inputText::content {color:red}}
    * @return the content as a String
    * e.g., "af|inputText::content {color:red}"
    */
  private String _getAtRuleContent(String atRule)
  {
    int openBraceIndex = atRule.indexOf('{');
    int endBraceIndex = atRule.lastIndexOf('}');
    if (endBraceIndex == -1)
      endBraceIndex = atRule.length();
      
    if (openBraceIndex == -1)
      return null;
    else
     return atRule.substring(openBraceIndex+1, endBraceIndex);
   
  }

  // Returns the accessibility properties in effect for the current selector
  private Set<String> _getSelectorAccProperties()
  {
    return _selectorAccPropertiesStack.isEmpty() ?
             null :
             _selectorAccPropertiesStack.getLast();
  }

  // When specifying multiple values in an @accessibility-profile rule, eg:
  //
  // @accessibility-profile high-contrast, large-fonts {
  //   .selector { property: value }
  // }
  //
  // We treat the ',' separator as a logical "or" - ie. we match if either
  // high-contrast or large-fonts is specified by the AccessibilityProfile.
  //
  // In order to "and" accessibility profile properties together, we need
  // to support nested @accessibility-profile rules, eg:
  //
  // @accessibility-profile high-contract {
  //   @accessibility-profile large-fonts {
  //      .selector { property: value}
  //   }
  // }
  //
  // Where we only match the inner rule if both the outer rule *and* the
  // inner rule are matched.  We need some way to represent the fact that
  // we both rules must match.  We can't simply add the individual values
  // into the accessibility properties Set - since we will match either
  // value rather than require that both are present.  Instead we create
  // new compound values, eg. "high-contrast&large-fonts".  This allows the
  // accessibility matching logic in StyleSheetNode to detect cases where
  // multiple properties are required in order to accept the match.
  private Set<String> _mergeAccProperties(
    Set<String> oldProperties,
    Set<String> newProperties)
  {
    // If we don't have any old properties, no merging to do, just use
    // the new properties.
    if ((oldProperties == null) || oldProperties.isEmpty())
      return newProperties;

    // If we don't have any new properties, no merging to do, but we
    // want to inherit the old properties, so make a copy.
    if ((newProperties == null) || newProperties.isEmpty())
      return new HashSet<String>(oldProperties);
    
    // We have both old and new properties.  We need to merge
    // these into a single set.  At the most the merged set contains
    // oldProperties.size() * newProperties.size().  (We double this to
    // avoid collisions/re-allocations.)
    int mergedSize = oldProperties.size() + newProperties.size();
    Set<String> mergedProperties = new HashSet<String>(mergedSize * 2);
    for (String oldProperty : oldProperties)
    {
      for (String newProperty : newProperties)
        mergedProperties.add(oldProperty + "&" + newProperty);
    }

    return mergedProperties;
  }
  
   /**
    * This Class contains a SkinSelectorPropertiesNode and a rtl direction.
    * We will use this information when creating a SkinStyleSheetNode.
    */
  private static class CompleteSelectorNode
  {
    public CompleteSelectorNode(
      String                     selectorName,
      List<PropertyNode>         propertyNodes,
      Set<Locale>                locales,
      int                        direction,
      AgentAtRuleMatcher         agentMatcher,
      int[]                      platforms,
      Set<String>                accProperties,
      int                        mode
      )
    {
      _node = new SkinSelectorPropertiesNode(selectorName, propertyNodes);
      _direction = direction;
      
      // copy the agents and platforms because these get nulled out
      // at the end of the @rule parsing.
      _agentMatcher = agentMatcher;
      _platforms = _copyIntArray(platforms);
      _locales = ((locales != null) ? new HashSet<Locale>(locales)
            :Collections.<Locale>emptySet());
      
      if (accProperties != null)
      {
        // Copy acc properties just to be safe.  Note that we don't
        // bother wrapping in an unmodifiable set - just following
        // the pattern used for the agents/platforms arrays.
        _accProperties = new HashSet<String>(accProperties);
      }
      else
      {
        _accProperties = null;
      }
      _mode = mode;
    }
    
    public SkinSelectorPropertiesNode getSkinSelectorPropertiesNode()
    {
      return _node;
    }
    
    public int getDirection()
    {
      return _direction;
    }

    /**
     * @return The AgentMatcher if any for this rule
     */
    public AgentAtRuleMatcher getAgentMatcher()
    {
      return _agentMatcher;
    }
    
    public int[] getPlatforms()
    {
      return _platforms;
    }

    public Set<Locale> getLocales()
    {
      return _locales;
    }

    public Set<String> getAccessibilityProperties()
    {
      return _accProperties;
    }

    public int getMode()
    {
      return _mode;
    }

    // Returns a copy of the int array
    private static int[] _copyIntArray(int[] array)
    {
      if (array == null)
        return null;
    
      int[] copy = new int[array.length];
      System.arraycopy(array, 0, copy, 0, array.length);
    
      return copy;
    }

    private final SkinSelectorPropertiesNode _node;
    private final int _direction;  // the reading direction
    private final AgentAtRuleMatcher _agentMatcher;
    private final int[] _platforms;
    private final Set<Locale> _locales; 
    private final Set<String> _accProperties;
    private int _mode;
  }

  private static final String _AT_AGENT = "@agent";
  private static final String _AT_PLATFORM = "@platform";
  private static final String _AT_LOCALE = "@locale";
  private static final String _AT_ACC_PROFILE = "@accessibility-profile";
  private static final String _AT_MODE = "@mode";
  private static final String _AT_IMPORT = "@import";
  private static final String _AT_NAMESPACE = "@namespace";
  private static final String _AT_CHARSET = "@charset";


  // below are properties that we set and reset
  // as the methods of this documentHandler get called.
  private boolean _inStyleRule = false;
  private List<PropertyNode> _propertyNodeList = null;
  // we build this list as we parse the skinning css file. We use this
  // list to create a list of SkinStyleSheetNodes
  private List <CompleteSelectorNode> _completeSelectorNodeList =
    new ArrayList<CompleteSelectorNode>();
  // these are the selector platform, agents and accessiblity properties of the
  // selectors we are currently parsing in this document.
  private int[] _selectorPlatforms = null;

  // matches the current Agent against the allowed agents
  private AgentAtRuleMatcher _agentAtRuleMatcher = null;

  // the locales of the selectors parsed in this document.
  private Set<Locale> _locales = null;

  //the mode for which the parsed selectors are valid.
  private int _mode = ModeUtils.MODE_DEFAULT;

  // Stack of accessibility property sets.  While java.util.Stack has the
  // push/pop API that we want, we don't need the synchronization, so we
  // just use a LinkedList instead and pretend its a stack.
  private LinkedList<Set<String>> _selectorAccPropertiesStack =
    new LinkedList<Set<String>>();

  private Map<String, String> _namespaceMap = new HashMap<String, String>();
  private ParseContext _parseContext;
  private List<List <SkinStyleSheetNode>> _imports;
  private boolean _ignoreImports = false;

  // Perhaps move to ShareConstants
  static private final String _SHARE_NAMESPACE  =
    "org.apache.myfaces.trinidadinternal.skin.SkinCSSDocumentHandler";
  static private final String _INCLUDE_STACK = "_includeStack";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SkinCSSDocumentHandler.class);
}
   
   
