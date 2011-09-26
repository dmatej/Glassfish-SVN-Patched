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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.net.URL;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidad.util.URLUtils;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludeCompactPropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludePropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludeStyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * Utility class for creating a StyleSheetDocument.
 * The main method is parseCSSSource which creates a StyleSheetEntry.
 * The interim object is SkinStyleSheetNode
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleSheetParserUtils.java#0 $) $Date: 10-nov-2005.18:59:00 $
 */
class SkinStyleSheetParserUtils
{
  /**
   * Parses a Skin style-sheet that is in the CSS-3 format.
   * @param context      the current ParseContext. Simply a place to set/get properties
   * @param resolver     a NameResolver to locate the target
   *                     ( Given a name, returns an InputStreamProvider.)
   * @param sourceName   the name of the target, relative to the current file
   * @param expectedType the expected Java type of the target.
   */
  static public StyleSheetEntry parseCSSSource(
    ParseContext  context,
    NameResolver  resolver,
    String        sourceName,
    Class<?>      expectedType) throws IOException
  {

    if (expectedType == null)
      throw new NullPointerException();
    if (resolver == null)
      throw new NullPointerException();
    if (sourceName == null)
      throw new NullPointerException();
    if (context == null)
      throw new NullPointerException();

    InputStreamProvider provider = resolver.getProvider(sourceName);
    Object cached = provider.getCachedResult();
    if ((cached != null) && expectedType.isInstance(cached))
      return (StyleSheetEntry)cached;

    InputStream stream = provider.openInputStream();

    try
    {
      // Store a resolver relative to the file we're about to parse. This will be used for imports.
      // Store the inputStreamProvider on the context;
      // this will be used to get the document's timestamp later on
      XMLUtils.setResolver(context, resolver.getResolver(sourceName));
      XMLUtils.setInputStreamProvider(context, provider);

      // PARSE!
      // create a SkinStyleSheetNode
      // (contains a namespaceMap and a List of SkinSelectorPropertiesNodes
      // and additional information like direction, locale, etc.)
      // (selectorName + a css propertyList))
      BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
      SkinCSSParser parser = new SkinCSSParser();
      // Send over the ParseContext so that we can get the resolver from it in case we encounter 
      // an @import in the CSS file.
      SkinCSSDocumentHandler documentHandler = new SkinCSSDocumentHandler(context);
      parser.parseCSSDocument(reader, documentHandler);
      List <SkinStyleSheetNode> skinSSNodeList = documentHandler.getSkinStyleSheetNodes();
      reader.close();

      // process the SkinStyleSheetNodes to create a StyleSheetEntry object
      StyleSheetEntry styleSheetEntry =
        _createStyleSheetEntry(context, sourceName, skinSSNodeList);

      // Store the cached result (if successful)
      // otherwise, if we don't do this, we will keep reparsing. Somehow
      // this affects whether the file has been modified. STRANGE!
      //     if (value != null)
      //    provider.setCachedResult(value);

      //    return value;
      provider.setCachedResult(styleSheetEntry);

      return styleSheetEntry;
    }
    finally
    {
      stream.close();
    }
  }

  /**
   * Trim the leading/ending quotes, if any.
   * We trim only matching quotes, as opposed to trimQuotes which trims any quotes.
   */
  public static String trimQuotes(String in)
  {
    if ( in == null )
      return in;

    boolean startsWithDoubleQuote = in.startsWith( "\"" );
    boolean startsWithSingleQuote = in.startsWith( "\'" );
    boolean endsWithDoubleQuote = in.endsWith( "\"" );
    boolean endsWithSingleQuote = in.endsWith( "\'" );
    
    if (( startsWithDoubleQuote && endsWithSingleQuote ) ||
       ( startsWithSingleQuote && endsWithDoubleQuote ))
    {
      if (_LOG.isWarning())
        _LOG.warning("ERR_PARSING", in);
    }
                                                          
    if ( startsWithDoubleQuote && endsWithDoubleQuote )
      return in.substring( 1, in.length() - 1 );
    if ( startsWithSingleQuote && endsWithSingleQuote )
      return in.substring( 1, in.length() - 1 );
    
    return in;
  }  


  /**
   * Given a List of SkinStyleSheetNode, create StyleSheetEntry.
   * A StyleSheetEntry is an object that contains:
   * styleSheetName, StyleSheetDocument
   * A StyleSheetDocument contains StyleSheetNodes. A StyleSheetNode contains
   * StyleNodes, IconNodes, and SkinPropertyNodes and additional info like
   * the direction, locale, etc. for this list of selectors.
   * @param context
   * @param sourceName
   * @param skinSSNodeList
   * @return
   */
  private static StyleSheetEntry _createStyleSheetEntry(
    ParseContext  context,
    String        sourceName,
    List <SkinStyleSheetNode> skinSSNodeList
    )
  {
    // Get each SkinStyleSheetNode, and for each SkinStyleSheetNode get a
    // styleNodeList. Also, build one iconNodeList and one skinPropertyNodeList.

    // initialize
    List<StyleSheetNode> ssNodeList = new ArrayList<StyleSheetNode>();
    String baseSourceURI = CSSUtils.getBaseSkinStyleSheetURI(sourceName);

    // loop through the selectors and its properties
    for (SkinStyleSheetNode skinSSNode : skinSSNodeList)
    {

      // selector and its properties
      List <SkinSelectorPropertiesNode> selectorNodeList =
        skinSSNode.getSelectorNodeList();
      //Map namespaceMap = styleSheetNode.getNamespaceMap();

      // initialize
      List <StyleNode> styleNodeList = new ArrayList<StyleNode>();
      List<IconNode> iconNodeList = new ArrayList<IconNode>();


      // process each selector and all its name+values
      for (SkinSelectorPropertiesNode cssSelector : selectorNodeList)
      {

        String selectorName = cssSelector.getSelectorName();
        // PropertyNode is the name+value, like font-size: 8px
        List<PropertyNode> propertyList = cssSelector.getPropertyNodes();
        int direction     = skinSSNode.getDirection();

        ResolvedSkinProperties resolvedProperties =
          _resolveProperties(propertyList);

        // trSkinPropertyNodeList, e.g., af|foo {-tr-show-last-item: true}
        // PropertyNode(-tr-show-last-item, true)
        List<PropertyNode> trSkinPropertyNodeList = new ArrayList<PropertyNode>();
        trSkinPropertyNodeList = resolvedProperties.getSkinPropertyNodeList();

        List<PropertyNode> noTrPropertyList =
          resolvedProperties.getNoTrPropertyList();

        if (StyleUtils.isIcon(selectorName))
        {
          // knock off the '.' if it is the first character.
          if (selectorName.charAt(0) == '.')
            selectorName = selectorName.substring(1);
          // strip out :alias
          selectorName = selectorName.replaceFirst(":alias", "");
          // add :rtl if the direction is rtl
          if (direction == LocaleUtils.DIRECTION_RIGHTTOLEFT)
            selectorName = selectorName.concat(StyleUtils.RTL_CSS_SUFFIX);

          // create an IconNode object and add it to the iconNodeList
          // This method returns hasContentProperty=false if there isn't a content attribute.
          boolean hasContentProperty = _addIconNode(sourceName,
                                              baseSourceURI,
                                              selectorName,
                                              noTrPropertyList,
                                              resolvedProperties.getTrRuleRefList(),
                                              resolvedProperties.getInhibitedProperties(),    
                                              iconNodeList);

          if (!hasContentProperty)
          {
            // if it doesn't have any includes AND it doesn't have properties, it shouldn't pass
            // the _isIcon test, so log a warning. This means the developer used the wrong 
            // selector name. It should end in 'icon-style' instead of 'icon', for example.
            
            if (resolvedProperties.getTrRuleRefList() == null || 
                resolvedProperties.getTrRuleRefList().isEmpty())
            {
              if (_LOG.isWarning())
                _LOG.warning("SELECTOR_SHOULD_NOT_END_IN_ICON", selectorName);
            }
            _addStyleNode(selectorName,
                          noTrPropertyList,
                          trSkinPropertyNodeList,
                          resolvedProperties.getTrRuleRefList(),
                          resolvedProperties.getIncludedPropertiesList(),
                          resolvedProperties.getIncludedCompactPropertiesList(),
                          resolvedProperties.getInhibitedProperties(),
                          resolvedProperties.isTrTextAntialias(),
                          styleNodeList);
          }
        }
        else
        {

          _addStyleNode(selectorName,
                        noTrPropertyList,
                        trSkinPropertyNodeList,
                        resolvedProperties.getTrRuleRefList(),
                        resolvedProperties.getIncludedPropertiesList(),
                        resolvedProperties.getIncludedCompactPropertiesList(),
                        resolvedProperties.getInhibitedProperties(),
                        resolvedProperties.isTrTextAntialias(),
                        styleNodeList);

        }
      }

      if ((styleNodeList.size() > 0) || (iconNodeList.size() > 0))
      {
        // we need to deal with the styleNodeList by building a StyleSheetNode
        // with this information.
        // create a StyleSheetNode, add to the ssNodeList
        StyleNode[] styleNodeArray = styleNodeList.toArray(new StyleNode[0]);
        StyleSheetNode ssNode =
          new StyleSheetNode(styleNodeArray,
                             iconNodeList,
                             skinSSNode.getLocales(),
                             skinSSNode.getDirection(),
                             skinSSNode.getAgentMatcher(),
                             skinSSNode.getPlatforms(),
                             skinSSNode.getMode(),
                             skinSSNode.getAcessibilityProperties());
        ssNodeList.add(ssNode);
      }

    } // end for each SkinStyleSheetNode


    // StyleSheetDocument contains StyleSheetNode[] styleSheets
    StyleSheetDocument ssDocument =
      _createStyleSheetDocument(context, ssNodeList);

    return new StyleSheetEntry(sourceName,
                               ssDocument);


  }

  /**
   * Loop thru every property in the propertyList and store them in
   * the ResolvedSkinProperties inner class.
   * @param propertyNodeList
   * @return
   */
  private static ResolvedSkinProperties _resolveProperties(
    List<PropertyNode> propertyNodeList)
  {

    List<PropertyNode> noTrPropertyList = new ArrayList<PropertyNode>();
    List<String> trRuleRefList = new ArrayList<String>();
    Set<String> inhibitedPropertySet = new TreeSet<String>();
    List<IncludePropertyNode> includedPropertiesList =
      new ArrayList<IncludePropertyNode>();
    List<IncludeCompactPropertyNode> includedCompactPropertiesList =
      new ArrayList<IncludeCompactPropertyNode>();
    List<PropertyNode> skinPropertyNodeList =
      new ArrayList<PropertyNode>();

    boolean trTextAntialias = false;

    // loop through each property in the propertyList
    // and resolve into
    // noTrPropertyList (properties that do not start with -tr-.
    //                  (or -ora- for backwards compatibility))
    // trRuleRefList (properties that start with -tr-rule-ref
    //                (or -ora-rule-ref for backwards compatibility))
    // boolean trTextAntialias (property value for -tr-text-antialias
    //                      (or -ora-text-antialias for backwards compatibility)
    // skinPropertyNodeList (all other properties that start with -tr-
    //                       (or -ora- for backwards compatibility))
    // includedPropertiesList (all the included property 'values')
    // These properties are stored in the ResolvedSkinProperties inner class.

    for(PropertyNode propertyNode : propertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();

      if(propertyName != null && propertyValue != null)
      {
        // Check for special propertyNames (-tr-rule-ref, -tr- skin properties)
        // or propertyValue (-tr-property-ref)
        
        // Check for -tr-property-ref first, either regular or compact
        if(propertyValue.indexOf(_INCLUDE_PROPERTY) != -1)
        {
          List<String> values = separateCompactValues(propertyValue);
          // border-color: -tr-property-ref(...) versus border: 1px solid -tr-property-ref();
          if (values.size() == 1)
          {
            // include property
            IncludePropertyNode node = _createIncludePropertyNode(
              propertyName, propertyValue.substring(_INCLUDE_PROPERTY.length()));
            includedPropertiesList.add(node);
          }
          else
          {
            // include compact property
            List<IncludePropertyNode> compactIncludePropNodes = new ArrayList<IncludePropertyNode>();
            StringBuilder builder = new StringBuilder();
            for (String value : values)
            {
              if (value.startsWith(_INCLUDE_PROPERTY))
              {
                IncludePropertyNode node = _createIncludePropertyNode(
                  propertyName, value.substring(_INCLUDE_PROPERTY.length()));
                compactIncludePropNodes.add(node);
              }
              else
              {
                builder.append(value);
                builder.append(" ");
              }
            }
            IncludeCompactPropertyNode iCPNode = 
              new IncludeCompactPropertyNode(builder.toString(), 
                                             compactIncludePropNodes, propertyName);
            includedCompactPropertiesList.add(iCPNode);
                        
          }
        }
        else
        {
          boolean oraProperty = propertyName.startsWith(_ORA_PROPERTY_PREFIX);
          boolean trProperty = propertyName.startsWith(_TR_PROPERTY_PREFIX);
          if( oraProperty || trProperty)
          {
            int suffixIndex = (oraProperty)?_ORA_PROPERTY_PREFIX.length(): _TR_PROPERTY_PREFIX.length();
            String propertyNameSuffix = propertyName.substring(suffixIndex);
            if (propertyNameSuffix.equals(_PROPERTY_RULE_REF))
            {
              // add the rule ref value to the list
              trRuleRefList.add(propertyValue);
            }
            else if (propertyNameSuffix.equals(_PROPERTY_TEXT_ANTIALIAS))
            {
              if ("true".equals(propertyValue))
                trTextAntialias = true;

            }
            else if (propertyNameSuffix.equals(_PROPERTY_INHIBIT))
            {
              for (String value : _SPACE_PATTERN.split(propertyValue))
              {
                inhibitedPropertySet.add(value);
              }
            }
            else
            {
              // create the PropertyNode for skin properties (e.g., -tr-show-last-item: true)
              PropertyNode node = new PropertyNode(propertyName, propertyValue);
              skinPropertyNodeList.add(node);
            }
          }
          else
          {
            noTrPropertyList.add(propertyNode);
          }
        }
      }
    }

    return new ResolvedSkinProperties(
      noTrPropertyList,
      trRuleRefList,
      includedPropertiesList,
      includedCompactPropertiesList,      
      inhibitedPropertySet,
      skinPropertyNodeList,
      trTextAntialias);
  }
  
  private static List<String> separateCompactValues(String propertyValue)
  {
     String[] test = _SPACE_PATTERN.split(propertyValue);
     List<String> propertyValueNoSpaces = new ArrayList<String>();
     boolean inTr = false;
     int inTrIndex = 0;
     for (int i=0; i < test.length; i++)
     {
        String string = test[i];
        if (string.startsWith(_INCLUDE_PROPERTY) && !string.endsWith(")"))
        {
           // keep looping through the pieces 
           // until we get to a string that endsWith ")".
           inTr = true;
           inTrIndex = i;            
        }
        else if (inTr)
        {
           if (string.endsWith(")"))
           {
              StringBuilder builder = new StringBuilder();
              for (int j=inTrIndex; j <= i; j++)
              {
                 builder.append(test[j]);
              }
              inTr = false;
              propertyValueNoSpaces.add(builder.toString());
           }
        }
        else
        {
           propertyValueNoSpaces.add(string);
        } 
     }
     return propertyValueNoSpaces;
  }

  /**
   * Create an IconNode and add it to the iconNodeList.
   * @param sourceName
   * @param baseSourceURI
   * @param selectorName
   * @param trRuleRefList -> This is -tr-rule-ref: selector(). 
   * Currently not supported for icons. See https://issues.apache.org/jira/browse/TRINIDAD-17
   * @param noTrPropertyNodeList -> these are properties, like width: 100px.
   * @param iconNodeList Once the IconNode is created, it is added to the iconNodeList to be
   * used outside this method.
   * @return boolean Returns true if an IconNode was created and added to iconNodeList. 
   * If false, then it means that the properties did not contain 'content', 
   * so it only had css styles. It could have a -tr-rule-ref that includes a 'content'.
   * TODO what to do about that???
   */
  private static boolean _addIconNode(
    String             sourceName,
    String             baseSourceURI,
    String             selectorName,
    List<PropertyNode> noTrPropertyNodeList,
    List<String>       trRuleRefList,
    Set<String>        inhibitedProperties,
    List<IconNode>     iconNodeList)
  {

    // these are icon properties.
    // create an IconNode.
    // get content property value. This is how i decide if it is an url or a text icon.
    //
    // loop through all the properties
    // TextIcons take text, rtlText, inlineStyle, styleClass
    // url icons take uri, rtluri, width, height, styleClass, inlineStyle
    // Icon selectors that end with :rtl will be a separate Icon object.
    // I won't combine :rtl icons with regular icons into the same object
    // like we did in 2.2.
    // af|breadCrumbs::separatorIcon {content: ">"}
    // af|breadCrumbs::separatorIcon:rtl {content: "<"}
    // this will create
    // key=af|breadCrumbs::separatorIcon with TextIcon(">", ">", style, inlineStyle)
    // and
    // key=af|breadCrumbs::separatorIcon:rtl with TextIcon("<", "<", rtlstyle, rtlinlineStyle)
    // then when I go to get the icon af|breadCrumbs::separatorIcon, the skin
    // will know to ask for af|breadCrumbs::separatorIcon:rtl or af|breadCrumbs::separatorIcon
    // depending upon the DIRECTION that is set on the context.
    // The current Icon classes code will not have to change.

    boolean hasContentProperty = false;
    
    // Create the propertyNode array now.
    PropertyNode[] propertyNodeArray = new PropertyNode[noTrPropertyNodeList.size()];
    int i = 0;

    // Loop through each property until we find the 'content' property, then change the url to
    // something we can use in StyleSheetDocument.
    for(PropertyNode propertyNode : noTrPropertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();
      
      // fix up the url
      if (propertyName.equals("content") && propertyValue != null)
      {
        hasContentProperty = true;
        // is it a text or uri
        if (_isURLValue(propertyValue))
        {
          
          // get the string that is inside of the 'url()'
          String uriString = _getURIString(propertyValue);
          boolean isAbsoluteURI = CSSUtils.isAbsoluteURI(uriString);
          
          // a leading / indicates context-relative
          //      (auto-prefix the servlet context)
          // a leading // indicates server-relative
          //      (don't auto-prefix the servlet context).
          if (!isAbsoluteURI)
          {
            boolean startsWithTwoSlashes = uriString.startsWith("//");
  
            if (!startsWithTwoSlashes && uriString.startsWith("/"))
            {
              uriString = uriString.substring(1);
            }
            else
            {
              // a. if it has two slashes, strip off one.
              // b. if it is an absolute url, don't do anything
              // c. if it a relative url, then it should be relative to
              // the skin file since they wrote the relative url in the skin file.
              
              if (startsWithTwoSlashes)
                uriString = uriString.substring(1);
              else
                uriString = CSSUtils.getAbsoluteURIValue(sourceName, baseSourceURI, uriString);
  
            }
          }
          // At this point, URIImageIcons start with '/' or 'http:',
          // whereas ContextImageIcons uri do not. This is how we will know which type of 
          // Icon to create in StyleSheetDocument. Wrap back up with the 'url()' string so that
          // we will know this is not a TextIcon.
          propertyNodeArray[i] = new PropertyNode(propertyName, _wrapWithURLString(uriString));
        }      
        else if (propertyValue.startsWith("inhibit"))
        {
          propertyNodeArray[i] = new PropertyNode(propertyName, propertyValue);
        }
        else
        {
          String text = trimQuotes(propertyValue);
          propertyNodeArray[i] = new PropertyNode(propertyName, text);
        }
        

      } // end if 'content'
      else
        propertyNodeArray[i] = new PropertyNode(propertyName, propertyValue);
      i++;
    }
 
    // TODO - Add -tr-rule-ref capability for icons.
    // I purposely do not include the -tr-rule-ref at this time.
    // See TRINIDAD-17 for details why. There is a hitch. Even though we treat selectors 
    // with -icon as Icon objects, if there is no content attribute, we also treat them as styles.
    // Well, if they use -tr-rule-ref to import 'content', then we will think it is a style, 
    // and we'll have extra styles. Yet we don't want to resolve selectors just to see if it is
    // really an icon. But we don't want to hurt the person that didn't abide by the -icon rule
    // because this wasn't an enforced rule.
    //
    // if the trRuleRefList is not empty, create IncludeStyleNodes.
    List<IncludeStyleNode> includeStyleNodes = new ArrayList<IncludeStyleNode>();

    for(String value : trRuleRefList)
    {
      // parse the value, which will be of this form:
      // -tr-rule-ref: selector(".AFBaseFont:alias") selector(".Foo")
      // where you have more than one selector in an -tr-rule-ref definition
      // or -tr-rule-ref: selector(".AFBaseFont:alias")
      // where you have only one selector in an -tr-rule-ref definition.
      // I want each selector value to be an IncludeStyleNode.

      _addIncludeStyleNodes(value, includeStyleNodes);

    }

    if (selectorName != null)
    {
      // Create a styleNode that we will add to the IconNode.
      StyleNode styleNode =
        new StyleNode(null, // name
                      selectorName,
                      propertyNodeArray,
                      null, //TODO jmw trSkinPropertyNodes for icons
                      includeStyleNodes.toArray(new IncludeStyleNode[0]),
                      null, //TODO jmw includePropertyNodes for icons
                      null, //TODO jmw includeCompactPropertyNodes for icons
                      inhibitedProperties
                      );
      
      // Create a new IconNode.
      // don't bother creating the Icon object now (the second property to new IconNode()); 
      // we create the Icon object when we resolve IconNodes in StyleSheetDocument
      // See StyleSheetDocument#getIcons(StyleContext context).      
      iconNodeList.add(new IconNode(selectorName, null, styleNode));
    }

    return hasContentProperty;

  }

  /**
   * Creates a StyleNode object and adds it to the styleNodeList.
   * The StyleNode object gets completely resolved (the trRuleRefList, includeProperyNodes, etc.,
   * get resolved in StyleSheetDocument#_resolveStyleNode). The StyleNode here is the unresolved
   * StyleNode - basically what we have in the skin css file.
   * @param selectorName
   * @param propertyNodeList
   * @param skinPropertyNodeList
   * @param trRuleRefList
   * @param includePropertyNodes
   * @param includeCompactPropertyNodes
   * @param inhibitedProperties
   * @param trTextAntialias
   * @param styleNodeList Once the StyleNode is created, it is added to the iconNodeList to be
   * used outside this method.
   *
   */
  private static void _addStyleNode(
    String                    selectorName,
    List<PropertyNode>        propertyNodeList,
    List<PropertyNode>        skinPropertyNodeList,
    List<String>              trRuleRefList,
    List<IncludePropertyNode> includePropertyNodes,
    List<IncludeCompactPropertyNode> includeCompactPropertyNodes,
    Set<String>               inhibitedProperties,
    boolean                   trTextAntialias,
    List<StyleNode>           styleNodeList)
  {

    StyleNode styleNode = _createStyleNode(selectorName, propertyNodeList, skinPropertyNodeList,
                                           trRuleRefList, 
                                           includePropertyNodes,
                                           includeCompactPropertyNodes,
                                           inhibitedProperties, 
                                           trTextAntialias);

    styleNodeList.add(styleNode);

  }

  private static StyleNode _createStyleNode(
    String                    selectorName,
    List<PropertyNode>        propertyNodeList,
    List<PropertyNode>        skinPropertyNodeList,
    List<String>              trRuleRefList,
    List<IncludePropertyNode> includePropertyNodes,
    List<IncludeCompactPropertyNode> includeCompactPropertyNodes,
    Set<String>               inhibitedProperties,
    boolean                   trTextAntialias)
  {
    // these are the styles.
    // At this point I have a selector name and the properties.
    // create a StyleNode based on this information.

    String name = null;
    String selector = null;
    int aliasIndex = selectorName.indexOf(":alias");
    if (aliasIndex > -1)
    {
      // :alias means do not output style; it is a namedStyle, so we set
      // the name and not the selector.
      // first, strip off the '.' at the beginning and the :alias bit.
      name = selectorName.substring(1, aliasIndex);
    }
    else
      selector = selectorName;

    // add text-antialias if it is set
    if (trTextAntialias)
    {
      propertyNodeList.add(new PropertyNode("text-antialias", "true"));
    }
    // convert to a PropertyNode[], because StyleNode takes this type.
    PropertyNode[] propertyArray =
      propertyNodeList.toArray(new PropertyNode[propertyNodeList.size()]);

    // if the trRuleRefList is not empty, create IncludeStyleNodes.
    int length = trRuleRefList.size();
    List<IncludeStyleNode> includeStyleNodes = new ArrayList<IncludeStyleNode>();

    if (length > 0)
    {
      for(String value : trRuleRefList)
      {
        // parse the value, which will be of this form:
        // -tr-rule-ref: selector(".AFBaseFont:alias") selector(".Foo")
        // where you have more than one selector in an -tr-rule-ref definition
        // or -tr-rule-ref: selector(".AFBaseFont:alias")
        // where you have only one selector in an -tr-rule-ref definition.
        // I want each selector value to be an IncludeStyleNode.

        _addIncludeStyleNodes(value, includeStyleNodes);

      }
    }

    // create a StyleNode
    StyleNode styleNode =
      new StyleNode(name,
                    selector,
                    propertyArray,
                    skinPropertyNodeList.isEmpty() ?
                      null : skinPropertyNodeList.toArray(new PropertyNode[0]),
                    includeStyleNodes.toArray(new IncludeStyleNode[0]),
                    includePropertyNodes.isEmpty() ? 
                      null : includePropertyNodes.toArray(new IncludePropertyNode[0]),
                    includeCompactPropertyNodes.isEmpty() ? 
                      null : includeCompactPropertyNodes.toArray(new IncludeCompactPropertyNode[0]),
                    inhibitedProperties,
                    false);
    
    return styleNode;
  }

  /**
   * Create an IncludePropertyNode.
   * The syntax for including a property from another selector is
   * af|commandButton { background-color: -tr-property-ref(".AFTestBackgroundColor:alias"); }
   * .AFTestForegroundColor:alias {color: yellow; font-style:italic}
   * .AFTestBackgroundColor:alias {background-color: -tr-property-ref(".AFTestForegroundColor:alias","color")}
   * @param propertyName name of the property like background-color, color
   * @param propertyValue value that want to include. This is stripped of the
   * -tr-property-ref, so it is of the form ("...")
   */
  private static IncludePropertyNode _createIncludePropertyNode(
    String propertyName, 
    String propertyValue)
  {
    // do a quick sanity check
    if (propertyValue == null || propertyValue.length() < 2)
      return null;
    if (propertyValue.startsWith("(") && propertyValue.endsWith(")"))
    {
      String valueString = propertyValue.substring(1, propertyValue.length() - 1);
      String values[] = valueString.split(",");
      String selectorValue = trimQuotes(values[0]);
      String includedProperty;
      if (values.length == 1)
      {
        includedProperty = propertyName;
      }
      else
      {
        includedProperty = trimQuotes(values[1].trim());
      }

      String selector = null;
      String name = null;
      if (selectorValue.endsWith(":alias"))
      {
        int aliasEndIndex = selectorValue.indexOf(":alias");
        int aliasStartIndex = 0;
        if (selectorValue.startsWith("."))
          aliasStartIndex = 1;
        name = selectorValue.substring(aliasStartIndex, aliasEndIndex);
      }
      else
      {
        selector = selectorValue;
      }
      return new IncludePropertyNode(name, selector, includedProperty, propertyName);
    }
    return null;
  }

  // This is for -tr-rule-ref properties on styles.
  private static void _addIncludeStyleNodes(
    String value,
    List <IncludeStyleNode> includeStyleNodes )
  {

    if (value != null)
    {
      // parse string and create IncludeStyleNode for each selector value.
      // the string will be of this form:
      // selector(".AFBaseFont:alias") selector(".MyDarkBackground")
      // or a single selector:
      // selector(".AFBaseFont:alias")
      // if it ends with :alias, it is a namedstyle.

      List<String> selectors = new ArrayList<String>();

      _parseValueIntoSelectors(value, selectors);

      // now take the selector List and convert it to IncludeStyleNodes.
      for (String includeStyle : selectors)
      {
        // if it has :alias at the end it is a named style
        if (includeStyle.endsWith(":alias"))
        {
          // strip off :alias first and the . at the beginning
          
          int endIndex = includeStyle.indexOf(":alias");
          int startIndex = 0;
          if (includeStyle.charAt(0) == '.')
            startIndex = 1;
          includeStyleNodes.add(new IncludeStyleNode(
                                includeStyle.substring(startIndex, endIndex),
                                null));
        }
        else
          includeStyleNodes.add(new IncludeStyleNode(null, includeStyle));
      }

    }
  }

  /**
   * Parse the value into a List<String> of selector names.
   * @param value - String of the form: selector(".AFBaseFont:alias") selector(".MyDarkBackground")
   * or a single selector: selector(".AFBaseFont:alias")
   * @param selectors a List<String> of selector names. This will be filled in during this method
   * call.
   */
  private static void _parseValueIntoSelectors(
    String value, 
    List<String> selectors)
  {
    String[] test = _SELECTOR_PATTERN.split(value);
    for (int i=0; i < test.length; i++)
    {
      int endIndex = test[i].indexOf(")");
      if (endIndex > -1)
      {
        String selectorValue = test[i].substring(0, endIndex);
        selectorValue = trimQuotes(selectorValue);
        selectors.add(selectorValue);
      }
    }
  }

  private static StyleSheetDocument _createStyleSheetDocument(
    ParseContext context,
    List<StyleSheetNode> ssNodeList)
  {

    long timestamp = _getDocumentTimestamp(context);

    return new StyleSheetDocument(ssNodeList.toArray(new StyleSheetNode[0]),
                                    null,
                                    timestamp);
  }

  // Returns the document timestamp for the style sheet that
  // is currently being parsed, taking into account timestamps
  // of any imported style sheets. (copied from StyleSheetDocumentParser)
  private static long _getDocumentTimestamp(ParseContext parseContext)
  {

    long timestamp = StyleSheetDocument.UNKNOWN_TIMESTAMP;

    // The only way to get the timestamp is through the
    // InputStreamProvider.
    InputStreamProvider provider = XMLUtils.getInputStreamProvider(parseContext);

    if (provider != null)
    {
      // And this only works if we are using a File-based or URL-based InputStream
      Object identifier = provider.getIdentifier();
      if (identifier instanceof File)
        timestamp = ((File)identifier).lastModified();
      else if (identifier instanceof URL)
      {
        try
        {
          timestamp = URLUtils.getLastModified((URL) identifier);
        }
        catch (IOException io)
        {
          _LOG.warning("CANNOT_GET_STYLESHEET_DOCUMENT_TIMESTAMP");
        }

      }
    }

    return timestamp;
  }




  // Tests whether the specified property value is an "url" property.
  private static boolean _isURLValue(String propertyValue)
  {
    // URL property values start with "url("
    return propertyValue.startsWith("url(");
  }

  // Returns the uri portion of the url property value
  private static String _getURIString(String propertyValue)
  {
    assert(_isURLValue(propertyValue));

    int uriEnd = propertyValue.indexOf(')');
    String uri = propertyValue.substring(4, uriEnd);

    return trimQuotes(uri);
  }
  
  // wraps the uri in 'url( )' and returns the new String.
  private static String _wrapWithURLString(String uri)
  {
    StringBuilder builder = new StringBuilder(5 + uri.length());
    builder.append("url(");
    builder.append(uri);
    builder.append(")");
    return builder.toString();
    
  }
  
  private static class ResolvedSkinProperties
  {


    ResolvedSkinProperties(
      List<PropertyNode>               noTrPropertyList,
      List<String>                     trRuleRefList,
      List<IncludePropertyNode>        includedPropertiesList,
      List<IncludeCompactPropertyNode> includedCompactPropertiesList,
      Set<String>                      inhibitedPropertySet,
      List<PropertyNode>               skinPropertyNodeList,
      boolean trTextAntialias)
    {
      _noTrPropertyList = noTrPropertyList;
      _trRuleRefList = trRuleRefList;
      _includedPropertiesList = includedPropertiesList;
      _includedCompactPropertiesList = includedCompactPropertiesList;
      _inhibitedPropertySet = inhibitedPropertySet;
      _skinPropertyNodeList = skinPropertyNodeList;
      _trTextAntialias = trTextAntialias;
    }

    public List<PropertyNode> getNoTrPropertyList()
    {
      return _noTrPropertyList;
    }

    public List<String> getTrRuleRefList()
    {
      return _trRuleRefList;
    }

    public List<IncludePropertyNode> getIncludedPropertiesList()
    {
      return _includedPropertiesList;
    }

    public List<IncludeCompactPropertyNode> getIncludedCompactPropertiesList()
    {
      return _includedCompactPropertiesList;
    }

    public List<PropertyNode> getSkinPropertyNodeList()
    {
      return _skinPropertyNodeList;
    }

    public Set<String> getInhibitedProperties()
    {
      return _inhibitedPropertySet;
    }

    public boolean isTrTextAntialias()
    {
      return _trTextAntialias;
    }

    private Set<String>               _inhibitedPropertySet;
    private List<PropertyNode>        _noTrPropertyList;
    private List<String>              _trRuleRefList;
    private List<IncludePropertyNode> _includedPropertiesList;
    private List<IncludeCompactPropertyNode> _includedCompactPropertiesList;
    private List<PropertyNode>        _skinPropertyNodeList;
    private boolean                   _trTextAntialias;
  }

  // Custom Trinidad css properties:
  //-tr-rule-ref, -tr-inhibit, -tr-text-antialias
  private static final String _TR_PROPERTY_PREFIX = "-tr-";
  // For backwards compatibility, keep the -ora- css properties in
  // addition to the -tr- css properties.
  private static final String _ORA_PROPERTY_PREFIX = "-ora-";
  private static final String _PROPERTY_RULE_REF = "rule-ref";
  private static final String _PROPERTY_INHIBIT = "inhibit";
  private static final String _INCLUDE_PROPERTY = "-tr-property-ref";
  private static final String _PROPERTY_TEXT_ANTIALIAS = "text-antialias";

  private static final Pattern _SPACE_PATTERN = Pattern.compile("\\s");
  private static final Pattern _SELECTOR_PATTERN = Pattern.compile("selector\\(");

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SkinStyleSheetParserUtils.class);
}