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

import java.beans.Beans;

import java.io.PrintWriter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;


/**
 * CSS-generation related utilities used when we write out our css-2 stylesheet
 * document based on the skin's css-3 stylesheet document.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/CSSUtils.java#0 $) $Date: 10-nov-2005.18:58:49 $
 */

public class CSSGenerationUtils
{
  /**
   * Converts the specified set of StyleNodes to CSS. We output either full styleclass names or
   * compressed styleclass names.
   *
   * @param context The current StyleContext
   * @param styleSheetName The stylesheet name that is registered with the skin.
   *     e.g. skins/purple/purpleSkin.css
   * @param styles The style nodes to convert
   * @param writerFactory The factory to obtain {@link PrintWriter} instances
   * @param compressStyles This tells us whether or not we want to output the short names.
   * @param shortStyleClassMap A Map which maps style
   *   class names to short names.
   * @param namespacePrefixArray an array with the namespace prefixes of our
   *  special selectors. e.g., "af|" or "tr|" .
   * @param afSelectorMap A Map which maps the namespaced component selectors
   *   to their base names (e.g., 'af|menuPath::step' maps to 'af|menuPath A')
   */
  public static void writeCSS(
    StyleContext        context,
    String              styleSheetName,
    StyleNode[]         styles,
    StyleWriterFactory  writerFactory,
    boolean             compressStyles,
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixArray,
    Map<String, String> afSelectorMap
    )
  {
    PrintWriter out = writerFactory.createWriter();
    if (out == null)
    {
      return;
    }

    // writeCSS() attempts to produce a minimal set of style rules
    // by combining selectors with identical properties into a
    // single rule.  For example, we convert the following two
    // styles:
    //
    //   .OraBGAccentVeryDark,.x8 {background-color:#999966}
    //   .OraTable,.x1x {background-color:#999966}
    //
    // into a single style:
    //
    //   .OraBGAccentVeryDark,.x8,.OraTable,.x1x {background-color:#999966}
    //
    // The intention of this optimization is to improve performance by
    // reducing the overall size of the generated CSS file - and hopefully
    // speed up download time.
    //
    // To implement this optimization, we perform two passes over the
    // set of styles.  During the first pass, we build up a hash map
    // which maps from the property string to an array of StyleNodes
    // which share the properties defined by the property string.
    // During the second pass over the styles, we write out all matching
    // selectors for each style followed by the shared set of properties.

    // We track styles with matching properties in the following HashMap
    // which maps property strings to StyleNode[]s.
    HashMap<String, StyleNode[]> matchingStylesMap =
      new HashMap<String, StyleNode[]>(101);

    // We also keep an array of the property strings that we generate
    // during this pass, since we need these strings during the second
    // pass to find matching StyleNodes.
    String[] propertyStrings = new String[styles.length];

    // at this point the styles StyleNode[] can contain both Styles with
    // non-null selector or non-null name(aka alias). We only generate
    // the styles where getSelector is non-null.
    for (int i = 0; i < styles.length; i++)
    {
      StyleNode style = styles[i];

      if (style.getSelector() != null)
      {
        // Get the property string (properties are sorted so that
        // the order doesn't affect whether styles match).
        String propertyString = _getSortedPropertyString(style);

        // See if we already have a StyleNode with the same properties
        StyleNode[] matchingStyles = matchingStylesMap.get(propertyString);

        if (matchingStyles == null)
        {
          // If we don't already have matching StyleNodes, add this
          // StyleNode to the map.
          propertyStrings[i] = propertyString;
          matchingStyles = new StyleNode[1];
          matchingStyles[0] = style;
        }
        else
        {
          // If we already have matching StyleNodes, add this StyleNode
          // to the end of the list of matching StyleNodes.
          int length = matchingStyles.length;

          StyleNode[] newMatchingStyles = new StyleNode[length + 1];
          System.arraycopy(matchingStyles,
                           0,
                           newMatchingStyles,
                           0,
                           length);
          newMatchingStyles[length] = style;
          matchingStyles = newMatchingStyles;
        }

        // Rehash with the new value
        matchingStylesMap.put(propertyString, matchingStyles);
      }
    }

    // We'll start writing the CSS file now.  First
    // write out the header with a time stamp
    Date date = new Date();
    if (!compressStyles)
      out.println("/* This CSS file generated on " + date + " */");

    // Keep track of the number of selectors written out. The reason? IE has a 4095 limit,
    // and we want to warn when we get to that limit.
    int numberSelectorsWritten = 0;

    // This is the second pass in which we write out the style rules
    // Get the baseURI up front so we don't have to recalculate it every time we find
    // a property value that contains url() and need to resolve the uri.
    String baseURI = CSSUtils.getBaseSkinStyleSheetURI(styleSheetName);
    for (int i = 0; i < styles.length; i++)
    {
      StyleNode style = styles[i];
      String propertyString = propertyStrings[i];

      // We only write out styles for which we have a property string.
      // All other entries correspond to styles which don't have selectors -
      // or styles which will be rendered as a "matching" style.
      if (propertyString != null && !(propertyString.equals("")))
      {
        // Get all of the styles which share this property string.
        StyleNode[] matchingStyles = matchingStylesMap.get(propertyString);

        // Actually, we should always have at least one StyleNode here
        assert (matchingStyles != null);

        // determine if the current CSS file can fit all of the CSS selectors, or if a new
        // one will be needed.
        // TODO: figure out why we write both the uncompressed & compressed styles for styles
        // without a '|' character, shouldn't the uncompressed be enough on its own? This results
        // in some ugly code here.
        int stylesToBeWritten = 0;
        String[] selectors = new String[matchingStyles.length];
        String[] mappedSelectors = new String[matchingStyles.length];

        for (int j = 0; j < matchingStyles.length; j++)
        {
          selectors[j] = matchingStyles[j].getSelector();

          // We should always have a selector at this point
          assert (selectors[j] != null);

          mappedSelectors[j] = getMappedSelector(afSelectorMap,
                                                 namespacePrefixArray,
                                                 selectors[j]);

          if (compressStyles && (mappedSelectors[j].indexOf('|') == -1))
          {
            stylesToBeWritten += 2;
          }
          else
          {
            stylesToBeWritten++;
          }
        }

        if (numberSelectorsWritten + matchingStyles.length >= _MSIE_SELECTOR_LIMIT
          && TrinidadAgent.Application.IEXPLORER == context.getAgent().getAgentApplication())
        {
          out.println("/* The number of CSS selectors in this file is " +
                      numberSelectorsWritten + " */");
          out = writerFactory.createWriter();
          if (out == null)
          {
            return;
          }
          numberSelectorsWritten = 0;
        }

        // Write out all of the style selectors for this property string
        for (int j = 0; j < matchingStyles.length; j++)
        {
          String validFullNameSelector = null;

          // write out the full selector if we aren't compressing styles or
          // it doesn't have a '|' in the name which means it may be a user's public styleclass
          // and we don't want to compress those; we will also write out the compressed
          // version the public styleclasses in the next step.
          if (!compressStyles || (mappedSelectors[j].indexOf('|') == -1))
          {
            validFullNameSelector =
              getValidFullNameSelector(mappedSelectors[j], namespacePrefixArray);

            if (validFullNameSelector != null)
            {
              out.print(validFullNameSelector);
              numberSelectorsWritten++;
            }
          }



          if (compressStyles)
          {
            String shortSelector = 
              getShortSelector(shortStyleClassMap, namespacePrefixArray, mappedSelectors[j]);

            // if the transformed full name is different than the shortSelector
            // then write out the shortSelector, too.
            if (shortSelector != null)
            {
              String validShortSelector =
                getValidFullNameSelector(shortSelector, namespacePrefixArray);

              // if we wrote out a full style, check to see if we need to write out the short, too.
              // if it is something different, write out the short, too.
              if (validFullNameSelector != null)
              {
                //Since validFullNameSelector is not null, we know we wrote out a full style
                // we write out a short style too in this case if it is different
                // example: .PublicStyleClass is written out fully even in compressed mode, but
                // it is different in compressed mode, so we write that out, too.
                if (!validFullNameSelector.equals(validShortSelector))
                {
                  out.print(',');
                  out.print(validShortSelector);
                  numberSelectorsWritten++;
                }
              }
              else
              {
                out.print(validShortSelector);
                numberSelectorsWritten++;
              }
            }
          }

          // Write out a separator between matching selectors
          if (j < (matchingStyles.length - 1))
            out.print(",");
        }

        // Now that we have written out the selectors, write out
        // the properties
        out.print(" {");

        // At this point, we could just write out the property string
        // that we already created, but this string contains the properties
        // in sorted order.  We prefer to attempt to preserve the order
        // of the properties as specified in the XSS document.  So,
        // we get the properties from the StyleNode object instead of
        // using the propertyString
        Iterable<PropertyNode> properties = style.getProperties();
        boolean first = true;

        for (PropertyNode property : properties)
        {
          String propName = property.getName();
          String propValue = property.getValue();

          if ((propName != null) &&
              (propValue != null) &&
              (propValue.length() > 0 ))
          {
            if (!first)
              out.print(";");
            else
              first = false;

            out.print(propName);
            out.print(":");
            String resolvedPropValue =
              CSSUtils.resolvePropertyValue(styleSheetName, baseURI, propName, propValue);
            out.print(resolvedPropValue);

          }
        }

        if (compressStyles)
          out.print("}"); // take out the newlines for performance
        else
          out.println("}");
      }
    }
    out.println("/* The number of CSS selectors in this file is " + numberSelectorsWritten + " */");
  }

  /**
   * Shorten (compress) the selector.
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param selector
   * @return the shortened selector, or selector if nothing could be shortened.
   */
  public static String getShortSelector(
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixArray,
    String              selector)
  {
    // shorten all the css-2 style class selectors (those that start with
    // '.' and don't have a namespace prefix in it)
    // and return the shortened string.
    // e.g., selector of '.OraBulletedList A' is shortened to '.xj A'
    // e.g., selector of af|inputText::content is not shortened since
    // it has no css-2 style class selector piece that starts with '.'.
    // e.g., selector of af|foo.Bar will shorten the '.Bar' piece
    // af|foo.xz
    // e.g., .Foo:hover -> .x0:hover
    String shortSelector = _getShortNonNamespacedSelector(selector,
                                             shortStyleClassMap);

    if (shortSelector == null)
      shortSelector = selector;

    // run it through a shortener one more time to shorten any
    // of the af component selectors.
    // e.g., 'af|menuPath' is shortened to '.x11'

    if (_hasNamespacePrefix(shortSelector, namespacePrefixArray))
    {
      String[] shortSelectorArray  = _splitStringByWhitespace(shortSelector);

      shortSelector =
        _getMappedNSSelector(shortStyleClassMap,
                             namespacePrefixArray,
                             shortSelector,
                             shortSelectorArray,
                             true);
    }
    return shortSelector;
  }

  /**
   * Tests whether the specified selector is a single style class
   * selector. A single style class selector is something like
   * ".AFInstructionText". Examples that are not single style class
   * selectors are "af|inputText" or ".AFFoo .AFBar" or ".foo:hover"
   */
  public static boolean isSingleStyleClassSelector(String selector)
  {
    if ((selector == null)      ||
        (selector.length() < 2) ||
        (selector.charAt(0) != '.'))
    {
      return false;
    }

    for (int i = 1; i < selector.length(); i++)
    {
      if (_isStyleClassTerminator(selector.charAt(i)))
        return false;
    }

    return true;
  }

  /**
   * Returns an
   * Iterator of all of the style class selectors included in the
   * specified selector.  For example, ".OraNav1Enabled" returns
   * a single element Iterator with the string "OraNav1Enabled".
   * "P.OraNav1Enabled SPAN.text" returns a two element Iterator
   * with "OraNav1Enabled" and "text".
   * .OraLink:visited returns "OraLink"
   * .star.moon returns "star" and "moon"
   */
  public static Iterator<String> getStyleClasses(String selector)
  {
    ArrayList<String> styleClasses = null;
    int styleClassStartIndex = -1;
    int length = selector.length();

    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);

      // If we aren't inside of a style class yet,
      // check to see if we are about to enter one.
      // Otherwise, check to see if we are at the
      // end of one.
      if (styleClassStartIndex == -1)
      {
        if (c == '.')
          styleClassStartIndex = i;
      }
      else
      {
        boolean end = _isStyleClassTerminator(c);
        if (!end)
        {
          // Check to see if we are at the end of the string
          if (i == (length - 1))
          {
            // We need to increment our counter to make sure
            // we include this last character when we pull out
            // the substring
            i++;
            end = true;
          }
        }

        // If we're at the end of the style class, add it to the list
        if (end)
        {
          String styleClass = selector.substring(styleClassStartIndex + 1, i);
          if (styleClasses == null)
            styleClasses = new ArrayList<String>(3);

          styleClasses.add(styleClass);
          // if c is ., then this means we've found another styleclass
          if (c == '.')
            styleClassStartIndex = i;
          else
            styleClassStartIndex = -1;
        }
      }
    }

    if (styleClasses == null)
      return null;

    return styleClasses.iterator();
  }

  /**
   * Called when creating the shortened styleclass map.
   * Returns an Iterator of all of the component selectors that begin with the
   * given namespace. All styleclasses and pseudo-classes are ignored and NOT
   * returned.
   * For example, ".OraNav1Enabled af|menuPath" returns
   * a single element Iterator with the string "af|menuPath".
   * "af|menuPath.OraNav1Enabled af|treeTable.text" returns a two
   * element Iterator with "af|menuPath" and "af|treeTable".
   * af|inputText:disabled af|inputText::content returns two
   * "af|inputText" and "af|inputText::content".
   * It also looks at the afSelectorMap to map any special selectors if
   * necessary.
   * e.g., "af|menuPath::step" maps to "af|menuPath A", so
   * ".OraNav1Enabled af|menuPath::step" returns "af|menuPath"
   */
  public static Iterator<String> getNamespacedSelectors(
    String              selector,
    String              namespace,
    Map<String, String> afSelectorMap)
  {
    if (selector == null)
      return null;
    int afIndex = selector.indexOf(namespace);

    // no namespace in the selector, so just return null
    if (afIndex == -1)
      return null;

    ArrayList<String> afUnmappedSelectorList = new ArrayList<String>();

    // now find each af| component selector and map
    // e.g., af|menuPath::step maps to af|menuPath A

    // order the pseudo elements and classes
    String base = selector.substring(afIndex);
    String[] afSelectors =
        _orderPseudoElementsAndClasses(base);

    // loop through each of the af| parts.
    for (int i=0; i < afSelectors.length; i++)
    {
      if (afSelectors[i].startsWith(namespace))
      {
        // get the component selector, which is just the main part, nothing
        // that includes a '.' ':', or a ' '.
        String afComponentSelector =
          _getNSComponentSelector(afSelectors[i], false);
        afUnmappedSelectorList.add(afComponentSelector);
      }
    }

    // ok, now everything in afSelectorList at this point is the unmapped
    // af|xxx component selectors. Now map them, and return the base key
    // in the selector.
    // loop through again and map them
    ArrayList<String> afSelectorList = new ArrayList<String>();

    int size = afUnmappedSelectorList.size();
    for (int i=0; i < size; i++)
    {
      String afComponentSelector = afUnmappedSelectorList.get(i);
      String mappedSelector = null;

      if (afSelectorMap != null)
      {
        mappedSelector = afSelectorMap.get(afComponentSelector);

      }
      if (mappedSelector != null)
      {
        // In case the mapped selector does not start with the namespace,
        // start the string where the namespace starts.
        int namespaceIndex = mappedSelector.indexOf(namespace);
        if (namespaceIndex > -1)
        {
          // get the selector up until the space.
          String[] baseSelector = _splitStringByWhitespace(mappedSelector.substring(namespaceIndex));
          afComponentSelector = baseSelector[0];
          afSelectorList.add(afComponentSelector);
        }
      }
      else
      {
        afSelectorList.add(afComponentSelector);
      }

    }
    return afSelectorList.iterator();

  }

  /**
   * Add to the namespacePrefixes Set any namespace prefixes found in this selector.
   * @param namespacePrefixes
   * @param selector
   */
  public static void getNamespacePrefixes(
    Set<String> namespacePrefixes,
    String selector)
  {

    int length = selector.length();
    int startSubstringIndex = 0;
    // Loop through each character of the selector looking for namespace prefixes.
    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);
      if (c == '|')
      {
        String prefix = selector.substring(startSubstringIndex, i+1);
        startSubstringIndex = i+1;
        // protect against just | in the prefix by checking length.
        if (prefix.length() > 1)
          namespacePrefixes.add(prefix);
      }
      else if(!_isStyleClassTerminator(c))
      {
        // keep going if it isn't a terminating character
      }
      else
      {
        // update the startSubstring index.
        startSubstringIndex = i+1;
      }
    }
    return;
  }

  /**
   * Called from getNamespacedSelectors.
   * Given a single selector that begins with a namespace prefix (most likely
   * af|), return the main portion -- the prefix+component+pseudo-element.
   * Styleclasses and pseudo-classes and anything after a space will be ignored,
   * and won't be returned.
   * @param singleAfSelector
   * @param allowPseudoClass
   * @return
   */
  private static String _getNSComponentSelector(
    String singleAfSelector,
    boolean allowPseudoClass)
  {
    int colonIndex = singleAfSelector.indexOf("::");

    // get the part after the ::.
    // colonIndex will be 0 if there is no ::.
    if (colonIndex != -1)
    {
      colonIndex += 2;
    }
    else
      colonIndex = 0;
    String afterDoubleColon = singleAfSelector.substring(colonIndex);

    // now find the part with a single ':', a ' ', or a '.'. That is where
    // I want to chop it to get my component selector.

    boolean end = false;
    int afterLength = afterDoubleColon.length();
    int endIndex=0;

    for (; ((endIndex < afterLength) && !end); endIndex++)
    {
      char c = afterDoubleColon.charAt(endIndex);
      if (allowPseudoClass)
        end = Character.isWhitespace(c);
      else
        end = _isStyleClassTerminator(c);
    }

    String afComponentSelector = null;
    if (end)
    {
      afComponentSelector =
        singleAfSelector.substring(0, colonIndex + endIndex-1);
    }
    else
    {
      // there was no end, so just take the entire string.
      afComponentSelector = singleAfSelector;
    }

    return afComponentSelector;
  }

  // Converts all style class selectors that begin with '.'
  // to the short version if
  // there is a short version. does not shorten styles that start with the
  // namespace
  // returns null if it can't shorten the selector
  private static String _getShortNonNamespacedSelector(
    String              selector,
    Map<String, String> shortStyleClassMap)
  {
    if (shortStyleClassMap == null)
      return null;

    // This will see if the selector is a single styleClass, and if so,
    // it will run it through the shortStyleClassMap and return the shortened
    // style class. A single style class selector is something like
    // ".AFInstructionText". Examples that are not single style class
    // selectors are "af|inputText" or ".AFFoo .AFBar" or ".foo:hover"
    if (isSingleStyleClassSelector(selector))
    {
      String shortStyleClass = shortStyleClassMap.get(selector.substring(1));

      return (shortStyleClass == null) ? null : "." + shortStyleClass;
    }

    // This will run if we do not have a one class selector definition
    // but a af|XXX class or a command class like .AFXYZ .AFzzz or
    // ".foo:hover" It shortens all the .xxx pieces.
    boolean isShorter = false;
    int length = selector.length();
    StringBuffer buffer = new StringBuffer(length);
    int styleClassStartIndex = -1;

    // loop through each character. If we don't come across a '.' character,
    // return the selector unchanged.
    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);

      if (styleClassStartIndex == -1)
      {
        if (c == '.')
          styleClassStartIndex = i;

        buffer.append(c);
      }
      else
      {
        // We are currently inside of a style class substring.
        // Check to see if the style class has been terminated.
        boolean end = _isStyleClassTerminator(c);
        if (!end)
        {
          // Check to see if we are at the end of the string
          if (i == (length - 1))
          {
            // We need to increment our counter to make sure
            // we include this last character when we pull out
            // the substring
            i++;
            end = true;
          }
        }

        if (end)
        {

          // If we are at the end of the style class, check to
          // see if we've got a shorter version
          String styleClass = selector.substring(styleClassStartIndex + 1, i);
          String shortStyleClass = null;
          // don't shorten the styles that contain a namespace, because we
          // do this in another step.
          if (styleClass.indexOf('|') == -1)
            shortStyleClass = shortStyleClassMap.get(styleClass);


          if (shortStyleClass == null)
          {
            buffer.append(styleClass);
          }
          else
          {
            buffer.append(shortStyleClass);
            isShorter = true;
          }

          // Don't forget the terminator character
          if (i < (length - 1))
            buffer.append(c);
          // if c is ., then this means we've found another styleclass
          if (c == '.')
            styleClassStartIndex = i;
          else
            styleClassStartIndex = -1;
        }
      }
    }

    // return the original selector if this isn't shorter.
    return isShorter ? buffer.toString() : selector;
  }
  
  /**
   * Runs a selector through a map. It returns the selector unchanged (except for converted
   * pseudo-classes) if there is no namespace in the selector.
   * This could be a map to convert the
   * public no-html selector to an internal selector that has html-specifics
   * (e.g., 'af|menuPath::step:hover' -> 'af|menuPath A:hover')
   * or it could be a map to shorten the selector
   * (e.g., 'af|menuPath A:hover' -> '.x11 A:hover')
   * We call this method first with the public->internal map, and then
   * to shorten it.
   * Only the pieces of the selector that start with the namespace are mapped.
   * @param afSelectorMap if shortenPass is true, then this map shortens the
   *                 af| selector. else, it maps the public af| selector
   *                 to the internal selector (a selector that is closer to what is written to the
*                    CSS file. 
*                    e.g., af|inputText:error::content becomes 
*                    af|inputText.p_AFError af|inputText::content
   * @param namespacePrefixArray   most likely, "af|". The selectors with this namespace
   *                               are the ones we map.
   * @param selector    selector to map.
   */
  public static String getMappedSelector (
    Map<String, String> afSelectorMap,
    String[]            namespacePrefixArray,
    String              selector)
  {
    String mappedSelector;
    if (_hasNamespacePrefix(selector, namespacePrefixArray))
    {
      String[] selectorArray =
        _orderPseudoElementsAndClasses(selector);

      // map selectors, if needed
      // any of the selectors that start with a namespace prefix will
      // be mapped.
      // e.g., "af|menuPath::step" maps to "af|menuPath A"
      // if the selector does not need to be 'mapped', it will be returned
      // untouched. e.g., .AFInstructionText maps to .AFInstructionText
      mappedSelector = _getMappedNSSelector(afSelectorMap,
                                            namespacePrefixArray,
                                            selector,
                                            selectorArray,
                                            false);
    }
    else
    {
      // there are no namespaces in this selector, but we still need to convert pseudo-classes.
      //Separate pseudoclasses and then call it
      int start = 0;
      StringBuilder b = new StringBuilder();
      for(int i = 0; i < selector.length(); i++)
      {
        char c = selector.charAt(i);
        if (c == ' ' )
        {
          if (start == i)
          {
            //group of spaces
            start = i+1; //Skip space
          }
          else
          {
            String subSelector = selector.substring(start,i);
            subSelector = _convertPseudoClassesInSelector(subSelector);
            start = i+1; //Skip space
            b.append(subSelector);
            b.append(' ');
          }
        }
      } // end for loop

      // We reached the end of the selector, now convert the last bit
      if (start == 0)
      {
        //there is no space in selector, but we still need to map pseudo-classes.
        mappedSelector = _convertPseudoClassesInSelector(selector);
      }
      else
      {
        String subSelector = selector.substring(start);
        subSelector = _convertPseudoClassesInSelector(subSelector);
        b.append(subSelector);
        mappedSelector = b.toString();
      }

    }
    return mappedSelector;
  }

  /**
   * Runs a namespaced selector through a map.
   * This could be a map to convert the
   * public no-html selector to an internal selector that has html-specifics
   * (e.g., 'af|menuPath::step:hover' -> 'af|menuPath A:hover')
   * or it could be a map to shorten the selector
   * (e.g., 'af|menuPath A:hover' -> '.x11 A:hover')
   * We call this method first with the public->internal map, and then
   * to shorten it.
   * Only the pieces of the selector that start with the namespace are mapped.
   * @param map         if shortenPass is true, then this map shortens the
   *                    af| selector. else, it maps the public af| selector
   *                    to the internal selector.
   * @param namespace   most likely, "af|". The selectors with this namespace
   *                    are the ones we map.
   * @param selector    selector to map.
   * @param selectorArray selectorArray is the selector split into pieces based on the ' '
   * @param shorten     if true, then we'll add the "." to the mapped selector.
   * @return            the selector, mapped.
   */
  private static String _getMappedNSSelector (
    Map<String, String> map,
    String[]            nsPrefixArray,
    String              selector,
    String[]            selectorArray,
    boolean             shorten)
  {
    // selectorArray is the selector broken into pieces.
    // Map each piece, then put back together.

    for (int i=0; i < selectorArray.length; i++)
    {
      // Assumption is the selector 'piece' only contains one namespace
      // prefix. af|foo.tr|bar is not a valid selector, so this assumption
      // is fine.
      int nsIndex = -1;
      int numNsPrefixes = nsPrefixArray.length;
      for (int j=0; (j <  numNsPrefixes) && (nsIndex == -1); j++)
      {
       nsIndex = selectorArray[i].indexOf(nsPrefixArray[j]);
      }

      if (nsIndex > -1)
      {
        selectorArray[i] = _getEachMappedSelector(map,
                                                  nsIndex,
                                                  selectorArray[i],
                                                  shorten);
      }
    }

    // build back the selectorArray into a string
    return _arrayToStringWithSpaces(selectorArray);
  }

  private static String _getEachMappedSelector(
    Map<String, String> map,
    int                 indexOfNSPrefix,
    String              selector,
    boolean             shorten)
  {
    // break apart the selector into 2 parts:
    // main (af|foo::pseudo-element)
    // end (anything after, ':' or including and after '.')
    // map the main and the ending (pseudo-classes could be mapped to private
    // styleclasses, :disabled -> .p_AFDisabled
    // piece  back together.
    if (indexOfNSPrefix == -1)
     return selector;
    if (map == null)
      return selector;


    String wholeAfSelector = selector.substring(indexOfNSPrefix);

    // get main piece
    int colonIndex = wholeAfSelector.indexOf("::");

    // get the part after the ::.
    // colonIndex will be 0 if there is no ::.
    if (colonIndex != -1)
    {
      if (_LOG.isWarning() &&
          (wholeAfSelector.lastIndexOf("::") != colonIndex))
      {
        _LOG.warning("UNSUPPORTED_CONSECUTIVE_SUB_ELEMENT_SYNTAX", selector);
      }

      colonIndex += 2;
    }
    else
      colonIndex = 0;
    String afterDoubleColon = wholeAfSelector.substring(colonIndex);

    // now find the part with a single ':', a ' ', a '[', or a '.'. That is where
    // I want to chop it to get my 'main' piece of the component selector.
    boolean end = false;
    int afterLength = afterDoubleColon.length();
    int endIndex=0;
    char c;
    for (; ((endIndex < afterLength) && !end); endIndex++)
    {
      c = afterDoubleColon.charAt(endIndex);
      end = (Character.isWhitespace(c)) || (c == '.') || (c == ':') || (c == '[');
    }

    // Set the main piece in the pieces object
    String mainSelector;
    if (endIndex == afterLength)
    {
      mainSelector = wholeAfSelector.substring(0, colonIndex + endIndex);
    }
    else
    {
      mainSelector = wholeAfSelector.substring(0, colonIndex + endIndex-1);
    }
    String afterMain = null;
    if (endIndex != afterLength)
    {
      // If I got here, that means there are characters after the 'main' part
      // of the selector.
      afterMain = wholeAfSelector.substring(colonIndex + endIndex-1);
      // map the afterMain part. It includes styleclasses and pseudo-classes.
      // we don't need to convert the pseudo-classes if we are shortening.
      if (!shorten)
        afterMain = _convertPseudoClassesInSelector(afterMain);
    }

    // piece back together the string
    StringBuffer buffer = new StringBuffer();

    // beginning. add '.' if needed.
    if (indexOfNSPrefix > 0)
    {
      buffer.append(selector.substring(0, indexOfNSPrefix));
      if (shorten && selector.charAt(indexOfNSPrefix-1) != '.')
        buffer.append('.');
    }
    else if (shorten)
      buffer.append('.');

    // map the mainSelector, and append the afterMain part.
    buffer.append(_runThroughMap(map, mainSelector));
    if (afterMain != null)
      buffer.append(afterMain);


    return buffer.toString();
  }

  private static boolean _hasNamespacePrefix(
    String   selector,
    String[] nsPrefixArray)
  {
    if (selector == null) return false;
    boolean hasNamespacePrefix = false;
    int numNamespaces = nsPrefixArray.length;
    for (int i=0; (i <  numNamespaces )&& !hasNamespacePrefix; i++)
      if (selector.indexOf(nsPrefixArray[i]) > -1)
        hasNamespacePrefix = true;
    return hasNamespacePrefix;
  }

  /*
   * Returns a string representation of the contents of the specified array
   * where adjacent elements are separated a space (" ").
   */
  private static String _arrayToStringWithSpaces(
    String[] stringArray
    )
  {
    int length = stringArray.length;
    // if only one thing in the array, just return it to save some time.
    if (stringArray.length == 1) return stringArray[0];

    // get the bufferSize
    int bufferSize = 0;
    for (int i=0; i < length; i++)
    {
      bufferSize += stringArray[i].length() + 1;
    }
    // turn the array into a space deliminated String
    StringBuffer returnString = new StringBuffer(bufferSize);
    for (int i=0; i < length; i++)
    {
      returnString.append(stringArray[i]);
      if (i+1 < length)
        returnString.append(' ');
    }
    return returnString.toString();
  }

  /*
   * if I see this: af|foo:class:class::element, return this
   * af|foo:class:class and af|foo::element in a String array.
   * if I see this: af|foo:p-class.StyleClass::element, return this:
   * 'af|foo:p-class.StyleClass' 'af|foo::element'
   * af|foo.StyleClass::element -> 'af|foo.StyleClass' 'af|foo::element'
   * If I see thiss: af|foo::p-element.StyleClass, return this
   * af|foo::p-element.StyleClass (leave alone).
   */
  private static String[]  _orderPseudoElementsAndClasses(
    String selector)
  {
    String[] input = _splitStringByWhitespace(selector);

    List<String> output = new ArrayList<String>();
    for (int i=0; i < input.length; i++)
    {

      int indexOfDoubleColon = input[i].indexOf("::");
      if (indexOfDoubleColon == -1)
      {
        // no double colon (aka pseudo-element)
        output.add(input[i]);
      }
      else
      {
        // you have a double colon index. Now look to see if we need to
        // reorder. We have to reorder if the pseudo-element comes after
        // the pseudo-class or composite style classes.
        int indexOfFirstColon = input[i].indexOf(':');
        int indexOfDot = input[i].indexOf('.');

        boolean pseudoClassBeforePseudoElement =
          (indexOfFirstColon < indexOfDoubleColon);
        boolean styleClassBeforePseudoElement =
          (indexOfDot != -1 && indexOfDot < indexOfDoubleColon);

        if (!(pseudoClassBeforePseudoElement ||
              styleClassBeforePseudoElement))
        {
          output.add(input[i]);
        }
        else
        {
          if (indexOfFirstColon == indexOfDoubleColon)
            indexOfFirstColon = -1;
          int indexOfClass = Math.min(indexOfFirstColon, indexOfDot);
          if (indexOfClass == -1)
            indexOfClass = Math.max(indexOfFirstColon, indexOfDot);

          // we have the condition where pseudo-class or styleClass is before
          // pseudo-element: af|foo:psdo-class::psdo-element
          // e.g.,
          // af|inputText:disabled::content
          // indexOfColon = 12, indexOfDoubleColon = 21
          // main = 'af|inputText'
          // mainPlusClasses = 'af|inputText:disabled'
          // end '::content'
          String main = input[i].substring(0, indexOfClass);
          String mainPlusClasses = input[i].substring(0, indexOfDoubleColon);
          String end = input[i].substring(indexOfDoubleColon);
          output.add(mainPlusClasses);
          output.add(main + end);
        }
      }

    }
    return output.toArray(new String[output.size()]);
  }



  /**
   * get rid of the | and :: that browsers don't like, and add the
   * '.' where needed to make the af| component selector
   * into a style class selector that is valid to be written to the css file.
   * @param selector
   * @return
   */
  public static String getValidFullNameSelector(
    String selector,
    String[] namespacePrefixArray)
  {
    if (selector.indexOf('|') == -1)
      return selector;

    // split on spaces.
    String[] spacerArray = _splitStringByWhitespace(selector);

    for (int i=0; i < spacerArray.length; i++)
    {
      // if this starts with any of our namespaces, then add a '.'
      if (spacerArray[i].indexOf('|') > -1)
      {
        for (int j=0; j < namespacePrefixArray.length; j++)
        {
          String nsPrefix = namespacePrefixArray[j];
          if (spacerArray[i].startsWith(nsPrefix))
          {
            spacerArray[i] = ".".concat(spacerArray[i]);
            break;
          }
        }
      }
    }
    return StyleUtils.convertToValidSelector(
      _arrayToStringWithSpaces(spacerArray));

  }


  // Tests whether the specified character or pseudo-class terminates a
  // style class selector
  private static boolean _isStyleClassTerminator(char c)
  {
    return (Character.isWhitespace(c) || (c == ':') || (c == '.') || (c == '['));
  }

  // Gets the properties of the specified StyleNode in sorted
  // order as a String.
  private static String _getSortedPropertyString(StyleNode style)
  {
    // First, pull the properties out of the StyleNode
    // -= Simon Lessard =-
    // TODO: Check if synchronization is needed, otherwise uses
    //       an ArrayList instead. Even if synchronization is needed
    //       Collections.synchronizedList(ArrayList) would probably be
    //       a better choice.
    Vector<PropertyNode> v = new Vector<PropertyNode>();
    Iterable<PropertyNode> propertyNodeList = style.getProperties();
    for (PropertyNode propertyNode : propertyNodeList)
      v.addElement(propertyNode);

    PropertyNode[] properties = new PropertyNode[v.size()];
    v.copyInto(properties);

    // Sort the properties so that the order of the properties won't
    // come into play when comparing property strings.
    Arrays.sort(properties,PropertyNodeComparator.sharedInstance());

    // Compute the StringBuffer size
    int bufferSize = 0;
    for (int i = 0; i < properties.length; i++)
    {
      PropertyNode property = properties[i];
      String name = property.getName();
      String value = property.getValue();

      if ((name != null) && (value != null))
      {
        bufferSize += property.getName().length();
        bufferSize += property.getValue().length();

        // Leave room for a separator
        bufferSize++;
      }
    }

    StringBuffer buffer = new StringBuffer(bufferSize);
    boolean first = true;

    for (int i = 0; i < properties.length; i++)
    {
      PropertyNode property = properties[i];
      String name = property.getName();
      String value = property.getValue();

      if ((name != null) && (value != null))
      {
        if (!first)
          buffer.append(";");
        else
          first = false;

        buffer.append(name);
        buffer.append(":");
        buffer.append(value);
      }
    }

    return buffer.toString();
  }

  /**
   * Given a selector, convert the pseudo-classes. Non css-2 pseudo-classes
   * get converted to styleclasses so they don't get passed through to the generated css.
   * @param selector String input selector. The assumption is there are no spaces.
   * If the original selector has spaces, then break the selector up into the pieces before
   * calling this method.
   * @return String the selector with the pseudo-classes converted, if needed.
   * e.g., .StyleClass:error:active -> .StyleClass.p_AFError:active
   */
  private static String _convertPseudoClassesInSelector(String selector)
  {
    if (selector == null) return selector;

    StringBuffer completeBuffer = new StringBuffer();
    StringBuffer pseudoClassBuffer = new StringBuffer();
    boolean inPseudoClass = false;

    for (int i=0; i < selector.length(); i++)
    {
      char x = selector.charAt(i);

      if ((x == ':') || (x == '.') || (x == '['))
      {
        if (inPseudoClass)
        {
          // if we are in a pseudo-class already, and we get a ':' or '.' that means
          // this pseudo-class is complete. Get ready for another one.
          String convertedPseudoClass = _convertPseudoClass(pseudoClassBuffer.toString());
          completeBuffer.append(convertedPseudoClass);
          pseudoClassBuffer = new StringBuffer();
          inPseudoClass = false;
        }
        if (x == ':')
        {
          inPseudoClass = true;
          pseudoClassBuffer.append(x);
        }
        else if (x == '.' || x == '[')
        {
          completeBuffer.append(x);
        }

      }
      else
      {
        if (!inPseudoClass)
          completeBuffer.append(x);
        else
          pseudoClassBuffer.append(x);
      }

    }
    if (inPseudoClass)
    {
      String mappedPseudoClass = _convertPseudoClass(pseudoClassBuffer.toString());
      completeBuffer.append(mappedPseudoClass);

    }
    return completeBuffer.toString();
  }

  /**
   * return the array of strings computed by splitting this string
   * around one or more whitespaces This calls Character.isWhitespace to determine if it is
   * whitespace. This is important because tabs, newlines, etc count as whitespace
   * in the selector strings. This is faster than String's split("\\s")
   * @param selector
   * @return String[] The array of Strings computed by splitting the input String
   * around one or more spaces. e.g, "af|foo    af|bar" returns "af|foo" and "af|bar" in
   * a String Array.
   */
  private static String[] _splitStringByWhitespace (
    String  selector)
  {
    // return a String[] with each piece that is deliminated by the inChar.
    int length = selector.length();
    StringBuffer buffer = new StringBuffer(length);
    List<String> splitList = new ArrayList<String>();
    boolean inWhitespace = false;

    for (int i=0; i < length; i++)
    {
      char c = selector.charAt(i);
      if (Character.isWhitespace(c))
      {
        // we hit the whitespace delimiter, so put it in the splitList and start a new buffer.
        // ignore spaces that are in a row
        if (!inWhitespace)
        {
          String bufferString = buffer.toString();
          if (bufferString.length() > 0)
          {
            splitList.add(bufferString);
            buffer = new StringBuffer(length);
            inWhitespace = true;
          }
        }
      }
      else
      {
        buffer.append(c);
        if (inWhitespace)
          inWhitespace = false;
      }
    }
    // we are done with all the characters
    String lastString = buffer.toString();
    if (lastString.length() > 0)
      splitList.add(lastString);

    return splitList.toArray(_EMPTY_STRING_ARRAY);

  }

  // run through the map. If it's not in the map, return the selector unchanged.
  private static String _runThroughMap(Map<String, String> map, String selector)
  {
    String mappedSelector = map.get(selector);
    return (mappedSelector != null) ? mappedSelector : selector;
  }


  // Comparator that sorts PropertyNodes by name
  private static class PropertyNodeComparator implements Comparator<PropertyNode>
  {
    public static Comparator<PropertyNode> sharedInstance()
    {
      return _sInstance;
    }

    public int compare(PropertyNode o1, PropertyNode o2)
    {
      String name1 = (o1 == null) ? null : o1.getName();
      String name2 = (o2 == null) ? null : o2.getName();

      if ((name1 == null) || (name2 == null) )
      {
        if (name1 == name2)
          return 0;
        else if (name1 != null)
          return 1;

        return -1;
      }

      return name1.compareTo(name2);
    }

    private PropertyNodeComparator() {}

    private static final Comparator<PropertyNode> _sInstance =
      new PropertyNodeComparator();
  }

  static private String _convertPseudoClass(String pseudoClass)
  {
    // The design time needs the browser-supported pseudo-classes to be converted so they
    // can show a preview of the skinned component.
    if (_BUILT_IN_PSEUDO_CLASSES.contains(pseudoClass) && !Beans.isDesignTime())
      return pseudoClass;
    StringBuilder builder = new StringBuilder(pseudoClass.length() + 3);
    builder.append(".");
    builder.append(SkinSelectors.STATE_PREFIX);

    for (String content : _DASH_PATTERN.split(pseudoClass.substring(1)))
    {
      if (content.length() > 0)
      {
        builder.append(Character.toUpperCase(content.charAt(0)));
        builder.append(content.substring(1));
      }
    }

    return builder.toString();
  }

  // We want to output to the css the browser-supported pseudo-classes as is
  static private final Set<String> _BUILT_IN_PSEUDO_CLASSES =
    new HashSet<String>();
  static
  {
    _BUILT_IN_PSEUDO_CLASSES.add(":first-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":link");
    _BUILT_IN_PSEUDO_CLASSES.add(":visited");
    _BUILT_IN_PSEUDO_CLASSES.add(":hover");
    _BUILT_IN_PSEUDO_CLASSES.add(":active");
    _BUILT_IN_PSEUDO_CLASSES.add(":focus");
  }

  private static final Pattern _DASH_PATTERN =  Pattern.compile("-");
  private static final String[] _EMPTY_STRING_ARRAY = new String[0];
  private static final int _MSIE_SELECTOR_LIMIT = 4095;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CSSGenerationUtils.class);
}
