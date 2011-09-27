package org.apache.myfaces.trinidad.style;

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

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Styles is a class that contains information about CSS Selectors and their
 * corresponding Style object (i.e., the css properties) for the css selectors  
 * that are written to the generated css file. The methods in this class
 * are useful if you need to know in the renderer code what css properties
 * a particular selector has.
 */
public abstract class Styles
{

  /**
   * Returns a Map containing the Selector Object as the key and the Style 
   * Object as the value. 
   * The Style object contains all the css property names/values.
   * This Map can then be used to get all the selector keys, or to get the 
   * Style Object for a Selector, or to get all the selectors that 
   * match some criteria, like they contain a certain simple selector.
   * 
   * @return unmodifiableMap of the resolved Selector -> Style map.
   */
  public abstract Map<Selector, Style> getSelectorStyleMap();
  
  /**
   * Returns the Selector in String form, converted to a format that
   * is suitable to be written to the client (e.g., a css-2 format that can be
   * read by a browser)
   * @param selector Selector
   * @return String the Selector in a String form that is suitable to be
   * written to the client.
   */
  public abstract String getNativeSelectorString(Selector selector);
  

  /**
   * Given a simple selector (as opposed to a complex selector), 
   * this method will return a Set of the Selectors that
   * contain the simple selector. It calls getSelectorStyleMap() and then
   * looks through each Selector to see if the simple selector is contained
   * within it. If so, it puts it in a Set.
   * The recommended usage is to call this for simple selectors, like
   * "af|inputText" or "af|treeTable" and not selectors like 
   * "af|inputText::content".
   * @param simpleSelector
   * @return an unmodfiable Set&lt;Selector> of all the complete selectors  
   * that contain the simple selector.
   */
  public Set<Selector> getSelectorsForSimpleSelector(String simpleSelector)
  {
    Map<Selector, Style> selectorStyleMap = getSelectorStyleMap();
    Set<Selector> set = null;
    
    // loop through each entry and find all simple selectors
    for (Map.Entry<Selector, Style> entry : selectorStyleMap.entrySet())
    {
      Selector completeSelector = entry.getKey();
      String completeSelectorString = completeSelector.toString();
      int index = completeSelectorString.indexOf(simpleSelector);
      boolean beforeSelectorOk = false;
      boolean afterSelectorOk = false;
      if (index > -1)
      {
        // found the simpleSelector within the complex selector
        // double check that it is the simpleSelector by looking at the characters before
        // and after. e.g., af|foobar does not contain the simple selector af|foo, whereas
        // af|foo.bar or af|foo bar or af|foo:bar does.
        if (index == 0)
        {
          beforeSelectorOk = true;
        }
        else
        {
          char c = completeSelectorString.charAt(index-1);
          if (Character.isWhitespace(c) || c == '.')
          {
            beforeSelectorOk = true;
          }
        }
        if (beforeSelectorOk)
        {
          // now check after the string.
          int endIndex = index + simpleSelector.length();
          if (endIndex < completeSelectorString.length())
          {
            char c = completeSelectorString.charAt(endIndex);
            if (Character.isWhitespace(c) || c == '.' || c == ':')
              afterSelectorOk = true;  
          }
          else
            afterSelectorOk = true;
        }
        if (beforeSelectorOk && afterSelectorOk)
        {
          if (set == null)
            set = new HashSet<Selector>(); 
          set.add(completeSelector); 
        }
      }
        
    }
    if (set == null)
      return Collections.emptySet();
    else
      return Collections.unmodifiableSet(set);
    
  }
  
}
