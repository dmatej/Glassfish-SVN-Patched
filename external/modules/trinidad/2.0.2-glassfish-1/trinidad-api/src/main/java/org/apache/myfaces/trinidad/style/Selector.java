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

/**
 * A Selector object holds a CSS selector. To create a new Selector, call the
 * static method Selector.createSelector(String selectorString).
 * This class makes the Styles Object APIs clearer,
 * since we have Map&lt;Selector, Style> now instead of Map&lt;String, Style>.
 * Also with this object we'll have the API in place in case we need to
 * hang methods off of this object (like
 * possibily reordering the pseudo-classes in alphabetical order when creating
 * a Selector object so that af|foo:bar:zoo and af|foo:zoo:bar are equal).
 * It was originally thought that we'd add a getNativeSelectorString method
 * here, but we decided to not add it here to keep a better separation of Selectors
 * and the maps that convert the Selectors to the native selector string.
 * @see Styles#getNativeSelectorString(org.apache.myfaces.trinidad.style.Selector) ;
 */
final public class Selector
{

  /**
   * Given a String that represents the selector, return a Selector object
   * @param selectorString
   * @return a Selector object
   * @throws IllegalArgumentException if selectorString is null or the empty String.
   */
  public static Selector createSelector(String selectorString)
  {
    if (selectorString == null || selectorString.equals(""))
      throw new IllegalArgumentException("selectorString must be non null and non empty.");
      
    return new Selector(selectorString);
  }

  // toString
  @Override
  public String toString()
  {
    return _selectorString;
  }
  
  @Override
  public int hashCode()
  {
    // delegate to the String's hashCode();
    return _selectorString.hashCode();
  }
  
  @Override
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (obj == null || !(obj instanceof Selector))
      return false;
      
    // obj at this point must be a Selector
    Selector test = (Selector)obj;

    return (_selectorString.equals(test._selectorString));
  }

  
  // Do not call directly. Call from Selector.createSelector
  private Selector(String selectorString)
  {
    _selectorString = selectorString;
  }

  
  private final String _selectorString;
  
    
}
