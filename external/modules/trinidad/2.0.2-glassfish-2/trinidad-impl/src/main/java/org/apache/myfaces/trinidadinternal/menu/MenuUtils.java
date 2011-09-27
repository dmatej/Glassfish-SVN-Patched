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
package org.apache.myfaces.trinidadinternal.menu;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ContainerUtils;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;

/**
 * Menu Utilities used by the Menu Model internal code.
 * All classes are package private.
 *
 */
class MenuUtils
{
  MenuUtils() {}

  //=======================================================================
  // String, Stringbuffer utilities
  //=======================================================================
  /**
   * Returns a String with the first occurrence of str1 replaced with str2
   * in fullstring
   * 
   * @param fullstring - The full string
   * @param str1 - String to be found in fullstring
   * @param str2 - String to replace the first occurrence of str1
   * @return String
   */
  static String stringReplaceFirst(String fullstring, String str1, String str2)
  {
    if (fullstring == null)
      return null;
    
    StringBuffer returnStr = 
      stringBufferReplaceFirst(new StringBuffer(fullstring), str1, str2);
    return returnStr.toString();
  }

  /**
   * Returns a StringBuffer with the first occurrence of str1 replaced with str2
   * in fullBuf
   * 
   * @param fullBuf - The full stringbuffer
   * @param str1 - String to be found in fullBuf
   * @param str2 - String to replace the first occurrence of str1
   * @return StringBuffer
   */
  static StringBuffer stringBufferReplaceFirst(StringBuffer fullBuf, String str1, 
                                               String str2)
  {
    if (fullBuf == null)
      return null;
    
    String fullstr = fullBuf.toString();
    
    // All Cases that just return fullBuf
    if (str1 == null || str2 == null)
      return fullBuf;
    if ("".equals(fullstr) && !"".equals(str1))
      return fullBuf;

    // if the string being replaced is not found, return    
    int startIdx = fullBuf.indexOf(str1);
    if (startIdx == -1)
      return fullBuf;
    
    // We are ok, now go ahead
    int foundLen = str1.length();
    int endIdx = startIdx + foundLen;
    
    StringBuffer returnBuf = fullBuf.replace(startIdx, endIdx, str2);
    return returnBuf;
  }

  //=======================================================================
  // Bound Value/EL Expression utilities
  //=======================================================================
  
  /**
   * Gets the bound value of an EL expression
   * 
   * @param elExpression - String representing an EL expression
   */
  static <T> T getBoundValue(String elExpression, Class<T> desiredClass)
  {
    try
    {
      if (desiredClass == null)
        throw new NullPointerException();

      FacesContext ctx = FacesContext.getCurrentInstance();
      return (T) ctx.getApplication().evaluateExpressionGet(ctx,
                                                            elExpression,
                                                            desiredClass);
    }
    catch (Exception ex)
    {
      _LOG.severe("EL Expression " + elExpression + 
                  " is invalid or returned a bad value.\n", ex);
      _LOG.severe(ex);
      return null;
    }
  }
  
  /**
   * Evaluate an attribute value string representing a boolean value 
   * and return its boolean value.  There are 3 possible valid values for
   * boolStr:
   * o EL Expression (that returns a boolean)
   * o the string "true"
   * o the string "false"
   * 
   * In the case of another string being passed in, the default value
   * of the attribute is returned.
   * 
   * @param boolStr - String to be evaluated into a boolean.
   * @param defaultVal - The default boolean value to be returned in the case
   *                     where and invalid boolstr is passed in.
   * @return boolean value equivalent of boolStr
   */
  static boolean evalBoolean (String boolStr, boolean defaultVal)
  {
    if (   boolStr != null 
        && ContainerUtils.isValueReference(boolStr)
       )
    {
      Boolean bValue = getBoundValue(boolStr, Boolean.class);
      return bValue.booleanValue();
    }
    else
    {    
      if ("true".equals(boolStr) || "false".equals(boolStr))
        return (Boolean.valueOf(boolStr)).booleanValue();
      else
        return defaultVal;
    }
  }

  /**
   * Evaluate a string representing an EL expression.
   * 
   * @param propVal - string to be evaluated.
   * @return the string bound value of the EL Expression
   */
  static String evalString(String propVal)
  {
    if (   propVal != null 
        && ContainerUtils.isValueReference(propVal)
       )
    {
      String elVal = getBoundValue(propVal, String.class);
      return elVal;
    }
    return propVal;
  }

  /**
   * Evaluate a string representing an EL expression.
   * 
   * @param propVal - string to be evaluated.
   * @return the int bound value of the EL Expression
   */
  static int evalInt(String propVal)
  {
    if (   propVal != null 
        && ContainerUtils.isValueReference(propVal)
       )
    {
      Integer elVal = getBoundValue(propVal, Integer.class);
      return elVal.intValue();
    }
    return Integer.parseInt(propVal);
  }
  
  /**
   * Create a ResourceBundle and put it on the Session map.
   * 
   * @param resBundle - String containing name of class containing the resource
   *                    bundle.
   * @param key - ThreadLocal key for the resource bundle being put on the
   *              requestMap
   */
  @SuppressWarnings("unchecked")
  static void loadBundle(String resBundle, ThreadLocal<String> key)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> applicationMap = 
      facesContext.getExternalContext().getApplicationMap();

    // Get the view root locale
    Locale requestLocale = facesContext.getViewRoot().getLocale();

    // Make sure it is not null
    if (requestLocale == null)
    {
      requestLocale = facesContext.getApplication().getDefaultLocale();
    }
    
    // Is there a bundle with this key already on the session map?
    _BundleMap bundleMap = (_BundleMap) applicationMap.get(key.get());
    
    // if so, get its locale.  If the locale has not 
    // changed, just return, i.e. use the existing bundle
    if (bundleMap != null)
    {
      Locale bundleLocale = bundleMap.getLocale();
      
      if (bundleLocale == null)
      {
        ResourceBundle rb = bundleMap.getBundle();
        bundleLocale = rb.getLocale();
      }
      
      if (requestLocale == bundleLocale)
      {
        // the bundle on the applicationMap is ok so just return
        return;
      }
    }

    String bundleName = null;

    if (resBundle != null) 
    {
      // if _bundleName is an EL, then get its value
      if (ContainerUtils.isValueReference(resBundle)) 
      {
        bundleName = MenuUtils.getBoundValue(resBundle, String.class);
      } 
      else
      {
        bundleName = resBundle ;
      }
    }

    final ResourceBundle bundle;
    
    try
    {
      bundle = ResourceBundle.getBundle(bundleName, requestLocale);
    }
    catch (MissingResourceException e)
    {
      _LOG.severe("RESOURCE_BUNDLE_NOT_FOUND", bundleName);
      _LOG.severe(e);
      return;
    }
 
    // Put the bundle in the map.  At this point the key is 
    // unique because of the handler Id we inserted when loadBundle
    // was called.
    applicationMap.put(key.get(), new _BundleMap(bundle, requestLocale));
  }

  /**
   * Create a ResourceBundle and put it on the Session map.
   * The key is made into a ThreadLocal to ensure that this the resource
   * bundle is threadsafe.
   * 
   * @param resBundleName - String containing name of class containing the 
   *                        resource bundle.
   * @param resBundleKey - String key for the resource bundle being put on the
   *                       requestMap
   */
  @SuppressWarnings("unchecked")
  static void loadBundle(String resBundleName, String resBundleKey)
  {
    ThreadLocal<String> bundleKey = ThreadLocalUtils.newRequestThreadLocal();
    
    bundleKey.set(resBundleKey);    
    loadBundle(resBundleName, bundleKey);
  }
  
  /**
   * Map containing a resource bundle and its key that is placed 
   * on the http request map.  This resource bundle is used by the menu
   * model metadata to externalize strings, such as tab labels, for 
   * translation.
   */
  @SuppressWarnings("unchecked")
  static private class _BundleMap implements Map<String, String>
  {
    private ResourceBundle _bundle;
    private Locale _locale;
    private List<String> _values;

    public _BundleMap(ResourceBundle bundle)
    {
      _bundle = bundle;
      _locale = bundle.getLocale();
    }

    public _BundleMap(ResourceBundle bundle, Locale locale)
    {
      _bundle = bundle;
      _locale = locale;
    }

    //Optimized methods
    public String get(Object key)
    {
      try 
      {
        return _bundle.getString(key.toString());
      } 
      catch (Exception e) 
      {
        return "!!!" + key + "!!!";
      }
    }

    public boolean isEmpty()
    {
      return !_bundle.getKeys().hasMoreElements();
    }

    public boolean containsKey(Object key)
    {
      return _bundle.getObject(key.toString()) != null;
    }

    //Unoptimized methods
    public Collection<String> values()
    {
        if (_values == null)
        {
          _values = new ArrayList<String>();
          for (Enumeration<String> enumer = _bundle.getKeys(); 
               enumer.hasMoreElements(); )
          {
            String v = _bundle.getString(enumer.nextElement());
            _values.add(v);
          }
        }
        return _values;
    }

    public int size()
    {
      return values().size();
    }

    public boolean containsValue(Object value)
    {
      return values().contains(value);
    }

    public Set<Map.Entry<String, String>> entrySet()
    {
      Set<Map.Entry<String, String>> set = new HashSet<Map.Entry<String, String>>();
      
      for (Enumeration<String> enumer = _bundle.getKeys(); enumer.hasMoreElements(); )
      {
        final String k = enumer.nextElement();
        set.add(new Map.Entry<String, String>()
        {
          public String getKey()
          {
            return k;
          }

          public String getValue()
          {
            return _bundle.getString(k);
          }

          public String setValue(String value)
          {
            throw new UnsupportedOperationException();
          }
        });
      }
      return set;
    }

    public Set<String> keySet()
    {
      Set<String> set = new HashSet<String>();
      for (Enumeration<String> enumer = _bundle.getKeys(); enumer.hasMoreElements(); )
      {
        set.add(enumer.nextElement());
      }
      return set;
    }

    //Unsupported methods
    public String remove(Object key)
    {
      throw new UnsupportedOperationException();
    }

    public void putAll(Map<? extends String, ? extends String> t)
    {
      throw new UnsupportedOperationException();
    }

    public String put(String key, String value)
    {
      throw new UnsupportedOperationException();
    }

    public void clear()
    {
      throw new UnsupportedOperationException();
    }
    
    public ResourceBundle getBundle()
    {
      return _bundle;
    }

    public Locale getLocale()
    {
      return _locale;
    }
  }  // endclass _BundleMap

  private final static TrinidadLogger _LOG = 
                        TrinidadLogger.createTrinidadLogger(MenuUtils.class);
}





