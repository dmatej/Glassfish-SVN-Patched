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
package org.apache.myfaces.trinidad.resource;

import java.lang.IllegalAccessException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Enumeration;
import java.util.ListResourceBundle;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.TestCase;
import junit.textui.TestRunner;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Class For testing MessageBundle
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/test/java/oracle/adf/view/faces/resource/MessageBundleTest.java#0 $) $Date: 14-oct-2005.14:49:17 $
 */
public class MessageBundleTest extends TestCase
{
  public MessageBundleTest(String testName)
  {
    super(testName);
  }

  public static void main(String[] args)
  {
    TestRunner.run(MessageBundleTest.class);
  }

  public void testBundles( /* String qualifiedBundleName */)
  {

    String qualifiedBundleName = "org.apache.myfaces.trinidad.resource.MessageBundle";
    ResourceBundle defBundle = _getDefaultBundle(qualifiedBundleName);
    BundleContext context
      = new BundleContext(qualifiedBundleName, null, defBundle);

    setUpAndValidateDefaultBundle(context);

    _testOtherBundles(qualifiedBundleName, locales);
  }

  private static void setUpAndValidateDefaultBundle(
    BundleContext  context)
  {
    ResourceBundle bundle = context.getLoadedBundle();
    _LOG.fine("Testing default bundle:{0}",context.getLoadedBundleName());
    Enumeration<String> en = bundle.getKeys();
    while (en.hasMoreElements())
    {
      String key = en.nextElement();
      Object value = bundle.getObject(key);

      assertNotNull(value);

      String valueStr = value.toString().trim();
      if (valueStr.length() == 0)
      {
        String errorMsg
         = "Error while testing bundle " + context.getBundleName() + "\n" +
           "check value for key " +
           key +  " " +
           "Null or zero length string is not allowed";
        _LOG.severe(errorMsg);
        fail(errorMsg);
      }

      _validateBundleValue(context, valueStr, key);

      _DEF_BUNDLE_PARAMS.put(key, _getPlaceHolders(value.toString()));
      _DEF_BUNDLE_KEYS.add(key);
    }
  }

  private static void _testOtherBundles(
    String   qualifiedBundleName,
    Locale[] locales)

  {
    for (int i = 0; i < locales.length; i++)
    {
      ResourceBundle bundle = _getBundle(qualifiedBundleName, locales[i]);
      BundleContext context
        = new BundleContext(qualifiedBundleName, locales[i], bundle);
      _LOG.fine("Testing bundle ", context.getBundleName());
      _validateBundle(context);
    }
  }

  // 1. Validate place holders
  // 2. Validate invalid charachter \uffffd
  // 3. Message warning if keys are not the same in bundle.
  private static void _validateBundle(
    BundleContext  context
    )
  {
    ResourceBundle bundle = context.getLoadedBundle();
    boolean isRequiredBundleLoaded
      = _validateBundleLocale(context, bundle.getLocale(), context.getLocale());

    if (isRequiredBundleLoaded)
    {
      Set<String> thisBundleKeys = new HashSet<String>();
      Enumeration<String> en = bundle.getKeys();
      while (en.hasMoreElements())
      {
        String key   = en.nextElement();
        thisBundleKeys.add(key);
        String value = bundle.getObject(key).toString();

        Set<String> params = _getPlaceHolders(value);

        if (_validateKey(context, key))
        {
          // only check parameters if the key is present in the 
          // english (default) bundle:
          _validateBundleParams(context, params, key);
        }
        _validateBundleValue(context, value, key);
      }
       _warnAbsenceOfBundleKeys(context, _getCurrentBundleKeys(bundle,
                                                               thisBundleKeys));
    }
  }

  // we have to pick up the bundle for the locale we specify and
  // we should not allow defaulting to its parent.
  // otherwise we  may miss to check for specific locale bundles
  private static boolean _validateBundleLocale(
    BundleContext context,
    Locale loadedBundleLocale,
    Locale expectedLocale
    )
  {
    boolean isValidBundleForLocale = loadedBundleLocale.equals(expectedLocale);
    if (!isValidBundleForLocale )
    {
      String errorMsg =
        "Expected bundle not loaded:\n" +
        " Loaded bundle is "   + context.getLoadedBundleName()  + "\n"  +
        " Expected bundle is " +  context.getBundleName()       + "\n";
        _LOG.warning(errorMsg);
      //fail(errorMsg);
    }
    return isValidBundleForLocale;
  }

  private static boolean _validateKey(
    BundleContext context,
    String        key)
  {
    if (!_DEF_BUNDLE_KEYS.contains(key))
    {
      String errorMsg = "Testing bundle "
                        + context.getBundleName() + "\n" +
                        "Key : " + key + "\n" +
                        "not available in default (English) bundle";
      _LOG.warning(errorMsg);
      // this error occurs whenever a key is present in a foreign bundle
      // but not present in the English (default) bundle.
      // That means that this error is triggered whenever we remove keys
      // or rename keys. This probably should not break the build:
      //fail(errorMsg);
      return false;
    }
    return true;
  }

  // just warns about missing keys in bundle and does not cause any failure.
  private static void _warnAbsenceOfBundleKeys(
    BundleContext context,
    Set<String>   bundleKeys)
  {
    if (!_isEqualsSets(bundleKeys, _DEF_BUNDLE_KEYS ))
    {
      // the variants - country specific bundle can override only required
      // keys of parent. So it is perfectly valid to have less keys than that
      // are already present in the parent. This check is just to warn in case
      // we would like to make sure we have all keys in the bundle.
      Set<String> temp = new HashSet<String>(_DEF_BUNDLE_KEYS);
      temp.removeAll(bundleKeys);

      _LOG.warning("Keys missing in bundle:{0} are \n{1}", 
                   new Object[] {context.getBundleName(), _getKeys(temp, '\n')});
    }
  }

  /**
   * Checks to see if parameters, like {0} and {1}, which are
   * used in the English text (in the default bundle) are present in the
   * translation (in the foreign bundle).
   * @param currentLocaleParams the set of parameters in the foreign translation 
   * @param key the key to use to get the English text from the default bundle.
   */
  private static void _validateBundleParams(
   BundleContext context,
   Set<String>   currentLocaleParams,
   String        key)
  {
    Set<String> defaultParams = _DEF_BUNDLE_PARAMS.get(key);
    if (_isEqualsSets(defaultParams, currentLocaleParams))
    return;
    else
    {
      String bundleName = context.getBundleName();
      String errorMsg =
        "Error while testing bundle " + context.getBundleName() +    "\n" +
        "Place holders mismatch in key " + key    +                  "\n" +
        "Default bundle params " + _getKeys(defaultParams, ' ')    + "\n" +
        bundleName  + " params " + _getKeys(currentLocaleParams, ' ');
      _LOG.severe(errorMsg);
      fail(errorMsg);
    }
  }

  private static void _validateBundleValue(
    BundleContext context,
    String        value,
    String        key
    )
  {
    char[] unicodeChars = value.toCharArray();
    for (int i = 0; i < unicodeChars.length; i++)
    {
      if (unicodeChars[i] == '\ufffd')
      {
         String errorMsg = "Conversion Error in bundle "     +
                           context.getBundleName()           +
                           " illegal value \\ufffd found \n" +
                           "for value " + value   +
                           " for key "   + key ;
         _LOG.severe(errorMsg);
         fail(errorMsg);
      }
    }
  }

  private static Set<String> _getCurrentBundleKeys(
    ResourceBundle bundle,
    Set<String>    currentBundleDefaultKeys
    )
  {
    if (bundle instanceof ListResourceBundle)
    {
      try
      {
        Method method  = bundle.getClass().getMethod("getContents");
        Object[][] keyValues = (Object[][])method.invoke(bundle);
        Set<String> currentBundleKeys = new HashSet<String>();
        for (int i = 0; i < keyValues.length; i++)
        {
          currentBundleKeys.add((String)keyValues[i][0]);
        }
        return currentBundleKeys;
      }
      catch (NoSuchMethodException e)
      {
        _LOG.finest(e);
        // do nothing return default one
      }
      catch (IllegalAccessException e)
      {
        _LOG.finest(e);
        // do nothing return default one
      }
      catch (InvocationTargetException e)
      {
        _LOG.finest(e);
        // do nothing return default one
      }
    }
    return currentBundleDefaultKeys;
  }

  private static boolean _isEqualsSets(Set<?> thisSet, Set<?> otherSet)
  {
    return thisSet.containsAll(otherSet);
  }

  // pick up place holder like {0}, {1} present in the value
  // part and add it to the set.
  private static Set<String> _getPlaceHolders(String value)
  {
    Set<String> params = new HashSet<String>(5);
    Matcher match = BundleContext.getPattern().matcher(value);

    while (match.find())
    {
      params.add(match.group());
    }
    return params;
  }

  private static ResourceBundle _getBundle(
    String qualifiedBundleName,
    Locale locale)
  {
    ResourceBundle bundle = ResourceBundle.getBundle(qualifiedBundleName,
                                                     locale);
    return bundle;
  }

  private static ResourceBundle _getDefaultBundle(String qualifiedBundleName)
  {
    ResourceBundle bundle = ResourceBundle.getBundle(qualifiedBundleName,
                                                     new Locale("",""));
    return bundle;
  }

  private static String _getKeys(
    Set<String> keys,
    char appendChar)
  {
    StringBuilder buff = new StringBuilder(64);
    for(String key : keys)
    {
      buff.append(key).append(appendChar);
    }
    
    return buff.toString();
  }

  // Holds the place holder set for the each key of the default bundle
  // so for each bundle we pick the value and check if the place holders
  // are same as in the default bundle. Any mismatch result in failure.
  private static final HashMap<String, Set<String>> _DEF_BUNDLE_PARAMS = 
    new HashMap<String, Set<String>>();

  private static final Set<String> _DEF_BUNDLE_KEYS = new HashSet<String>();

  private static final Locale[]
    locales = {
                new Locale("ar"),
                new Locale("cs"),
                new Locale("da"),
                Locale.GERMAN,        //de
                new Locale("el"),
                Locale.ENGLISH,       //en
                new Locale("es"),
                new Locale("fi"),
                Locale.FRENCH,        //fr
                new Locale("hu"),
                new Locale("it"),
                new Locale("iw"),
                Locale.JAPANESE,      //ja
                new Locale("ko"),
                new Locale("nl"),
                new Locale("no"),
                new Locale("pl"),
                new Locale("pt"),
                new Locale("pt","BR"),
                new Locale("ro"),
                new Locale("ru"),
                new Locale("sk"),
                new Locale("sv"),
                new Locale("th"),
                new Locale("tr"),
                new Locale("zh", "CN"),
                new Locale("zh", "TW"),
              };

  private static class BundleContext
  {
      BundleContext(
        String         qualifiedBundleName,
        Locale         locale,
        ResourceBundle loadedBundle)
      {
        _bundleName = qualifiedBundleName;
        this._locale = locale;
        _loadedBundle = loadedBundle;
      }

      public String getBundleName()
      {
        return _getBundleName(_locale) ;
      }

      public String getLoadedBundleName()
      {
        return _getBundleName(_loadedBundle.getLocale());
      }

      public static Pattern getPattern()
      {
        return _PATTERN;
      }

      public Locale getLocale()
      {
        return _locale;
      }

      private String _getBundleName(Locale locale)
      {
        int index = _bundleName.lastIndexOf(".");
        String bundleName = _bundleName.substring(index + 1);

        if (locale.getLanguage().length() == 0)
          return bundleName;

        bundleName +=   "_" +  locale.getLanguage();

        if (locale.getCountry().length() != 0)
          bundleName = bundleName  + "_" + locale.getCountry();
        return bundleName;
      }

      public ResourceBundle getLoadedBundle()
      {
        return _loadedBundle;
      }

      private String  _bundleName;
      private Locale  _locale;
      private ResourceBundle _loadedBundle;

      // precompiled pattern of expression {[0-9]}
      // escape { - since it is a meta character
      // \d is short form for [0-9]
      private static final Pattern _PATTERN = Pattern.compile("\\{\\d}");
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(MessageBundleTest.class);
}
