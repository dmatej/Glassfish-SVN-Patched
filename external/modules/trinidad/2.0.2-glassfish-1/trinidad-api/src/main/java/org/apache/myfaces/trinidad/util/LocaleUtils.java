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
package org.apache.myfaces.trinidad.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Used for getting localized strings for Validators and Converters from the
 * Resource Bundle.&nbsp; First checks for strings in application specific
 * bundle.&nbsp; If not found checks for the resource in faces bundle.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/LocaleUtils.java#0 $) $Date: 10-nov-2005.19:08:38 $
 */
final class LocaleUtils
{
  private LocaleUtils()
  {
  }

  // Looks for the key and key_detail values in the resource bundle and
  // returns it after formatting it with the parameters in the place holders
  // of the obtained value for the given key.
  static ErrorMessages __getErrorMessages(
    FacesContext context,
    String resourceId
    )
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, resourceId);
    String summary = info.getSummary();
    if (summary == null)
    {
      summary = "???" + resourceId + "???";
    }
    ResourceBundle bundle  = info.getBundle();
    if (null == bundle)
    {
      throw new NullPointerException(_LOG.getMessage(
        "BUNDLE_NOT_FOUND"));
    }

    // Look up for key_detail now
    String detailKey = _getDetailKey(resourceId);
    String detail = _getBundleString(bundle, detailKey);
    if (detail == null)
    {
      detail = "???" + resourceId + "_detail???";
    }

    return new ErrorMessages(summary, detail);
  }

  static String __getSummaryString(
    FacesContext context,
    String messageId)
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, messageId);
    return info.getSummary();
  }

  static String __getDetailString(
    FacesContext context,
    String messageId)
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, messageId);
    ResourceBundle bundle  = info.getBundle();
    if (null == bundle)
    {
      throw new NullPointerException(_LOG.getMessage(
        "BUNDLE_NOT_FOUND"));
    }

    // Look up for key_detail now
    String detailKey = _getDetailKey(messageId);
    return _getBundleString(bundle, detailKey);

  }

  private static ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = ClassLoader.getSystemClassLoader();

    return loader;
  }

  private static String _getDetailKey(
    String messageId)
  {
    return messageId + "_detail";
  }

  private static BundleSummaryInfo _identifyBundleSummaryInfo(
    FacesContext context,
    String resourceId
    )
  {
    _assertContextNotNull(context);

    if (resourceId == null)
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_RESOURCEID"));
    }

    Locale locale = _getLocale(context);
    ClassLoader loader = _getClassLoader();

    // First we look in the application specific bundle
    ResourceBundle bundle = _getApplicationFacesMessageBundle(context, locale, loader);
    BundleSummaryInfo summary = _getBundleSummaryInfo(bundle, resourceId);
    if(summary == null)
    {
      // The application did not override the resource, let look in the 
      // private Trinidad bundle
      bundle = _getTrinidadMessageBundle(locale, loader);
      summary = _getBundleSummaryInfo(bundle, resourceId);
      
      if(summary == null)
      {
        // The internal bundle does not know of the requested resource either,
        // let check the default JSF IMPL message bundle
        bundle = _getDefaultFacesMessageBundle(locale, loader);
        if(bundle == null)
        {
          // That bundle is from the spec, it should never be null
          throw new NullPointerException(_LOG.getMessage(
            "CANNOT_FIND_DEFAULT_FACESMESSAGE"));
        }
        
        summary = _getBundleSummaryInfo(bundle, resourceId);
        if(summary == null)
        {
          summary = new BundleSummaryInfo(null, "???" + resourceId + "???");
        }
      }
    }
    
    return summary;
  }

  /**
   * Return the bundle for the appropriate locale.&nbsp;First checks for the
   * ADF Faces application level Message bundle in the cache, if not found
   * looks up for faces bundle.&nbsp; Once the bundle is found it is
   * cached for faster retrieval in subsequent calls.
   *
   * @param locale Locale for which the bundle is to be indentifed
   * @param loader ClassLoader to pickup the bundle
   * @return Resource bundle for the given locale.
   */
  private static ResourceBundle _getTrinidadMessageBundle(
    Locale locale,
    ClassLoader loader
    )
  {
    ResourceBundle bundle = _getCachedBundle(locale);

    // if not available in cache
    if (null == bundle)
    {
      try
      {
        bundle = ResourceBundle.getBundle(_APACHE_TRINIDAD_MESSAGE_BUNDLE,
                                          locale, loader);
        // let us cache the found bundle
        _cacheBundle(locale, bundle);
      }
      catch (MissingResourceException missingResource)
      {
        _LOG.severe("UNABLE_LOAD_MESSAGE_BUNDLE",_APACHE_TRINIDAD_MESSAGE_BUNDLE);
        _LOG.severe(missingResource);
      }
    }

    return bundle;
  }
  
  private static BundleSummaryInfo _getBundleSummaryInfo(
    ResourceBundle bundle,
    String resourceId)
  {
    assert resourceId != null;
    
    if(bundle != null)
    {
      // The bundle exists
      String summary = _getBundleString(bundle, resourceId);
      if(summary != null)
      {
        // The resource exists
        return new BundleSummaryInfo(bundle, summary);
      }
    }
    
    return null;
  }

  private static ResourceBundle _getApplicationFacesMessageBundle(
    FacesContext context,
    Locale locale,
    ClassLoader loader
    )
  {
    assert context != null;
    assert locale  != null;
    assert loader  != null;
    
    Application application = context.getApplication();
    if(application == null)
    {
      // Should not happen, but better check than a NullPointerException
      return null;
    }
    
    String bundleName = application.getMessageBundle();
    if(bundleName == null)
    {
      // There's no specified message bundle in faces-config.xml
      return null;
    }
    
    try
    {
      return ResourceBundle.getBundle(bundleName, locale, loader);
    }
    catch (MissingResourceException missingResource)
    {
      _LOG.warning("Unable to load faces-config.xml defined message bundle {0}", bundleName);
      _LOG.warning(missingResource);
      return null;
    }
  }

  private static ResourceBundle _getDefaultFacesMessageBundle(
    Locale locale,
    ClassLoader loader
    )
  {
    ResourceBundle bundle = null;
    try
    {
      bundle =
        ResourceBundle.getBundle(FacesMessage.FACES_MESSAGES, locale, loader);
    }
    catch (MissingResourceException missingResource)
    {
      _LOG.severe("UNABLE_LOAD_FACES_BUNDLE", FacesMessage.FACES_MESSAGES);
      _LOG.severe(missingResource);
    }
    return bundle;
  }

  private static Locale _getLocale(FacesContext context)
  {
    Locale locale = null;
    if (context.getViewRoot() != null)
      locale = context.getViewRoot().getLocale();

    if (locale == null)
      locale = Locale.getDefault();

    return locale;
  }

  private static ResourceBundle _getCachedBundle(Locale locale)
  {
    return _bundleCache.get(locale);
  }

  private static void _cacheBundle(Locale locale, ResourceBundle bundle)
  {
    _bundleCache.put(locale, bundle);
  }

  private static void _assertContextNotNull(FacesContext context)
  {
    if (null == context)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_FACESCONTEXT"));
  }

  /**
   * @param bundle Bundle in which translated string is to be found for given key
   * @param key
   * @return
   */
  private static String _getBundleString(ResourceBundle bundle, String key)
  {
    try
    {
      Object localeStr = bundle.getObject(key);
      return localeStr == null ? null : localeStr.toString();
    }
    catch (MissingResourceException mre)
    {
      _LOG.finer("Key {0} not found in {1}", new Object[]{key, bundle});
      return null;
    }
  }


  // Encapuslation which stores the identified bundle and the summary message.
  private static class BundleSummaryInfo
  {
    BundleSummaryInfo(ResourceBundle bundle, String summary)
    {
      _bundle  = bundle;
      _summary = summary;
    }

    public ResourceBundle getBundle()
    {
      return _bundle;
    }

    public String getSummary()
    {
      return _summary;
    }

    private ResourceBundle _bundle;
    private String  _summary;
  }


  private static final String _APACHE_TRINIDAD_MESSAGE_BUNDLE
    = "org.apache.myfaces.trinidad.resource.MessageBundle";

  // cache Bundles based on locale
  private static  Map<Locale, ResourceBundle>  _bundleCache;

  static
  {
    _bundleCache = Collections.synchronizedMap(new HashMap<Locale, ResourceBundle>(13));
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(LocaleUtils.class);
}
