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
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;

import java.net.URL;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import javax.faces.application.Application;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.StringContentResourceLoader;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;

import org.apache.myfaces.trinidadinternal.share.nls.LocaleContextImpl;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

abstract public class TranslationsResourceLoader
                           extends StringContentResourceLoader
{
  /**
   * Constructs a dynamic resouce loader for this path which serves up
   * translations.
   * 
   * @param path the path of this dynamic resource loader
   */
  public TranslationsResourceLoader(String path)
  {
    super(path);
  }

  abstract protected String getJSVarName();

  abstract protected String getBundleName();

  /**
   * Override to increase the default size of the buffer.
   */
  protected int getDefaultSize()
  {
    return 10000;
  }

  protected String getLocaleString(FacesContext context)
  {
    Object localeObj = context.getExternalContext().getRequestParameterMap().
      get("loc");
    return (localeObj == null || "".equals(localeObj))
      ? null : localeObj.toString();
  }

  @Override
  protected String getContentType(String path)
  {
    return _CONTENT_TYPE;
  }
  
  @Override
  protected URL findResource(
    String path) throws IOException
  {
    return getURL(path);
  }

  @Override
  protected String getString(String path) throws IOException
  {
    FacesContext context = FacesContext.getCurrentInstance();
    String localeStr = getLocaleString(context);
    // Make sure it's in IANA format
    if (localeStr != null)
      localeStr = localeStr.replace('_', '-');

    Locale locale = LocaleUtils.getLocaleForIANAString(localeStr);
    if (locale == null)
      locale = Locale.getDefault();

    ResourceBundle bundle;
    try
    {
      bundle = _getResourceBundle(locale);
    }
    catch (MissingResourceException mre)
    {
      _LOG.severe("CANNOT_FIND_BUNDLE", getBundleName());
      _LOG.severe(mre);
      return "/* COULD NOT FIND BUNDLE " + getBundleName() + " */";
    }
    
    // see TRINIDAD-915
    // we load messages from application specific bundle 
    ResourceBundle applicationBundle = _getApplicationFacesMessageBundle(context, locale);

    // we put both, regular and application specific bundle to a map,
    // to avoid sending down duplicated key/value pairs
    Map<String, String> messages = new HashMap<String, String>();
    _addMessagesToMap(messages, bundle);
    
    if(applicationBundle != null)
      _addMessagesToMap(messages, applicationBundle, true);
    

    // FIXME: would be much better to directly stream the contents
    // rather than using StringContentResourceLoader
    StringBuilder builder = new StringBuilder(getDefaultSize());

    builder.append(getJSVarName())
      .append("=")
      .append("{\n");

    _processBundle(context, builder, messages, locale);

    builder.append("\n}");

    return builder.toString();
  }
  
  private ResourceBundle _getResourceBundle(Locale locale)
    throws MissingResourceException
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    return ResourceBundle.getBundle(getBundleName(),
                                    locale,
                                    loader);
  }

  private ResourceBundle _getApplicationFacesMessageBundle(
    FacesContext context,
    Locale locale)
  {
    assert context != null;
    assert locale  != null;

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
    
    ClassLoader loader = Thread.currentThread().getContextClassLoader();

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
  
  protected Skin getSkin(FacesContext context)
  {
    Skin skin = null;
    SkinFactory skinFactory = SkinFactory.getFactory();
    Object skinIdObj = context.getExternalContext().getRequestParameterMap().
      get("skinId");
    if (skinIdObj != null)
      skin = skinFactory.getSkin(context, skinIdObj.toString());

    return skin;
  }

  private void _addMessagesToMap(
    Map<String, String> map,
    ResourceBundle bundle,
    boolean onlyReplaceExisting)
  {
    Enumeration<String> keys = bundle.getKeys();
    while (keys.hasMoreElements())
    {
      String key = keys.nextElement();
      String value = null;
      if(onlyReplaceExisting)
      {
        // just add only those key/value pairs, that already 
        // were present in the original bundle, to not sent
        // down never used (custom) messages
        if(map.containsKey(key))
        {
          value = bundle.getString(key);
          map.put(key, value);
        }
      }
      else
      {
        value = bundle.getString(key);
        map.put(key, value);
      }
    }
  }
  
  private void _addMessagesToMap(
    Map<String, String> map,
    ResourceBundle bundle)
  {
    _addMessagesToMap(map, bundle, false);
  }
  
  private void _processBundle(
    FacesContext        context,
    StringBuilder       builder,
    Map<String, String> messages,
    Locale              locale)
  {
    Skin             skin = getSkin(context);
    LocaleContext    lc = new LocaleContextImpl(locale);

    // We get the keys from the bundle, but try to get the values from
    // the skin if possible
    Set<String> keys = messages.keySet();
    boolean writtenOne = false;
    for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();)
    {
      if (writtenOne)
        builder.append(",\n");
      else
        writtenOne = true;

      String key = iterator.next();
      String value;
      // If we can get it from the skin, that's better, but if not,
      // go to the bundle
      if (skin == null)
        value = messages.get(key);
      else
        value = skin.getTranslatedString(lc, key);
      
      builder.append("'");
      builder.append(key);
      builder.append("':'");
      _appendUnicodeString(builder, value);
      builder.append("'");
      
    }
  }

  private void _appendUnicodeString(
    StringBuilder builder,
    String        value)
  {
    if (value == null)
      return;

    int length = value.length();
    for (int i = 0; i < length; i++)
    {
      char c = value.charAt(i);
      if ((c >= 0x20) && (c < 0x80))
      {
        if (c == '\'')
          builder.append("\\'");
        else if (c == '\\')
          builder.append("\\\\");
        else
          builder.append(c);
      }
      else
      {
        // Unicode escape any non-ascii characters
        builder.append("\\u");
        String hex = Integer.toHexString(c);
        int hexLen = hex.length();
        // Javascript is lame, and requires padding Unicode escapes
        // to four-digits
        if (hexLen == 1)
          builder.append("000");
        else if (hexLen == 2)
          builder.append("00");
        else if (hexLen == 3)
          builder.append("0");
        builder.append(hex);
      }
    }
  }

  private static final String _CONTENT_TYPE = "text/javascript";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
                                       TranslationsResourceLoader.class);
}
