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
package org.apache.myfaces.trinidadinternal.el;

import javax.faces.context.FacesContext;

import java.util.concurrent.ConcurrentHashMap;

import java.util.Locale;

/**
 * <p>
 * OracleHelpProvider is a HelpProvider implementation for Oracle Help
 * for the Web.  Oracle Help for the Web (OHW) is a full-featured,
 * context-sensitive help system for web applications implemented
 * as a Java servlet using the UIX Framework.
 *
 * Users of this class provide the location of their OHW Servlet
 * instance (populated with the help for their application).  The
 * OracleHelpProvider implementation of getHelpTopicURL() returns
 * a URL to the OHW instance with parameters requesting the appropriate
 * topic-id.  Similarly, the OracleHelpProvider implementation of
 * getHelpSystemURL() returns a URL to the OHW instance with
 * a parameter requesting that OHW return the appropriate help
 * system page for the given HelpProvider key constant.
 *
 * Users may optionally register additional OHW Servlet instances
 * for specific locales.  See the registerLocaleSpecificServlet
 * method for details.
 *
 * For more information on using a HelpProvider see the HelpProvider
 * abstract class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/el/OracleHelpProvider.java#0 $) $Date: 10-nov-2005.19:06:18 $
 */
public class OracleHelpProvider extends SecondaryWindowHelpProvider
{
  public OracleHelpProvider(String ohwServletLocation)
  {
    if (ohwServletLocation != null)
    {
      //JRS -- because of OHW's current requirements we require
      //a single ending slash.  Originally, the OHP was removing
      //the ending slash, and for a workaround, users were using
      //a double slash.  So as not to break those people by fixing
      //this bug, we explicitly test for a double slash and reduce
      //it to a single slash.
      if (ohwServletLocation.endsWith("//"))
      {
        ohwServletLocation =
          ohwServletLocation.substring(0, ohwServletLocation.length() - 1);
      }
    }

    _defaultOHWServletLocation = ohwServletLocation;
    _localeSpecificServlets = new ConcurrentHashMap<Locale, String>(13);
  }

  /**
   * Register a different OHW servlet instance for a given
   * locale.  When a request comes in from the end user's
   * browser with a specific locale, the OracleHelpProvider
   * will attempt to find a HelpProvider registered for that
   * Locale.  If a match for language, country, and variant
   * cannot be found, it checks if a Locale has been registered
   * with matching language and country.  If a language and country
   * match cannot be found,it checks if a Locale has been registred
   * for just the language.  If no match can be found, it uses the
   * default OHW servlet instance registered upon creation to handle
   * the request.
   * <p>
   * @param locale the Locale for which this OHW instance should be used
   * @param localeSpecificServletLocation the location of the ohw servlet
   */
  public void registerLocaleSpecificServlet(Locale locale,
                                            String localeSpecificServletLocation)
  {
    if ((locale != null) && (localeSpecificServletLocation != null))
    {
      _localeSpecificServlets.put(locale, localeSpecificServletLocation);
    }
  }

  /**
   * The getHelpTopicURL() method should return a string URL
   * for the given key string (topic-id)
   * <p>
   * @param key criterion (topic-id)
   */
  @Override
  protected String getHelpTopicURL(Object key)
  {
    String helpURL = null;
    if (key != null)
    {
      FacesContext context = FacesContext.getCurrentInstance();
      Locale locale = context.getViewRoot().getLocale();
      String servletLocation = _getLocaleSpecificServlet(context);
      boolean hasQueryParams = servletLocation.indexOf('?') >= 0;
      helpURL = (servletLocation + (hasQueryParams ? "&" : "?") +
                 _TOPIC_PARAM + "=" + key.toString() + "&" +
                 _LOCALE_PARAM + "=" + locale.toString());
    }

    return helpURL;
  }

  /**
   * The getHelpSystemURL() method should return a string URL
   * for the given key string (HelpProvider key constant)
   * <p>
   * @param key criterion (HelpProvider key constant)
   */
  @Override
  protected String getHelpSystemURL(Object key)
  {
    if (HelpProvider.FRONT_PAGE_KEY.equals(key))
    {
      FacesContext context = FacesContext.getCurrentInstance();
      Locale locale = context.getViewRoot().getLocale();
      String servletLocation = _getLocaleSpecificServlet(context);
      if (servletLocation != null)
      {
        boolean hasQueryParams = servletLocation.indexOf('?') >= 0;
        servletLocation  = servletLocation + (hasQueryParams ? "&" : "?") +
             _LOCALE_PARAM + "=" + locale.toString();
      }

      return servletLocation;
    }

    return null;
  }


  /**
   * Determines which of the registered OHW servlet
   * locations is appropriate for the current request
   * using an algorithm similar to resource bundle loading
   * <p>
   * @param context the current Faces context
   */
  private String _getLocaleSpecificServlet(FacesContext context)
  {
    String servletLocation = _defaultOHWServletLocation;
    Locale desiredLocale = context.getViewRoot().getLocale();
    Locale matchingLocale = null;


    //Check if we have a direct match, or a cached result, if
    //not then check language/country and lang only locales
    if (_localeSpecificServlets.containsKey(desiredLocale))
    {
      servletLocation = _localeSpecificServlets.get(desiredLocale);
    }
    else
    {
      Locale langCountryOnly = new Locale(desiredLocale.getLanguage(),
                                          desiredLocale.getCountry());
      if (_localeSpecificServlets.containsKey(langCountryOnly))
      {
         matchingLocale = langCountryOnly;
      }
      else
      {
        Locale langOnly = new Locale(desiredLocale.getLanguage(), "");
        if (_localeSpecificServlets.containsKey(langOnly))
        {
          matchingLocale = langOnly;
        }
      }

      if (matchingLocale != null)
      {
        servletLocation = _localeSpecificServlets.get(matchingLocale);
      }

      //cache the result of the search
      if (servletLocation != null)
        _localeSpecificServlets.put(desiredLocale, servletLocation);
    }

    return servletLocation;
  }

  private static final String _TOPIC_PARAM = "topic";
  private static final String _LOCALE_PARAM = "locale";
  private String _defaultOHWServletLocation = null;
  private ConcurrentHashMap<Locale, String> _localeSpecificServlets = null;
}

