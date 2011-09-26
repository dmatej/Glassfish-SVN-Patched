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
package org.apache.myfaces.trinidadinternal.util.nls;

import java.util.Locale;

import javax.faces.context.FacesContext;

import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Utility class dealing with Locale-related issues, including
 * common direction and alignment constants.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/nls/LocaleUtils.java#0 $) $Date: 10-nov-2005.18:49:13 $
 */
public final class LocaleUtils
{


  /**
   * Reading direction constant
   */
  public static final int DIRECTION_DEFAULT     = 0;

  /**
   * Reading direction constant
   */
  public static final int DIRECTION_LEFTTORIGHT = 1;

  /**
   * Reading direction constant
   */
  public static final int DIRECTION_RIGHTTOLEFT = 2;

  
  /**
   * Conversion function to go from LocaleContext to the obsolete
   * reading direction API.
   */
  public static int getReadingDirection(LocaleContext localeContext)
  {
    return localeContext.isRightToLeft()
             ? DIRECTION_RIGHTTOLEFT
             : DIRECTION_LEFTTORIGHT;
  }


  /**
   * Given a locale, returns the default reading direction.
   */
  public static int getReadingDirectionForLocale(
    Locale loc
    )
  {
    if (loc == null)
    {
      loc = Locale.getDefault();
    }

    String language = loc.getLanguage();

    // arabic and hebrew are right-to-left languages.  We treat "iw" as
    // hebrew because "iw" was the old code for Hebrew and the JDK
    // still uses it
    if (language.equals("ar") ||
        language.equals("he") ||
        language.equals("iw"))
    {
      return DIRECTION_RIGHTTOLEFT;
    }
    else
    {
      return DIRECTION_LEFTTORIGHT;
    }
  }


  /**
   * Decodes an IANA string (e.g., en-us) into a Locale object.
   */
  public static Locale getLocaleForIANAString(String ianaString)
  {
    if ((ianaString == null) || "".equals(ianaString))
      return null;

    String language;
    String country = "";
    String variant = "";

    int dashIndex = ianaString.indexOf('-');
    if (dashIndex < 0)
    {
      language = ianaString;
    }
    else
    {
      language = ianaString.substring(0, dashIndex);
      int start = dashIndex + 1;
      dashIndex = ianaString.indexOf('-', start);
      if (dashIndex < 0)
      {
        country = ianaString.substring(start);
      }
      else
      {
        country = ianaString.substring(start, dashIndex);
        variant = ianaString.substring(dashIndex + 1);
      }
    }

    /*
     * Validate the rules for Locale per its Javadoc:
     * - The language argument is a valid ISO Language Code.
     *   These codes are the lower-case, two-letter codes as defined by ISO-639.
     * - The country argument is a valid ISO Country Code. These
     *   codes are the upper-case, two-letter codes as defined by ISO-3166.
     *   
     *   Rather than checking a list, we check the length and case and ignore
     *   the arguments which fail to meet those criteria (use defaults instead).
     */
    if (language.length() != 2)
    {
      language = "";
      _LOG.warning("INVALID_LOCALE_LANG_LENGTH", ianaString);
    }
    else
    {
      if (Character.isUpperCase(language.charAt(0)) ||
         Character.isUpperCase(language.charAt(1)))
      {
        language = "";
        _LOG.warning("INVALID_LOCALE_LANG_CASE", ianaString);
      }
    }
    if (language.length() == 0)
    {
      FacesContext fc = FacesContext.getCurrentInstance();
      if (fc.getViewRoot() != null)
      {
        return (fc.getViewRoot().getLocale());
      }
      else
      {
        // ViewRoot may be null, e.g. when TranslationsResourceLoader is called 
        // from the ResourceServlet. 
        //
        // Cast to a servletRequest here because this should only happen
        // when run from the Trinidad ResourceServlet.  Even in a portlet
        // environment we should have a servlet request in this circumstance.    
        // If we need to handle getting the locale from portlet objects, we
        // should add a utility to do so in ExternalContextUtils.
        ServletRequest req = (ServletRequest)
                             fc.getExternalContext().getRequest();
        return (req.getLocale());         
      }
    }

    if (country.length() > 0)
    {
      if (country.length() != 2)
      {
        country = "";
        _LOG.warning("INVALID_LOCALE_COUNTRY_LENGTH", ianaString);
      }
      else
      {
        if (Character.isLowerCase(country.charAt(0)) ||
            Character.isLowerCase(country.charAt(1)))
        {
          country = "";
          _LOG.warning("INVALID_LOCALE_COUNTRY_CASE", ianaString);
        }
      }
    }

    if (variant.indexOf('/') > 0)
    {
      // Disallow slashes in the variant to avoid XSS
      variant = "";
      _LOG.warning("INVALID_LOCALE_VARIANT_HAS_SLASH", ianaString);
    }



    return new Locale(language, country, variant);
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(LocaleUtils.class);

}
