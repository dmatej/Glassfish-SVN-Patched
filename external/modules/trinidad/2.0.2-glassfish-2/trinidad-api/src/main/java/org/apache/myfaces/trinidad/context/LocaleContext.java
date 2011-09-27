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
package org.apache.myfaces.trinidad.context;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TimeZone;

/**
 * Context for locale-specific operations and properties available
 * during rendering.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/nls/LocaleContext.java#0 $) $Date: 10-nov-2005.19:00:03 $
 */
abstract public class LocaleContext
{
  protected LocaleContext()
  {
  }

  /**
   * Returns the locale that should be used for formatting.
   */
  abstract public Locale getFormattingLocale();

  /**
   * Returns the formatting Locale in IANA String format.
   */
  abstract public String getFormattingIANALocaleString();


  /**
   * Returns the locale that should be used for formatting.
   * @deprecated use getFormattingLocale()
   */
  @Deprecated
  public Locale getLocale()
  {
    return getFormattingLocale();
  }

  /**
   * Returns the Locale in IANA String format.
   * @deprecated use getFormattingIANALocaleString()
   */
  @Deprecated
  public String getIANALocaleString()
  {
    return getFormattingIANALocaleString();
  }



  /**
   * Returns the locale that should be used for translations..
   */
  abstract public Locale getTranslationLocale();


  /**
   * Returns the translation Locale in IANA String format.
   */
  abstract public String getTranslationIANALocaleString();

  abstract public boolean isRightToLeft();

  /**
   * Returns the TimeZone of the application, as specified in 
   * trinidad-config.xml. If unspecified, defaults to the server timezone.
   */
  abstract public TimeZone getTimeZone();

  /**
   * Returns the resource bundle with the specified name, for this
   * <strong>translation</strong> locale.
   * <p>
   * As the LocaleContext maintains a cache of found ResourceBundles,
   * this is much faster than using
   * <code>ResourceBundle.getBundle</code>
   * <p>
   * @see java.util.ResourceBundle#getBundle
   */
  abstract public ResourceBundle getBundle(
    String baseBundleName) throws MissingResourceException;

  /**
   * Returns the year offset for parsing years with only two digits.
   */
  public abstract int getTwoDigitYearStart();

  /**
   * Returns the character used to separate number groups.
   * If zero (NUL), the default separator for the Locale
   * will be used.
   */
  public abstract char getGroupingSeparator();

  /**
   * Returns the character used as a decimal separator.
   * If zero (NUL), the default separator for the Locale
   * will be used.
   */
  public abstract char getDecimalSeparator();
}
