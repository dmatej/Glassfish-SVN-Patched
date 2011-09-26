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
package org.apache.myfaces.trinidadinternal.share.nls;

import java.util.HashMap;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TimeZone;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Context for locale-specific operations and properties.  All of the properties
 * should initially default to those of the base Locale, while allowing
 * the locale-specific properties to be overridden.
 * <p>
 * It is expected that additional properties will be added to this class
 * over time in order to support overriding the date and number formats.
 * <p>
 * Clients should never subclass this class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/nls/LocaleContext.java#0 $) $Date: 10-nov-2005.19:00:03 $
 */
public class LocaleContextImpl extends LocaleContext
{
  /**
   * Creates a LocaleContext based off of the specified Locale.
   */
  public LocaleContextImpl(
    Locale locale
    )
  {
    this(locale, locale);
  }


  /**
   * Creates a LocaleContext based off of the specified Locale and using
   * a different Locale for translations.  Applications that only provide
   * translations for a subset of the Locales provided by subcomponents
   * can use the translation Locale to force subcomponents to only
   * use translations in a language supported by the application.
   * <p>
   * @param formattingLocale Locale providing default formatting
   *    behavior for the LocaleContext.
   *                   If not specified, the defualt Locale is used.
   * @param translationLocale Locale to use for translations.  If not
   *                          specified, the formattingLocale is used.
   */
  public LocaleContextImpl(
    Locale formattingLocale,
    Locale translationLocale
    )
  {
    if (!getClass().getName().startsWith("org.apache.myfaces.trinidadinternal.share.nls."))
      throw new IllegalStateException(_LOG.getMessage(
        "USER_DEFINED_SUBCLASSES_NOT_SUPOORTED"));

    if (formattingLocale == null)
    {
      formattingLocale = Locale.getDefault();
    }

    _formattingLocale = formattingLocale;

    // default the translation locale to the baseLocale
    if (translationLocale == null)
      translationLocale = formattingLocale;

    _transLocale = translationLocale;

    setTimeZone(null);
  }

  /**
   * Returns the locale that should be used for translations.
   */
  @Override
  public Locale getTranslationLocale()
  {
    return _transLocale;
  }

  /**
   * Returns the locale that should be used for formatting.
   */
  @Override
  public Locale getFormattingLocale()
  {
    return _formattingLocale;
  }


  /**
   * Returns the Locale in IANA String format.
   */
  @Override
  public String getFormattingIANALocaleString()
  {
    if (_formattingIanaLocale == null)
    {
      String localeString = _formattingLocale.toString();

      _formattingIanaLocale = localeString.replace('_', '-');
    }

    return _formattingIanaLocale;
  }


  /**
   * Returns the translation Locale in IANA String format.
   */
  @Override
  public String getTranslationIANALocaleString()
  {
    if (_transIanaLocale == null)
    {
      String transLocaleString = getTranslationLocale().toString();

      _transIanaLocale = transLocaleString.replace('_', '-');
    }

    return _transIanaLocale;
  }

  @Override
  public boolean isRightToLeft()
  {
    return (getReadingDirection() == LocaleUtils.DIRECTION_RIGHTTOLEFT);
  }

  /**
   * Returns the reading direction that should be used for rendering.
   * @return a reading direction from
   * <code>org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils</code>.
   * This method will never return
   * <code>LocaleUtils.DIRECTION_DEFAULT</code>.
   * <p>
   * @see #setReadingDirection
   * @see org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils
   */
  public int getReadingDirection()
  {
    if (_readingDirection != LocaleUtils.DIRECTION_DEFAULT)
    {
      return _readingDirection;
    }
    else
    {
      return LocaleUtils.getReadingDirectionForLocale(getTranslationLocale());
    }
  }


  /**
   * Sets the new reading direction to be one of the reading directions
   * defined in <code>org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils</code>.
   *
   * @deprecated  use MutableLocaleContext.setReadingDirection() instead
   * @see org.apache.myfaces.trinidadinternal.share.nls.MutableLocaleContext
   */
  @Deprecated
  public void setReadingDirection(
    int newReadingDirection
    )
  {
    if ((newReadingDirection != LocaleUtils.DIRECTION_DEFAULT) &&
        (newReadingDirection != LocaleUtils.DIRECTION_LEFTTORIGHT) &&
        (newReadingDirection != LocaleUtils.DIRECTION_RIGHTTOLEFT))
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "UNKNOWN_READING_DIRECTION", newReadingDirection));
    }

    _readingDirection = newReadingDirection;
  }


  /**
   * Returns the TimeZone that the user is running in.
   */
  @Override
  public TimeZone getTimeZone()
  {
    return _timeZone;
  }


  /**
   * Sets the TimeZone that the user is running in.  Setting this value
   * to null will set the TimeZone to the default TimeZone.
   *
   * @deprecated  use MutableLocaleContext.setTimeZone() instead
   * @see org.apache.myfaces.trinidadinternal.share.nls.MutableLocaleContext
   */
  @Deprecated
  public void setTimeZone(
    TimeZone newTimeZone
    )
  {
    if (newTimeZone == null)
    {
      newTimeZone = TimeZone.getDefault();
    }

    _timeZone = newTimeZone;
  }


  /**
   * Override of Object.toString().
   */
  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer(super.toString());

    buffer.append(" translationLocale=");
    buffer.append(getTranslationLocale());
    buffer.append(", formattingLocale=");
    buffer.append(getFormattingLocale());
    buffer.append(", direction=");
    buffer.append(getReadingDirection());
    buffer.append(", timeZone=");
    buffer.append(getTimeZone());
    buffer.append(", dateFormatContext=");
    buffer.append(getDateFormatContext());
    buffer.append(", decimalFormatContext=");
    buffer.append(getDecimalFormatContext());

    return buffer.toString();
  }


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
  @Override
  public ResourceBundle getBundle(
    String baseBundleName
    ) throws MissingResourceException
  {
    if (_bundles == null)
    {
      _bundles = new HashMap<String, ResourceBundle>(13);
    }

    ResourceBundle bundle = _bundles.get(baseBundleName);

    if (bundle == null)
    {
      ClassLoader loader = ClassLoaderUtils.getContextClassLoader();

      Locale translationLocale = getTranslationLocale();

      if (loader == null)
      {
        bundle = ResourceBundle.getBundle(baseBundleName, translationLocale);
      }
      else
      {
        bundle = ResourceBundle.getBundle(baseBundleName,
                                          translationLocale,
                                          loader);
      }

      // cache the bundle
      _bundles.put(baseBundleName, bundle);
    }

    return bundle;
  }


  /**
   * Returns the year offset for parsing years with only two digits.
   */
  @Override
  public int getTwoDigitYearStart()
  {
    return getDateFormatContext().getTwoDigitYearStart();
  }

  /**
   * Returns the character used to separate number groups.
   * If zero (NUL), the default separator for the Locale
   * will be used.
   */
  @Override
  public char getGroupingSeparator()
  {
    return getDecimalFormatContext().getGroupingSeparator();
  }

  /**
   * Returns the character used as a decimal separator.
   * If zero (NUL), the default separator for the Locale
   * will be used.
   */
  @Override
  public char getDecimalSeparator()
  {
    return getDecimalFormatContext().getDecimalSeparator();
  }

  /**
   * Returns the DateFormatContext containing all date format parameters,
   * falling back on defaults when <code>getDateFormatContextImpl</code>
   * returns null;
   */
  public final DateFormatContext getDateFormatContext()
  {
    DateFormatContext dfc = getDateFormatContextImpl();

    if (dfc == null)
      dfc = _sDefaultDateFormatContext;

    return dfc;
  }

  /**
   * Returns the DecimalFormatContext containing all number format parameters,
   * falling back on defaults when <code>getDecimalFormatContextImpl</code>
   * returns null;
   */
  public final DecimalFormatContext getDecimalFormatContext()
  {
    DecimalFormatContext dfc = getDecimalFormatContextImpl();

    if (dfc == null)
      dfc = _sDefaultDecimalFormatContext;

    return dfc;
  }


  /**
   * Override of Object.hashCode().
   */
  @Override
  public int hashCode()
  {
    return getFormattingLocale().hashCode();
  }

  /**
   * Override of Object.equals().
   */
  @Override
  public boolean equals(Object obj)
  {
    if (obj == this)
      return true;

    if (!(obj instanceof LocaleContextImpl))
      return false;

    LocaleContextImpl that = (LocaleContextImpl)obj;

    return
      (this.getTranslationLocale().equals(that.getTranslationLocale())   &&
       this.getFormattingLocale().equals(that.getFormattingLocale())     &&
       this.getTimeZone().equals(that.getTimeZone())                     &&
       (this.getReadingDirection() == that.getReadingDirection())        &&
       this.getDateFormatContext().equals(that.getDateFormatContext())   &&
       this.getDecimalFormatContext().equals(that.getDecimalFormatContext()));
  }

  /**
   * Returns the DateFormatContext containing all date format parameters.
   * If this method returns null, <code>getDateFormatContext</code> will
   * use the default value instead.
   */
  protected DateFormatContext getDateFormatContextImpl()
  {
    // use defaults
    return null;
  }

  /**
   * Returns the DecimalFormatContext containing all number format parameters.
   * If this method returns null, <code>getDecimalFormatContext</code> will
   * use the default value instead.
   */
  protected DecimalFormatContext getDecimalFormatContextImpl()
  {
    // use defaults
    return null;
  }


  static private class DefaultDecimal extends DecimalFormatContext
  {
    @Override
    public char getGroupingSeparator()
    {
      return (char) 0;
    }

    @Override
    public char getDecimalSeparator()
    {
      return (char) 0;
    }
  }

  static private class DefaultDate extends DateFormatContext
  {
    @Override
    public int getTwoDigitYearStart()
    {
      return 1950;
    }
  }

  private static final DateFormatContext _sDefaultDateFormatContext =
                                                new DefaultDate();
  private static final DecimalFormatContext _sDefaultDecimalFormatContext =
                                                new DefaultDecimal();

  private HashMap<String, ResourceBundle> _bundles;

  private Locale   _formattingLocale;
  private Locale   _transLocale;
  private transient String _formattingIanaLocale;
  private transient String _transIanaLocale;
  private TimeZone _timeZone;

  private int _readingDirection = LocaleUtils.DIRECTION_DEFAULT;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    LocaleContextImpl.class);
}
