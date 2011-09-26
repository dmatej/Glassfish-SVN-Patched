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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs;

import java.io.IOException;

import java.util.HashMap;
import java.util.Locale;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LocaleList;


/**
 * Scriptlet for registering locale information.

 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/LocaleInfoScriptlet.java#0 $) $Date: 10-nov-2005.19:02:47 $
 */
public class LocaleInfoScriptlet extends LibraryScriptlet
{
  static public final String LOCALE_INFO_KEY          = "LocaleInfo";

  static public Scriptlet sharedInstance()
  {
    return _sInstance;
  }

  public LocaleInfoScriptlet()
  {
    super(LOCALE_INFO_KEY, null, new Object[]{XhtmlScriptletFactory.LOCALE_LIB});
  }


  /**
   * @todo Revisit separate translation and internationalization
   * locales.
   */
  @Override
  protected void outputScriptletImpl(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    LocaleContext lc = arc.getLocaleContext();

    writer.startElement("script", null);
    XhtmlRenderer.renderScriptDeferAttribute(context, arc);

    // Bug #3426092:
    // Render the type="text/javascript" attribute in accessibility mode
    XhtmlRenderer.renderScriptTypeAttribute(context, arc);

    // write out the i18n Locale as a js variable
    writer.writeText("var _locale='", null);
    writer.writeText(lc.getFormattingIANALocaleString(), null);
    writer.writeText("';", null);

    // write out the translation Locale as a js variable
    writer.writeText("var _tLocale='", null);
    writer.writeText(lc.getTranslationIANALocaleString(), null);
    writer.writeText("';", null);

    char groupingSeparator = lc.getGroupingSeparator();

    if (groupingSeparator != 0)
    {
      writer.writeText("var _groupingSep='", null);
      writer.writeText(XhtmlUtils.escapeJS(
                          String.valueOf(groupingSeparator)),
                       null);
      writer.writeText("';", null);
    }

    char decimalSeparator = lc.getDecimalSeparator();

    if (decimalSeparator != 0)
    {
      writer.writeText("var _decimalSep='", null);
      writer.writeText(XhtmlUtils.escapeJS(
                          String.valueOf(decimalSeparator)),
                       null);
      writer.writeText("';", null);
    }

    writer.endElement("script");

    super.outputScriptletImpl(context, arc);
  }

  @Override
  protected String getLibraryName(
    FacesContext        context,
    RenderingContext arc)
  {
    Locale elementsLocale = _getJSLocaleElementsLocale(getFormattingLocale(arc));
    String var = getSupportedLocaleVariant(arc);
    if (var != null)
    {
      elementsLocale = new Locale(elementsLocale.getLanguage(),
                                  elementsLocale.getCountry(),
                                  var);
    }
    String locStr = elementsLocale.toString();

    StringBuffer buffer = new StringBuffer(_RESOURCE_BASE.length()
                                           + ((var == null)
                                              ? 0
                                              : (var.length() + 1))
                                           + locStr.length());

    buffer.append(_RESOURCE_BASE);
    buffer.append(locStr);
    return buffer.toString();
  }


  /* return extra parameter "?loc=en", so that we can know what locale
   * the translations in LocaleElements should be in. */
  protected String getExtraParameters(
  FacesContext        context,
  RenderingContext    arc)
  {
    String locStr = arc.getLocaleContext().getTranslationLocale().toString();

    String extraParams = "?loc=" + locStr;

    return extraParams;
  }

  protected Locale getFormattingLocale(
    RenderingContext rc
    )
  {
    return rc.getLocaleContext().getFormattingLocale();
  }

  /**
   * Returns the locale variant type to use when formatting dates. The locale
   * variant is a configuration parameter. It defaults to Java style locale
   * data, but it can be set to a variant Locale (e.g. 'ORACLE10G' or
   * 'ORACLE9I') in which case the appropriate data should be used.
   *
   * Currently, the only Locale variant supported is 'ORACLE10G'.
   *
   * @param context The current RenderingContext.
   */
  public String getSupportedLocaleVariant(RenderingContext arc)
  {
    LocaleContext lc  = arc.getLocaleContext();
    Locale l = lc.getFormattingLocale();
    String variant = l.getVariant();
    variant = variant.toUpperCase();

    if (variant.startsWith("ORACLE"))
      return variant;

    return null;
  }

  /**
   * Returns the Locale to use for loading a JavaScript resource,
   * given an input Locale
   */
  private static Locale _getJSLocaleElementsLocale(
    Locale inLocale
    )
  {
    HashMap<Locale, Locale> supportedMap =
      LocaleList.getSupportedLocales();

    Locale outLocale = inLocale;
    if(supportedMap.containsKey(outLocale))
    {
      return outLocale;
    }
    else
    {
      String variant  = outLocale.getVariant();
      String language = outLocale.getLanguage();

      boolean isSupportedLocale = false;

      if (variant.length() != 0)
      {
        outLocale = new Locale(language, outLocale.getCountry());
        isSupportedLocale = supportedMap.containsKey(outLocale);
      }

      if (!isSupportedLocale)
      {
        outLocale = new Locale(language, "");

        if (!supportedMap.containsKey(outLocale))
        {
          // use default Locale
          outLocale = Locale.getDefault();
        }
      }

      return outLocale;
    }
  }


  static private final String    _RESOURCE_BASE = "resources/LocaleElements_";
  static private final Scriptlet _sInstance = new LocaleInfoScriptlet();
}
