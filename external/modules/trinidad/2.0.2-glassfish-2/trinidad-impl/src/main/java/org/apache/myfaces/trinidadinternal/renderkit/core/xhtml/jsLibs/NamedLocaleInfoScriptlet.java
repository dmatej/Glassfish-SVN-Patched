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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LocaleList;


/**
 * Scriptlet for registering locale information. This scriptlet can be used to 
 * locale information on demand, e.g. when a component runs in a different locale
 * than the page locale.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/LocaleInfoScriptlet.java#0 $) $Date: 10-nov-2005.19:02:47 $
 */
class NamedLocaleInfoScriptlet extends LocaleInfoScriptlet
{
  public static void registerNamedLocales () 
  {
    HashMap<Locale, Locale> supportedMap = LocaleList.getSupportedLocales();
    Iterator<Locale> locales =  supportedMap.keySet().iterator();
    
    while (locales.hasNext())
    {
      Locale l = locales.next();
      NamedLocaleInfoScriptlet ns = new NamedLocaleInfoScriptlet (l);
      ns.registerSelf();
    }      
  }

  public NamedLocaleInfoScriptlet (Locale namedlocale)
  {
     super ();
     
    // The namedLocale is an instance returned from LocaleList.getSupportedLocales()
    // getSupportedLocales() is derived from Locale.getAvailableLocales()
    // which accounts for <Country, Language> but not the Oracle variant. Use
    // similar code to getLibraryName() to set the Locale variant. 
    // See JSLocaleElementsGenerator.java in maven-i18n-plugin to check
    // how LocaleList.java is generated
    String var = getSupportedLocaleVariant(RenderingContext.getCurrentInstance());
    if (var != null)
    {
      namedlocale = new Locale(namedlocale.getLanguage(),
                               namedlocale.getCountry(),
                               var);
    }
      
    _locale = namedlocale;
  }


  /*
   * Append an argument to the URL to indicate if this is the page locale
   */
  protected String getExtraParameters(
  FacesContext        context,
  RenderingContext    arc)
  {
    StringBuffer sb = new StringBuffer (super.getExtraParameters (context, arc));
    if (!(_locale.equals(arc.getLocaleContext().getFormattingLocale())))
    {
      sb.append("&skipTranslations=true");
    }
    return sb.toString();
  }
  
  /* Register as a distinct scriptlet for each locale, i.e. <country, language, Oraclevariant> */
  @Override
  public Object getScriptletKey()
  {
    String lib = (String) super.getScriptletKey();
    String sLoc = _locale.toString();
    
    StringBuffer sb = new StringBuffer (lib.length() + 1 +  sLoc.length());
    sb.append (lib);
    sb.append ("_");
    sb.append (sLoc);
    return sb.toString();
  }
  
  protected Locale getFormattingLocale(RenderingContext arc) 
  {
    return _locale;
  }
   
  private Locale _locale;
}
