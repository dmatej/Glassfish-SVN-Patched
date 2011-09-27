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

import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/XhtmlScriptletFactory.java#0 $) $Date: 10-nov-2005.19:02:49 $
 */
public class XhtmlScriptletFactory
{
  static public final String CORE_LIB = "Core";

  static final String LOCALE_LIB                   = "Locale";
  static final String LOCALE_INFO_LIB              = "LocaleInfoLib";
  static final String CORE_FORMAT_LIB              = "CoreFormat";
  static final String CHAR_SETS_LIB                = "CharSets";
  static final String DATE_FORMAT_LIB              = "DateFormat";
  static final String DATE_FIELD_FORMAT_LIB        = "DateFieldFormat";
  static final String DATE_FIELD_LIB               = "DateField";
  static final String DATE_FORMAT_INFO_LIB         = "DateFormatInfoLib";
  static final String TABLE_PROXY_LIB              = "TableProxy";
  static final String COLOR_FORMAT_LIB             = "ColorFormat";
  static final String COLOR_FIELD_FORMAT_LIB       = "ColorFieldFormat";
  static final String SHUTTLE_LIB                  = "Shuttle";
  static final String POLL_LIB                     = "Poll";


  static public Scriptlet getCoreScriptlet()
  {
    return _sCoreScriptlet;
  }

  static synchronized public void registerAllScriptlets()
  {
    // Block from running twice, now that it's invoked from both
    // the Faces-major and legacy sides
    if (_sAlreadyRegistered)
      return;

    _sAlreadyRegistered = true;
    AliasedScriptlet.registerAliases();
    _sLocaleScriptlet.registerSelf();
    _sCoreScriptlet.registerSelf();
    _sDateFormatScriptlet.registerSelf();
    _sCoreFormatScriptlet.registerSelf();
    _sCharSetsScriptlet.registerSelf();
    _sShuttleScriptlet.registerSelf();
    _sTableProxyScriptlet.registerSelf();
    _sColorFormatScriptlet.registerSelf();
    _sColorFieldFormatScriptlet.registerSelf();
    _sDateFieldFormatScriptlet.registerSelf();
    _sDateFieldScriptlet.registerSelf();
    _sPollScriptlet.registerSelf();
    LocaleInfoScriptlet.sharedInstance().registerSelf();
    NamedLocaleInfoScriptlet.registerNamedLocales();
    ColorFormatInfoScriptlet.sharedInstance().registerSelf();
    DateFormatInfoScriptlet.sharedInstance().registerSelf();
    JspDirScriptlet.sharedInstance().registerSelf();
    ConfigurationScriptlet.sharedInstance().registerSelf();
    GlobalVariablesScriptlet.sharedInstance().registerSelf();
    DialogStyleScriptlet.sharedInstance().registerSelf();
    PanelBorderIE6Scriptlet.sharedInstance().registerSelf();
  }


  private XhtmlScriptletFactory()
  {
  }

  static private Scriptlet _sLocaleScriptlet;
  static private Scriptlet _sCoreScriptlet;
  static private Scriptlet _sDateFormatScriptlet;
  static private Scriptlet _sCoreFormatScriptlet;
  static private Scriptlet _sCharSetsScriptlet;
  static private Scriptlet _sShuttleScriptlet;
  static private Scriptlet _sTableProxyScriptlet;
  static private Scriptlet _sColorFormatScriptlet;
  static private Scriptlet _sColorFieldFormatScriptlet;
  static private Scriptlet _sDateFieldFormatScriptlet;
  static private Scriptlet _sDateFieldScriptlet;
  static private Scriptlet _sPollScriptlet;

  static
  {
    _sLocaleScriptlet =
        new LibraryScriptlet(LOCALE_LIB, new String[]{
                              "getUserLanguage()",
                              "getJavaLanguage()",
                              "TrConverter()", 
                              "TrValidator()",
                              "FastMessageFormatUtils()",
                              "isDigit()",
                              "parseDigit()",
                              "isNotLowerCase()",
                              "isLowerCase()",
                              "isUpperCase()",
                              "isNotUpperCase()",
                              "isLetter()",
                              "getLocaleSymbols()"}
                             );

    //    new LocaleInfoScriptlet();
    //new DateFormatInfoScriptlet();
    _sCoreScriptlet =
      new AliasedScriptlet(CORE_LIB,
                           new String[]
                           {
                             "openWindow()",
                             "submitForm()",
                             "_submitOnEnter()",
                             "resetForm()",
                             "dump()",
                             "isNavDirty()",
                             "t()",
                             "_isEmpty()",
                             "_launchDialog()",
                             "_unloadADFDialog()",
                             "_commandChoice()",
                           },
                           new String[]
                           {
                             GlobalVariablesScriptlet.GLOBAL_VARIABLES_KEY,
                             LOCALE_LIB
                           })
      {
        // =-=AEW  The Core library needs to be rendered even
        // when it's outside of a partial page request, since the
        // partial page library itself needs it.
        @Override
        boolean __isOutsidePartialPage(RenderingContext context)
        {
          return false;
        }
      };

    _sDateFormatScriptlet =
      new LibraryScriptlet(DATE_FORMAT_LIB,
                           new String[]
                           {
                             "TrDateTimeConverter()",
                           },
                           new String[]
                           {
                             "TrConverter()",
                             LocaleInfoScriptlet.LOCALE_INFO_KEY,
                             DateFormatInfoScriptlet.DATE_FORMAT_INFO_KEY,
                           });
    _sCoreFormatScriptlet =
      new AliasedScriptlet(CORE_FORMAT_LIB,
                           new String[]
                           {
                             "TrNumberConverter()",
                             "TrLengthValidator()",
                             "TrRegExpValidator()",
                           },
                           new String[]
                           {
                             "TrConverter()",
                             LocaleInfoScriptlet.LOCALE_INFO_KEY,
                           });

    _sCharSetsScriptlet =
      new AliasedScriptlet(CHAR_SETS_LIB,
                           new String[]
                           {
                             "Utf8Format()",
                             "CjkFormat()",
                             "SBFormat()",
                           },
                           new String[]
                           {
                             "TrValidator()",
                           });

    _sShuttleScriptlet =
      new AliasedScriptlet(SHUTTLE_LIB,
                           new String[]
                           {
                             "ShuttleProxy()",
                           });

    _sPollScriptlet =
      new AliasedScriptlet(POLL_LIB,
                           new String[]
                           {
                             "PollManager()",
                           });

    _sTableProxyScriptlet =
      new AliasedScriptlet(TABLE_PROXY_LIB,
                           new String[]
                           {
                             "getTableName()",
                             "getTableRow()",
                             "tableSelectAll()",
                             "tableSelectNone()",
                             "TableProxy()",
                           });
   _sColorFormatScriptlet =
      new LibraryScriptlet(COLOR_FORMAT_LIB,
                           new String[]
                           {
                             "TrColor()",
                             "ColorFormat()",
                             "TrColorConverter()",
                           },
                           new String[]
                           {
                             "TrConverter()",
                             ColorFormatInfoScriptlet.COLOR_FORMAT_INFO_KEY,
                             LocaleInfoScriptlet.LOCALE_INFO_KEY,
                           });
    _sDateFieldFormatScriptlet =
      new AliasedScriptlet(DATE_FIELD_FORMAT_LIB,
                           new String[]
                           {
                             "_fixDFF()",
                             "_getDateFieldFormat()",
                           },
                           new String[]
                           {
                             "TrDateTimeConverter()",
                           });
    _sDateFieldScriptlet =
      new AliasedScriptlet(DATE_FIELD_LIB,
                           new String[]
                           {
                             "_dfsv()",
                             "_calsd()",
                             "_updateCal()",
                             "_doCancel()",
                             "_selectDate()"
                           },
                           new String[]
                           {
                             "openWindow()",
                             "_getDateFieldFormat()",
                             "_unloadADFDialog()"
                           });

    _sColorFieldFormatScriptlet =
      new AliasedScriptlet(COLOR_FIELD_FORMAT_LIB,
                           new String[]
                           {
                             "_fixCFF()",
                             "_getColorFieldFormat()",
                           },
                           new String[]
                           {
                             "TrColorConverter()",
                           });

  }

  static private boolean _sAlreadyRegistered = false;
}
