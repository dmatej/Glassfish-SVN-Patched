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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;


import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererFactoryImpl;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLookAndFeel;


/**
 * LookAndFeel implementation for HTML browsers
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PdaHtmlLookAndFeel.java#0 $) $Date: 10-nov-2005.18:55:02 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PdaHtmlLookAndFeel extends XhtmlLookAndFeel
                                implements XhtmlLafConstants
{

  public PdaHtmlLookAndFeel()
  {

  }

  public static RendererFactoryImpl createDefaultFactory()
  {
    RendererFactoryImpl rendererFactory = XhtmlLookAndFeel.createDefaultFactory();
    rendererFactory.registerRenderers(createInstantiators(_PREFIX,
                                                          _SUPPORTED_NAMES));
    rendererFactory.registerRenderers(
                                createInstantiators(_DESKTOP_PREFIX,
                                                    _DESKTOP_SHARED_NAMES));

    // register renderers for beans that shouldn't be rendered
    registerNullRenderers(rendererFactory, _NULL_NAMES);

    // register renderers for beans that should only render children
    registerSimpleRenderers(rendererFactory, _SIMPLE_NAMES);

    // register renderers for beans that aren't supported
    registerUnsupportedRenderers(rendererFactory, _UNSUPPORTED_NAMES);

    return rendererFactory;
  }

  @Override
  protected RendererFactory getDefaultFactory()
  {
    return _FACTORY;
  }

  private static final String _PREFIX 
    = "org.apache.myfaces.trinidadinternal.ui.laf.base.pda.";

  //
  // List of registered renderers.
  //
  private static final String[] _SUPPORTED_NAMES =
  {
    BORDER_LAYOUT_NAME,
    CELL_FORMAT_NAME,
    COLUMN_NAME,
    COLUMN_GROUP_NAME,
    CONTENT_FOOTER_NAME,
    DATE_BUTTON_NAME,
    DATE_FIELD_NAME,
    GLOBAL_BUTTON_NAME,
    GLOBAL_BUTTON_BAR_NAME,
    GLOBAL_HEADER_NAME,
    HEADER_NAME,
    HTML_NAME,
    LINK_NAME,
    MENU_LIST_NAME,
    MESSAGE_BOX_NAME,
    NAVIGATION_BAR_NAME,
    PAGE_HEADER_LAYOUT_NAME,
    PAGE_LAYOUT_NAME,
    PROCESS_CHOICE_BAR_NAME,
    PROCESS_TRAIN_NAME,
    SEPARATOR_NAME,
    SHOW_ITEM_NAME,
    SHOW_ONE_TAB_NAME,
    SUBMIT_BUTTON_NAME,
    SUB_TAB_BAR_NAME,       //support ?
    TAB_BAR_NAME,
    TREE_NAME,

  };

  private static final String _DESKTOP_PREFIX 
    = "org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.";

  //
  // List of registered renderers which are shared with the desktop.
  //
  private static final String[] _DESKTOP_SHARED_NAMES =
  {
    APPLICATION_SWITCHER_NAME,
  };

  //
  // List of unsupported renderers
  //
  private static final String[] _NULL_NAMES =
  {

    FOOTER_NAME,
    INLINE_DATE_PICKER_NAME,
    //STYLE_SHEET_NAME,
    SIDE_BAR_NAME,

  };

  //
  // List of Names of components that only render their children
  //
  private static final String[] _SIMPLE_NAMES =
  {
//    SHOW_ONE_TABS_NAME,
//    TIP_NAME,                  // Support
  };

  //
  // List of Names of components that it is an error to use
  //
  private static final String[] _UNSUPPORTED_NAMES =
  {
//    APPLICATION_SWITCHER_NAME,
    SIDE_NAV_NAME,
    //SUB_TAB_BAR_NAME,
    //TREE_NAME,                // Support?
  };

  private static final RendererFactory _FACTORY = createDefaultFactory();

    }
