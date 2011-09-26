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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererFactoryImpl;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLookAndFeel;

/**
 * LookAndFeel implementation for HTML browsers
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/BaseDesktopLookAndFeel.java#0 $) $Date: 10-nov-2005.18:55:09 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseDesktopLookAndFeel extends XhtmlLookAndFeel
  implements BaseDesktopConstants
{
  /**
   * Constructs a BaseDesktopLookAndFeel instance
   */
  public BaseDesktopLookAndFeel()
  {
  }

  /**
   * Returns the id for the desktop implementation of the Base
   * Look And Feel: "base.desktop".
   */
  @Override
  public String getId()
  {
    return BaseDesktopConstants.BASE_DESKTOP_ID;
  }

  /**
   * Returns the family for the Base
   * Look And Feel: "base".
   */
  @Override
  public String getFamily()
  {
    return "base";
  }

  /**
   * Returns the name of the XSS style sheet for this LookAndFeel.
   */
  public String getStyleSheetName()
  {
    return "META-INF/adf/styles/base-desktop.css";
  }

  @Override
  public RendererManager getRendererManager(String facet)
  {
    if (FACET_PRINTABLE.equals(facet))
    {
      return _getPrintableRendererManager();
    }
    else if (FACET_PORTLET.equals(facet))
    {
      return _getPortletRendererManager();
    }
    else if (FACET_EMAIL.equals(facet))
    {
      return _getEmailRendererManager();
    }
    else if (_EDITABLE_FACET.equals(facet))
    {
      return _getEditableRendererManager();
    }

    return super.getRendererManager(facet);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator<String> getSupportedFacets()
  {
    if (_SUPPORTED_FACETS!=null)
    {
      return (Arrays.asList(_SUPPORTED_FACETS)).iterator();
  }
    else
    return (Collections.EMPTY_LIST).iterator();

  }

  public static RendererFactoryImpl createDefaultFactory()
  {
    RendererFactoryImpl rendererFactory = XhtmlLookAndFeel.createDefaultFactory();

    rendererFactory.registerRenderers(createInstantiators(_PREFIX,
                                                          _SUPPORTED_NAMES));


    return rendererFactory;
  }


  /**
   * Applies any changes needed for a non-default facet to
   * a renderer factory.
   */
  public static void applyFacet(
    RendererFactoryImpl rendererFactory,
    String              facet)
  {
    // And call through to the superclass.
    XhtmlLookAndFeel.applyFacet(rendererFactory, facet);

    if (FACET_PRINTABLE.equals(facet) ||
        FACET_EMAIL.equals(facet))
    {
      rendererFactory.registerRenderer(PAGE_LAYOUT_NAME,
                                       new PrintablePageLayoutRenderer());
    }
  }

  @Override
  protected RendererFactory getDefaultFactory()
  {
    return _FACTORY;
  }

  @Override
  protected RendererFactory getFactory(String facet)
  {
    if (FACET_PRINTABLE.equals(facet))
    {
      return _createPrintableFactory();
    }
    else if (FACET_PORTLET.equals(facet))
    {
      return getDefaultFactory();
    }
    else if (FACET_EMAIL.equals(facet))
    {
      return _createEmailFactory();
    }
    else if (_EDITABLE_FACET.equals(facet))
    {
      return getDefaultFactory();
    }

    return super.getFactory(facet);
  }



  // Create the RendererManager for printable pages
  synchronized private RendererManager _getPrintableRendererManager()
  {
    if (_printableRendererManager == null)
      _printableRendererManager = createRendererManager(FACET_PRINTABLE);

    return _printableRendererManager;
  }


  // Create the RendererManager for email pages
  synchronized private RendererManager _getEmailRendererManager()
  {
    if (_emailRendererManager == null)
      _emailRendererManager = createRendererManager(FACET_EMAIL);

    return _emailRendererManager;
  }

  // Create the RendererManager for editor
  synchronized private RendererManager _getEditableRendererManager()
  {
    if (_editableRendererManager == null)
      _editableRendererManager = createRendererManager(_EDITABLE_FACET);

    return _editableRendererManager;
  }

  // Create the RendererManager for portlets
  synchronized private RendererManager _getPortletRendererManager()
  {
    if (_portletRendererManager == null)
      _portletRendererManager = createRendererManager(FACET_PORTLET);

    return _portletRendererManager;
  }

  // Create the RendererFactory for printable pages
  private RendererFactoryImpl _createPrintableFactory()
  {
    RendererFactoryImpl rendererFactory = createDefaultFactory();
    applyFacet(rendererFactory, FACET_PRINTABLE);
    return rendererFactory;
  }


  // Create the RendererFactory for email pages
  private RendererFactoryImpl _createEmailFactory()
  {
    RendererFactoryImpl rendererFactory = createDefaultFactory();
    applyFacet(rendererFactory, FACET_EMAIL);
    return rendererFactory;
  }

  private static final String _PREFIX = "org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.";

  /**
   * List of registered renderers.
   */
  private static final String[] _SUPPORTED_NAMES =
  {
    APPLICATION_SWITCHER_NAME,
    BORDER_LAYOUT_NAME,
    BREAD_CRUMBS_NAME,
    CELL_FORMAT_NAME,
    COLOR_BUTTON_NAME,
    COLOR_FIELD_NAME,
    COLOR_PALETTE_NAME,
    COLOR_SWATCH_NAME,
    COLUMN_NAME,
    COLUMN_GROUP_NAME,
    CONTENT_FOOTER_NAME,
    DATE_BUTTON_NAME,
    DATE_FIELD_NAME,
    HTML_NAME,
    FOOTER_NAME,
    HEADER_NAME,
    GLOBAL_BUTTON_NAME,
    GLOBAL_HEADER_NAME,
    HTML_NAME,
    MENU_LIST_NAME,
    NAVIGATION_PATH_NAME,
    NAVIGATION_TREE_NAME,
    MESSAGE_BOX_NAME,
    NAVIGATION_BAR_NAME,
    PAGE_HEADER_LAYOUT_NAME,
    PAGE_LAYOUT_NAME,
    PAGE_MENU_BAR_NAME,   
    PAGE_MENU_LIST_NAME, 
    PAGE_NAVIGATION_PATH_NAME,
    PAGE_MENU_TABS_NAME,
    PAGE_NAVIGATION_TREE_NAME,
    PROCESS_CHOICE_BAR_NAME,
    PROCESS_TRAIN_NAME,
    PROCESSING_NAME,
    SEPARATOR_NAME,
    SHOW_ONE_TAB_NAME,
    SIDE_BAR_NAME,
    SIDE_NAV_NAME,
    SUB_TAB_BAR_NAME,
    TAB_BAR_NAME,
    TABLE_FOOTER_NAME,
    TABLE_NAME,
    TREE_NAME
  };


  private static final RendererFactory _FACTORY = createDefaultFactory();

  private RendererManager _printableRendererManager;
  private RendererManager _emailRendererManager;
  private RendererManager _editableRendererManager;
  private RendererManager _portletRendererManager;

  static private final String _EDITABLE_FACET = "editable";
  static private final String[] _SUPPORTED_FACETS =
    new String[]
    {
      FACET_DEFAULT,
      FACET_PRINTABLE,
      FACET_EMAIL,
      FACET_PORTLET,
      _EDITABLE_FACET
    };


}
