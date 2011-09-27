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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererFactoryImpl;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.html.HTMLRendererFactory;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLookAndFeel;


/**
 * LookAndFeel implementation for XHTML
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlLookAndFeel.java#0 $) $Date: 10-nov-2005.18:54:20 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class XhtmlLookAndFeel extends BaseLookAndFeel
                              implements XhtmlLafConstants
{
  public XhtmlLookAndFeel()
  {
    super();
  }

  @Override
  public RendererManager createRendererManager(String facet)
  {
    RendererManager manager = super.createRendererManager(facet);

    // register the html factory - do the same thing for all facets
    HTMLRendererFactory.registerSelf(manager);
    return manager;
  }




  public static RendererFactoryImpl createDefaultFactory()
  {
    RendererFactoryImpl rendererFactory = BaseLookAndFeel.createDefaultFactory();

    rendererFactory.registerRenderers(createInstantiators(_PREFIX,
                                                          _SUPPORTED_NAMES));

    // Register the CalendarRenderer for the inlineDatePicker component
    rendererFactory.registerRenderer(
      INLINE_DATE_PICKER_NAME,
      "org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.CalendarRenderer");

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
    BaseLookAndFeel.applyFacet(rendererFactory, facet);
  }

  @Override
  protected RendererFactory getDefaultFactory()
  {
    return _FACTORY;
  }

  private static final String _PREFIX = "org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.";

  /**
   * List of registered renderers.
   */
  private static final String[] _SUPPORTED_NAMES =
  {
    BORDER_LAYOUT_NAME,
    BREAD_CRUMBS_NAME,
    BUTTON_NAME,
    CELL_FORMAT_NAME,
    CHOICE_NAME,
    COMMAND_ITEM_NAME,
    DATE_FIELD_NAME,
    FLOW_LAYOUT_NAME,
    FORM_VALUE_NAME,
    GLOBAL_BUTTON_BAR_NAME,
    HTML_NAME,
    ICON_NAME,
    ICON_KEY_NAME,
    IMAGE_NAME,
    LINK_NAME,
    NAVIGATION_PATH_NAME,
    OPTION_NAME,
    PAGE_NAME,
    PAGE_MENU_BUTTONS_NAME,
    PAGE_NAVIGATION_PATH_NAME,
    ROW_LAYOUT_NAME,
    SCRIPT_NAME,
    SELECT_OPTION_NAME,
    SEPARATOR_NAME,
    SHOW_ITEM_NAME,
    SINGLE_SELECTION_NAME,
    SPACER_NAME,
    STACK_LAYOUT_NAME,
    STYLED_TEXT_NAME,
    SUBMIT_BUTTON_NAME,
    TABLE_LAYOUT_NAME,

    // Alias elements
    COMMAND_NAVIGATION_ITEM_NAME,
  };

  private static final RendererFactory _FACTORY = createDefaultFactory();


}
