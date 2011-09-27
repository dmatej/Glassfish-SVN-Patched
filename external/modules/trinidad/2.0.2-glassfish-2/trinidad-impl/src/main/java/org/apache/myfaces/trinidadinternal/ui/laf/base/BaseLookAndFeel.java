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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererFactoryImpl;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Abstract Base LookAndFeel implementation
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/BaseLookAndFeel.java#0 $) $Date: 10-nov-2005.18:52:58 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class BaseLookAndFeel extends LookAndFeel
                                      implements UIConstants,
                                                 LafIconProviderProvider
{
  public BaseLookAndFeel()
  {
    _rendererManager = createRendererManager(FACET_DEFAULT);
  }

  /**
   * Returns the RendererManager for this LookAndFeel.
   */
  @Override
  public RendererManager getRendererManager()
  {
    return _rendererManager;
  }


  /**
   * Creates a new RendererManager for a particular facet of
   * this LookAndFeel.
   */
  public RendererManager createRendererManager(String facet)
  {
    RendererManager rendererManager = new RendererManager(facet);
    rendererManager.registerFactory(MARLIN_NAMESPACE, getFactory(facet));
    return rendererManager;
  }


  protected RendererFactory getFactory(String facet)
  {
    if (FACET_DEFAULT.equals(facet))
      return getDefaultFactory();

    throw new IllegalArgumentException(_LOG.getMessage(
      "FACET_NOT_SUPPORTED", new Object[]{facet, getClass().getName()}));
  }


  protected abstract RendererFactory getDefaultFactory();


  protected static RendererFactoryImpl createDefaultFactory()
  {
    RendererFactoryImpl rendererFactory =
            new RendererFactoryImpl(createInstantiators(_PREFIX,
                                                        _SUPPORTED_NAMES));

    return rendererFactory;
  }

  /**
   * Applies any changes needed for a non-default facet to
   * a renderer factory.
   */
  protected static void applyFacet(
    RendererFactoryImpl rendererFactory,
    String              facet)
  {
    // No supported facets
  }


  /**
   * Registers NullRenderers for each of the local names in the
   * array of empty names, causing neither the node nore its
   * children to be rendered.
   */
  protected static void registerNullRenderers(
    RendererFactoryImpl factory,
    String[]            nullNames
    )
  {
    registerRenderers(factory, nullNames, NullRenderer.getInstance());
  }


  /**
   * Registers the ChildRenderer for each of the local names in the
   * array of empty names, causing the children, but not the node
   * itself to be rendered.
   */
  protected static void registerSimpleRenderers(
    RendererFactoryImpl factory,
    String[]            simpleNames
    )
  {
    registerRenderers(factory, simpleNames, ChildRenderer.getInstance());
  }


  /**
   * Registers an UnsupportedRenderer for each of the local names in the
   * array of empty names, causing nothing to be rendered and an
   * error to be logged whenever the node is rendered.
   */
  protected static void registerUnsupportedRenderers(
    RendererFactoryImpl factory,
    String[]            unsupportedNames
    )
  {
    registerRenderers(factory,
                      unsupportedNames,
                      UnsupportedRenderer.getInstance());
  }


  /**
   * Registers the same instance of a Renderer for several different
   *
   */
  protected static void registerRenderers(
    RendererFactoryImpl factory,
    String[]            localNames,
    Renderer            renderer
    )
  {
    if ((factory != null) && (localNames != null) && (renderer != null))
    {
      int nameCount = localNames.length;

      for (int i = 0; i < nameCount; i++)
      {
        factory.registerRenderer(localNames[i], renderer);
      }
    }
  }


  /**
   * Method for subclasses to call to create their lazy instantiators
   */
  public static String[] createInstantiators(
    String   prefix,
    String[] supportedNames
    )
  {
    int supportedCount = supportedNames.length;

    String[] instantiators = new String[supportedCount * 2];

    for (int i = 0; i < supportedCount; i++)
    {
      String localName = supportedNames[i];

      instantiators[i * 2]     = localName;
      char initialCaps = Character.toUpperCase(localName.charAt(0));
      instantiators[i * 2 + 1] = prefix  + initialCaps +
                                 localName.substring(1) + "Renderer";
    }

    return instantiators;
  }

  /**
   * Returns the Object for resolving colorized icons provided by the
   *  LookAndFeel
   */
  public LafIconProvider getLafIconProvider()
  {
    return null;
  }



  private static final String _PREFIX = "org.apache.myfaces.trinidadinternal.ui.laf.base.";

  /**
   * List of registered renderers.
   */
  private static final String[] _SUPPORTED_NAMES =
  {
    RAW_TEXT_NAME,
    SWITCHER_NAME,
    TEXT_NAME,
  };

  private RendererManager _rendererManager;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    BaseLookAndFeel.class);
}
