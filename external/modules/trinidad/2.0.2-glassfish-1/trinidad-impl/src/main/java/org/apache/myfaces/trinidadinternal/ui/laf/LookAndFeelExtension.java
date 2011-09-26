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
package org.apache.myfaces.trinidadinternal.ui.laf;

import java.util.Iterator;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererFactoryImpl;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;



/**
 * A LookAndFeel which extends another LookAndFeel, possibly adding
 * customizations.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/LookAndFeelExtension.java#0 $) $Date: 10-nov-2005.18:50:30 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class LookAndFeelExtension extends LookAndFeel
{
  /**
   * Creates a LookAndFeel which extends the specified base
   * LookAndFeel.
   *
   * @param baseLookAndFeel The base LookAndFeel that this custom
   *        LookAndFeel "extends".
   * @param id A string which can be used to uniquely identify the
   *        custom LookAndFeel implementation.
   * @param family The look and feel family name that this
   *               LookAndFeelExtension belongs to
   */
  public LookAndFeelExtension(
    LookAndFeel baseLookAndFeel,
    String id,
    String family
    )
  {
    if ((baseLookAndFeel == null)||(id == null)||(family == null))
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ARGUMENT"));
    }

    _baseLAF = baseLookAndFeel;
    _id = id;
    _family = family;
  }

  /**
   * Returns the base LookAndFeel which this custom LookAndFeel
   * "extends".
   */
  public LookAndFeel getBaseLookAndFeel()
  {
    return _baseLAF;
  }

  /**
   * Returns the id of this custom LookAndFeel.
   */
  @Override
  public String getId()
  {
    return _id;
  }

  /**
   * Returns the name of the look and feel family that this
   * LookAndFeelExtension belongs to.
   */
  @Override
  public String getFamily()
  {
    return _family;
  }

  /**
   * Implementation of LookAndFeel.getRendererManager() which
   * delegates to the base LookAndFeel.
   */
  @Override
  public RendererManager getRendererManager()
  {
    return getRendererManager(UIConstants.FACET_DEFAULT);
  }

  /**
   * Implementation of LookAndFeel.getRendererManager() which
   * delegates to the base LookAndFeel.
   */
  @Override
  public RendererManager getRendererManager(String facet)
  {
    if (facet == null)
      facet = UIConstants.FACET_DEFAULT;

    // Check to see if we already have a RendererManager
    // for the specified facet.
    RendererManager manager = (RendererManager)ArrayMap.get(_rendererManagers,
                                                            facet);

    if (manager == null)
    {
      // If we don't already have a RendererManager, check to
      // see if the base LookAndFeel has one
      manager = getBaseLookAndFeel().getRendererManager(facet);

      if (manager != null)
      {
        // Create a proxy for the RendererManager and stash it away
        manager = new RendererManagerProxy(manager, facet);
        _rendererManagers = ArrayMap.put(_rendererManagers, facet, manager);
      }
    }

    return manager;
  }

  /**
   * Implementation of LookAndFeel.getSupportedFacets() which
   * delegates to the base LookAndFeel.
   */
  @Override
  public Iterator<String> getSupportedFacets()
  {
    return getBaseLookAndFeel().getSupportedFacets();
  }

  /**
   * Registers a custom Renderer for the specified component
   * namespace/name.  This Renderer will be used in place of the Renderer
   * provided by the base Look And Feel.
   * @param namespace The namespace of the component for which the
   *                  custom Renderer should be used.
   * @param name The name of the component for which the custom Renderer
   *             should be used.
   * @param facet The name of the facet for which the custom Renderer
   *              should be used.  If null, the Renderer will be registered
   *              for all supported facets.
   * @param Renderer The custom Renderer
   */
  public void registerRenderer(
    String   namespace,
    String   name,
    String   facet,
    Renderer renderer
    )
  {
    if (facet!= null)
    {
      _registerRenderer(namespace, name, facet, renderer);
    }
    else
    {
      Iterator<String> facets = getSupportedFacets();

      while (facets.hasNext())
      {
        facet = facets.next();
        _registerRenderer(namespace, name, facet, renderer);
      }
    }
  }

  /**
   * Registers a custom Renderer for the specified component using
   * the Renderer class name. The Renderer instance is instantiated
   * the first time it is requested.
   *
   * @param namespace The namespace of the component for which the
   *                  custom Renderer should be used.
   * @param name The name of the component for which the custom Renderer
   *             should be used.
   * @param facet The name of the facet for which the custom Renderer
   *              should be used.  If null, the Renderer will be registered
   *              for all supported facets.
   * @param className The clss name of the Renderer
   */
  public void registerRenderer(
    String   namespace,
    String   name,
    String   facet,
    String   className
    )
  {
    if (facet!= null)
    {
      _registerRenderer(namespace, name, facet, className);
    }
    else
    {
      Iterator<String> facets = getSupportedFacets();

      while (facets.hasNext())
      {
        facet = facets.next();
        _registerRenderer(namespace, name, facet, className);
      }
    }
  }




  // Registers a Renderer on the specific facet
  private void _registerRenderer(
    String   namespace,
    String   name,
    String   facet,
    Renderer renderer
    )
  {
    assert (facet != null);

    RendererManagerProxy manager = (RendererManagerProxy)getRendererManager(
                                                           facet);
    RendererFactoryProxy factory = (RendererFactoryProxy)
                                     manager.getFactory(namespace);

    if (factory == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_FACTORY_REGISTERED", namespace));
    }

    factory.__getLocalFactory().registerRenderer(name, renderer);
  }

  // Registers a Renderer on the specific facet using the
  // Renderer's class name.
  private void _registerRenderer(
    String   namespace,
    String   name,
    String   facet,
    String   className
    )
  {
    assert (facet != null);

    RendererManagerProxy manager = (RendererManagerProxy)getRendererManager(
                                                           facet);
    RendererFactoryProxy factory = (RendererFactoryProxy)
                                     manager.getFactory(namespace);

    if (factory == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_FACTORY_REGISTERED", namespace));
    }

    factory.__getLocalFactory().registerRenderer(name, className);
  }

  private LookAndFeelExtension() {}

  // Proxy RendererManager implementation
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  static private class RendererManagerProxy extends RendererManager
  {
    public RendererManagerProxy(
      RendererManager rendererManager,
      String facet
      )
    {
      super(facet);

      _rendererManager = rendererManager;
    }

    // Returns the RendererManager that is being proxied
    public RendererManager getRendererManager()
    {
      return _rendererManager;
    }

    @Override
    public RendererFactory getFactory(String namespace)
    {
      // First, check to see if we have already created a
      // proxy RendererFactory for this namespace
      RendererFactory factory = (RendererFactory)ArrayMap.get(_proxyFactories,
                                                              namespace);
      if (factory != null)
        return factory;

      // If we don't have a factory yet, see if the
      // the proxied RendererManager has one for us
      factory = getRendererManager().getFactory(namespace);

      // If we found a factory, register it on ourselves - and
      // get the proxy factory
      if (factory != null)
        factory = _registerFactory(namespace, factory);

      return factory;
    }

    @Override
    synchronized public void registerFactory(
       String namespace, RendererFactory factory)
    {
      _registerFactory(namespace, factory);
    }

    @Override
    synchronized public void unregisterFactory(String namespace)
    {
      _proxyFactories = ArrayMap.remove(_proxyFactories, namespace);
    }

    // Registers a RendererFactory, wrapping it in a RendererFactoryProxy
    private RendererFactoryProxy _registerFactory(
      String          namespace,
      RendererFactory factory
      )
    {
      // Wrap all factories in a proxy factory which knows
      // how to obtain Renderers from other component providers.
      RendererFactoryProxy proxyFactory = new RendererFactoryProxy(factory);
      _proxyFactories = ArrayMap.put(_proxyFactories,
                                     namespace.intern(),
                                     proxyFactory);

      return proxyFactory;
    }

    // The RendererManager that is being proxied
    private RendererManager _rendererManager;

    // ArrayMap of proxied RendererFactories
    private Object[]  _proxyFactories;
  }

  // Proxy RendererFactory implementation.
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  static private class RendererFactoryProxy implements RendererFactory
  {
    // Note: It might make sense for RendererFactoryProxy to
    // extend RendererFactoryImpl, rather than creating a
    // separate RendererFactoryImpl to use as a local Renderer
    // cache.  However, I don't want customers casting to
    // RendererFactoryImpl (since some day we might want to
    // use something else).  So, we avoid extending RendererFactoryImpl -
    // but instead we create a RendererFactoryImpl that we use for
    // local storage of Renderers.

    public RendererFactoryProxy(
      RendererFactory factory
      )
    {
      _factory = factory;
      _localFactory = new RendererFactoryImpl();
    }

    public Renderer getRenderer(String name)
    {
      // First, check our local cache
      Renderer renderer = _localFactory.getRenderer(name);
      if (renderer != null)
        return renderer;

      // If we still don't have Renderer, try to get one
      // from the proxied RendererFactory
      renderer = _factory.getRenderer(name);

      // If we found a Renderer, store a reference to it
      // in our local cache.
      if (renderer != null)
        _localFactory.registerRenderer(name, renderer);

      return renderer;
    }

    // Technically speaking we shouldn't expose the local RendererFactoryImpl.
    // But for expediency, we expose this so that we don't have to add a
    // bunch of Renderer registration methods.
    RendererFactoryImpl __getLocalFactory()
    {
      return _localFactory;
    }

    // The RendererFactory that is being proxied
    private RendererFactory _factory;

    // A local cache of Renderers - either retrieved and cached from
    // the proxied RendererFactory - or simply registered
    private RendererFactoryImpl _localFactory;
  }



  private String      _id;
  private String      _family;
  private LookAndFeel _baseLAF;

  // Proxy RendererManagers hashed by facet
  private Object[] _rendererManagers;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    LookAndFeelExtension.class);
}
