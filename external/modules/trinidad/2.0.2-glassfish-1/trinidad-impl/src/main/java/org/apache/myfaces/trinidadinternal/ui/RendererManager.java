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
package org.apache.myfaces.trinidadinternal.ui;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopLookAndFeel;

/**
 * RendererManager maintains a table of RendererFactories, keyed
 * by namespace.  Clients can use a single RendererManager, or
 * create their own.
 * <p>
 * <h4>Setting up a RendererManager</h4>
 * Clients can always set the  RendererManager explicitly on
 * a RenderingContext, but our implementations of
 * the RenderingContext will use the following defaulting strategy:
 * <ol>
 *  <li>If the client explicitly set a RendererManager, use it
 *  <li>Check the context's <code>Configuration</code> object for a
 *      RendererManager with the RENDERER_MANAGER key.
 *  <li>Retrieve the RendererManager from the LookAndFeel
 *  <li>Otherwise, use a default RendererManager containing
 *      the Oracle browser-based look-and-feel and plain HTML (e.g,
        HTMLWebBean) support
 * </ol>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/RendererManager.java#0 $) $Date: 10-nov-2005.18:50:19 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RendererManager
{
  /**
   * Creates a new, empty RendererManager.
   */
  public RendererManager()
  {
    this(UIConstants.FACET_DEFAULT);
  }

  /**
   * Creates a new, empty RendererManager.
   */
  public RendererManager(String facet)
  {
    _facet = facet;
  }

  public String getFacet()
  {
    return _facet;
  }

  /**
   * Returns a globally shared instance of RendererManager.  This
   * instance is initialized with the Oracle  browser-based
   * look-and-feel renderer factory and the HTML renderer factory.
   * <p>
   * @deprecated RendererManagers should be retrieved from the
   *             LookAndFeel returned from the LookAndFeelManager
   * <p>
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager#getLookAndFeel
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel#getRendererManager
   */
  @Deprecated
  static public RendererManager getDefaultRendererManager()
  {
    if (_sDefaultInstance == null)
      _sDefaultInstance = createDefaultRendererManager();

    return _sDefaultInstance;
  }


  /**
   * Returns a new RendererManager, initialized with the UIX
   * browser-based look-and-feel renderer factory and the HTML
   * renderer factory.
   * <p>
   * @deprecated RendererManagers should be retrieved from the
   *             LookAndFeel returned from the LookAndFeelManager
   * <p>
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager#getLookAndFeel
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel#getRendererManager
   */
  @Deprecated
  static public RendererManager createDefaultRendererManager()
  {
    return new BaseDesktopLookAndFeel().getRendererManager();
  }


  /**
   * Utility method for retrieving a renderer for a UINode.
   */
  final public Renderer getRenderer(UINode node)
  {
    RendererFactory factory = getFactory(node.getNamespaceURI());
    if (factory == null)
      return null;
    return factory.getRenderer(node.getLocalName());
  }


  /**
   * Utility method for retrieving a renderer by both
   * namespace and name.
   */
  final public Renderer getRenderer(String namespace, String name)
  {
    RendererFactory factory = getFactory(namespace);
    if (factory == null)
      return null;
    return factory.getRenderer(name);
  }


  /**
   * Gets the factory registered for the namespace.
   */
  public RendererFactory getFactory(String namespace)
  {
    return (RendererFactory) ArrayMap.get(_factories, namespace);
  }


  /**
   * Registers a factory for a namespace.
   */
  synchronized public void registerFactory(
     String namespace, RendererFactory factory)
  {
    _factories = ArrayMap.put(_factories,
                              namespace.intern(),
                              factory);
  }


  /**
   * Unregisters a factory for a namespace.
   */
  synchronized public void unregisterFactory(String namespace)
  {
    _factories = ArrayMap.remove(_factories, namespace);
  }

  private Object[]  _factories;
  private String    _facet;
  static private RendererManager _sDefaultInstance;
}
