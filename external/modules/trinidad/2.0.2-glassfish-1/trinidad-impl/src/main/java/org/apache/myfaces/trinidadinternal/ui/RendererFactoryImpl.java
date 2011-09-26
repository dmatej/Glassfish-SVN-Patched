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

import java.util.concurrent.ConcurrentHashMap;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * A default implementation of a RendererFactory.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/RendererFactoryImpl.java#0 $) $Date: 10-nov-2005.18:50:18 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RendererFactoryImpl implements RendererFactory
{
  /**
   * Creates an empty RendererFactory.
   */
  public RendererFactoryImpl()
  {
    this(null);
  }

  /**
   * Creates a RendererFactory with a list of Renderers
   * to be lazily instantiated.
   * @param nameClassList an array that alternates
   *   local UINode names with full class names for the renderer
   *   implementation
   */
  public RendererFactoryImpl(String[] nameClassList)
  {
    if (nameClassList != null)
      registerRenderers(nameClassList);
  }

  /**
   * Given a node name, returns the renderer used
   * to render that node.
   * @param name the local name of the UINode
   */
  public Renderer getRenderer(String name)
  {
    Object o = _renderers.get(name);
    if (o instanceof RendererInstantiator)
    {
      // =-=AEW Not thread safe;  the renderer can get instantiated twice
      Renderer renderer = ((RendererInstantiator) o).instantiate();
      registerRenderer(name, renderer);
      return renderer;
    }
    else if (o instanceof Renderer)
    {
      return (Renderer) o;
    }

    return null;
  }

  /**
   * Registers a renderer instance for use with a local name
   * @param name the local name of the UINode
   * @param renderer a Renderer instance
   */
  public void registerRenderer(String name, Renderer renderer)
  {
    if (renderer == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ATTEMP_TO_REGISTER_NULL_RENDERER", name));
    }
    _renderers.put(name, renderer);
  }

  /**
   * Registers a Renderer class name for use with a local name.  The
   * class will be loaded and instantiated the first time the
   * renderer is needed.
   * @param name the local name of the UINode
   * @param className the class name of the Renderer
   */
  public void registerRenderer(String name, String className)
  {
    _renderers.put(name, new ClassRendererInstantiator(className));
  }



  /**
   * Registers a series of renderers for lazy instantiation.
   * @param nameClassList an array that alternates
   *   local UINode names with full class names for the renderer
   *   implementation
   */
  public void registerRenderers(String[] nameClassList)
  {
    for (int i = 0; i < nameClassList.length; i += 2)
    {
      registerRenderer(nameClassList[i], nameClassList[i + 1]);
    }
  }


  /**
   * Removes a renderer.
   * @param name the local name of the UINode
   */
  public void unregisterRenderer(String name)
  {
    _renderers.remove(name);
  }


  private ConcurrentHashMap<String, Object> _renderers = 
    new ConcurrentHashMap<String, Object>(101);
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RendererFactoryImpl.class);
}
