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
package org.apache.myfaces.trinidadinternal.ui.composite;

import java.util.Map;
import java.util.MissingResourceException;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;


import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

import org.apache.myfaces.trinidad.context.PartialPageContext;

import org.apache.myfaces.trinidadinternal.style.StyleContext;

import org.apache.myfaces.trinidadinternal.ui.LogicalNodeRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.expl.UIVariableResolver;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/CompositeRenderingContext.java#0 $) $Date: 10-nov-2005.18:56:50 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class CompositeRenderingContext extends LogicalNodeRenderingContext
{
  /**
   * Get a CompositeRenderingContext that delegates to the
   * <code>parentContext</code>
   * <p>
   * @see #recycleContext
   */
  static synchronized CompositeRenderingContext
                             __getCompositeRenderingContext(
    UIXRenderingContext parentContext
    )
  {
    CompositeRenderingContext currContext = _sContexts;

    if (currContext == null)
    {
      currContext = new CompositeRenderingContext();
    }
    else
    {
      // the list has one fewer Runnable
      _sCurrCompositeCount--;

      _sContexts = currContext._nextContext;

      // unlink from list
      currContext._nextContext = null;
    }

    // initialize
    currContext.initialize(parentContext);

    return currContext;
  }


  /**
   * Recycles the CompositeRenderingContext for reuse.
   * <p>
   * @see #getCompositeRenderingContext
   */
  static synchronized void __recycleContext(
    CompositeRenderingContext context
    )
  {
    if (context == null)
      throw new IllegalArgumentException();

    // return the context to the list if there is room
    if (_sCurrCompositeCount < _MAX_COMPOSITE_COUNT)
    {
      // the list has another Context
      _sCurrCompositeCount++;

      //
      // link returned context to head of available list
      //
      context._nextContext = _sContexts;
      _sContexts = context;
    }

    // reset the contents of the context
    context.reset();
  }


  /**
   * Creates a CompositeRenderingContext.
   */
  protected CompositeRenderingContext()
  {
    super();
  }

  protected void initialize(
    UIXRenderingContext parentContext
    )
  {
    if (parentContext == null)
      throw new NullPointerException();

    _parentContext = parentContext;
    _nonCompositeContext = _getNonCompositeContext(parentContext);

    // Reset the data scope to the default (private)
    __setGlobalCurrentDataObject(false);

    setCurrentDataObject(parentContext.getCurrentDataObject());
  }

  @Override
  protected void reset()
  {
    super.reset();

    // release references
    _parentContext = null;
    _nonCompositeContext = null;
    _skinResourceKeyMap = null;
  }

  @Override
  public FacesContext getFacesContext()
  {
    return _nonCompositeContext.getFacesContext();
  }


  /**
   * Returns the LookAndFeel that should be used when rendering.
   */
  public LookAndFeel getLookAndFeel()
  {
    return _nonCompositeContext.getLookAndFeel();
  }

  /**
   * Returns the Skin that should be used when rendering.
   */
  public Skin getSkin()
  {
    return _nonCompositeContext.getSkin();
  }

  /**
   * Returns the RendererManager that should be used
   * for finding renderers.
   */
  public RendererManager getRendererManager()
  {
    return _nonCompositeContext.getRendererManager();
  }

  /**
   * Returns the ResponseWriter that should be used
   * for rendering text.
   */
  public ResponseWriter getResponseWriter()
  {
    return _nonCompositeContext.getResponseWriter();
  }

  public void setResponseWriter(ResponseWriter writer)
  {
    _nonCompositeContext.setResponseWriter(writer);
  }

  /**
   * Returns an Agent object that can be used to identify
   * what device will display the rendering.
   */
  public TrinidadAgent getAgent()
  {
    return _nonCompositeContext.getAgent();
  }

  /**
   * Returns the LocaleContext that should be used for rendering.
   */
  public LocaleContext getLocaleContext()
  {
    return _nonCompositeContext.getLocaleContext();
  }

  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   * For example, if the renderer uses a new HideShowBean, it will need
   * to map the HideShowBean's keys to its keys. It can store the map
   * here, so that context.getTranslatedValue(key) can use this map to get
   * the correct translated value key.
   * @param mapping
   */
  public void setSkinResourceKeyMap(Map<String, String> mapping)
  {
    _skinResourceKeyMap = mapping;
  }

  /**
   * Get the _skinResourceKeyMap Map.
   * @param mapping
   */
  public Map<String, String> getSkinResourceKeyMap()
  {
    return _skinResourceKeyMap;
  }

  /**
   * Returns the number of rendered nodes in the path form the current
   * node being rendered to the root of the tree of nodes being rendered.
   */
  public int getRenderedAncestorNodeCount()
  {
    return _nonCompositeContext.getRenderedAncestorNodeCount();
  }


  /**
   * Returns an ancestor of the node currently being processed.  The
   * zero-based index from least to most distant -
   * <code>getRenderedAncestor(0)</code> will always return the current
   * rendering node.  Returns null if the index is greater than or equal to
   * the number of ancestors.
   */
  public UINode getRenderedAncestorNode(
    int index
    )
  {
    return _nonCompositeContext.getRenderedAncestorNode(index);
  }

  @Override
  public void pushRenderedChild(
    UIXRenderingContext currentContext,
    UINode renderedChild
    )
  {
    // push the child onto the stack of rendered children
    getParentContext().pushRenderedChild(currentContext, renderedChild);

    // handle initialization of dataobjects
    super.pushRenderedChild(currentContext, renderedChild);
  }

  @Override
  public void popRenderedChild(UIXRenderingContext currentContext)
  {
    // handle cleaning up any of the dataproviders
    super.popRenderedChild(currentContext);

    // pop the child off of the stack of rendered children
    getParentContext().popRenderedChild(currentContext);
  }



  /**
   * Returns the DataObject for the given namespace and name pair.
   */
  @Override
  public DataObject getDataObject(
    UIXRenderingContext context,
    String namespaceURI,
    String name
    )
  {
    // give our dataproviders first crack at the dataobject
    DataObject data = super.getDataObject(context, namespaceURI, name);

    if (data == null)
    {
      data = getParentContext().getDataObject(context, namespaceURI, name);
    }

    return data;
  }


  /**
   * Returns a DataObject for the current node (such as a row
   * of a table).
   */
  @Override
  public DataObject getCurrentDataObject()
  {
    if (_globalCurrentDataObject)
      return getParentContext().getCurrentDataObject();
    return super.getCurrentDataObject();
  }

  /**
   * Sets the new currentDataObject, returning the current
   * currentDataObject.
   * <p>
   * @see #getCurrentDataObject
   */
  @Override
  public DataObject setCurrentDataObject(
    DataObject newDataObject
    )
  {
    if (_globalCurrentDataObject)
      return getParentContext().setCurrentDataObject(newDataObject);
    return super.setCurrentDataObject(newDataObject);
  }


  /**
   * gets the variableResolver to use with the current render cycle
   */
  public final UIVariableResolver getVariableResolver()
  {
    return getParentContext().getVariableResolver();
  }

  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(
    String namespace,
    Object key
    )
  {
    return _nonCompositeContext.getProperty(namespace, key);
  }


  /**
   * Stores a property on the context.  Since RendererContexts
   * are not persistent, state stored on a RendererContext will
   * not be present in subsequent rendering passes.
   */
  public void setProperty(
    String namespace,
    Object key,
    Object value
    )
  {
    _nonCompositeContext.setProperty(namespace, key, value);
  }


  /**
   * Sets a property on the stack frame of the currently rendering UINode.
   * <p>
   * This method is for use by rendering code that needs to save some
   * state while it is rendering.
   * <p>
   * @param key Key used to identify this property on the stack frame.
   * @param value Value to store.  <code>null</code> is an acceptable value.
   * <p>
   * @see #getLocalProperty
   */
  public void setLocalProperty(
    Object key,
    Object value
    )
  {
    _nonCompositeContext.setLocalProperty(key, value);
  }

  /**
   * Retrieves the specifed property from the stack frame of a
   * rendering UINode.  If the property is not present,
   * <code>defaultValue</code> is returned.
   * <p>
   * This method is for use by rendering code that needs to save some
   * state while it is rendering.
   * <p>
   * @param ancestorIndex index into the rendered ancestor stack.  If 0,
   *                uses the currently rendering node
   * @param key Key used to identify the property on the stack frame to
   *            retrieve.
   * @param defaultValue Value to return if the property doesn't exist
   *                     in the stack frame.
   * <p>
   * @see #setLocalProperty
   */
  public Object getLocalProperty(
    int    ancestorIndex,
    Object key,
    Object defaultValue
    )
  {
    return _nonCompositeContext.getLocalProperty(ancestorIndex,
                                               key,
                                               defaultValue);
  }

  /**
   * Returns a translated value from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */
  public Object getTranslatedValue(String key)
  {
    String mappedKey = getSkinResourceMappedKey(key);

    if (mappedKey != null)
    {
      try{
        return getParentContext().getTranslatedValue(mappedKey);
      }
      catch (MissingResourceException e)
      {
        // log the error and return
        _LOG.severe(e);

        return null;
      }
    }
    else
      return null;

  }

  /**
   * Returns a translated String from the skin's resource bundle.
   * Logs a severe message if there is a MissingResourceException.
   */
  public String getTranslatedString(String key)
  {
    String mappedKey = getSkinResourceMappedKey(key);

    if (mappedKey != null)
    {
      try{
        return getParentContext().getTranslatedString(mappedKey);
      }
      catch (MissingResourceException e)
      {
        // log the error and return
        _LOG.severe(e);

        return null;
      }
    }
    else
      return null;

  }

  public Icon getIcon(
    String  iconName
    )
  {
    String mappedKey = getSkinResourceMappedKey(iconName);

    if (mappedKey != null)
    {
      return getParentContext().getIcon(mappedKey);
    }
    else
      return null;
  }

  public Object getStyleClass(String  key)
  {
    String mappedKey = getSkinResourceMappedKey(key);

    if (mappedKey != null)
    {
      return getParentContext().getStyleClass(mappedKey);
    }
    else
      return null;
  }


  /**
   * @see UIXRenderingContext#getFormEncoder()
   */
  public FormEncoder getFormEncoder()
  {
    return _nonCompositeContext.getFormEncoder();
  }

  /**
   * @see UIXRenderingContext#getURLEncoder()
   */
  public URLEncoder getURLEncoder()
  {
    return _nonCompositeContext.getURLEncoder();
  }

  /**
   * Returns a Configuration object that will be used to
   * locate paths and return global properties.
   */
  public Configuration getConfiguration()
  {
    return _nonCompositeContext.getConfiguration();
  }

  /**
   * Get an interface that can be used for image lookups and rendering.
   */
  public ImageContext getImageContext()
  {
    return _nonCompositeContext.getImageContext();
  }

  /**
   * Get an interface that can be used for style lookups and generation.
   */
  public StyleContext getStyleContext()
  {
    return _nonCompositeContext.getStyleContext();
  }

  public UIXRenderingContext getParentContext()
  {
    return _parentContext;
  }


  public PartialPageContext getPartialPageContext()
  {
    return _nonCompositeContext.getPartialPageContext();
  }

  @Override
  public Object clone()
  {
    CompositeRenderingContext context = (CompositeRenderingContext)super.clone();

    // clone the delegated context
    context._parentContext = (UIXRenderingContext)_parentContext.clone();

    return context;
  }


  void __setGlobalCurrentDataObject(boolean b)
  {
    _globalCurrentDataObject = b;
  }


  private UIXRenderingContext _getNonCompositeContext(UIXRenderingContext parent)
  {
    while (parent instanceof CompositeRenderingContext)
    {
      parent = ((CompositeRenderingContext) parent).getParentContext();
    }

    assert (parent != null);

    return parent;
  }

  private static final int _MAX_COMPOSITE_COUNT = 200;

  // linked list of available rendering contexts
  private static CompositeRenderingContext _sContexts;
  private static int _sCurrCompositeCount;

  private CompositeRenderingContext _nextContext;
  private UIXRenderingContext _parentContext;
  private UIXRenderingContext _nonCompositeContext;
  private Map<String, String> _skinResourceKeyMap;

  private boolean _globalCurrentDataObject;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CompositeRenderingContext.class);


}

