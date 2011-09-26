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

import java.util.Map;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;

import org.apache.myfaces.trinidadinternal.style.StyleContext;

import org.apache.myfaces.trinidadinternal.ui.expl.UIVariableResolver;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;
import org.apache.myfaces.trinidadinternal.ui.path.Path;


/**
 * Context for a single rendering pass over a tree of UINodes.
 * RendererContexts do not last longer than a single rendering, so
 * any state stored on the context will not be persistent.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/RenderingContext.java#0 $) $Date: 10-nov-2005.18:50:20 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface UIXRenderingContext extends Cloneable
{
  /**
   * This property is used to retrieve the file system path
   * corresponding to the root of the current servlet context.
   * ServletRenderingContext will automatically provide this
   * value.  Clients that need to explicitly set this value
   * must also set the CONTEXT_URI_PROPERTY to the correct
   * corresponding value, but such cients should instead use
   * a non-default Configuration object.
   */
  public static final Object CONTEXT_PATH_PROPERTY = "contextPath";

  /**
   * This property is used to retrieve the URI
   * corresponding to the root of the current servlet context.
   * ServletRenderingContext will automatically provide this
   * value.  Clients that need to explicitly set this value
   * must also set the CONTEXT_PATH_PROPERTY to the correct
   * corresponding value, but such cients should instead use
   * a non-default Configuration object.  This value must
   * not be terminated with a "/".
   */
  public static final Object CONTEXT_URI_PROPERTY  = "contextURI";

  public FacesContext getFacesContext();

  /**
   * Returns the LookAndFeel to use for this render.
   */
  public LookAndFeel getLookAndFeel();

  /**
   * Returns the Skin to use for this render.
   */
  public Skin getSkin();
  
  /**
   * Returns the RendererManager that should be used
   * for finding renderers.
   */
  public RendererManager getRendererManager();

  
  /**
   * Returns the ResponseWriter that should be used 
   * for rendering text.
   */
  public ResponseWriter getResponseWriter();
  

  /**
   * Sets the ResponseWriter that should be used
   * for rendering text.
   */
  public void setResponseWriter(ResponseWriter writer);

  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   * For example, if the renderer uses a new HideShowBean, it will need
   * to map the HideShowBean's keys to its keys. It can store the map
   * here, so that context.getTranslatedValue(key) can use this map to get
   * the correct translated value key.
   * @param mapping 
   */
  public void setSkinResourceKeyMap(Map<String, String> mapping);

  
  /**
   * Get the translatedValue Map.
   * @param mapping 
   */ 
  public Map<String, String> getSkinResourceKeyMap();
  
  /**
   * Returns a translated value from the skin.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   */ 
  public Object getTranslatedValue(String key);
  
  /**
   * Returns a translated string from the skin.
   */   
  public String getTranslatedString(String key);
  
  public Icon getIcon(String  iconName);

  public Object getStyleClass(String key);

  
  /**
   * Returns an Agent object that can be used to identify
   * what device will display the rendering.
   */
  public TrinidadAgent getAgent();

  /**
   * Returns the LocaleContext that should be used for rendering.
   * The LocaleContext is the owner of all LocaleSpecific information
   * about the Locale.
   */
  public LocaleContext getLocaleContext();


  /**
   * Returns the number of logical nodes in the path form the current node to
   * the root of the tree of nodes being rendered.
   * <p>
   * @see #getAncestorNode
   * @see #getRenderedAncestorNodeCount
   * @see #getPath
   */
  public int getAncestorNodeCount();

  /**
   * Returns a logical ancestor of the node currently being processed.  The
   * zero-based index from least to most distant -
   * <code>getAncestorNode(0)</code> will always return the current
   * node.  Returns null if the index is greater than or equal to
   * the number of ancestors.
   * <p>
   * @see #getAncestorNodeCount
   * @see #getRenderedAncestorNode
   * @see #getPath
   */
  public UINode getAncestorNode(int index);

  /**
   * Returns the number of rendered nodes in the path form the current
   * node being rendered to the root of the tree of nodes being rendered.
   * <p>
   * Because a single logical node may be transformed into multiple
   * nodes for rendering, the rendered node count will always be equal
   * to or greater than the logical node count.
   * <p>
   * Typically, Renderer implementations are the only callers to this method.
   * <p>
   * @see #getAncestorNodeCount
   */
  public int getRenderedAncestorNodeCount();

  /**
   * Returns an ancestor of the node currently being processed.  The
   * zero-based index from least to most distant -
   * <code>getRenderedAncestorNode(0)</code> will always return the current
   * rendering node.  Returns null if the index is greater than or equal to
   * the number of ancestors.
   * <p>
   * Because a single logical node may be transformed into multiple
   * nodes for rendering, the rendered node count will always be equal
   * to or greater than the logical node count.
   * <p>
   * Typically, Renderer implementations are the only callers to this method.
   * <p>
   * @see #getAncestorNode
   */
  public UINode getRenderedAncestorNode(int index);


  /**
   * Returns the path to the current logical node.
   * <p>
   * @see #getAncestorNodeCount
   * @see #getAncestorNode
   */
  public Path getPath();


  /**
   * Adds the logical UINode to render to the logical stack.
   * Clients should never call this method. They should instead
   * subclass BaseRenderer, which calls this method when needed.
   * <p>
   * @see #popChild
   * @see #getAncestorNode
   * @see #getAncestorNodeCount
   * @see #getPath
   */
  public void pushChild(
    UINode child,
    String childName,
    int    childIndex);

  /**
   * Removes a logical UINode from the stack.  Clients should never call this
   * method;  they should instead subclass BaseRenderer, which calls
   * this method when needed.
   * <p>
   * @see #pushChild
   * @see #getAncestorNode
   * @see #getAncestorNodeCount
   * @see #getPath
   */
  public void popChild();

  /**
   * Adds the UINode to actually use to render to the rendered UINode stack.
   * <p>
   * Clients shuld never need to call this method as calls to this method
   * are typically made on their behalf by the composite UINode framework.
   * <p>
   * @param currentContext The RenderingContext that is used when rendering
   *                       the specified child node.
   * @param renderedChild The UINode that is being rendered.
   *
   * @see #popRenderedChild
   * @see #getRenderedAncestorNode
   * @see #getRenderedAncestorNodeCount
   */
  public void pushRenderedChild(
    UIXRenderingContext currentContext,
    UINode renderedChild
    );

  /**
   * Removes the UINode to actually use to render from the rendered UINode
   * stack.
   * <p>
   * Clients shuld never need to call this method as calls to this method
   * are typically made on their behalf by the composite UINode framework.
   * <p>
   * @param currentContext  The current RenderingContext.  That is, the
   *                        context that was used to render the UINode
   *                        that is being popped from the rendered child
   *                        stack.
   * @see #pushRenderedChild
   * @see #getRenderedAncestorNode
   * @see #getRenderedAncestorNodeCount
   */
  public void popRenderedChild(UIXRenderingContext currentContext);


  /**
   * Returns the DataObject for the given namespace and name pair.
   * It will call each data provider added with addDataProvider,
   * starting with the most recently added, until one returns non-null.
   * It can then check for any implementation-specific DataObjects.
   * @see #addDataProvider
   */
  public DataObject getDataObject(
    String namespaceURI,
    String name);

  /**
   * Returns a DataObject for the current node (such as a row
   * of a table).
   * <p>
   * @see#setCurrentDataObject
   */
  public DataObject getCurrentDataObject();

  /**
   * Sets the new currentDataObject, returning the current
   * currentDataObject.
   * <p>
   * @see #getCurrentDataObject
   */
  public DataObject setCurrentDataObject(DataObject newDataObject);

  /**
   * gets the VariableResolver to use for this render cycle
   */
  public UIVariableResolver getVariableResolver();

  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(String namespace, Object key);

  /**
   * Stores a property on the context.  Since RendererContexts
   * are not persistent, state stored on a RendererContext will
   * not be present in subsequent rendering passes.
   */
  public void setProperty(String namespace, Object key, Object value);

  /**
   * Sets a property on the stack frame of the currently rendering UINode
   * using a key.
   * <p>
   * This method is for use by rendering code that needs to save some
   * state while it is rendering.
   * <p>
   * @param key Key used to identify this property on the stack frame.
   * @param value Value to store.  <code>null</code> is an acceptable value.
   * <p>
   * @see #getLocalProperty
   */
  public void setLocalProperty(Object key, Object value);

  /**
   * Retrieves the specifed property from the stack frame of a
   * rendering UINode.  If the property is not present,
   * <code>defaultValue</code> is returned.
   * <p>
   * The keys are compared by identity for performance.
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
    int ancestorIndex, Object key, Object defaultValue);


  /**
   * @return the encoder that must be used for form values and client
   *  parameters added by JavaScript.
   */
  public FormEncoder getFormEncoder();

  /**
   * @return the encoder that must be used for form parameter names and
   *  URLs
   */
  public URLEncoder getURLEncoder();

  /**
   * Returns a Configuration object that will be used to
   * locate paths and return global properties.
   */
  public Configuration getConfiguration();

  /**
   * Get an interface that can be used for image lookups and rendering.
   */
  public ImageContext getImageContext();

  /**
   * Get an interface that can be used for style lookups and generation.
   */
  public StyleContext getStyleContext();

  /**
   * Returns the RenderingContext that this context should delegate calls
   * to <code>pushRenderedChild</code>, <code>popRenderedChild</code>,
   * and <code>addDataProvider</code> to.  The parent context acts to
   * segregate the logical node structure internal to a composite UINode
   * from the deceloper's logical UINode structure.  By calling
   * <code>getParentContext.getAncestorNode(0)</code> the composite UINode
   * gains access to the UINode that the composite UINode is attempting
   * to render.
   * <p>
   * Typically, only composite UINode implementors ever need to call this
   * method.
   */
  public UIXRenderingContext getParentContext();


  public PartialPageContext getPartialPageContext();
 
  /**
   * Internal version of getDataObject().  <em>Do not call this
   * function</em> unless you are yourself a RenderingContext.
   */
  public DataObject getDataObject(
    UIXRenderingContext outerContext,
    String namespaceURI,
    String name);

  /**
   * Returns a clone of the RenderingContext.
   */
  public Object clone();
}
