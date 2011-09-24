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
package org.apache.myfaces.trinidad.component;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;

import java.net.URL;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.el.ELContext;
import javax.el.ELException;
import javax.el.MethodExpression;
import javax.el.ValueExpression;

import javax.faces.FacesException;
import javax.faces.application.ProjectStage;
import javax.faces.component.ContextCallback;
import javax.faces.component.NamingContainer;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.behavior.ClientBehaviorHolder;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ComponentSystemEvent;
import javax.faces.event.ComponentSystemEventListener;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.event.PostAddToViewEvent;
import javax.faces.event.PreRemoveFromViewEvent;
import javax.faces.event.PreRenderComponentEvent;
import javax.faces.event.SystemEvent;
import javax.faces.event.SystemEventListener;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFComponent;
import org.apache.myfaces.trinidad.bean.AttachedObjects;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanFactory;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.bean.util.ValueMap;
import org.apache.myfaces.trinidad.change.AttributeComponentChange;
import org.apache.myfaces.trinidad.change.RowKeySetAttributeChange;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.AttributeChangeEvent;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.render.ExtendedRenderer;
import org.apache.myfaces.trinidad.render.LifecycleRenderer;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;


/**
 * Base implementation of components for all of Trinidad.  UIXComponentBase
 * offers a number of features not supplied by the standard UIComponentBase
 * class:
 * <ul>
 * <li>Use of FacesBean for better and easier state saving</li>
 * <li>Support of the LifecycleRenderer class for greater Renderer
 *  control over the lifecycle</li>
 * <li>Built-in support for both the "partialTriggers" attribute
 *   (declarative support for being a PPR target) and for triggering
 *   such components (for being a the source of a PPR-causing event).</li>
 * </ul>
 * <h3>FacesBean and UIXComponentBase</h3>
 * <p>
 * UIXComponentBase differs from UIXComponent most particularly
 * in its use of FacesBeans to store all state.  This offers
 * a number of advantages:
 * <ul>
 * <li>Subclassers - if they use FacesBean for their state as well -
 *   do not need to write overrides of saveState() and restoreState().
 *   </li>
 * <li>State is optimized by default</li>
 * <li>Future optimizations - partly exposed today with
 *    markInitialState() - can offer major state saving improvements.
 * </ul>
 * </p>
 */
// TODO Write Class Javadoc
// TODO Thorough review against UIComponentBase
@JSFComponent
abstract public class UIXComponentBase extends UIXComponent
{
  // Created up top to ensure it's present while we're processing
  // class initialization code.
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXComponentBase.class);

  static public final FacesBean.Type TYPE = _createType();
  static public final PropertyKey ID_KEY =
    TYPE.registerKey("id", String.class, PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey RENDERED_KEY =
    TYPE.registerKey("rendered", Boolean.class, Boolean.TRUE);
  static public final PropertyKey BINDING_KEY =
    TYPE.registerKey("binding");
  static public final PropertyKey TRANSIENT_KEY =
    TYPE.registerKey("transient", Boolean.class,
                     PropertyKey.CAP_NOT_BOUND |
                     PropertyKey.CAP_TRANSIENT);
  static public final PropertyKey RENDERER_TYPE_KEY =
    TYPE.registerKey("rendererType", String.class, PropertyKey.CAP_NOT_BOUND);
  static private final PropertyKey _LISTENERS_KEY =
    TYPE.registerKey("listeners", FacesListener[].class, PropertyKey.CAP_LIST);
  static private final PropertyKey _ATTRIBUTE_CHANGE_LISTENER_KEY =
    TYPE.registerKey("attributeChangeListener", MethodExpression.class);
  static private final PropertyKey _CLIENT_BEHAVIORS_KEY =
    TYPE.registerKey("clientBehaviors", AttachedObjects.class,
                     PropertyKey.CAP_NOT_BOUND|PropertyKey.CAP_PARTIAL_STATE_HOLDER|PropertyKey.CAP_STATE_HOLDER);
  static private final PropertyKey _SYSTEM_EVENT_LISTENERS_KEY =
    TYPE.registerKey("systemEventListeners", AttachedObjects.class,
                     PropertyKey.CAP_NOT_BOUND|PropertyKey.CAP_PARTIAL_STATE_HOLDER|PropertyKey.CAP_STATE_HOLDER);
  // =-=AEW "parent", "rendersChildren", "childCount", "children",
  // "facets", "facetsAndChildren", "family" all are technically
  // bean properties, but they aren't exposed here...

  static
  {
    // Register a couple of PropertyKeys against names that
    // the RI's UIComponentTag implementation is shoving
    // into all components.  This is purely an optimization, but
    // a very useful one.
    TYPE.registerKey("javax.faces.webapp.COMPONENT_IDS",
                     List.class,
                     PropertyKey.CAP_NOT_BOUND);
    TYPE.registerKey("javax.faces.webapp.FACET_NAMES",
                     List.class,
                     PropertyKey.CAP_NOT_BOUND);
    TYPE.lock();
  }

  public UIXComponentBase()
  {
  }

  public UIXComponentBase(String rendererType)
  {
    setRendererType(rendererType);
  }

  protected FacesBean createFacesBean(
    String rendererType)
  {
    FacesBean bean = FacesBeanFactory.createFacesBean(getClass(),
                                                      rendererType);
    UIXFacesBean uixBean = (UIXFacesBean) bean;
    uixBean.init(this, getBeanType());
    return uixBean;
  }

  protected PropertyKey getPropertyKey(String name)
  {
    PropertyKey key = getBeanType().findKey(name);
    if (key == null)
      key = PropertyKey.createPropertyKey(name);

    return key;
  }

  protected FacesBean.Type getBeanType()
  {
    return TYPE;
  }

  @Override
  public FacesBean getFacesBean()
  {
    if (_facesBean == null)
      _init(null);

    return _facesBean;
  }

  @Override
  public String getContainerClientId(FacesContext context, UIComponent child)
  {
    return getContainerClientId(context);
  }

  @Override
  public void addAttributeChangeListener(AttributeChangeListener acl)
  {
    addFacesListener(acl);
  }

  @Override
  public void removeAttributeChangeListener(AttributeChangeListener acl)
  {
    removeFacesListener(acl);
  }

  @Override
  public AttributeChangeListener[] getAttributeChangeListeners()
  {
    return (AttributeChangeListener[])
      getFacesListeners(AttributeChangeListener.class);
  }

  @Override
  public void setAttributeChangeListener(MethodExpression mb)
  {
    setProperty(_ATTRIBUTE_CHANGE_LISTENER_KEY, mb);
  }

  @Deprecated
  public void setAttributeChangeListener(MethodBinding mb)
  {
    setAttributeChangeListener(adaptMethodBinding(mb));
  }

  @Override
  public MethodExpression getAttributeChangeListener()
  {
    return (MethodExpression) getProperty(_ATTRIBUTE_CHANGE_LISTENER_KEY);
  }


  @Override
  public ValueExpression getValueExpression(String name)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);

    // Support standard RI behavior where getValueBinding()
    // doesn't complain about being asked for a ValueBinding -
    // but continue supporting strict behavior at FacesBean layer.
    if (!key.getSupportsBinding())
      return null;

    return getFacesBean().getValueExpression(key);
  }

  @Override
  public void setValueExpression(String name,
                                 ValueExpression expression)
  {
    if (name == null)
      throw new NullPointerException();

    if ((expression != null) && expression.isLiteralText())
    {
      ELContext context =
          FacesContext.getCurrentInstance().getELContext();
      getAttributes().put(name, expression.getValue(context));
    }
    else
    {
      PropertyKey key = getPropertyKey(name);
      getFacesBean().setValueExpression(key, expression);
    }
  }

  /**
   */
  @Override
  public ValueBinding getValueBinding(String name)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);

    // Support standard RI behavior where getValueBinding()
    // doesn't complain about being asked for a ValueBinding -
    // but continue supporting strict behavior at FacesBean layer.
    if (!key.getSupportsBinding())
      return null;

    return getFacesBean().getValueBinding(key);
  }

  @Override
  public void setValueBinding(String name, ValueBinding binding)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);
    getFacesBean().setValueBinding(key, binding);
  }

  @Override
  public Map<String, Object> getAttributes()
  {
    if (_attributes == null)
      _init(null);

    return _attributes;
  }

  @Override
  protected Iterator<UIComponent> getRenderedFacetsAndChildren(
    FacesContext facesContext)
  {
    _cacheRenderer(facesContext);
    return super.getRenderedFacetsAndChildren(facesContext);
  }

  // ------------------------------------------------------------- Properties

  /**
   * Calculates the clientId for the component
   */
  private String _calculateClientId(FacesContext context)
  {
    // the clientId is always at least the id of the current component
    // our implementation of getId() guarantees that this always
    // returns a non-null value
    String clientId = getId();

    // Search for an ancestor that is a naming container
    UIComponent containerComponent = getParent();
    while (null != containerComponent)
    {
      if (containerComponent instanceof NamingContainer)
      {
        String contClientId;

        // Pass additional context information to naming containers which extend UIXComponent:
        if (containerComponent instanceof UIXComponent)
          contClientId = ((UIXComponent)containerComponent).getContainerClientId(context, this);
        else
          contClientId = containerComponent.getContainerClientId(context);

        StringBuilder bld = __getSharedStringBuilder();
        bld.append(contClientId).append(NamingContainer.SEPARATOR_CHAR).append(clientId);
        clientId = bld.toString();
        break;
      }

      containerComponent = containerComponent.getParent();
    }

    Renderer renderer = getRenderer(context);
    if (null != renderer)
      clientId = renderer.convertClientId(context, clientId);

    return clientId;
  }

  @Override
  public String getClientId(FacesContext context)
  {
    if (_isClientIdCachingEnabled())
    {
      String clientId = _clientId;

      if (clientId == null)
      {
        // This should not be called when the parent has not been set. Should it be
        // called, the value will not be re-calculated correctly. This will be the case
        // if someone attempts to invoke this function during facelets view construction.
        if (_parent == null)
        {
          _LOG.warning("INVALID_CALL_TO_GETCLIENTID", getId());
          // Return the value, even if not valid, for backward compatibility
          return _calculateClientId(context);
        }

        clientId = _calculateClientId(context);

        if (_usesFacesBeanImpl)
        {
          _clientId = clientId;
        }
      }
      else if (_isClientIdDebuggingEnabled())
      {
        _warnClientIdCachingConfig(context);

        // for now validate success by checking the cached result against the dynamically
        // generated result
        String realID = _calculateClientId(context);

        if (!clientId.equals(realID))
          throw new IllegalStateException(
            String.format("Cached client id %s for %s doesn't match client id: %s",
              clientId, this, realID));
      }

      return clientId;
    }
    else
    {
      _warnClientIdCachingConfig(context);

      return _calculateClientId(context);
    }
  }

  /**
   * Gets the identifier for the component.  This implementation
   * never returns a null id.
   */
  @Override
  public String getId()
  {
    // determine whether we can use the optimized code path or not
    if (_usesFacesBeanImpl)
    {
      // optimized path

      // make sure that we always have an id
      if (_id == null)
      {
        _id = FacesContext.getCurrentInstance().getViewRoot().createUniqueId();
      }

      return _id;
    }
    else
    {
      // unoptimized path
      FacesBean facesBean = getFacesBean();

      String id = (String)facesBean.getProperty(ID_KEY);

      // make sure that we always have an id
      if (id == null)
      {
        id = FacesContext.getCurrentInstance().getViewRoot().createUniqueId();

        facesBean.setProperty(ID_KEY, id);
      }

      return id;
    }
  }

  /**
   * Sets the identifier for the component.  The identifier
   * must follow a subset of the syntax allowed in HTML:
   * <ul>
   * <li>Must not be a zero-length String.</li>
   * <li>First character must be an ASCII letter (A-Za-z) or an underscore ('_').</li>
   * <li>Subsequent characters must be an ASCII letter or digit (A-Za-z0-9), an underscore ('_'), or a dash ('-').
   * </ul>
   */
  @Override
  public void setId(String id)
  {
    FacesBean facesBean = getFacesBean();

    // if we are using a FacesBeanImpl, then the FacesBean will
    // delegate all calls to set the id back to us and we can store
    // the value localy.  Otehrwise,w e need to store it in
    // the FacesBean
    if (_usesFacesBeanImpl)
    {
      // only validate if the id has actually changed
      if ((_id == null) || !_id.equals(id))
      {
        _validateId(id);
        _id = id;

        // if we're a NamingContainer then changing our id will invalidate the clientIds of all
        // of our children
        if ((_clientId != null) && (this instanceof NamingContainer))
        {
          clearCachedClientIds(this);
        }
      }
    }
    else
    {
      _validateId(id);
      facesBean.setProperty(ID_KEY, id);
    }

    _clientId = null;
  }

  @Override
  abstract public String getFamily();

  @Override
  public UIComponent getParent()
  {
    return _parent;
  }

  /**
   * <p>Set the parent <code>UIComponent</code> of this
   * <code>UIComponent</code>.</p>
   *
   * @param parent The new parent, or <code>null</code> for the root node
   *  of a component tree
   */
  @Override
  public void setParent(UIComponent parent)
  {
    // do we add this component ?
    if (parent != _parent)
    {
      if (parent != null)
      {
        // set the reference
        _parent = parent;
        _resetClientId();

        if (parent.isInView())
        {
          // trigger the ADD_EVENT and call setInView(true)
          // recursive for all kids/facets...
          // Application.publishEvent(java.lang.Class, java.lang.Object)  must be called, passing
          // PostAddToViewEvent.class as the first argument and the newly added component as the second
          // argument.
          _publishPostAddToViewEvent(getFacesContext(), this);
        }
      }
      else
      {
        if (_parent != null && _parent.isInView())
        {
          // trigger the "remove event" lifecycle
          // and call setInView(false) for all children/facets
          // doing this => recursive
          _publishPreRemoveFromViewEvent(getFacesContext(), this);
        }

        // (un)set the reference
        _parent = parent;
        _resetClientId();
      }
    }
  }

  private void _resetClientId()
  {
    // clear cached client ids if necessary
    if (_clientId != null)
    {
      String newClientId = _calculateClientId(FacesContext.getCurrentInstance());

      // if our clientId changed as a result of being reparented (because we moved
      // between NamingContainers for instance) then we need to clear out
      // all of the cached client ids for our subtree
      if (!_clientId.equals(newClientId))
      {
        clearCachedClientIds();
        _clientId = newClientId;
      }
    }
  }

  @Override
  public boolean isRendered()
  {
    return getBooleanProperty(RENDERED_KEY, true);
  }

  @Override
  public void setRendered(boolean rendered)
  {
    setBooleanProperty(RENDERED_KEY, rendered);
  }

  public boolean isTransient()
  {
    return getBooleanProperty(TRANSIENT_KEY, false);
  }

  public void setTransient(boolean newTransient)
  {
    setBooleanProperty(TRANSIENT_KEY, newTransient);
  }

  @Override
  public String getRendererType()
  {
    // Don't create the FacesBean just to get the renderer type;
    // Generally, rendererType will be the first property
    // set, which will trigger the code below in setRendererType()
    // to run correctly.
    if (_facesBean == null)
      return null;

    return (String) getProperty(RENDERER_TYPE_KEY);
  }

  @Override
  public void setRendererType(String rendererType)
  {
    String oldRendererType = getRendererType();
    if (oldRendererType == null)
    {
      if (rendererType == null)
        return;
    }
    else if (oldRendererType.equals(rendererType))
    {
      return;
    }

    // prepare the faces bean
    _init(rendererType);
    setProperty(RENDERER_TYPE_KEY, rendererType);
  }

  @Override
  public boolean getRendersChildren()
  {
    Renderer renderer = getRenderer(getFacesContext());
    if (renderer == null)
      return false;

    return renderer.getRendersChildren();
  }

  // ------------------------------------------------ Tree Management Methods

  @Override
  public UIComponent findComponent(String id)
  {
    if (id == null)
      throw new NullPointerException();

    if ("".equals(id))
      throw new IllegalArgumentException();

    UIComponent from = this;
    // If it starts with the separator character, it's
    // an absolute path: start at the top
    if (id.charAt(0) == NamingContainer.SEPARATOR_CHAR)
    {
      id = id.substring(1);
      while (from.getParent() != null)
        from = from.getParent();
    }
    // If it's a NamingContainer, start right here
    else if (this instanceof NamingContainer)
    {
      ;
    }
    // Otherwise, go up to look for NamingContainer (or the top)
    else
    {
      while (from.getParent() != null)
      {
        from = from.getParent();
        if (from instanceof NamingContainer)
          break;
      }
    }

    // Evaluate each part of the expression
    String searchId;
    int separatorIndex = id.indexOf(NamingContainer.SEPARATOR_CHAR);
    if (separatorIndex < 0)
      searchId = id;
    else
      searchId = id.substring(0, separatorIndex);

    if (searchId.equals(from.getId()))
    {
      // Don't need to look inside if we're already there
      ;
    }
    else
    {
      from = _findInsideOf(from, searchId);
    }

    // Final ID:  break, and return whatever we've found
    if (separatorIndex < 0)
    {
      return from;
    }
    // Just an intermediate step.  Make sure we're at a NamingContainer,
    // and then ask it to find the rest of the expression.
    else
    {
      if (from == null)
        return null;

      if (!(from instanceof NamingContainer))
        throw new IllegalArgumentException();
      return from.findComponent(id.substring(separatorIndex + 1));
    }
  }

  /**
   * <p>Create (if necessary) and return a List of the children associated
   * with this component.</p>
   */
  @Override
  public List<UIComponent> getChildren()
  {
    if (_children == null)
      _children = new ChildArrayList(this);

    return _children;
  }

  @Override
  public int getChildCount()
  {
    if (_children == null)
      return 0;
    else
      return getChildren().size();
  }

  /**
   * <p>Create (if necessary) and return a Map of the facets associated
   * with this component.</p>
   */
  @Override
  public Map<String, UIComponent> getFacets()
  {

    if (_facets == null)
      _facets = new FacetHashMap(this);

    return _facets;
  }

  @Override
  public UIComponent getFacet(String facetName)
  {
    if (facetName == null)
      throw new NullPointerException();

    if (_facets == null)
      return null;
    else
      return getFacets().get(facetName);
  }

  /**
   * Returns an Iterator over the names of all facets.
   * Unlike getFacets().keySet().iterator(), this does
   * not require instantiating a Map if there are
   * no facets.  (Note that this is not part of the
   * UIComponent API.)
   */
  public Iterator<String> getFacetNames()
  {
    if (_facets == null)
      return _EMPTY_STRING_ITERATOR;
    else
      return _facets.keySet().iterator();
  }

  @Override
  public Iterator<UIComponent> getFacetsAndChildren()
  {
    // =-=AEW Is this supposed to be an immutable Iterator?
    if (_facets == null)
    {
      if (_children == null)
        return _EMPTY_UICOMPONENT_ITERATOR;

      return _children.iterator();
    }
    else
    {
      if (_children == null)
        return _facets.values().iterator();
    }

    return new CompositeIterator<UIComponent>(_children.iterator(), _facets.values().iterator());
  }

  // ------------------------------------------- Event processing methods

  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    if (event == null)
      throw new NullPointerException();

    if (_LOG.isFine())
      _LOG.fine("Broadcasting event " + event + " to " + this);

    UIComponent component = event.getComponent();
    if (component != null && satisfiesPartialTrigger(event))
    {
      RequestContext adfContext = RequestContext.getCurrentInstance();
      if (adfContext != null)
        adfContext.partialUpdateNotify(component);
    }

    Iterator<FacesListener> iter =
      (Iterator<FacesListener>)getFacesBean().entries(_LISTENERS_KEY);

    while (iter.hasNext())
    {
      FacesListener listener = iter.next();
      if (event.isAppropriateListener(listener))
      {
        event.processListener(listener);
      }
    }

    if (event instanceof AttributeChangeEvent)
    {
      broadcastToMethodExpression(event, getAttributeChangeListener());
    }
  }

  /**
   * Check if a faces event broadcast to this component should trigger the partial updates of the
   * target listeners of this component. By default, all events trigger a partial update of the listeners.
   *
   * @param event The event to check
   * @return true if the partial triggers should be updated by this event being broadcast
   */
  protected boolean satisfiesPartialTrigger(
    FacesEvent event)
  {
    return true;
  }

  // ------------------------------------------- Lifecycle Processing Methods

  @Override
  public void decode(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    // Find all the partialTriggers and save on the context
    Map<String, Object> attrs = getAttributes();
    Object triggers = attrs.get("partialTriggers");
    if (triggers instanceof String[])
    {
      RequestContext adfContext = RequestContext.getCurrentInstance();
      if (adfContext != null)
        adfContext.addPartialTriggerListeners(this, (String[]) triggers);
    }

    __rendererDecode(context);
  }

  @Override
  public void encodeBegin(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    // Call UIComponent.pushComponentToEL(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
    pushComponentToEL(context, this);

    if (!isRendered())
      return;

    context.getApplication().publishEvent(context,  PreRenderComponentEvent.class, UIComponent.class, this);

    _cacheRenderer(context);
    Renderer renderer = getRenderer(context);

    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.encodeBegin(context, this);
    }
  }

  @Override
  public void encodeChildren(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    Renderer renderer = getRenderer(context);
    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.encodeChildren(context, this);
    }
  }

  @Override
  public void encodeEnd(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    try
    {
      if (isRendered())
      {
        Renderer renderer = getRenderer(context);
        // if there is a Renderer for this component
        if (renderer != null)
        {
          renderer.encodeEnd(context, this);
        }
      }
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  @Override
  public void queueEvent(FacesEvent event)
  {
    if (event == null)
      throw new NullPointerException();

    UIComponent parent = getParent();
    if (parent == null)
      throw new IllegalStateException();

    parent.queueEvent(event);
  }

  // ----------------------------------------------- Lifecycle Phase Handlers

  @Override
  public void processDecodes(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    pushComponentToEL(context, this);

    try
    {
      // Process all facets and children of this component
      decodeChildren(context);

      // Process this component itself
      decode(context);
    }
    finally
    {
      // Call UIComponent.popComponentFromEL(javax.faces.context.FacesContext) from inside of a finally
      // block, just before returning.

      popComponentFromEL(context);
    }
  }

  @Override
  public void processValidators(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    pushComponentToEL(context, this);

    try
    {
      // Process all facets and children of this component
      validateChildren(context);
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  @Override
  public void processUpdates(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    pushComponentToEL(context, this);

    try
    {
      // Process all facets and children of this component
      updateChildren(context);
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  @Override
  public Object processSaveState(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (_LOG.isFiner())
      _LOG.finer("processSaveState() on " + this);

    pushComponentToEL(context, this);

    Object state = null;

    try
    {
      if (((_children == null) || _children.isEmpty()) &&
          ((_facets == null) || _facets.isEmpty()))
      {
        state = saveState(context);
      }
      else
      {
        TreeState treeState = new TreeState();
        treeState.saveState(context, this);
        if (treeState.isEmpty())
          state = null;

        state = treeState;
      }
    }
    catch (RuntimeException e)
    {
      _LOG.warning(_LOG.getMessage("COMPONENT_CHILDREN_SAVED_STATE_FAILED", this));

      throw e;
    }

    finally
    {
      popComponentFromEL(context);
    }

    return state;
  }

  // TODO  will have deep problems if UIComponent.saveState() ever
  //   returns a String.
  // TODO crashes and burns if there are fewer children or missing
  //  facets from when state was saved.
  @Override
  public void processRestoreState(FacesContext context, Object state)
  {
    if (context == null)
      throw new NullPointerException();

    if (_LOG.isFiner())
      _LOG.finer("processRestoreState() on " + this);

    pushComponentToEL(context, this);

    try
    {
      // If we saved a "TreeState", use it to restore everything
      if (state instanceof TreeState)
      {
        ((TreeState) state).restoreState(context, this);
      }
      // Otherwise, we had no children or facets, and just use
      // the "state" object
      else
      {
        restoreState(context, state);
      }
    }
    finally
    {
      popComponentFromEL(context);
    }
  }

  @Override
  public void markInitialState()
  {
    // -= Simon Lessard =-
    // FIXME: Set to true, but never read
    //_initialStateMarked = true;
    getFacesBean().markInitialState();
  }

  @Override
  public void clearInitialState()
  {
    getFacesBean().clearInitialState();
  }

  @Override
  public boolean initialStateMarked()
  {
    return getFacesBean().initialStateMarked();
  }

  public Object saveState(FacesContext facesContext)
  {
    Object state = getFacesBean().saveState(facesContext);

    // if component state serialization checking is on, attempt to Serialize the
    // component state immediately in order to determine which component's state
    // failed state saving.
    if (StateUtils.checkComponentStateSerialization(facesContext))
    {
      try
      {
        new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(state);
      }
      catch (IOException e)
      {
        throw new RuntimeException(_LOG.getMessage("COMPONENT_SAVED_STATE_FAILED", this), e);
      }
    }

    return state;
  }

  public void restoreState(
    FacesContext facesContext,
    Object       stateObj)
  {
    getFacesBean().restoreState(facesContext, stateObj);
  }

  @Override
  public String toString()
  {
    String className = getClass().getName();
    int periodIndex = className.lastIndexOf('.');
    if (periodIndex >= 0)
      className = className.substring(periodIndex + 1);

    return className + "[" + getFacesBean().toString() + ", id=" + getId() + "]";
  }

  /**
   * <p>Return the {@link FacesContext} instance for the current request.</p>
   */
  @Override
  protected FacesContext getFacesContext()
  {
    // If we ever have a way for a component to get notified
    // when it's finished being used for a given request,
    // we could cache this as an instance variable.
    return FacesContext.getCurrentInstance();
  }

  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls decodeChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void decodeChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a HierarchyRenderer for this component
    if (renderer != null)
    {
      if (renderer.decodeChildren(context, this))
        return;
    }

    decodeChildrenImpl(context);
  }

  /**
   * Calls processDecodes on all facets and children of this
   * component.
   * @param context the current FacesContext
   */
  protected void decodeChildrenImpl(FacesContext context)
  {
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processDecodes(context);
    }
  }

  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls validateChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void validateChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a ExtendedRenderer for this component
    if (renderer != null)
    {
      if (renderer.validateChildren(context, this))
        return;
    }

    validateChildrenImpl(context);
  }

  /**
   * Calls processValidators on all facets and children of this
   * component.
   * @param context the current FacesContext
   */
  protected void validateChildrenImpl(FacesContext context)
  {
    // Process all the facets and children of this component
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processValidators(context);
    }
  }

  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls upateChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void updateChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a ExtendedRenderer for this component
    if (renderer != null)
    {
      if (renderer.updateChildren(context, this))
        return;
    }

    updateChildrenImpl(context);
  }

  protected void updateChildrenImpl(FacesContext context)
  {
    // Process all the facets and children of this component
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processUpdates(context);
    }
  }

  @Override
  protected void addFacesListener(FacesListener listener)
  {
    if (listener == null)
      throw new NullPointerException();

    getFacesBean().addEntry(_LISTENERS_KEY, listener);
  }

  @Override
  protected void removeFacesListener(FacesListener listener)
  {
    if (listener == null)
      throw new NullPointerException();

    getFacesBean().removeEntry(_LISTENERS_KEY, listener);
  }

  @Override
  protected FacesListener[] getFacesListeners(Class clazz)
  {
    if (clazz == null)
      throw new NullPointerException();

    if (!FacesListener.class.isAssignableFrom(clazz))
      throw new IllegalArgumentException();

    return (FacesListener[])
       getFacesBean().getEntries(_LISTENERS_KEY, clazz);
  }

  protected void addAttributeChange(
    String attributeName,
    Object attributeValue)
  {
    AttributeComponentChange aa;

    FacesContext context = getFacesContext();

    if (attributeValue instanceof RowKeySet)
    {
      aa = new RowKeySetAttributeChange(getClientId(context),
                                        attributeName,
                                        attributeValue);
    }
    else
    {
      aa = new AttributeComponentChange(attributeName, attributeValue);
    }

    RequestContext adfContext = RequestContext.getCurrentInstance();
    adfContext.getChangeManager().addComponentChange(context, this, aa);
  }

  void __rendererDecode(FacesContext context)
  {
    _cacheRenderer(context);
    Renderer renderer = getRenderer(context);
    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.decode(context, this);
    }
  }

  /**
   * Publish PostAddToViewEvent to the component and all facets and children.
   *
   * @param context the current FacesContext
   * @param component the current UIComponent
   */
  private void _publishPostAddToViewEvent(
    FacesContext context,
    UIComponent component)
  {
    component.setInView(true);
    context.getApplication().publishEvent(context, PostAddToViewEvent.class, UIComponent.class, component);

    if (component.getChildCount() > 0)
    {
      List<UIComponent> children = component.getChildren();
      UIComponent child = null;
      UIComponent currentChild = null;
      int i = 0;
      while (i < children.size())
      {
        child = children.get(i);

        // Iterate over the same index if the component was removed
        // This prevents skip components when processing
        do
        {
          _publishPostAddToViewEvent(context, child);
          currentChild = child;
        }
        while ((i < children.size()) &&
               ((child = children.get(i)) != currentChild) );
        i++;
      }
    }

    if (component.getFacetCount() > 0)
    {
      for (UIComponent child : component.getFacets().values())
      {
        _publishPostAddToViewEvent(context, child);
      }
    }
  }

  /**
   * Publish PreRemoveFromViewEvent to the component and all facets and children.
   *
   * @param context the current FacesContext
   * @param component the current UIComponent
   */
  private void _publishPreRemoveFromViewEvent(
    FacesContext context,
    UIComponent component)
  {
    component.setInView(false);
    context.getApplication().publishEvent(context, PreRemoveFromViewEvent.class, UIComponent.class, component);

    if (component.getChildCount() > 0)
    {
      for (UIComponent child : component.getChildren())
      {
        _publishPreRemoveFromViewEvent(context, child);
      }
    }

    if (component.getFacetCount() > 0)
    {
      for (UIComponent child : component.getFacets().values())
      {
        _publishPreRemoveFromViewEvent(context, child);
      }
    }
  }

  private void _cacheRenderer(FacesContext context)
  {
    Renderer renderer = _getRendererImpl(context);
    _cachedRenderer = renderer;

    // cache the lifecycle renderer
    if (renderer instanceof LifecycleRenderer)
    {
      _cachedLifecycleRenderer = (LifecycleRenderer)renderer;
    }
    else
    {
      _cachedLifecycleRenderer = null;
    }
  }

  private Renderer _getRendererImpl(FacesContext context)
  {
    String rendererType = getRendererType();
    if (rendererType != null)
    {
      RenderKit renderKit = context.getRenderKit();
      Renderer renderer = renderKit.getRenderer(getFamily(), rendererType);
      if (renderer == null)
      {
        _LOG.warning("CANNOT_FIND_RENDERER", new Object[]{this, rendererType});
      }

      return renderer;
    }

    return null;
  }

  private LifecycleRenderer _getLifecycleRendererImpl(FacesContext context)
  {
    Renderer renderer = _getRendererImpl(context);
    if (renderer instanceof LifecycleRenderer)
    {
      return (LifecycleRenderer)renderer;
    }

    return null;
  }

  @Override
  protected Renderer getRenderer(FacesContext context)
  {
    Renderer renderer = _cachedRenderer;
    if (renderer != _UNDEFINED_RENDERER)
      return renderer;

    return _getRendererImpl(context);
  }

  protected LifecycleRenderer getLifecycleRenderer(FacesContext context)
  {
    LifecycleRenderer renderer = _cachedLifecycleRenderer;
    if (renderer != _UNDEFINED_LIFECYCLE_RENDERER)
      return renderer;

    return _getLifecycleRendererImpl(context);

  }

  protected void setProperty(PropertyKey key, Object value)
  {
    getFacesBean().setProperty(key, value);
  }

  protected Object getProperty(PropertyKey key)
  {
    return getFacesBean().getProperty(key);
  }

  protected void setBooleanProperty(PropertyKey key, boolean value)
  {
    getFacesBean().setProperty(key, value ? Boolean.TRUE : Boolean.FALSE);
  }

  protected boolean getBooleanProperty(PropertyKey key, boolean defaultValue)
  {
    Object o = getFacesBean().getProperty(key);
    if (defaultValue)
      return !Boolean.FALSE.equals(o);
    else
      return Boolean.TRUE.equals(o);
  }

  protected void setIntProperty(PropertyKey key, int value)
  {
    getFacesBean().setProperty(key, Integer.valueOf(value));
  }

  protected int getIntProperty(PropertyKey key, int defaultValue)
  {
    Number n = (Number) getFacesBean().getProperty(key);
    if (n == null)
      return defaultValue;

    return n.intValue();
  }

  /**
   * Return the number of facets.  This is more efficient than
   * calling getFacets().size();
   */
  @Override
  public int getFacetCount()
  {
    if (_facets == null)
      return 0;

    return _facets.size();
  }

  /**
   * Broadcast an event to a MethodBinding.
   * This can be used to support MethodBindings such as the "actionListener"
   * binding on ActionSource components:
   * &lt;tr:commandButton actionListener="#{mybean.myActionListener}">
   * @deprecated
   */
  protected final void broadcastToMethodBinding(
    FacesEvent event,
    MethodBinding method) throws AbortProcessingException
  {
    if (method != null)
    {
      try
      {
        FacesContext context = getFacesContext();
        method.invoke(context, new Object[] { event });
      }
      catch (EvaluationException ee)
      {
        // Checking for AbortProcessingExceptions, and unwrapping
        // it if the underlying exception is AbortProcessingExceptions.
        Throwable currentThrowable = ee.getCause();
        while (currentThrowable != null)
        {
          if (currentThrowable instanceof AbortProcessingException)
          {
            throw ((AbortProcessingException)currentThrowable);
          }
          currentThrowable = currentThrowable.getCause();
        }
        throw ee;
      }
    }
  }

  /**
   * Given a MethodBinding, create a MethodExpression that
   * adapts it.
   */
  static public MethodExpression adaptMethodBinding(MethodBinding binding)
  {
    return new MethodBindingMethodExpression(binding);
  }

  /**
   * Broadcast an event to a MethodExpression.
   * This can be used to support MethodBindings such as the "actionListener"
   * binding on ActionSource components:
   * &lt;tr:commandButton actionListener="#{mybean.myActionListener}">
   */
  protected final void broadcastToMethodExpression(
    FacesEvent event,
    MethodExpression method) throws AbortProcessingException
  {
    if (method != null)
    {
      try
      {
        FacesContext context = getFacesContext();
        method.invoke(context.getELContext(), new Object[] { event });
      }
      catch (ELException ee)
      {
        Throwable t = ee.getCause();
        // Unwrap AbortProcessingExceptions
        if (t instanceof AbortProcessingException)
          throw ((AbortProcessingException) t);
        throw ee;
      }
    }
  }

  /**
   * Convenience method to call <code>invokeOnComponent</code> on all of the
   * children of a component, surrounding the invocation with calls to
   * <code>setup/tearDownChildrenVisitingContext</code>.
   * This is useful when a component sometimes optimizes
   * away calling <code>invokeOnComponent</code> on its children.
   * @see UIXComponent#setupChildrenVisitingContext
   * @see UIXComponent#tearDownChildrenVisitingContext
   */
  protected final boolean invokeOnChildrenComponents(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {
    setupChildrenVisitingContext(context);

    boolean found = false;

    try
    {
      Iterator<UIComponent> children = getFacetsAndChildren();

      while (children.hasNext() && !found)
      {
        found = children.next().invokeOnComponent(context, clientId, callback);
      }
    }
    finally
    {
      tearDownChildrenVisitingContext(context);
    }

    return found;
  }

  /**
   * <p>
   * Optimized implementation of <code>invokeOnComponent</code> for NamingContainers.
   * If the clientId isn't within the NamingContainer, invocation of the
   * NamingContainer's children is skipped.
   * </p>
   * <p>Subclasses implementing NamingContainer should override
   * <code>invokeOnComponent</code> and delegate to this method.</p>
   */
  protected final boolean invokeOnNamingContainerComponent(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {
    assert this instanceof NamingContainer : "Only use invokeOnNamingContainerComponent on NamingContainers";

    boolean invokedComponent;

    setupVisitingContext(context);

    try
    {
      String thisClientId = getClientId(context);

      if (clientId.equals(thisClientId))
      {
        pushComponentToEL(context, null);

        try
        {
          // this is the component we want, so invoke the callback
          callback.invokeContextCallback(context, this);
        }
        finally
        {
          popComponentFromEL(context);
        }

        invokedComponent = true;
      }
      else
      {
        // if this is a NamingContainer, only traverse into it if the clientId we are looking for
        // is inside of it
        if ((!clientId.startsWith(thisClientId) ||
            (clientId.charAt(thisClientId.length()) != NamingContainer.SEPARATOR_CHAR)))
        {
          invokedComponent = false;
        }
        else
        {
          // iterate through children.
          // We inline this code instead of calling super in order
          // to avoid making an extra call to getClientId().
          invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
        }
      }
    }
    finally
    {
      // teardown the context now that we have visited the children
      tearDownVisitingContext(context);
    }

    return invokedComponent;
  }

  /**
   * Override to calls the hooks for setting up and tearing down the
   * context before the children are visited.
   * @see #setupVisitingContext
   * @see #tearDownVisitingContext
   */
  @Override
  public boolean invokeOnComponent(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {
    boolean invokedComponent;

    // set up the context for visiting the children
    setupVisitingContext(context);

    try
    {
      String thisClientId = getClientId(context);

      if (clientId.equals(thisClientId))
      {
        pushComponentToEL(context, null);

        try
        {
          // this is the component we want, so invoke the callback
          callback.invokeContextCallback(context, this);
        }
        finally
        {
          popComponentFromEL(context);
        }

        // we found the component
        invokedComponent = true;
      }
      else
      {
        // set up the children visiting context to iterate through children. We inline this
        // code instead of calling super in order
        // to avoid making an extra call to getClientId().
        invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
      }
    }
    finally
    {
      // teardown the context now that we have visited the component
      tearDownVisitingContext(context);
    }

    return invokedComponent;
  }


  @Override
  public void subscribeToEvent(Class<? extends SystemEvent> eventClass, ComponentSystemEventListener componentListener)
  {
    if (eventClass == null)
    {
        throw new NullPointerException("eventClass required");
    }
    if (componentListener == null)
    {
        throw new NullPointerException("componentListener required");
    }

    FacesBean bean = getFacesBean();

    AttachedObjects<Class<? extends SystemEvent>, SystemEventListener> eventStorage =
      (AttachedObjects<Class<? extends SystemEvent>, SystemEventListener>)bean.getProperty(_SYSTEM_EVENT_LISTENERS_KEY);

    if (eventStorage == null)
    {
      eventStorage = new AttachedObjects<Class<? extends SystemEvent>, SystemEventListener>();
      bean.setProperty(_SYSTEM_EVENT_LISTENERS_KEY, eventStorage);
    }

    eventStorage.addAttachedObject(eventClass, new ComponentSystemEventListenerWrapper(componentListener, this));
  }

  @Override
  public void unsubscribeFromEvent(Class<? extends SystemEvent> eventClass,
                                   ComponentSystemEventListener componentListener)
  {
    if (eventClass == null)
    {
        throw new NullPointerException("eventClass required");
    }
    if (componentListener == null)
    {
        throw new NullPointerException("componentListener required");
    }

    FacesBean bean = getFacesBean();

    AttachedObjects<Class<? extends SystemEvent>, SystemEventListener> eventStorage =
      (AttachedObjects<Class<? extends SystemEvent>, SystemEventListener>)bean.getProperty(_SYSTEM_EVENT_LISTENERS_KEY);

    if (eventStorage == null)
    {
      return;
    }

    // ComponentSystemEventListenerWrapper implements equals() to compare listener and component
    eventStorage.removeAttachedObject(eventClass, new ComponentSystemEventListenerWrapper(componentListener, this));
  }

  @Override
  public List<SystemEventListener> getListenersForEventClass(Class<? extends SystemEvent> eventClass)
  {
    FacesBean bean = getFacesBean();

    AttachedObjects<Class<? extends SystemEvent>, SystemEventListener> eventStorage =
      (AttachedObjects<Class<? extends SystemEvent>, SystemEventListener>)bean.getProperty(_SYSTEM_EVENT_LISTENERS_KEY);

    if (eventStorage == null)
    {
      return Collections.emptyList();
    }

    return eventStorage.getAttachedObjectList(eventClass);
  }

  // ------------------------- Client behavior holder methods -------------------------

  /**
   * Utility method to assist sub-classes in the implementation of the
   * {@link javax.faces.component.behavior.ClientBehaviorHolder} interface.
   * <p>This method must only
   * be called by classes that implement the interface, doing otherwise will result in an exception.
   * </p>
   * @param eventName The event name
   * @param behavior The behavior to add
   * @see javax.faces.component.behavior.ClientBehaviorHolder#addClientBehavior(String, ClientBehavior)
   */
  protected void addClientBehavior(
    String         eventName,
    ClientBehavior behavior)
  {
    // This will throw a class cast exception when illegally called by a class that does not
    // implement ClientBehaviorHolder
    Collection<String> events = ((ClientBehaviorHolder)this).getEventNames();

    // This will throw a null pointer exception if the component author did not correctly implement
    // the ClientBehaviorHolder contract which requires a non-empty collection to be returned from
    // getEventNames
    if (!events.contains(eventName))
    {
      return;
    }

    FacesBean bean = getFacesBean();

    AttachedObjects<String, ClientBehavior> behaviors = (
            AttachedObjects<String, ClientBehavior>)bean.getProperty(_CLIENT_BEHAVIORS_KEY);

    if (behaviors == null)
    {
      behaviors = new AttachedObjects<String, ClientBehavior>();
      bean.setProperty(_CLIENT_BEHAVIORS_KEY, behaviors);
    }

    behaviors.addAttachedObject(eventName, behavior);
  }

  // Note, we do not need to provide a default implementation for the event names, as client
  // behavior holder components must provide a non-empty list of event names. UIComponentBase
  // decided to return a non-valid null in their code, but that is only confusing to the user, it
  // is better to not implement the method and force the users to write the method upon interface
  // implementation.
  //protected Collection<String> getEventNames() {}

  /**
   * Utility method to assist sub-classes in the implementation of the
   * {@link javax.faces.component.behavior.ClientBehaviorHolder} interface.
   * <p>This method must only
   * be called by classes that implement the interface, doing otherwise will result in an exception.
   * </p>
   * @see javax.faces.component.behavior.ClientBehaviorHolder#getClientBehaviors()
   * @return Read-only map of the client behaviors for this component
   */
  protected Map<String, List<ClientBehavior>> getClientBehaviors()
  {
    AttachedObjects<String, ClientBehavior> behaviors = (
            AttachedObjects<String, ClientBehavior>)getFacesBean().getProperty(_CLIENT_BEHAVIORS_KEY);

    if (behaviors == null)
    {
      return Collections.emptyMap();
    }

    return behaviors.getAttachedObjectMap();
  }

  /**
   * Utility method to assist sub-classes in the implementation of the
   * {@link javax.faces.component.behavior.ClientBehaviorHolder} interface.
   * <p>This method must only
   * be called by classes that implement the interface, doing otherwise will result in an exception.
   * </p>
   * @return null
   * @see javax.faces.component.behavior.ClientBehaviorHolder#getDefaultEventName()
   */
  protected String getDefaultEventName()
  {
    _ensureClientBehaviorHolder();
    return null;
  }

  private void _ensureClientBehaviorHolder()
  {
    if (!(this instanceof ClientBehaviorHolder))
    {
      throw new IllegalStateException("Component must implement ClientBehaviorHolder in order " +
        "to make use of this method.");
    }
  }
  // ------------------------- End of the client behavior holder methods -------------------------

  /**
   * <p>
   * This gets a single threadlocal shared stringbuilder instance, each time you call
   * __getSharedStringBuilder it sets the length of the stringBuilder instance to 0.
   * </p><p>
   * This allows you to use the same StringBuilder instance over and over.
   * You must call toString on the instance before calling __getSharedStringBuilder again.
   * </p>
   * Example that works
   * <pre><code>
   * StringBuilder sb1 = __getSharedStringBuilder();
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * StringBuilder sb2 = __getSharedStringBuilder();
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   * <br><br>
   * Example that doesn't work, you must call toString on sb1 before
   * calling __getSharedStringBuilder again.
   * <pre><code>
   * StringBuilder sb1 = __getSharedStringBuilder();
   * StringBuilder sb2 = __getSharedStringBuilder();
   *
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   *
   */
  static StringBuilder __getSharedStringBuilder()
  {
    StringBuilder sb = _STRING_BUILDER.get();

    if (sb == null)
    {
      sb = new StringBuilder();
      _STRING_BUILDER.set(sb);
    }

    // clear out the stringBuilder by setting the length to 0
    sb.setLength(0);

    return sb;
  }

  /**
   * render a component. this is called by renderers whose
   * getRendersChildren() return true.
   * @deprecated {@link UIComponent#encodeAll(FacesContext)} should be used instead of this method
   */
  @Deprecated
  void __encodeRecursive(FacesContext context, UIComponent component)
    throws IOException
  {
    component.encodeAll(context);
  }

  static private UIComponent _findInsideOf(
    UIComponent from,
    String id)
  {
    Iterator<UIComponent> kids = from.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      if (id.equals(kid.getId()))
        return kid;

      if (!(kid instanceof NamingContainer))
      {
        UIComponent returned = _findInsideOf(kid, id);
        if (returned != null)
          return returned;
      }
    }

    return null;
  }

  /**
   * <p>Verify that the specified component id is safe to add to the tree.
   * </p>
   *
   * @param id The proposed component id to check for validity
   *
   * @exception IllegalArgumentException if <code>id</code>
   *  is <code>null</code> or contains invalid characters
   */
  private void _validateId(String id)
  {
    if (id == null)
      return;


    int n = id.length();
    if (0 == n ||
        NamingContainer.SEPARATOR_CHAR == id.charAt(0))
      _throwBadId(id);

    for (int i = 0; i < n; i++)
    {
      char c = id.charAt(i);
      if (i == 0)
      {
        if (!Character.isLetter(c) && (c != '_'))
          _throwBadId(id);
      }
      else
      {
        if (!(Character.isLetter(c) ||
              Character.isDigit(c) ||
              (c == '-') || (c == '_')))
        {
          _throwBadId(id);
        }
      }
    }
  }

  private void _throwBadId(String id)
  {
    throw new IllegalArgumentException(_LOG.getMessage(
      "ILLEGAL_ID", id));
  }

  private void _init(
    String rendererType)
  {
    FacesBean oldBean = _facesBean;
    FacesBean newBean = createFacesBean(rendererType);;

    if (oldBean != null)
      newBean.addAll(oldBean);

    _attributes = new ValueMap(newBean);

    _facesBean = newBean;

    // determine whether it is ok to store the attributes locally.  We cache the result since
    // this can be a little involved
    boolean usesFacesBeanImpl = false;

    if (newBean instanceof UIXFacesBeanImpl)
      usesFacesBeanImpl = true;
    else
    {
      // handle the wrapped case
      FacesBean currImpl = newBean;

      while (currImpl instanceof FacesBeanWrapper)
      {
        currImpl = ((FacesBeanWrapper)currImpl).getWrappedBean();

        if (currImpl instanceof UIXFacesBeanImpl)
        {
          usesFacesBeanImpl = true;
          break;
        }
      }
    }

    _usesFacesBeanImpl = usesFacesBeanImpl;
  }

  private FacesBean                _facesBean;
  private List<UIComponent>        _children;
  private Map<String, Object>      _attributes;
  private Map<String, UIComponent> _facets;
  private UIComponent              _parent;
  private String                   _id;
  private String                   _clientId;
  private boolean                  _usesFacesBeanImpl;

  // Cached instance of the Renderer for this component.
  // The instance will be re-retrieved in encodeBegin()
  private transient Renderer _cachedRenderer = _UNDEFINED_RENDERER;
  private transient LifecycleRenderer _cachedLifecycleRenderer =
                                                _UNDEFINED_LIFECYCLE_RENDERER;

  // -= Simon Lessard =-
  // FIXME: _initialStateMarked is never read
  //        So commented out, is that ok? If so, this attribute should be deleted
  //private transient boolean _initialStateMarked;

  private static final Iterator<String> _EMPTY_STRING_ITERATOR = CollectionUtils.emptyIterator();
  private static final Iterator<UIComponent> _EMPTY_UICOMPONENT_ITERATOR =
    CollectionUtils.emptyIterator();

  static private final ThreadLocal<StringBuilder> _STRING_BUILDER =
                                                          ThreadLocalUtils.newRequestThreadLocal();

  static private FacesBean.Type _createType()
  {
    try
    {
      ClassLoader cl = _getClassLoader();
      URL url = cl.getResource("META-INF/faces-bean-type.properties");
      if (url != null)
      {
        Properties properties = new Properties();
        InputStream is = url.openStream();
        try
        {
          properties.load(is);
          String className = (String)
            properties.get(UIXComponentBase.class.getName());
          return (FacesBean.Type) cl.loadClass(className).newInstance();
        }
        finally
        {
          is.close();
        }
      }
    }
    catch (Exception e)
    {
      _LOG.severe("CANNOT_LOAD_TYPE_PROPERTIES", e);
    }

    // For testing purposes, return a valid Type
    return new FacesBean.Type();
  }

  static private ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = FacesBeanFactory.class.getClassLoader();
    return loader;
  }

  static private class RendererImpl extends Renderer
  {
  }

  static private class ExtendedRendererImpl extends ExtendedRenderer
  {
  }

  private static boolean _isClientIdCachingEnabled()
  {
    return (_CLIENT_ID_CACHING != ClientIdCaching.OFF);
  }

  private static boolean _isClientIdDebuggingEnabled()
  {
    return (_CLIENT_ID_CACHING == ClientIdCaching.DEBUG);
  }

  // Warn if caching is disabled + production environment since this is
  // undesirable from a performance perspective.
  private static void _warnClientIdCachingConfig(FacesContext context)
  {
    if (_CLIENT_ID_CACHING != ClientIdCaching.ON &&
         context.isProjectStage(ProjectStage.Production) &&
         !_warnedClientIdCachingConfig(context))
    {
      _LOG.warning(
        "The org.apache.myfaces.trinidad.CLIENT_ID_CACHING system property is set to: " +
         _CLIENT_ID_CACHING +
        ".  For best performance, client id caching should be ON in production environments.");

      _clientIdCachingConfigWarned(context);
    }
  }

  // Tests whether we have already warned about the caching config.
  // We only want to warn once per run.
  private static boolean _warnedClientIdCachingConfig(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Map<String, Object> appMap = external.getApplicationMap();

    return Boolean.TRUE.equals(appMap.get(_WARNED_CLIENT_ID_CACHING_KEY));
  }

  // Marks the fact that we have now warned about the caching config.
  private static void _clientIdCachingConfigWarned(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Map<String, Object> appMap = external.getApplicationMap();

    appMap.put(_WARNED_CLIENT_ID_CACHING_KEY, Boolean.TRUE);
  }

  // Utility for deriving initial CLIENT_ID_CACHING value.
  private static ClientIdCaching _initClientIdCaching()
  {
    String cachingProperty = _getClientIdCachingSystemProperty();
    ClientIdCaching caching = _toClientIdCachingEnum(cachingProperty);

    _LOG.config("Client id caching configuration: " + caching);

    return caching;
  }

  private static String _getClientIdCachingSystemProperty()
  {
    try
    {
      return System.getProperty(_SYSTEM_PROP_CLIENT_ID_CACHING);
    }
    catch (Throwable t)
    {
      _LOG.warning(t);
    }

    return null;
  }

  private static ClientIdCaching _toClientIdCachingEnum(String cachingProperty)
  {
    try
    {
      return Enum.valueOf(ClientIdCaching.class,
                           (cachingProperty == null) ?
                             "ON" :
                             cachingProperty.toUpperCase());
    }
    catch (IllegalArgumentException e)
    {
      _LOG.warning("Invalid value specified for " +
                   _SYSTEM_PROP_CLIENT_ID_CACHING +
                   " system property: " +
                   cachingProperty +
                   ". Valid values are: on | off | debug.");

    }

    return ClientIdCaching.ON;
  }

  // Little enum for tracking the client id caching behavior.
  private enum ClientIdCaching
  {
    ON,
    OFF,
    DEBUG
  }


  private static class ComponentSystemEventListenerWrapper implements SystemEventListener, StateHolder
  {
    ComponentSystemEventListenerWrapper(ComponentSystemEventListener listener, UIComponent component)
    {
      _delegate = listener;
      _componentClass = component.getClass();
    }

    // Default constructor for state restoration
    public ComponentSystemEventListenerWrapper()
    {
    }

    @Override
    public boolean equals(Object o)
    {
      if (o == this)
      {
        return true;
      }
      else if (o instanceof ComponentSystemEventListenerWrapper)
      {
        ComponentSystemEventListenerWrapper other = (ComponentSystemEventListenerWrapper) o;
        return _componentClass.equals(other._componentClass) && _delegate.equals(other._delegate);
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      return _componentClass.hashCode() + _delegate.hashCode();
    }

    // SystemEventListener implementation

    @Override
    public void processEvent(SystemEvent event) throws AbortProcessingException
    {
      assert (event instanceof ComponentSystemEvent);
      _delegate.processEvent((ComponentSystemEvent)event);
    }

    @Override
    public boolean isListenerForSource(Object source)
    {
      if (_delegate instanceof SystemEventListener)
      {
        return ((SystemEventListener)_delegate).isListenerForSource(source);
      }

      // From the spec: and its implementation of SystemEventListener.isListenerForSource(java.lang.Object) must return true
      // if the instance class of this UIComponent is assignable from the argument to isListenerForSource.
      return _componentClass.isAssignableFrom(source.getClass());
    }

    // StateHolder Implementation

    @Override
    public Object saveState(FacesContext context)
    {
      if (_delegate instanceof UIComponent)
      {
        return null;
      }

      Object[] state = new Object[2];
      state[0] = StateUtils.saveStateHolder(context, _delegate);
      state[1] = _componentClass;

      return state;
    }

    @Override
    public void restoreState(FacesContext context, Object state)
    {
      if (state == null)
      {
        return;
      }

      Object[] stateArr = (Object[]) state;
      Object saved = stateArr[0];

      _delegate = (ComponentSystemEventListener) ((saved == null) ? UIComponent .getCurrentComponent(context)
                                                : StateUtils.restoreStateHolder(context, saved));
      _componentClass = (Class<?>)stateArr[1];
    }

    @Override
    public boolean isTransient()
    {
      if (_delegate instanceof StateHolder)
      {
          return ((StateHolder)_delegate).isTransient();
      }
      return false;
    }

    @Override
    public void setTransient(boolean isTransient)
    {
      if (_delegate instanceof StateHolder)
      {
        ((StateHolder)_delegate).setTransient(isTransient);
      }
    }


    private ComponentSystemEventListener _delegate;
    private Class<?> _componentClass;
  }

  private static final ClientIdCaching _CLIENT_ID_CACHING = _initClientIdCaching();

  // System property controlling whether client ID caching is enabled
  private static final String _SYSTEM_PROP_CLIENT_ID_CACHING =
    "org.apache.myfaces.trinidad.CLIENT_ID_CACHING";

  // Application map key indicating that we've already warned once
  // that the client id caching configuration is not optimized for
  // production mode.
  private static final String _WARNED_CLIENT_ID_CACHING_KEY =
    "org.apache.myfaces.trinidad.WARNED_CLIENT_ID_CACHING";

  static private final LifecycleRenderer _UNDEFINED_LIFECYCLE_RENDERER =
                                                new ExtendedRendererImpl();
  static private final Renderer _UNDEFINED_RENDERER = new RendererImpl();
}
