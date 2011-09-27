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
package org.apache.myfaces.trinidad.render;


import java.beans.Beans;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.application.ResourceHandler;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.UIViewRoot;
import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.behavior.ClientBehaviorContext;
import javax.faces.component.behavior.ClientBehaviorHolder;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Basic implementation of the core rendering functionality
 * across render kits.
 */
public class CoreRenderer extends Renderer
{
  // TODO Move elsewhere?
  static public final char CHAR_UNDEFINED = (char) -1;
  static public final int NO_CHILD_INDEX = -1;

  protected CoreRenderer()
  {
  }

  /**
   * Allows the rendered to specify what components should be involved with rendered children
   * life-cycle operations and methods.
   *
   * @param facesContext the faces context
   * @param component the component from which to get the rendered facets and children
   * @see UIXComponentBase#getRenderedFacetsAndChildren(FacesContext)
   * @return A list of components to process as rendered components. Defaults to all facets and
   * children of a component
   */
  public Iterator<UIComponent> getRenderedFacetsAndChildren(
    FacesContext facesContext,
    UIComponent  component)
  {
    return component.getFacetsAndChildren();
  }

  /**
   * <p>
   * Called when visiting the CoreRenderer's component during optimized partial page encoding so
   * that the CoreRenderer can modify what is actually encoded.  For example tab controls often
   * render the tabs for the ShowDetailItems in the tab bar before delegating to the
   * disclosed ShowDetailItem to render the tab content.  As a result, the tab control
   * needs to encode its tab bar if any of its ShowDetailItems are partial targets so that
   * the tab labels, for example, are up-to-date.
   * </p>
   * <p>
   * The default implementation calls the VisitCallback and returns its result if this UIXComponent
   * is a partial target of the current encoding.
   * </p>
   * @param visitContext VisitContext to pass to the VisitCallback
   * @param partialContext PartialPageContext for the current partial encoding
   * @param component The component for the CoreRenderer to visit
   * @param callback VisitCallback to call if this component is a partial target
   * @return The VisitResult controlling continued iteration of the visit.
   */
  public VisitResult partialEncodeVisit(
    VisitContext       visitContext,
    PartialPageContext partialContext,
    UIComponent        component,
    VisitCallback      callback)
  {
    if (partialContext.isPossiblePartialTarget(component.getId()) &&
        partialContext.isPartialTarget(component.getClientId(visitContext.getFacesContext())))
    {
      // visit the component instance
      return callback.visit(visitContext, component);
    }
    else
    {
      // Not visiting this component, but allow visit to
      // continue into this subtree in case we've got
      // visit targets there.
      return VisitResult.ACCEPT;
    }
  }

  /**
   * <p>
   * Called before rendering the current comopnent's children in order to set
   * up any special context.
   * </p>
   * <p>If <code>setupEncodingContext</code> succeeds then
   * <code>tearDownEncodingContext</code> will be called for the same component.
   * </p>
   * <p>The default implementation does nothing</p>
   * @param context FacesContext for this request
   * @param rc RenderingContext for this encoding pass
   * @ param component Component to encode using this Renderer
   * @see #tearDownEncodingContext
   */
  public void setupEncodingContext(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component)
  {
    // TODO Remove after one release
    if (component instanceof UIXComponent)
      setupEncodingContext(context, rc, (UIXComponent)component);

  }

  // TODO Remove after one release
  @Deprecated
  public void setupEncodingContext(
    @SuppressWarnings("unused") FacesContext context,
    @SuppressWarnings("unused") RenderingContext rc,
    @SuppressWarnings("unused") UIXComponent component)
  {
  }

  /**
   * <p>
   * Called before rendering the current component's children in order to set
   * up any special context.
   * </p>
   * <p>If <code>setupChildrenEncodingContext</code> succeeds then
   * <code>tearDownChildrenEncodingContext</code> will be called for the same component.
   * </p>
   * <p>The default implementation does nothing</p>
   * @param context FacesContext for this request
   * @param rc RenderingContext for this encoding pass
   * @param component Component to encode using this Renderer
   * @see #tearDownChildrenEncodingContext
   */
  public void setupChildrenEncodingContext(
    @SuppressWarnings("unused") FacesContext context,
    @SuppressWarnings("unused") RenderingContext rc,
    @SuppressWarnings("unused") UIComponent component)
  {
  }

  /**
   * <p>
   * Called after rendering the current component in order to tear
   * down any special context.
   * </p>
   * <p>
   * <code>tearDownEncodingContext</code> will be called on the component if
   * <code>setupEncodingContext</code> succeeded.
   * </p>
   * <p>The default implementation does nothing</p>
   * @param context FacesContext for this request
   * @param rc RenderingContext for this encoding pass
   * @param component Component to encode using this Renderer
   * @see #setupEncodingContext
   */
  public void tearDownEncodingContext(
    FacesContext     context,
    RenderingContext rc,
    UIComponent     component)
  {
    // TODO Remove after one release
    if (component instanceof UIXComponent)
      tearDownEncodingContext(context, rc, (UIXComponent)component);
  }

  /**
   * Hook to allow the renderer to customize the visitation of the children components
   * of a component during the visitation of a component during rendering.
   *
   * @param component the component which owns the children to visit
   * @param visitContext the visitation context
   * @param callback the visit callback
   * @return <code>true</code> if the visit is complete.
   * @see UIXComponent#visitChildren(VisitContext, VisitCallback)
   */
  public boolean visitChildrenForEncoding(
    UIXComponent  component,
    VisitContext  visitContext,
    VisitCallback callback)
  {
    // visit the children of the component
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(
                                   visitContext.getFacesContext(), component);

    while (kids.hasNext())
    {
      // If any kid visit returns true, we are done.
      if (kids.next().visitTree(visitContext, callback))
      {
        return true;
      }
    }

    return false;
  }

  // TODO Remove after one release
  @Deprecated
  public void tearDownEncodingContext(
    @SuppressWarnings("unused") FacesContext context,
    @SuppressWarnings("unused") RenderingContext rc,
    @SuppressWarnings("unused") UIXComponent     component)
  {
  }

  /**
   * <p>
   * Called after rendering the current component's children in order to tear
   * down any special context.
   * </p>
   * <p>
   * <code>tearDownChildrenEncodingContext</code> will be called on the component if
   * <code>setupChildrenEncodingContext</code> succeeded.
   * </p>
   * <p>The default implementation does nothing</p>
   * @param context FacesContext for this request
   * @param rc RenderingContext for this encoding pass
   * @param component Component to encode using this Renderer
   * @see #setupChildrenEncodingContext
   */
  public void tearDownChildrenEncodingContext(
    @SuppressWarnings("unused") FacesContext context,
    @SuppressWarnings("unused") RenderingContext rc,
    @SuppressWarnings("unused") UIComponent component)
  {
  }

  // Note this should probably be made final, but since it is new, doing so could
  // break compatibility with present sub-classes

  /**
   * {@inheritDoc}
   * <p>
   * Sub-classes should override the
   * {@link #decode(FacesContext, UIComponent, FacesBean, String)} method
   * to perform their own decoding logic
   * </p>
   *
   * @see #decode(FacesContext, UIComponent, FacesBean, String)
   */
  @Override
  public final void decode(
    FacesContext facesContext,
    UIComponent  component)
  {
    FacesBean facesBean = getFacesBean(component);
    String clientId = null;
    if (facesBean != null)
    {
      clientId = decodeBehaviors(facesContext, component, facesBean);
    }
    decode(facesContext, component, facesBean, clientId);
  }

  /**
   * Hook for sub-classes to perform their own decode logic
   * @param facesContext the faces context
   * @param component the component to decode
   * @param facesBean the faces bean for the component
   * @param clientId the client ID if it has been retrieved already
   * during decoding, otherwise it will be null. Passed in for performance
   * reasons, so that if it has already been retrieved it will not need to be
   * retrieved again
   */
  protected void decode(
    @SuppressWarnings("unused") FacesContext facesContext,
    @SuppressWarnings("unused") UIComponent  component,
    @SuppressWarnings("unused") FacesBean    facesBean,
    @SuppressWarnings("unused") String       clientId)
  {
    // No-op
  }

  //
  // COERCION HELPERS
  //

  /**
   * Coerces an object into a String.
   */
  static public String toString(Object o)
  {
    if (o == null)
      return null;
    return o.toString();
  }

  /**
   * Coerces an object into a resource URI, calling the view-handler.
   */
  static public String toResourceUri(FacesContext fc, Object o)
  {
    if (o == null)
      return null;

    String uri = o.toString();

    // *** EL Coercion problem ***
    // If icon or image attribute was declared with #{resource[]} and that expression
    // evaluates to null (it means ResourceHandler.createResource returns null because requested resource does not exist)
    // EL implementation turns null into ""
    // see http://www.irian.at/blog/blogid/unifiedElCoercion/#unifiedElCoercion
    if (uri.length() == 0)
    {
      return null;
    }


    // With JSF 2.0 url for resources can be done with EL like #{resource['resourcename']}
    // and such EL after evalution contains context path for the current web application already,
    // -> we dont want call viewHandler.getResourceURL()
    if (uri.contains(ResourceHandler.RESOURCE_IDENTIFIER))
    {
      return uri;
    }

    // Treat two slashes as server-relative
    if (uri.startsWith("//"))
    {
      return uri.substring(1);
    }
    else
    {
      // If the specified path starts with a "/",
      // following method will prefix it with the context path for the current web application,
      // and return the result
      return fc.getApplication().getViewHandler().getResourceURL(fc, uri);
    }
  }

  /**
   * Coerces an object into an action URI, calling the view-handler.
   */
  static public String toActionUri(FacesContext fc, Object o)
  {
    if (o == null)
      return null;

    String uri = o.toString();

    // Treat two slashes as server-relative
    if (uri.startsWith("//"))
    {
      return uri.substring(1);
    }
    else
    {
      return fc.getApplication().getViewHandler().getActionURL(fc, uri);
    }
  }

  /**
   * Coerces an object into a resource URI, calling the view-handler.
   * @deprecated use toResourceUri
   */
  @Deprecated
  static public String toUri(Object o)
  {
    return toResourceUri(FacesContext.getCurrentInstance(),o);
  }

  /**
   * Returns the integer value of an object;  this does
   * not support null (which must be substituted with a default
   * before calling).
   */
  static public int toInt(Object o)
  {
    return ((Number) o).intValue();
  }

  /**
   * Returns the integer value of an object;  this does
   * not support null (which must be substituted with a default
   * before calling).
   */
  static public long toLong(Object o)
  {
    return ((Number) o).longValue();
  }

  /**
   * Returns the character value of an object, XhtmlConstants.CHAR_UNDEFINED
   * if there is none.
   */
  static public char toChar(Object o)
  {
    if (o == null)
      return CHAR_UNDEFINED;

    char c;
    if (o instanceof Character)
    {
      c = ((Character) o).charValue();
    }
    else
    {
      // If it's not a Character object, then let's turn it into
      // a CharSequence and grab the first character
      CharSequence cs;
      if (o instanceof CharSequence)
      {
        cs = (CharSequence) o;
      }
      else
      {
        cs = o.toString();
      }

      if (cs.length() == 0)
        c = CHAR_UNDEFINED;
      else
        c = cs.charAt(0);
    }

    // Handle the occasional odd bit of code that likes
    // returning null, and treat it identically to UNDEFINED.
    if (c == '\u0000')
      c = CHAR_UNDEFINED;

    return c;
  }

  @Override
  public final void encodeBegin(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    if (!getRendersChildren())
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      if (rc == null)
        throw new IllegalStateException(_LOG.getMessage(
          "NO_RENDERINGCONTEXT"));

      FacesBean bean = getFacesBean(component);

      beforeEncode(context, rc, component, bean);
      encodeBegin(context, rc, component, bean);
    }
  }

  @Override
  public final void encodeChildren(
    FacesContext context,
    UIComponent  component)
  {
    // encodeChildren() is fairly useless - it's simpler to just
    // put the output in encodeEnd(), or use the encodeAll() hook
  }

  @Override
  public final void encodeEnd(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    if (rc == null)
      throw new IllegalStateException(_LOG.getMessage(
        "NO_RENDERINGCONTEXT"));

    FacesBean bean = getFacesBean(component);
    RuntimeException re = null;
    try
    {
      if (getRendersChildren())
      {
        beforeEncode(context, rc, component, bean);
        encodeAll(context, rc, component, bean);
      }
      else
      {
        encodeEnd(context, rc, component, bean);
      }
    }
    catch (RuntimeException ex)
    {
      re = ex;
    }
    finally
    {
      try
      {
        afterEncode(context, rc, component, bean);
      }
      catch (RuntimeException ex)
      {
        if (re == null)
        {
          throw ex;
        }
        _LOG.warning(ex);
      }

      if (re != null)
      {
        throw re;
      }
    }
  }

  /**
   * Hook for rendering the start of a component;  only
   * called if getRendersChildren() is <em>false</em>.
   */
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (getRendersChildren())
      throw new IllegalStateException();
  }

  /**
   * Hook for rendering the component resources for the <code>target</code>.
   * @param context Current <code>FacesContext</code> object for this request.
   * @param target The target for the resources (e.g. head/body/form)
   *
   * @throws IOException
   */
  protected final void encodeComponentResources(
    FacesContext context,
    String       target
    ) throws IOException
  {
    if(target != null)
    {
      UIViewRoot viewRoot = context.getViewRoot();
      for(UIComponent componentResource : viewRoot.getComponentResources(context, target))
      {
        componentResource.encodeAll(context);
      }
    }
  }

  /**
   * Hook for rendering the end of a component;  only
   * called if getRendersChildren() is <em>false</em>.
   */
  protected void encodeEnd(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (getRendersChildren())
      throw new IllegalStateException();
  }

  /**
   * Hook for rendering all of a component;  only
   * called if getRendersChildren() is <em>true</em>.
   */
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!getRendersChildren())
      throw new IllegalStateException();
  }

  /**
   * Hook for encoding a child;  this assumes that isRendered()
   * has already been called. (RenderUtils.encodeRecursive()
   * can be used if you don't need that check.)
   * =-=AEW Ugh.
   */
  @SuppressWarnings("unchecked")
  protected void encodeChild(
    FacesContext context,
    UIComponent  child
    ) throws IOException
  {
    assert(child.isRendered());
    child.encodeAll(context);
  }

  @SuppressWarnings("unchecked")
  protected void encodeAllChildren(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    int childCount = component.getChildCount();
    if (childCount == 0)
      return;

    for (UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        encodeChild(context, child);
      }
    }
  }

  protected void delegateRenderer(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    CoreRenderer     renderer
    ) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      renderer.encodeAll(context, rc, component, bean);
    }
    else
    {
      throw new IllegalStateException();
    }
  }

  protected void delegateRendererBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    CoreRenderer     renderer
    ) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      throw new IllegalStateException();
    }
    else
    {
      renderer.encodeBegin(context, rc, component, bean);
    }
  }

  protected void delegateRendererEnd(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    CoreRenderer     renderer
    ) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      throw new IllegalStateException();
    }
    else
    {
      renderer.encodeEnd(context, rc, component, bean);
    }
  }

  /**
   * Renders the client ID as an "id".
   */
  protected void renderId(
    FacesContext context,
    UIComponent component
    ) throws IOException
  {
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      context.getResponseWriter().writeAttribute("id", clientId, "id");
    }
  }

  /**
   * Returns the client ID that should be used for rendering (if
   * {@link #shouldRenderId} returns true).
   */
  protected String getClientId(
    FacesContext context,
    UIComponent  component)
  {
    return component.getClientId(context);
  }

  /**
   * Returns true if the component should render an ID.  Components
   * that deliver events should always return "true".
   */
  // TODO Is this a bottleneck?  If so, optimize!
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent component)
  {
    String id = component.getId();

    // Otherwise, if ID isn't set, don't bother
    if (id == null)
      return false;

    // ... or if the ID was generated, don't bother
    if (id.startsWith(UIViewRoot.UNIQUE_ID_PREFIX))
      return false;

    return true;
  }

  protected boolean skipDecode(
    FacesContext context)
  {
    return false;
  }

  protected FacesBean getFacesBean(
    UIComponent component)
  {
    return ((UIXComponent) component).getFacesBean();
  }

  static protected final Object getRenderingProperty(
    RenderingContext rc,
    Object           key)
  {
    return rc.getProperties().get(key);
  }

  static protected final Object setRenderingProperty(
    RenderingContext rc,
    Object           key,
    Object           value)
  {
    return rc.getProperties().put(key, value);
  }

  /**
   * Gets a facet, verifying that the facet should be rendered.
   */
  static public UIComponent getFacet(
    UIComponent component,
    String      name)
  {
    UIComponent facet = component.getFacet(name);
    if ((facet == null) || !facet.isRendered())
      return null;

    return facet;
  }

  /**
   * Returns true if the component has children and at least
   * one has rendered=="true".
   */
  @SuppressWarnings("unchecked")
  static public boolean hasRenderedChildren(
    UIComponent component)
  {
    int count = component.getChildCount();
    if (count == 0)
      return false;

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        return true;
      }
    }

    return false;
  }

  /**
   * Returns the total number of children with rendered=="true".
   */
  @SuppressWarnings("unchecked")
  static public int getRenderedChildCount(
    UIComponent component)
  {
    int count = component.getChildCount();
    if (count == 0)
      return 0;

    int total = 0;
    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        total++;
      }
    }

    return total;
  }

 /**
   * @param afterChildIndex The children coming after this index, will
   * be considered.
   * @return the index of the next child that must be rendered, or
   * {@link #NO_CHILD_INDEX} if there is none.
   */
  public static int getNextRenderedChildIndex(
    List<UIComponent> components,
    int               afterChildIndex
    )
  {
    int childIndex = afterChildIndex + 1;
    Iterator<UIComponent> iter = components.listIterator(childIndex);
    for(; iter.hasNext(); childIndex++)
    {
      if(iter.next().isRendered())
      {
        return childIndex;
      }
    }

    return NO_CHILD_INDEX;
  }

  //
  // AGENT CAPABILITY CONVENIENCE METHODS
  //

  static public boolean isDesktop(RenderingContext rc)
  {
    return (Agent.TYPE_DESKTOP.equals(rc.getAgent().getType()));
  }

  static public boolean isPDA(RenderingContext rc)
  {
    return (Agent.TYPE_PDA.equals(rc.getAgent().getType()));
  }

  static public boolean isIE(RenderingContext rc)
  {
    return (Agent.AGENT_IE.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isKonqueror(RenderingContext rc)
  {
    return (Agent.AGENT_KONQUEROR.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isGecko(RenderingContext rc)
  {
    return (Agent.AGENT_GECKO.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isWebKit(RenderingContext rc)
  {
    return (Agent.AGENT_WEBKIT.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isOpera(RenderingContext rc)
  {
    return (Agent.AGENT_OPERA.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isIPhone(RenderingContext rc)
  {
    return (Agent.PLATFORM_IPHONE.equals(rc.getAgent().getPlatformName()));
  }

  static public boolean isGenericPDA(RenderingContext rc)
  {
    return (Agent.PLATFORM_GENERICPDA.equals(rc.getAgent().getPlatformName()));
  }

  /**
   * This method returns true if a user-agent's platform is NokiaS60
   * @param arc - RenderingContext of a request
   * @return boolean
   */
  static public boolean isNokiaS60(RenderingContext rc)
  {
    return (Agent.PLATFORM_NOKIA_S60.equals(rc.getAgent().getPlatformName()));
  }

  static public boolean isInaccessibleMode(RenderingContext rc)
  {
    return (rc.getAccessibilityMode() ==
            RequestContext.Accessibility.INACCESSIBLE);
  }

  static public boolean isScreenReaderMode(RenderingContext rc)
  {
    return (rc.getAccessibilityMode() ==
            RequestContext.Accessibility.SCREEN_READER);
  }

  //
  // Encoding hook methods for sub-classes
  //

  /**
   * Hook method that gets invoked before the component is encoded
   *
   * @see #encodeBegin(FacesContext, RederingContext, UIComponent, FacesBean)
   * @see #encodeAll(FacesContext, RederingContext, UIComponent, FacesBean)
   */
  protected void beforeEncode(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    setupEncodingContext(context, rc, component);
  }

  /**
   * Hook method that gets invoked after the component is encoded
   *
   * @see #encodeEnd(FacesContext, RederingContext, UIComponent, FacesBean)
   * @see #encodeAll(FacesContext, RederingContext, UIComponent, FacesBean)
   */
  protected void afterEncode(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    tearDownEncodingContext(context, rc, component);
  }

  //
  // Rendering convenience methods.
  //

  /**
   * Decodes the behaviors of this component, if it is the component that is the source
   * of the call to the server and the event matches behaviors that are attached to
   * the component
   *
   * @param facesContext the faces context
   * @param component the component
   * @param bean the faces bean
   * @return the client ID if it was retrieved, null otherwise
   */
  protected final String decodeBehaviors(
    FacesContext facesContext,
    UIComponent  component,
    FacesBean    bean)
  {
    if (!(component instanceof ClientBehaviorHolder))
    {
      return null;
    }

    // Check if there are client behaviors first as it should be faster to access then
    // getting the behavior event from the request parameter map (fewer method calls)
    Map<String, List<ClientBehavior>> behaviorsMap = ((ClientBehaviorHolder)component).getClientBehaviors();
    if (behaviorsMap.isEmpty())
    {
      return null;
    }

    // Get the behavior event sent by the client, if any
    Map<String, String> requestParams = facesContext.getExternalContext().getRequestParameterMap();
    String event = requestParams.get(_BEHAVIOR_EVENT_KEY);
    if (event == null)
    {
      return null;
    }

    // Does the component have behaviors for this event type?
    List<ClientBehavior> behaviors = behaviorsMap.get(event);
    if (behaviors == null || behaviors.isEmpty())
    {
      return null;
    }

    // See if this is the submitting component
    String clientId = component.getClientId(facesContext);
    String sourceClientId = requestParams.get("javax.faces.source");
    if (clientId.equals(sourceClientId))
    {
      // Decode the behaviors
      for (ClientBehavior behavior: behaviors)
      {
        behavior.decode(facesContext, component);
      }
    }

    return clientId;
  }

  /**
   * Get a collection of all the parameters that are children of the current component as
   * client behavior parameters.
   * @param component The component
   * @return Collection of parameters (will be non-null)
   */
  public static Collection<ClientBehaviorContext.Parameter> getBehaviorParameters(
    UIComponent component)
  {
    int childCount = component.getChildCount();
    if (childCount > 0)
    {
      List<ClientBehaviorContext.Parameter> list = null;
      for (UIComponent child : component.getChildren())
      {
        if (!(child instanceof UIParameter)) { continue; }

        if (list == null)
        {
          // leave plenty of room to hold the parameters
          list = new ArrayList<ClientBehaviorContext.Parameter>(childCount);
        }
        UIParameter param = (UIParameter) child;
        list.add(new ClientBehaviorContext.Parameter(param.getName(), param.getValue()));
      }

      if (list != null)
      {
        return list;
      }
    }

    return Collections.<ClientBehaviorContext.Parameter>emptyList();
  }

  protected void renderEncodedActionURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeActionURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }

  protected void renderEncodedResourceURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeResourceURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }

  /**
   * Render a generic CSS styleClass (not one derived from an attribute).
   * The styleclass will be passed through the RenderingContext
   * getStyleClass() API.
   * @param context  the FacesContext
   * @param styleClass the style class
   */
  static public void renderStyleClass(
    FacesContext     context,
    RenderingContext rc,
    String           styleClass) throws IOException
  {
    if (styleClass != null)
    {
      String compressedStyleClass = rc.getStyleClass(styleClass);
      context.getResponseWriter().writeAttribute("class", compressedStyleClass, null);

      if (Beans.isDesignTime())
        context.getResponseWriter().writeAttribute("rawClass", styleClass, null);
    }
  }

  /**
   * Render an array of CSS styleClasses as space-separated values.
   * @param context  the FacesContext
   * @param styleClasses the style classes
   */
  static public void renderStyleClasses(
    FacesContext     context,
    RenderingContext rc,
    String[]         styleClasses) throws IOException
  {
    int length = styleClasses.length;
    if (length == 0)
      return;

    String value;
    // Optimize one-element arrays
    if (length == 1)
    {
      value = rc.getStyleClass(styleClasses[0]);
    }
    // Otherwise, build up the array of mutated style classes.
    else
    {
      // Assume that styleclass compression is active in terms of sizing
      // this buffer - this is not true for portlets, but this isn't a
      // huge optimizations, and the relatively smaller content delivered
      // to portlets makes this still less important
      StringBuilder builder =
        new StringBuilder((_COMPRESSED_LENGTH + 1) * length);
      for (int i = 0; i < length; i++)
      {
        if (styleClasses[i] != null)
        {
          String styleClass = rc.getStyleClass(styleClasses[i]);
          if (styleClass != null)
          {
            if (builder.length() != 0)
              builder.append(' ');
            builder.append(styleClass);
          }
        }
      }

      if (builder.length() == 0)
        value = null;
      else
        value = builder.toString();
    }

    context.getResponseWriter().writeAttribute("class", value, null);

    if (Beans.isDesignTime())
    {
      StringBuilder builder = new StringBuilder();
      for (int i = 0; i < length; i++)
      {
        if (styleClasses[i] != null)
        {
          String styleClass = styleClasses[i];
          if (styleClass != null)
          {
            if (builder.length() != 0)
              builder.append(' ');
            builder.append(styleClass);
          }
        }
      }

      if (builder.length() > 0)
        context.getResponseWriter().writeAttribute("rawClass", builder.toString(), null);
    }

  }

  // Heuristic guess of the maximum length of a typical compressed style
  private static final int _COMPRESSED_LENGTH = 4;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    CoreRenderer.class);
  private static final String _BEHAVIOR_EVENT_KEY = "javax.faces.behavior.event";
}
