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

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

import javax.el.MethodExpression;
import javax.faces.component.NamingContainer;
import javax.faces.component.StateHelper;
import javax.faces.component.UIComponent;
import javax.faces.component.UINamingContainer;
import javax.faces.component.UIPanel;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import javax.faces.render.Renderer;

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFComponent;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.CoreRenderer;


/**
 * Pure abstract base class for all UIX components.
 */
@JSFComponent
abstract public class UIXComponent extends UIComponent
{
  /**
   * Helper function called by Renderers to iterate over a flattened view of a group of
   * potentially FlattenedComponent instances rooted at a single child of the component to collect
   * information about these children prior to encoding the children using
   * <code>encodeFlattenedChild(FacesContext, ComponentProcessor, UIComponent, Object)</code>.
   * <p>
   * If the child is a FlattenedComponent, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   *  If the Renderer accidentally passes in the component to be processed instead of one
   * of its children, the result will almost certainly be an infinite recursion and stack overflow.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    UIComponent child,
    S callbackContext) throws IOException
  {
    return processFlattenedChildren(context,
                                    new ComponentProcessingContext(),
                                    childProcessor,
                                    child,
                                    callbackContext);
  }

  /**
   * Helper function called by Renderers to encode a flattened view of a group of
   * potentially FlattenedComponent instances rooted at a single child of the component,
   * invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.  This method must  be called
   * when the childProcessor is actually encoding and the childProcessor must not attempt
   * to encode the same component instances more than once per request.
   * <p>
   * If a Renderer needs to
   * collect information about its possibly flattened children before calling
   * <code>encodeFlattenedChild(FacesContext, ComponentProcessor, UIComponent, Object)</code>,
   * it should call <code>processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)</code>
   * to collect the information.
   * <p>
   * If the child is a FlattenedComponent, the <code>childProcessor</code> will
   * be called  on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * FlattenedComponents that wish to check whether they are processed for the purpose of
   * encoding can check the ProcessingHints of the ComponentProcessingContext for the
   * presence of <code>PROCESS_FOR_ENCODING hint</code>.
   * <p>
   * If the Renderer accidentally passes in the component to be encoded instead of one
   * of its children, the result will almost certainly be an infinite recursion and stack overflow.
   * @return <code>true</code> If any children were processed
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean encodeFlattenedChild(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    UIComponent child,
    S callbackContext) throws IOException
  {
    ComponentProcessingContext processingContext = new ComponentProcessingContext();
    processingContext.__setIsRendering();

    return processFlattenedChildren(context,
                                    processingContext,
                                    childProcessor,
                                    child,
                                    callbackContext);
  }

  /**
   * Helper function called by FlattenedComponent to iterate over a flattened view of a group of
   * potentially FlattenedComponent instances rooted at a single child of the FlattenedComponent,
   * invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.
   * <p>
   * If the child is a FlattenedComponent, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * This method is typically used to flatten the contents of a facet of the FlattenedComponent.
   * If the FlattenedComponent accidentally passes in itself instead of one
   * of its children, the result will almost certainly be an infinite recursion and stack overflow.
   * @return <code>true</code> If any children were processed
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessingContext cpContext,
    ComponentProcessor<S> childProcessor,
    UIComponent child,
    S callbackContext) throws IOException
  {
    if (child.isRendered())
    {
       // component is an action FlattenedComponent.  Ask it to flatten its children
      if ((child instanceof FlattenedComponent) &&
          ((FlattenedComponent)child).isFlatteningChildren(context))
      {
        return ((FlattenedComponent)child).processFlattenedChildren(context,
                                                                    cpContext,
                                                                    childProcessor,
                                                                    callbackContext);
      }
      else
      {
        boolean processed = true;
        child.pushComponentToEL(context, null);

        try
        {
          if (isFlattenableCoreComponent(child))
          {
            processed =
                processFlattenedChildren(context, cpContext, childProcessor,
                                         child.getChildren(),
                                         callbackContext);
          }
          else
          {
            try
            {
              // not a FlattenedComponent, pass the component directly to the ComponentProcessor
              childProcessor.processComponent(context, cpContext, child,
                                              callbackContext);
            }
            finally
            {
              // if startDepth is > 0, only the first visible child will be marked as starting a group
              cpContext.resetStartDepth();
            }
          }
        }
        finally
        {
          child.popComponentFromEL(context);
        }

        return processed;
      }
    }
    else
    {
      // component not rendered
      return false;
    }
  }

  /**
   * Helper function called by Renderers to iterate over a flattened view of the
   * children, potentially containing FlattenedComponents, of the component the Renderer is
   * encoding, invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.
   * <p>
   * For each FlattenedComponent child, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * This method is typically used to flatten the children of the FlattenedComponent to
   * be encoded.
   * @return <code>true</code> If any children were processed
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    Iterable<UIComponent> children,
    S callbackContext) throws IOException
  {
    return processFlattenedChildren(context,
                                    new ComponentProcessingContext(),
                                    childProcessor,
                                    children,
                                    callbackContext);
  }

  /**
   * Helper function called by Renderers to encode a flattened view of their children,
   * invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.  This method must  be called
   * when the childProcessor is actually encoding and the childProcessor must not attempt
   * to encode the same component instances more than once per request.
   * <p>
   * If a Renderer needs to
   * collect information about its possibly flattened children before calling
   * <code>encodeFlattenedChild(FacesContext, ComponentProcessor, Iterable&lt;UIComponent&gt;, Object)</code>,
   * it should call
   * <code>processFlattenedChildren(FacesContext, ComponentProcessor, Iterable&lt;UIComponent&gt;, Object)</code>
   * to collect the information.
   * <p>
   * For each FlattenedComponent child, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * FlattenedComponents that wish to check whether they are processed for the purpose of
   * encoding can check the ProcessingHints of the ComponentProcessingContext for the
   * presence of <code>PROCESS_FOR_ENCODING hint</code>.
   * @param context FacesContext
   * @param childProcessor ComponentProcessor to call on each flattened child
   * @param children Initial set of children to flatten
   * @param callbackContext context object to pass to the childProcessor on each invocation
   * @return <code>true</code> If any children were processed
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean encodeFlattenedChildren(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    Iterable<UIComponent> children,
    S callbackContext) throws IOException
  {
    ComponentProcessingContext processingContext = new ComponentProcessingContext();
    processingContext.__setIsRendering();

    return processFlattenedChildren(context,
                                    processingContext,
                                    childProcessor,
                                    children,
                                    callbackContext);
  }


  /**
   * Helper function called by FlattenedComponents to iterate over a flattened view of their
   * children, potentially themselves FlattenedComponents, invoking the <code>childProcessor</code>
   * with its <code>callbackContext</code> on each renderable instance.
   * <p>
   * For each FlattenedComponent child, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessingContext cpContext,
    ComponentProcessor<S> childProcessor,
    Iterable<UIComponent> children,
    S callbackContext) throws IOException
  {
    // we haven't processed a child yet
    boolean processedChild = false;

    for (UIComponent currChild : children)
    {
      // latch processed child to the first child processed
      processedChild |= processFlattenedChildren(context,
                                                 cpContext, childProcessor,
                                                 currChild,
                                                 callbackContext);
    }

    return processedChild;
  }

  /**
   * <p>Perform a tree visit starting at
   * this node in the tree.</p>
   *
   * <p>UIXComponent.visitTree() implementations do not invoke the
   * {@code VisitCallback}directly, but instead call
   * {@code VisitContext.invokeVisitCallback()}to invoke the
   * callback.  This allows {@code VisitContext}implementations
   * to provide optimized tree traversals, for example by only
   * calling the {@code VisitCallback}for a subset of components.</p>
   *
   * @param visitContext the <code>VisitContext</code> for this visit
   * @param callback the <code>VisitCallback</code> instance
   * whose <code>visit</code> method will be called
   * for each node visited.
   * @return component implementations may return <code>true</code>
   * to indicate that the tree visit is complete (eg. all components
   * that need to be visited have been visited).  This results in
   * the tree visit being short-circuited such that no more components
   * are visited.
   *
   * @see VisitContext#invokeVisitCallback VisitContext.invokeVisitCallback()
   */
  public boolean visitTree(
    VisitContext visitContext,
    VisitCallback callback)
  {
    return visitTree(visitContext, this, callback);
  }

  /**
   * Specifies what facets and children components should be processed as rendered for life-cycle
   * methods. Any components not returned will not be processed during methods such as decoding,
   * validating, updating the model, rendered-only tree visiting, etc.
   *
   * @param facesContext the facesContext
   * @return An iterator of components to process. Must not return null (return an empty iterator
   * if no children components should be processed).
   */
  protected Iterator<UIComponent> getRenderedFacetsAndChildren(
    FacesContext facesContext)
  {
    Renderer renderer = getRenderer(facesContext);
    if (renderer instanceof CoreRenderer)
    {
      return ((CoreRenderer)renderer).getRenderedFacetsAndChildren(facesContext, this);
    }
    else
    {
      return getFacetsAndChildren();
    }
  }

 /**
  * Hook for subclasses to override the manner in which the component's children are visited.  The default
  * implementation visits all of the children and facets of the Component.
  * <code>setupChildrenVisitingContext</code> will have been called before this method is
  * invoked and <code>tearDownChildrenVisitingContext</code> will be called after.
  * respectively.  If the purpose of this visit was to encode the component and the
  * component uses a CoreRenderer, the CoreRenderer's
  * <code>setupChildrenEncodingContext</code> and <code>tearDownChildrenEncodingContext</code>
  * will be called before and after this method is invoked, respectively.
  * @param visitContext the <code>VisitContext</code> for this visit
  * @param callback the <code>VisitCallback</code> instance
  * @return <code>true</code> if the visit is complete.
  * @see #setupChildrenVisitingContext
  * @see #tearDownChildrenVisitingContext
  * @see org.apache.myfaces.trinidad.render.CoreRenderer#setupChildrenEncodingContext
  * @see org.apache.myfaces.trinidad.render.CoreRenderer#tearDownChildrenEncodingContext
  */
  protected boolean visitChildren(
    VisitContext visitContext,
    VisitCallback callback)
  {
    // See if this is during encoding, if so, allow the renderer to control the visitation of
    // the children so that any special encoding context may be applied around the visitation
    // of each child.
    if (_isEncodingVisit(visitContext))
    {
      Renderer renderer = getRenderer(visitContext.getFacesContext());
      if (renderer instanceof CoreRenderer)
      {
        CoreRenderer coreRenderer = (CoreRenderer)renderer;
        return coreRenderer.visitChildrenForEncoding(this, visitContext, callback);
      }
    }

    // visit all of the children of the component
    return visitAllChildren(visitContext, callback);
  }

  /**
   * Default implementation of visiting children that visits all children without iterating
   * @param visitContext the <code>VisitContext</code> for this visit
   * @param callback the <code>VisitCallback</code> instance
   * @return <code>true</code> if the visit is complete.
   */
  protected final boolean visitAllChildren(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    // visit the children of the component
    Iterator<UIComponent> kids =
      visitContext.getHints().contains(VisitHint.SKIP_UNRENDERED) ?
        getRenderedFacetsAndChildren(visitContext.getFacesContext()) :
        getFacetsAndChildren();

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

  /**
   * Returns <code>true</code> if the components are being visited
   * for the purpose of encoding.
   */
  private static boolean _isEncodingVisit(VisitContext visitContext)
  {
    return(visitContext.getHints().contains(VisitHint.EXECUTE_LIFECYCLE) &&
           FacesContext.getCurrentInstance().getCurrentPhaseId() == PhaseId.RENDER_RESPONSE);
  }

  /**
   * <p>Perform a tree visit starting at the specified node in the tree.</p>
   *
   * <p>UIXComponent.visitTree() implementations do not invoke the
   * {@code VisitCallback}directly, but instead call
   * {@code VisitContext.invokeVisitCallback()}to invoke the
   * callback.  This allows {@code VisitContext}implementations
   * to provide optimized tree traversals, for example by only
   * calling the {@code VisitCallback}for a subset of components.</p>
   *
   * @param visitContext the <code>VisitContext</code> for this visit
   * @param component the <code>UIComponent</code> to start the visit from
   * @param callback the <code>VisitCallback</code> instance
   * whose <code>visit</code> method will be called
   * for each node visited.
   * @return component implementations may return <code>true</code>
   * to indicate that the tree visit is complete (eg. all components
   * that need to be visited have been visited).  This results in
   * the tree visit being short-circuited such that no more components
   * are visited.
   *
   * @see VisitContext#invokeVisitCallback VisitContext.invokeVisitCallback()
   */
  public static boolean visitTree(
    VisitContext  visitContext,
    UIComponent   component,
    VisitCallback callback)
  {
    if (!(component instanceof UIXComponent))
    {
      // hopefully the subview implementations have the subId optimization
      return component.visitTree(visitContext, callback);
    }
    else
    {
      UIXComponent uixComponent = (UIXComponent)component;

      FacesContext context = visitContext.getFacesContext();

      // delegate to the UIXComponent
      if (!uixComponent.isVisitable(visitContext))
        return false;

      // set up the EL Context with the component.  Note that since we do this after call
      // isVisitable, any attributes retrieved (liek rendered) that are bound with EL referring
      // to the current component will be evaluated correctly, however, in the specific case
      // of rendered, rendered already has this problem in normal JSF traversal since it
      // is evaluated by the parent component
      component.pushComponentToEL(context, null);

      boolean doneVisiting = false;
      RuntimeException re = null;

      try
      {
        RenderingContext rc = (_isEncodingVisit(visitContext))
                                ? RenderingContext.getCurrentInstance()
                                : null;

        // UIXComponents are allowed to set up their context differently for encoding
        // than normal processing, so behave differently if this is the RenderResponse
        // phase.  In order to allow the visitcallback to call encodeAll in the ppr case,
        // we don't call setupEncodingContext before we call the visitContext, since this
        // would result in setupEncodingContext being called twice on the partial roots,
        // instead we only do so if the visitCallback returns ACCEPT
        if (rc == null)
        {
          uixComponent.setupVisitingContext(context);
        }

        VisitResult visitResult = VisitResult.REJECT;

        try
        {
          // invoke the callback for this component
          visitResult = visitContext.invokeVisitCallback(component, callback);

          if (visitResult == VisitResult.COMPLETE)
            doneVisiting = true;
          else if (visitResult == VisitResult.ACCEPT)
          {
            // now determine whether we need to visit the children

            // assume that all UIXComponent NamingContainers always act as NamingContainers,
            // (unlike <h:form>) and this it is OK to put the optimization where we
            // don't visit the children if we know that we don't have any ids in this
            // subtree to visit
            boolean skipChildren = (uixComponent instanceof NamingContainer) &&
                                   visitContext.getSubtreeIdsToVisit(uixComponent).isEmpty();

            // visit the children of the component if we aren't supposed to skip them
            if (!skipChildren)
            {
              // setup encoding context before visiting children, since we didn't do so
              // before calling the visitCallback
              if (rc != null)
              {
                uixComponent.setupEncodingContext(context, rc);
              }

              try
              {
                doneVisiting = visitChildren(visitContext, uixComponent, callback);
              }
              finally
              {
                // teardown the encoding context if we set it up
                if (rc != null)
                {
                  uixComponent.tearDownEncodingContext(context, rc);
                }
              }
            }
          }
          else
          {
            // don't visit the children
            assert(visitResult == VisitResult.REJECT);
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
            // tear down the context we set up in order to visit our component
            if (rc == null)
            {
              uixComponent.tearDownVisitingContext(context);
            }
          }
          catch (RuntimeException ex)
          {
            if (re == null)
            {
              throw ex;
            }
            else
            {
              _LOG.warning(ex);
            }
          }
        }
      }
      finally
      {
        component.popComponentFromEL(context);

        if (re != null)
        {
          throw re;
        }
      }

      // if we got this far, we're not done
      return doneVisiting;
    }
  }

  /**
   * Utility method to allow the visiting of children components while visiting a parent using
   * a new visit callback or visit context. The method may only be called when the parent is
   * is the target of a visitation to ensure that it is properly in context.
   * <p>Example usage:</p>
   * <pre>@Override
   * public VisitResult visit(
   *   VisitContext visitContext,
   *   UIComponent  target)
   * {
   *   if (someCondition)
   *   {
   *     UIXComponent.visitChildren(target, visitContext, new VisitCallback() {...});
   *     return VisitResult.COMPLETE;
   *   }
   *   ...
   * }</pre>
   *
   * @param visitContext the <code>VisitContext</code> for this visit
   * @param parentComponent the <code>UIComponent</code> to visit the children. The parent component
   * must be actively being visited in order to call this method.
   * @param callback the <code>VisitCallback</code> instance
   * whose <code>visit</code> method will be called
   * for each node visited.
   * @return component implementations may return <code>true</code>
   * to indicate that the tree visit is complete (eg. all components
   * that need to be visited have been visited).  This results in
   * the tree visit being short-circuited such that no more components
   * are visited.
   */
  public static boolean visitChildren(
    VisitContext  visitContext,
    UIComponent   parentComponent,
    VisitCallback callback)
  {
    if (!(parentComponent instanceof UIXComponent))
    {
      // Not a UIXComponent, there is no extra functionality necessary in order to visit the
      // children.
      for (Iterator<UIComponent> iter = parentComponent.getFacetsAndChildren(); iter.hasNext();)
      {
        UIComponent child = iter.next();

        if (child.visitTree(visitContext, callback))
        {
          return true;
        }
      }

      return false;
    }

    UIXComponent uixParentComponent = (UIXComponent)parentComponent;
    FacesContext context = visitContext.getFacesContext();
    RenderingContext rc = (_isEncodingVisit(visitContext))
                            ? RenderingContext.getCurrentInstance()
                            : null;
    boolean doneVisiting = false;
    RuntimeException re = null;

    // setup any context needed for visiting the children of the component as opposed
    // to the component itself

    if (parentComponent instanceof UIXComponent)
    {
      if (rc != null)
      {
        uixParentComponent.setupChildrenEncodingContext(context, rc);
      }
      else
      {
        uixParentComponent.setupChildrenVisitingContext(context);
      }
    }

    try
    {
      doneVisiting = uixParentComponent.visitChildren(visitContext, callback);
    }
    catch (RuntimeException ex)
    {
      re = ex;
    }
    finally
    {
      try
      {
        // teardown any context initialized above
        if (rc != null)
        {
          uixParentComponent.tearDownChildrenEncodingContext(context, rc);
        }
        else
        {
          uixParentComponent.tearDownChildrenVisitingContext(context);
        }
      }
      catch (RuntimeException ex)
      {
        if (re == null)
        {
          throw ex;
        }
        else
        {
          _LOG.warning(ex);
        }
      }

      if (re != null)
      {
        throw re;
      }
    }

    return doneVisiting;
  }

  /**
   * Add a component as a partial target to the current request. This code handles the
   * delegation to {@link #setPartialTarget(FacesContext, PartialPageContext)}
   * for UIXComponents or assumes for {@link UIComponent} that components with a renderer
   * type are able to produce DOM elements that have IDs that can be replaced.
   *
   * @param facesContext the faces context
   * @param partialContext the partial page context
   * @param component the component to add as a target
   */
  public static void addPartialTarget(FacesContext facesContext,
    PartialPageContext partialContext, UIComponent component)
  {
    if(component == null)
    {
      throw new NullPointerException("UIComponent is null");
    }

    if (component instanceof UIXComponent)
    {
      ((UIXComponent)component).setPartialTarget(facesContext, partialContext);
    }
    else
    {
      // default to using the renderer type implementation
      _addPartialTargetImpl(facesContext, partialContext, component);
    }
  }

  /**
   * Clears all of the cached clientIds in this component subtree
   */
  public final void clearCachedClientIds()
  {
    clearCachedClientIds(this);
  }

  /**
   * Clears all of the cached clientIds in the component's subtree
   * @param component Component subtree to clear the cached client ids for
   */
  public static void clearCachedClientIds(UIComponent component)
  {
    String id = component.getId();
    component.setId(id);

    Iterator<UIComponent> allChildren = component.getFacetsAndChildren();

    while (allChildren.hasNext())
      clearCachedClientIds(allChildren.next());
  }

  /**
   * Marks this component as a partial target for this request. Typically called
   * by the {@link org.apache.myfaces.trinidad.context.RequestContext RequestContext}.
   * The component should add the client ID the desired rendered component to the context.
   * This allows components that do not render a replacable DOM element with an ID
   * to choose an alternative component, like a parent.
   *
   * @param facesContext the faces context
   * @param partialContext the partial page context
   */
  protected void setPartialTarget(FacesContext facesContext,
    PartialPageContext partialContext)
  {
    UIXComponent._addPartialTargetImpl(facesContext, partialContext, this);
  }

  /**
   * <p>Called by
   * {@link UIXComponent#visitTree(VisitContext,VisitCallback) UIXComponent.visitTree()}to determine
   * whether this component is "visitable" - ie. whether this component
   * satisfies the {@link VisitHint}returned by
   * {@link VisitContext#getHints VisitContext.getHints()}.</p>
   * <p>If this component is not visitable (ie. if this method returns
   * false), the tree visited is short-circuited such that neither the
   * component nor any of its descendents will be visited></p>
   * <p>Custom {@code treeVisit()}implementations may call this method
   * to determine whether the component is visitable before performing
   * any visit-related processing.</p>
   * @param visitContext VisitingContext to use to determine if the component is visitable
   * @return true if this component should be visited, false otherwise.
   */
  protected boolean isVisitable(VisitContext visitContext)
  {
    return _isVisitable(visitContext, this);
  }

  /**
   * @see #addPartialTarget(FacesContext, PartialPageContext, UIComponent)
   * @see #setPartialTarget(FacesContext, PartialPageContext)
   */
  private static void _addPartialTargetImpl(
    FacesContext facesContext, PartialPageContext partialContext, UIComponent component)
  {
    if (component.getRendererType() == null)
    {
      if (component.getParent() != null)
      {
        // delegate to the parent component, assuming that no renderer type means that
        // there is no suitable replacable DOM element for this component
        addPartialTarget(facesContext, partialContext, component.getParent());
      }
    }
    else
    {
      partialContext.addPartialTarget(component.getClientId(facesContext));
    }
  }

  /**
   * default implementation checking the <code>VisitHint.SKIP_TRANSIENT</code> and
   * <code>VisitHint.SKIP_UNRENDERED</code> hints.
   */
  private static boolean _isVisitable(VisitContext visitContext, UIComponent component)
  {
    Collection<VisitHint> hints = visitContext.getHints();

    if (hints.contains(VisitHint.SKIP_TRANSIENT) && component.isTransient())
      return false;

    if (hints.contains(VisitHint.SKIP_UNRENDERED) && !component.isRendered())
      return false;

    return true;
  }

  /**
   * <p>
   * Called when visiting the component during optimized partial page encoding so that the
   * component can modify what is actually encoded.  For example tab controls often
   * render the tabs for the ShowDetailItems in the tab bar before delegating to the
   * disclosed ShowDetailItem to render the tab content.  As a result, the tab control
   * needs to encode its tab bar if any of its ShowDetailItems are partial targets so that
   * the tab labels, for example, are up-to-date.
   * </p>
   * <p>
   * The default implementation delegates to the CoreRenderer if this component has one, otherwise
   * it calls the VisitCallback and returns its result if this UIXComponent is a partial
   * target of the current encoding.
   * </p>
   * @param visitContext VisitContext to pass to the VisitCallback
   * @param partialContext PartialPageContext for the current partial encoding
   * @param callback VisitCallback to call if this component is a partial target
   * @return The VisitResult controlling continued iteration of the visit.
   */
  public VisitResult partialEncodeVisit(
    VisitContext visitContext,
    PartialPageContext partialContext,
    VisitCallback callback)
  {
    FacesContext context  = visitContext.getFacesContext();
    Renderer     renderer = getRenderer(context);

    if (renderer instanceof CoreRenderer)
    {
      // delegate to the CoreRenderer
      return ((CoreRenderer)renderer).partialEncodeVisit(visitContext,
                                                         partialContext,
                                                         this,
                                                         callback);
    }
    else
    {
      // check that this is a component instance that should be encoded
      if (partialContext.isPossiblePartialTarget(getId()) &&
          partialContext.isPartialTarget(getClientId(context)))
      {
        // visit the component instance
        return callback.visit(visitContext, this);
      }
      else
      {
        // Not visiting this component, but allow visit to
        // continue into this subtree in case we've got
        // visit targets there.
        return VisitResult.ACCEPT;
      }
    }
  }

  /**
   * <p>Sets up the context necessary to visit or invoke the component for all phases.</p>
   * <p>The default implementation does nothing.</p>
   * <p>If a subclass overrides this method, it should override
   * <code>tearDownVisitingContext</code> as well.</p>
   * <p>It is guaranteed that if <code>setupVisitingContext</code> completes
   * <code>tearDownVisitingContext</code> will be called for this component</p>
   * @param context FacesContext
   * @see #tearDownVisitingContext
   * @see #setupEncodingContext
   * @see #tearDownEncodingContext
   * @see #setupChildrenVistingContext
   *
   */
  protected void setupVisitingContext(@SuppressWarnings("unused") FacesContext context)
  {
    // do nothing
  }

  /**
   * <p>Sets up the context necessary to visit or invoke the children of a component for all phases.
   * </p>
   * <p>The default implementation does nothing.</p>
   * <p>If a subclass overrides this method, it should override
   * <code>tearDownChildrenVisitingContext</code> as well.</p>
   * <p>It is guaranteed that if <code>setupChildrenVisitingContext</code> completes
   * <code>tearDownChildrenVisitingContext</code> will be called for this component</p>
   * @param context FacesContext
   * @see #visitChildren
   * @see #tearDownChildrenVisitingContext
   * @see setupVisitingContext
   */
  protected void setupChildrenVisitingContext(@SuppressWarnings("unused") FacesContext context)
  {
    // do nothing
  }

  /**
   * <p>Tears down context created in order to visit or invoke the component
   * for all phases.</p>
   * <p>The default implementation does nothing.</p>
   * <p>A subclass should only override this method if it overrode
   * <code>setupVisitingContext</code> as well</p>
   * <p>It is guaranteed that <code>tearDownVisitingContext</code> will be called only after
   * <code>setupVisitingContext</code> has been called for this component</p>
   * @param context FacesContext
   * @see #setupVisitingContext
   * @see #setupEncodingContext
   * @see #tearDownEncodingContext
   */
  protected void tearDownVisitingContext(@SuppressWarnings("unused") FacesContext context)
  {
    // do nothing
  }

  /**
   * <p>Tears down context created in order to visit or invoke the children of a component
   * for all phases.</p>
   * <p>The default implementation does nothing.</p>
   * <p>A subclass should only override this method if it overrode
   * <code>setupChildrenVisitingContext</code> as well</p>
   * <p>It is guaranteed that <code>tearDownChildrenVisitingContext</code> will be called only after
   * <code>setupChildrenVisitingContext</code> has been called for this component</p>
   * @param context FacesContext
   * @see #setupChildrenVisitingContext
   * @see #visitChildren
   */
  protected void tearDownChildrenVisitingContext(@SuppressWarnings("unused") FacesContext context)
  {
    // do nothing
  }

  @Deprecated
  protected final void setUpEncodingContext(FacesContext context, RenderingContext rc)
  {
    setupEncodingContext(context, rc);
  }

  /**
   * <p>Sets up the context necessary to encode the component.</p>
   * <p>The default implementation delegates to
   * <code>CoreRenderer.setupEncodingContext</code> and then calls
   * <code>setupVisitingContext</code></p>
   * <p>If a subclass overrides this method, it should override
   * <code>tearDownEncodingContext</code> as well.</p>
   * <p>It is guaranteed that if <code>setUpEncodingContext</code> completes
   * <code>tearDownEncodingContext</code> will be called for this component</p>
   * <p>
   * During partial page rendering traversals, <code>setupEncodingContext</code> is not called
   * before the <code>VisitCallback</code> is invoked.  This behavior is different than for
   * <code>setupVisitingContext</code>, which is always called before the <code>VisitCallback</code>
   * is invoked for non-partial page rendering visits.  This difference in behavior allows the
   * <code>VisitCallback</code> in a partial page rendering visit to safely call
   * <code>UIComponent.encodeAll</code>, which in the case of a UIXComponent, will call
   * <code>UIXComponent.setupEncodeContext</code>.
   * </p>
   * @param context The FacesContext
   * @param rc      RenderingContext to use for encoding
   * @see #setupVisitingContext
   * @see #setupChildrenEncodingContext
   * @see #tearDownVisitingContext
   * @see #tearDownEncodingContext
   * @see org.apache.myfaces.trinidad.render.CoreRenderer#setupEncodingContext(FacesContext, RenderingContext, UIComponent)
   */
  protected void setupEncodingContext(FacesContext context, RenderingContext rc)
  {
    setupVisitingContext(context);

    Renderer renderer = getRenderer(context);

    if (renderer instanceof CoreRenderer)
    {
      CoreRenderer coreRenderer = (CoreRenderer)renderer;

      coreRenderer.setupEncodingContext(context, rc, (UIComponent)this);
    }
  }

  /**
   * Sets the context necessary to encode the children of a component.
   * @param context The FacesContext
   * @param rc      RenderingContext to use for encoding
   * @see #setupChildrenVisitingContext
   * @see #tearDownChildrenEncodingContext
   * @see #setupEncodingContext
   * @see org.apache.myfaces.trinidad.render.CoreRenderer#setupChildrenEncodingContext
   */
  public void setupChildrenEncodingContext(FacesContext context, RenderingContext rc)
  {
    setupChildrenVisitingContext(context);

    Renderer renderer = getRenderer(context);

    if (renderer instanceof CoreRenderer)
    {
      CoreRenderer coreRenderer = (CoreRenderer)renderer;

      coreRenderer.setupChildrenEncodingContext(context, rc, this);
    }
  }

  /**
   * <p>Tears down the context created in order to encode the component</p>
   * <p>The default implementation delegates to
   * <code>CoreRenderer.tearDownEncodingContext</code> and then calls
   * <code>tearDownVisitingContext</code></p>
   * <p>A subclass should only override this method if it overrode
   * <code>setupEncodingContext</code> as well</p>
   * <p>It is guaranteed that <code>tearDownEncodingContext</code> will be called only after
   * <code>setupEncodingContext</code> has been called for this component</p>
   * @param context The FacesContext
   * @param rc      RenderingContext to use for encoding
   * @see #setupEncodingContext
   * @see #tearDownVisitingContext
   * @see #setupEncodingContext
   * @see #tearDownChildrenEncodingContext
   * @see org.apache.myfaces.trinidad.render.CoreRenderer#tearDownEncodingContext(FacesContext, RenderingContext, UIComponent)
   */
  protected void tearDownEncodingContext(
    FacesContext context,
    RenderingContext rc)
  {
    Renderer renderer = getRenderer(context);

    try
    {
      if (renderer instanceof CoreRenderer)
      {
        CoreRenderer coreRenderer = (CoreRenderer)renderer;

        coreRenderer.tearDownEncodingContext(context, rc, (UIComponent)this);
      }
    }
    finally
    {
      tearDownVisitingContext(context);
    }
  }

  /**
   * Tears down the context necessary to encode the children of a component.
   * @param context The FacesContext
   * @param rc      RenderingContext to use for encoding
   * @see #setupChildrenVisitingContext
   * @see #tearDownChildrenEncodingContext
   * @see #tearDownEncodingContext
   * @see org.apache.myfaces.trinidad.render.CoreRenderer#setupChildrenEncodingContext
   */
  public void tearDownChildrenEncodingContext(
    FacesContext context,
    RenderingContext rc)
  {
    Renderer renderer = getRenderer(context);

    try
    {
      if (renderer instanceof CoreRenderer)
      {
        CoreRenderer coreRenderer = (CoreRenderer)renderer;

        coreRenderer.tearDownChildrenEncodingContext(context, rc, this);
      }
    }
    finally
    {
      tearDownChildrenVisitingContext(context);
    }
  }

  /**
   * Returns the FacesBean used for storing the component's state.
   */
  abstract public FacesBean getFacesBean();

  /**
   * Adds an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void addAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Removes an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void removeAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Gets the registered AttributeChangeListeners.
   */
  abstract public AttributeChangeListener[] getAttributeChangeListeners();

  /**
   * Sets a method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void setAttributeChangeListener(MethodExpression me);

  /**
   * Gets the method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public MethodExpression getAttributeChangeListener();

  abstract public void markInitialState();

  /**
   * Provides additional context (the target child component for which the container
   * client ID is requested) to a naming container for constructing a client ID.
   * This is useful for components such as @link UIXTable and @link UIXTreeTable which need
   * to return different container client IDs for stamped and non-stamped child components.
   * @see UIXComponentBase#getClientId(FacesContext context)
   */
  abstract public String getContainerClientId(FacesContext context, UIComponent child);


  /**
   * Provides a logical parent for this component (a parent in the context of the document where this component was
   * defined). The default implementation will simply call getParent(). Components that get relocated during
   * tag execution will return their original parent
   * @return logical parent component
   */
  public UIComponent getLogicalParent()
  {
    return getParent();
  }

  /**
   * Provides a logical parent for the component (a parent in the context of the document where the component was
   * defined). The default implementation will simply call getParent() on the component. Components that get relocated during
   * tag execution should have their original parent returned (if available).
   * @param component - child component whose parent is being retrieved
   * @return logical parent component
   */
  public static UIComponent getLogicalParent(UIComponent component)
  {
    if (component instanceof UIXComponent)
    {
      return ((UIXComponent)component).getLogicalParent();
    }

    return component.getParent();
  }

  /**
   * We are using FacesBean to save state, which does not implement StateHelper, so
   * calling this method will call UnsupportedOperationException
   */
  @Override
  protected StateHelper getStateHelper()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * We are using FacesBean to save state, which does not implement StateHelper, so
   * calling this method will call UnsupportedOperationException
   */
  @Override
  protected StateHelper getStateHelper(boolean create)
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Determine if we can flatten a core JSF component.
   * @param component The component
   * @return true if the component is a core JSF component and we can
   * flatten it successfully.
   */
  private static boolean isFlattenableCoreComponent(UIComponent component)
  {
    // Optimize the cases of UINamingContainer (<f:subview>) and UIPanel -
    // we will treat these components as FlattenedComponents because they do not render
    // any DOM.
    // Also note that as of JSF 2.0, UINamingContainer components are built
    // by f:subview, as well as composite components.
    Class<? extends UIComponent> componentClass = component.getClass();

    if (UINamingContainer.class == componentClass)
    {
      // Check to see if this component was created as a composite
      // component, which we cannot flatten
      return component.getFacet(UIComponent.COMPOSITE_FACET_NAME) == null;
    }

    // Note that JSF 2.0 creates UIPanel wrappers around multiple components
    // inside of <f:facet>
    return UIPanel.class == componentClass;
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(UIXComponent.class);
}
