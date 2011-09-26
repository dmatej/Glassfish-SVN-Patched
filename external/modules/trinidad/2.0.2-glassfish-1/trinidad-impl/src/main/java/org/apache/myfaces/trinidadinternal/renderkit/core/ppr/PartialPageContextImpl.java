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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import javax.faces.component.NamingContainer;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.component.UIXComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;
import javax.faces.component.visit.VisitResult;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidad.context.PartialPageContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Context object which is used to track the targets of a partial
 * page render during the partial page rendering pass.
 * Clients never need to explicitly create PartialPageContext
 * objects.
 * <p>
 * During the partial rendering pass, some Renderer implementations
 * may modify the set of partial targets that are rendered.
 * (For example, the FormRenderer adds a partial target for its
 * shared hidden fields if any children of the form are rendered.)
 * After the partial render pass, getPartialTargets() can be
 * called to determine the actual set of partial targets that were
 * rendered.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/ppr/PartialPageContext.java#0 $) $Date: 10-nov-2005.19:02:58 $
 */
public class PartialPageContextImpl extends PartialPageContext
{
  /**
   * Creates a PartialPageContext to use to render the partial targets with
   * the specified ids.
   */
  public PartialPageContextImpl(
    FacesContext   context,
    RequestContext reqContext)
  {
    _targets = new HashMap<String, Boolean>();
    _renderedTargets = new HashSet<String>();
    _targetIds = new HashSet<String>();

    // Intialize subtreeClientIds collection
    _subtreeClientIds = new HashMap<String, Collection<String>>();
    
    // Pre-allocate the rendered stack
    _currentTargetStack = new Stack<String>();
    
    // Create the VisitContext delegating back to the
    // PartialPageContext
    _visitContext = new PartialPageVisitContext();

    _context = (context != null)
                ? context
                : FacesContext.getCurrentInstance(); 
    
    if (reqContext instanceof RequestContextImpl)
    {
      // Components may add themselves to the partialTargets list in earlier
      // phases (we don't get here until render response). If so, the IDs have
      // been kept on the RequestContext. We'll grab them now and add them to the
      // target list.
      RequestContextImpl requestContext =
        (RequestContextImpl) reqContext;
      Iterator<String> targetIter = requestContext.getPartialTargets();
      while (targetIter.hasNext())
        addPartialTarget(targetIter.next());
    }
    
    if (_targets.isEmpty())
    {
      _LOG.fine("PPR is about to render without any targets");
    }
    
    // unmodifiable list of targets for use from VisitContext
    _unmodifiableTargets = Collections.unmodifiableSet(_targets.keySet());
  }

  /**
   * Returns the set of partial targets for this rendering pass.
   */
  @Override
  public Iterator<String> getPartialTargets()
  {
    return _targets.keySet().iterator();
  }



  /**
   * Tests whether the specified id is the client id of a UIComponent that
   * should be rendered as part of the partial rendering pass.
   */
  @Override
  public boolean isPartialTarget(String clientId)
  {
    return (clientId != null) && _targets.containsKey(clientId);
  }

  /**
   * <p>
   * Tests whether the specified component id is a component id of a UIComponent that
   * might be rendered in this partial rendering pass.
   * </p>
   * <p>
   * As calculating clientIds is expensive, this method allows a cheap test to reject components
   * that shouldn't be rendered. If this method returns true, a more
   * exact test using <code>isPartialTarget</code> with the desired clientId should be performed.
   * </p>
   * @return <code>true</code> if a component with this id should be rendered.
   * @see #isPartialTarget
   */
  @Override
  public boolean isPossiblePartialTarget(String componentId)
  {
    return (componentId != null) && _targetIds.contains(componentId);
  }

  /**
   * Tests whether the specified partial target has been rendered.
   */
  @Override
  public boolean isPartialTargetRendered(String id)
  {
    return _renderedTargets.contains(id);
  }

  /**
   * Returns <code>true</code> if all of the partial targets have been rendered.
   * @return <code>true</code> if all of the partial targets have been rendered.
   */
  @Override
  public boolean areAllTargetsProcessed()
  {
    return _renderedTargets.size() == _targets.size();
  }

  /**
   * Adds a new partial target to render.
   * <p>
   * This method may be called during the partial rendering pass to
   * add to the set of partial targets, but only if the pass has
   * not yet been completed.  Clients should first check to see
   * whether the partial rendering pass has finished by calling
   * isPartialPassComplete() before calling this method.
   *
   * @param clientId The clientId of the partial target to render
   */
  @Override
  public void addPartialTarget(String clientId)
  {
    _targets.put(clientId, Boolean.FALSE);
    
    int lastFragmentIndex = clientId.lastIndexOf(NamingContainer.SEPARATOR_CHAR);
    
    String id = (lastFragmentIndex != -1)
                  ? clientId.substring(lastFragmentIndex + 1)
                  : clientId;
    
    _targetIds.add(id);

    // Update the subtree ids collection
    _addSubtreeClientId(clientId);
  }

  /**
   * Returns true if we are inside of a partial target.
   */
  @Override
  public boolean isInsidePartialTarget()
  {
    return _getCurrentPartialTarget() != null;
  }

  /**
   * Adds a partial target that has already been rendered;  this
   * is needed if the "clientId" of a component does not match
   * up to the top element (or elements).
   */
  @Override
  public void addRenderedPartialTarget(String clientId)
  {
    _renderedTargets.add(clientId);
  }

  @Override
  public Iterator<String> getRenderedPartialTargets()
  {
    return _renderedTargets.iterator();
  }
  
  /**
   * Returns the VisitContext to use when partial rendering.
   * @return the VisitContext to use when partial rendering.
   */
  @Override
  public VisitContext getVisitContext()
  {
    return _visitContext;
  }

  /**
   * Notifies the PartialPageContext that the specified partial target is
   * about to be rendered.
   * <p>
   * This method is called automatically by Trinidad during the partial
   * rendering pass when a partial target is about to be rendered.
   * Clients should never need to call this method.
   *
   * @param clientId The clientId of the partial target that is about to be rendered
   * @see #popRenderedPartialTarget
   */
  public void pushRenderedPartialTarget(
    String clientId
    )
  {
    if (_LOG.isFine())
    {
      if (!_targets.containsKey(clientId))
        _LOG.fine("Rendering partial target {0}, which was not requested", clientId);
    }

    _targets.put(clientId, Boolean.TRUE);
    _currentTargetStack.push(clientId);

    if (_LOG.isFiner())
    {
      _LOG.finer("Pushed rendered PPR target " + clientId);
    }
  }

  /**
   * Notifies the PartialPageContext that the current partial target
   * has finished rendering.
   * <p>
   * This method is called automatically by Trinidad during the partial
   * rendering pass when a partial target has finished rendering.
   * Clients should never need to call this method.
   */
  public void popRenderedPartialTarget()
  {
    _currentTargetStack.pop();
  }


  /**
   * Returns the ID of the partial target that is currently being
   * rendered, if any.
   */
  private String _getCurrentPartialTarget()
  {
    if (_currentTargetStack.empty())
      return null;

    return _currentTargetStack.peek();
  }

  // Given a single client id, populate the subtree map with all possible
  // subtree client ids
  private void _addSubtreeClientId(String clientId)
  {
    // Loop over the client id and find the substring corresponding to
    // each ancestor NamingContainer client id.  For each ancestor
    // NamingContainer, add an entry into the map for the full client
    // id.
    char separator = NamingContainer.SEPARATOR_CHAR;
    
    int length = clientId.length();

    for (int i = 0; i < length; i++)
    {
      if (clientId.charAt(i) == separator)
      {
        // We found an ancestor NamingContainer client id - add 
        // an entry to the map.
        String namingContainerClientId = clientId.substring(0, i);

        // Check to see whether we've already ids under this
        // NamingContainer client id.  If not, create the 
        // Collection for this NamingContainer client id and
        // stash it away in our map
        Collection<String> c = _subtreeClientIds.get(namingContainerClientId);

        if (c == null)
        {
          // TODO: smarter initial size?
          c = new ArrayList<String>();
          _subtreeClientIds.put(namingContainerClientId, c);
        }

        // Stash away the client id
        c.add(clientId);
      }
    }
  }

  /**
   * Internal implementation of VisitContext used for optimised PPR rendering
   * ensuring a single source of truth by delegating back to the PartialPageContext
   * so that additions to the PartialPageContext's partial targets are reflected
   * in the set of visited components
   */
  private class PartialPageVisitContext extends VisitContext
  {
    public FacesContext getFacesContext()
    {
      return _context;
    }

    public PhaseId getPhaseId()
    {
      return PhaseId.RENDER_RESPONSE;
    }

    public Collection<String> getIdsToVisit()
    {
      // because we don't delegate back to the PartialPageContextImpl, this needs to
      // prevent modification
      return _unmodifiableTargets;
    }

    public Collection<String> getSubtreeIdsToVisit(UIComponent component)
    {
      // Make sure component is a NamingContainer
      if (!(component instanceof NamingContainer))
      {
        throw new IllegalArgumentException("Component is not a NamingContainer: " + component);
      }

      String clientId = component.getClientId(getFacesContext());
      Collection<String> ids = _subtreeClientIds.get(clientId);

      if (ids == null)
        return Collections.emptyList();
      else
        return Collections.unmodifiableCollection(ids);     
    }

    public VisitResult invokeVisitCallback(UIComponent component,
                                           VisitCallback callback)
    {
      // First sure that we should visit this component - ie.
      // that this component is represented in our id set.
      VisitResult result;
     
      if (component instanceof UIXComponent)
      {
        // delegate to the UIXComponent to let it control partial encoding behavior
        result = ((UIXComponent)component).partialEncodeVisit(this,
                                                              PartialPageContextImpl.this,
                                                              callback);
      }
      else
      {
        // check that this component should be encoded
        if (_targetIds.contains(component.getId()) &&
            _targets.containsKey(component.getClientId(_context)))
        {
          // If we made it this far, the component matches one of
          // client ids, so perform the visit.
          result = callback.visit(this, component);         
        }
        else
        {
          // Not visiting this component, but allow visit to
          // continue into this subtree in case we've got
          // visit targets there.
          result = VisitResult.ACCEPT;
        }
      }

      // If we've encoded everything.
      // Return VisitResult.COMPLETE to terminate the visit.
      if (areAllTargetsProcessed())
        return VisitResult.COMPLETE;
      else
      {
        // Otherwise, just return the callback's result 
        return result;
      }
    }

    public Set<VisitHint> getHints()
    {
      return _PPR_VISIT_HINTS;
    }                                                             
  }

  private static final Set<VisitHint> _PPR_VISIT_HINTS = Collections.unmodifiableSet(
                                                         EnumSet.of(VisitHint.SKIP_UNRENDERED, VisitHint.EXECUTE_LIFECYCLE));
  private final FacesContext _context;
  
  // if the value is TRUE, then this target has been rendered.  If false, it has yet to be rendered
  private final Map<String, Boolean> _targets;
  private final Set<String> _renderedTargets;
  
  // Set of target ids to render.  This is an optimization so allow
  // fewer calls to getClientId
  private final Set<String> _targetIds;
  private final Set<String> _unmodifiableTargets;

  // This map contains the information needed by getIdsToVisit().
  // The keys in this map are NamingContainer client ids.  The values
  // are collections containing all of the client ids to visit within
  // corresponding naming container.
  private final Map<String, Collection<String>> _subtreeClientIds;

  // The stack of partial targets that are currently being rendered
  // -= Simon Lessard =-
  // FIXME: java.util.Stack... enough said... ArrayList or LinkedList please
  // =-= btsulliv Wait until we can use ArrayDeque
  private final Stack<String> _currentTargetStack;

  private final VisitContext _visitContext;
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PartialPageContextImpl.class);
}
