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
package org.apache.myfaces.trinidad.change;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.ComponentUtils;

import org.apache.myfaces.trinidad.webapp.UIXComponentELTag;

import org.w3c.dom.Document;


/**
 * A ChangeManager implementation that manages persisting the added Changes at the session. 
 * This means the lifetime of Changes added such is within the session scope. If any of the changes
 * are managed as state changes and restored by JSF state saving mechanism, the SessionChangeManager
 * will not re-apply such changes. For example: AttributeComponentChanges are not applied during
 * a postback unless its target component happened to be a result of any move/add operation, this is
 * because attribute changes are handled by state manager during postback for common cases.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/SessionChangeManager.java#0 $) $Date: 10-nov-2005.19:06:35 $
 */
public class SessionChangeManager extends BaseChangeManager
{
  /**
   * {@inheritDoc}
   * @param context The FacesContext instance for the current request.
   */
  @Override
  public void applyComponentChangesForCurrentView(FacesContext context)
  {
    _applyComponentChanges(context, null);
  }

  /**
   * {@inheritDoc}
   * @param context The FacesContext instance for the current request.
   */
   @Override
  public void applyComponentChangesForSubtree(
    FacesContext    context,
    NamingContainer root
    )
  {
    String rootId = null;
    
    if (root != null)
    {
      if (!(root instanceof UIComponent))
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "INVALID_TYPE", root));
      }
      
      rootId = ComponentUtils.getScopedIdForComponent((UIComponent)root, context.getViewRoot());
    }

    _applyComponentChanges(context, rootId);
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void applySimpleComponentChanges(FacesContext context, UIComponent component)
  {
    // we don't need to apply the component changes if we restored the state, since the
    // attributes will be up-to-date
    if (!_isStateRestored(context))
    {
      String         sessionKey     = _getSessionKey(context);
      ChangesForView changesForView = _getChangesForView(context, sessionKey, false);
      
      if (changesForView != null)
      {
        changesForView.applySimpleComponentChanges(context, component);
      }
    }
  }

  /**
   * Adds a ComponentChange and registers against the supplied component.
   * Changes added thus live at Session scope.
   * Use applyComponentChangesForCurrentView() to apply these changes.
   * @param context The FacesContext instance for the current request.
   * @param targetComponent The target component against which this change needs 
   * to be registered and applied later on.
   * @param componentChange The ComponentChange to add.
   */
  protected void addComponentChangeImpl(
    FacesContext    context,
    UIComponent     targetComponent,
    ComponentChange componentChange)
  {    
    // try to collapse AttributeComponentChanges, handling component movement so that
    // we can collapse any attribute change on the same component
    if (componentChange instanceof AttributeComponentChange)
    {
      _replaceAttributeChange(context,
                              targetComponent,
                              (AttributeComponentChange)componentChange,
                              false); // replace no matter what
    }
    else
    {
      String         sessionKey     = _getSessionKey(context);
      ChangesForView changesForView = _getChangesForView(context, sessionKey, true);
      
      // get the absolute scopedId for the target component so that we have a unique identifier
      // to compare
      String scopedIdForTargetComponent = 
        ComponentUtils.getScopedIdForComponent(targetComponent, context.getViewRoot());
      
      String logicalScopedIdForTargetComponent = 
        ComponentUtils.getLogicalScopedIdForComponent(targetComponent, context.getViewRoot());
      
      _insertComponentChange(changesForView, scopedIdForTargetComponent, logicalScopedIdForTargetComponent, componentChange);
      
      // dirty the key in the session for failover
      context.getExternalContext().getSessionMap().put(sessionKey, changesForView);
    }
  }
  
  /**
   * @inheritDoc
   */
  @Override
  public AttributeComponentChange replaceAttributeChangeIfPresent(
    FacesContext             context,
    UIComponent              component,
    AttributeComponentChange attributeComponentChange)
  {
    return _replaceAttributeChange(context, component, attributeComponentChange, true);
  }  

  /** 
   * We don't support DocumentChange persistence
   */
  @Override
  protected Document getDocument(FacesContext context)
  {
    return null;
  }

  /**
   *
   * @param context
   * @param component
   * @param attributeComponentChange
   * @param onlyIfPresent If true, we only insert a new changed if we removed an old one
   * @return the removed AttributeComponentChange, if any
   */
  private AttributeComponentChange _replaceAttributeChange(
    FacesContext             context,
    UIComponent              component,
    AttributeComponentChange attributeComponentChange,
    boolean                  onlyIfPresent)
  {    
    // get the absolute scopedId for the target component so that we have a unique identifier
    // to compare
    String scopedIdForTargetComponent = ComponentUtils.getScopedIdForComponent(
                                                                            component,
                                                                            context.getViewRoot());
    
    // check if we have an existing attribute change for the same attribute name, 
    // if found, remove it
    String         sessionKey     = _getSessionKey(context);
    ChangesForView changesForView = _getChangesForView(context, sessionKey, true);

    AttributeComponentChange replaced = _extractAttributeChange(changesForView, 
                                                                scopedIdForTargetComponent, 
                                                                attributeComponentChange);
    
    // if found, we insert the new change instance
    if (!onlyIfPresent || (replaced != null))
    {
      String logicalScopedIdForTargetComponent = ComponentUtils.getLogicalScopedIdForComponent(
                                                                              component,
                                                                              context.getViewRoot());
      
      _insertComponentChange(changesForView, scopedIdForTargetComponent, logicalScopedIdForTargetComponent, attributeComponentChange);

      // dirty the key in the session for failover
      context.getExternalContext().getSessionMap().put(sessionKey, changesForView);
    }
    
    return replaced;
  }  

  /**
   * Check if we have an existing attribute change for the same attribute name: 
   * - if not found, return null
   * - if found, remove and return the old change instance
   * 
   * @param changesForView
   * @param scopedIdForTargetComponent
   * @param attributeChange
   * @return the old change instance, null if not found
   */
  private AttributeComponentChange _extractAttributeChange(
    ChangesForView           changesForView,
    String                   scopedIdForTargetComponent,
    AttributeComponentChange attributeChange)
  {
    AttributeComponentChange extracted = null;
        
    String attributeName = attributeChange.getAttributeName();

    // would really rather use a Deque here and iterate backwards, which would also make
    // handling the rename changes easier
    Iterator<QualifiedComponentChange> changes =
                                          changesForView.getComponentChangesForView().iterator();
    
    // list of changes that have renamed the scoped id of this component.  We need to
    // handle this aliasing when traversing through the changes looking for matches
    Iterator<MoveChildComponentChange> renameChanges =
                                     changesForView.getRenameChanges(scopedIdForTargetComponent);
    
    // we need to look through the rename list to map from the current names to
    // the new names
    MoveChildComponentChange nextRenameChange;
    String currTargetScopedId;
    
    if (renameChanges.hasNext())
    {
      // we have at least one rename change, so get it and find the name that this
      // component was originally known by
      nextRenameChange = renameChanges.next();
      currTargetScopedId = nextRenameChange.getSourceScopedId();
    }
    else
    {
      nextRenameChange = null;
      currTargetScopedId = scopedIdForTargetComponent;
    }
    
    // loop forward through the changes looking for AttributeChanges to collapse
    while (changes.hasNext())
    {
      QualifiedComponentChange currQualifiedChange = changes.next();
      
      if (currQualifiedChange.getComponentChange() == nextRenameChange)
      {
        // we got a match, so update the scoped id we should be looking for
        currTargetScopedId = nextRenameChange.getDestinationScopedId();
        
        nextRenameChange = (renameChanges.hasNext())
                             ? renameChanges.next()
                             : null;
      }
      else if (currQualifiedChange.getTargetComponentScopedId().equals(currTargetScopedId))
      {
        // found a change on the same component.  Check if it's an AttributeChange
        ComponentChange currChange = currQualifiedChange.getComponentChange();
        
        if (currChange instanceof AttributeComponentChange)
        {
          AttributeComponentChange currAttributeChange = (AttributeComponentChange)currChange;
          
          // Check if the AttributeChange is for the same attribute
          if (attributeName.equals(currAttributeChange.getAttributeName()))
          {
            // the old AttributeChange is for the same attribute, so remove it since the
            // new AttributeChange would eclipse it anyway.
            changes.remove();
            extracted = currAttributeChange;
            break;
          }
        }
      }
    }

    return extracted;    
  }

  /**
   * insert a component change for a specific component
   * 
   * @param changesForView
   * @param scopedIdForTargetComponent
   * @param logicalScopedIdForTargetComponent
   * @param componentChange
   */
  private void _insertComponentChange(ChangesForView changesForView,
                                      String scopedIdForTargetComponent,
                                      String logicalScopedIdForTargetComponent,
                                      ComponentChange componentChange) 
  { 
    QualifiedComponentChange newQualifiedChange = 
      new QualifiedComponentChange(scopedIdForTargetComponent,
                                   logicalScopedIdForTargetComponent,
                                   componentChange);
    
    changesForView.addChange(newQualifiedChange);
  }

  /**
   * Implementation shared by applyComponentChangesForCurrentView() and
   * applyComponentChangesForSubtree().
   * @param context The FacesContext instance for this request.
   * @param rootId The scoped id of theNamingContainer that contains the 
   * component subtree to which ComponentChanges should be applied.  If null, 
   * all changes are applied.
   */
  private void _applyComponentChanges(
    FacesContext   context,
    String         rootId
    )
  {
    ChangesForView changesForView = _getReadOnlyChangesForView(context);
    
    UIViewRoot viewRoot = context.getViewRoot();
    
    // retrieve the ComponentChanges for this current viewid    
    boolean isStateRestored = _isStateRestored(context);
    
    // components that have their attribute application forced because a change that would confuse
    // state saving has been applied
    Set<String> attributeForcedComponents = new HashSet<String>();
    
    // loop through the viewId's changes, applying the changes
    for (QualifiedComponentChange qualifiedChange : changesForView.getComponentChangesForView())
    {
      ComponentChange componentChange = qualifiedChange.getComponentChange();
      String targetComponentScopedId  = qualifiedChange.getTargetComponentScopedId();

      // We do not apply attribute changes if it is a postback, because we expect that
      // 1. Users calling ChangeManager.addComponentChange() would also apply the change right at
      //  that point in time (maybe by calling ComponentChange.changeComponent() method).
      // 2. If #1 was done, JSF state manager will consider this a state change and will store and
      //  restore it during subsequent postbacks, so there is no need for applying attribute changes
      //  for postback cases. There are few exceptions where the state management will not help, for
      //  which we force the attribute changes even when it is a postback.
      if (isStateRestored &&
          componentChange instanceof AttributeComponentChange &&
          !attributeForcedComponents.contains(targetComponentScopedId))
      {
        continue;
      }
      
      // If change target for the qualified change is not inside of the specified root, skip
      if (!_acceptChange(qualifiedChange, rootId))
        continue;
      
      UIComponent targetComponent = viewRoot.findComponent(targetComponentScopedId);
      
      // Possible that the target component no more exists in the view, if yes, skip
      if (targetComponent == null)
      {
        _LOG.info(this.getClass().getName(),
                  "applyComponentChangesForCurrentView",
                  "TARGET_COMPONENT_MISSING_CHANGE_FAILED",
                  targetComponentScopedId);
        continue;
      }
     
      // Apply the change
      componentChange.changeComponent(targetComponent);
      
      // Now that the change is applied, we can identify if the components altered by the currently
      //  applied change needs forced application of any further changes regardless of request 
      //  being a postback.
      if (componentChange instanceof MoveChildComponentChange)
      {
        String destinationScopedId = 
                             ((MoveChildComponentChange)componentChange).getDestinationScopedId();
        
        // we no longer need to force the old scoped id, if any, but we do need to force the new one
        attributeForcedComponents.remove(targetComponentScopedId);
        attributeForcedComponents.add(destinationScopedId);
      }
      else if (componentChange instanceof SetFacetChildComponentChange)
      {
        String facetName = ((SetFacetChildComponentChange)componentChange).getFacetName();
        UIComponent facetComponent = targetComponent.getFacet(facetName);
        
        if (facetComponent != null)
        {
          String facetScopedId = ComponentUtils.getScopedIdForComponent(facetComponent, viewRoot);
          
          attributeForcedComponents.add(facetScopedId);
        }
      }
      else if (componentChange instanceof AddChildComponentChange)
      {
        // Get the added component from AddComponentChange, this component is actually re-created 
        //  from the proxy, and not the actual added component. 
        //  Bit hacky but this is only way to get Id.
        String addedComponentId = ((AddChildComponentChange)componentChange).getComponent().getId();
        
        // Now get the actual added component finding from the parent to which it was added to
        UIComponent addedComponent = ComponentUtils.findRelativeComponent(targetComponent,
                                                                          addedComponentId);
                
        if (addedComponent != null)
        {
          String addedChildComponentScopedId= ComponentUtils.getScopedIdForComponent(addedComponent,
                                                                                     viewRoot);
          attributeForcedComponents.add(addedChildComponentScopedId);
        }
      }
    }  
  }
  
  /**
   * Is the state restored by JSF state manager in this request. This is usually true if this is a
   *  postback request. Additionally check if the document tag created a document component, because
   *  if this is the case, we are sure that there was no state restoration.
   */
  private boolean _isStateRestored(FacesContext facesContext)
  {
    /*
     * We will always return false for now. The reason is, if the page has a included fragment,
     * and the fragment gets replaced during ppr, the changes inside the region will be lost.
     */
    return false;
    //boolean docCompCreated = Boolean.TRUE.equals(facesContext.getExternalContext().
    //                               getRequestMap().get(UIXComponentELTag.DOCUMENT_CREATED_KEY));
    //return (docCompCreated) ? false : RequestContext.getCurrentInstance().isPostback();
  }  

  /**
   * Tests whether the specified change should be applied based on the
   * specified root id.  If root id is null, all changes are accepted/applied.
   * If the root id is non-null, only changes which target ids underneath
   * the root id are accepted/applied.
   * 
   * @param qualifiedChange the change to test
   * @param rootId the scoped id of the NamingContainer for which we
   *   are applying changes
   * @return true if rootId is null, or if the qualifiedChange targets a
   *   component underneath the NamingContainer identified by the rootId.
   */
  private boolean _acceptChange(
    QualifiedComponentChange qualifiedChange,
    String rootId
    )
  {
    if (rootId != null)
    {
      String id = qualifiedChange.getTargetComponentScopedId();
      return (id.startsWith(rootId) && (id.length() != rootId.length()));    
    }
    else
    {
      return true;
    }
  }

  private ChangesForView _getReadOnlyChangesForView(FacesContext context)
  {
    String sessionKey = _getSessionKey(context);
    
    return _getChangesForView(context, sessionKey, false);
  }

  /**
   * Gets the in-order list of component changes for the given view.
   * @param context The FacesContext instance for this request.
   * @param sessionKey The composite session key based on the viewId 
   * @param createIfNecessary Indicates whether the underlying datastructures
   * that store the component changes needs to be created if absent.
   * @return The ChangesForView object containing information about the changes for the specified
   * viewId, including in-order list of component changes for the supplied view. This
   * will be in the same order in which the component changes were added through
   * calls to <code>addComponentChange()</code>.
   */
  private ChangesForView _getChangesForView(
    FacesContext context,
    String       sessionKey,
    boolean      createIfNecessary)
  {
    ExternalContext extContext = context.getExternalContext();
    Map<String, Object> sessionMap = extContext.getSessionMap();

    Object changes = sessionMap.get(sessionKey);
    
    if (changes == null)
    {
      if (createIfNecessary)
      {
        // make sure we only create this viewId's changes entry once
        Object session = extContext.getSession(true);
        
        synchronized (session)
        {
          changes = sessionMap.get(sessionKey);
          
          if (changes == null)
          {
            ChangesForView changesForView = new ChangesForView(true);
            
            sessionMap.put(sessionKey, changesForView);
            
            return changesForView;  // return the newly created changes
          }
          else
          {
            return (ChangesForView)changes;  // another thread created the changes for us          
          }
        }
      }
      else
      {
        return _EMPTY_CHANGES;  // no changes and we aren't allowed to create them
      }
    }
    else
    {
      return (ChangesForView)changes;  // we have the changes
    }
  }
  
  /**
   * Return the Session key to store the changes for this viewId in.  We store each viewId under
   * a different key to avoid needing to failover all of the changes whenever the changes for
   * a particular viewId are modified.
   * @param context
   * @return
   */
  private String _getSessionKey(FacesContext context)
  {
    String viewId = context.getViewRoot().getViewId();
    
    StringBuilder builder = new StringBuilder(viewId.length() +
                                              _COMPONENT_CHANGES_MAP_FOR_SESSION_KEY.length());
    
    return builder.append(_COMPONENT_CHANGES_MAP_FOR_SESSION_KEY).append(viewId).toString();
  }

  /**
   * Tracks the component changes for a particular view as well as all the movement
   * changes so that component aliasing can be tracked
   */
  private static final class ChangesForView implements Serializable
  {
    protected ChangesForView(boolean rw)
    {      
      if (rw)
      {
        _componentChangesForView = new ConcurrentLinkedQueue<QualifiedComponentChange>();
        _renameChanges = new CopyOnWriteArrayList<MoveChildComponentChange>();
      }
      else
      {
        _componentChangesForView = CollectionUtils.emptyQueue();
        _renameChanges = Collections.emptyList();
      }
    }

    @Override
    public String toString()
    {
      return super.toString() + "[componentChange=" + _componentChangesForView +
             " renameChanges=" + _renameChanges + "]";
    }

    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;
      
      if (!(o instanceof ChangesForView))
        return false;
      
      ChangesForView other = (ChangesForView)o;
      
      return _componentChangesForView.equals(other._componentChangesForView) &&
             _renameChanges.equals(other._renameChanges);
    }
    
    @Override
    public int hashCode()
    {
      return _componentChangesForView.hashCode() + 37 * _renameChanges.hashCode();
    }
    
    /** 
     * Returns the QualifiedComponentChanges for this viewId
     */
    protected Iterable<QualifiedComponentChange> getComponentChangesForView()
    {
      return _componentChangesForView;
    }
    
    /** 
     * Adds a change to the QualifiedComponentChanges for this viewId, handling
     * MoveChildComponentChanges specially to handle cases where the clientId
     * of a component changes as a result of a rename operation
     */
    protected void addChange(QualifiedComponentChange qualifiedChange)
    {
      // make sure that we don't add changes while getAttrChanges() is rebuilding the
      // per-component changes
      _componentChangesForView.add(qualifiedChange);
        
      ComponentChange componentChange = qualifiedChange.getComponentChange();

      if (componentChange instanceof AttributeComponentChange)
      {
        // update the attribute changes with the current change
        _updateAttributeChange(_attrChanges, _renameMap, qualifiedChange);
      }
      else if (componentChange instanceof MoveChildComponentChange)
      {
        // we only need to remove moves that actually changed the absolute scoped id of the
        // component
        MoveChildComponentChange moveComponentChange = (MoveChildComponentChange)componentChange;
        
        if (!moveComponentChange.getSourceScopedId().equals(moveComponentChange.getDestinationScopedId()))
        {
          _renameChanges.add(moveComponentChange);
          
          // update the rename map to account for this change
          
          _updateRenameMap(_renameMap, moveComponentChange);
        }
      }
    }
      
    /**
     * Returns the Iterator of rename changes that affect the current scoped id in ComponentChange order
     * @return
     */
    protected Iterator<MoveChildComponentChange> getRenameChanges(String targetScopedId)
    {
      if (!_renameChanges.isEmpty())
      {
        String currTargetScopedId = targetScopedId;
        List<MoveChildComponentChange> renameChanges = null;
        
        // iterate from the back of the List determining the MoveChildComponentChange
        // that are aliased to this scoped id
        ListIterator<MoveChildComponentChange> moveChanges =
                                                _renameChanges.listIterator(_renameChanges.size());
        
        while (moveChanges.hasPrevious())
        {
          MoveChildComponentChange currMoveChange = moveChanges.previous();
          
          if (currTargetScopedId.equals(currMoveChange.getDestinationScopedId()))
          {
            // lazily create the list the first time we need it
            if (renameChanges == null)
              renameChanges = new ArrayList<MoveChildComponentChange>();
            
            renameChanges.add(currMoveChange);
            
            // get the new id to search for
            currTargetScopedId = currMoveChange.getSourceScopedId();
          }
        }
        
        if (renameChanges != null)
        {
          if (renameChanges.size() > 1)
          {
            // reverse the list to match the order that we will see these items when traversing
            // the changes from the forward direction
            Collections.reverse(renameChanges);
          }
          
          return renameChanges.iterator();
        }  
      }
      
      return CollectionUtils.emptyIterator();
    }

    /**
     * Apply the attribute changes for this component
     * @param context
     * @param component
     */
    protected void applySimpleComponentChanges(FacesContext context, UIComponent component)
    {
      // Simple component changes always use logical scoped ids because they are consistent across
      // all phases including tag execution
      String scopedId = ComponentUtils.getLogicalScopedIdForComponent(component, context.getViewRoot());
      
      ConcurrentMap<String, ComponentChange> componentChanges = _attrChanges.get(scopedId);
      
      if (componentChanges != null)
      {
        for (ComponentChange change : componentChanges.values())
        {
          change.changeComponent(component);
        }
      }
    }

    private void _updateAttributeChange(
      ConcurrentMap<String, ConcurrentMap<String, ComponentChange>> attrChanges,
      ConcurrentMap<String, String>                                 renameMap,
      QualifiedComponentChange                                      qAttrChange)
    {
      // update the current attribute values for the scoped id
      String currScopedId = qAttrChange.getTargetComponentLogicalScopedId();
      
      // apply any move rename
      String originalScopedId = renameMap.get(currScopedId);
      
      // we don't add rename mapping until a move, so if there is no entry, the origina
      // value is good
      if (originalScopedId == null)
        originalScopedId = currScopedId;
      
      // get the map for this component, creating one if necessary
      ConcurrentMap<String, ComponentChange> changesForComponent = 
                                                           attrChanges.get(originalScopedId);
      
      // if we haven't registered a Map yet, create one and register it
      if (changesForComponent == null)
      {
        // =-= bts There probably aren't that many different changes per component.  Maybe
        //         we need something smaller and more efficient here
        changesForComponent = new ConcurrentHashMap<String, ComponentChange>();
        attrChanges.put(originalScopedId, changesForComponent);
      }
      
      AttributeComponentChange attrChange = (AttributeComponentChange)
                                            qAttrChange.getComponentChange();
      
      // update the current AttributeComponentChange for this attribute
      String attrName = attrChange.getAttributeName();
      
      changesForComponent.put(attrName, attrChange);
      
    }
    
    /**
     * Update the renamemap with a change
     * @param renameMap
     * @param moveChange
     */
    private void _updateRenameMap(
      ConcurrentMap<String, String> renameMap,
      MoveChildComponentChange      moveChange)
    {
      String sourceScopedId      = moveChange.getSourceLogicalScopedId();
      String destinationScopedId = moveChange.getDestinationLogicalScopedId();
      
      // we only need to update the map if we actually changed scoped ids
      if (!(sourceScopedId.equals(destinationScopedId)))
      {
        // remove the old mapping for source
        String originalScodeId = renameMap.remove(sourceScopedId);
        
        // we don't bother adding mappings if there has been no rename, plus there might
        // not be any attribute changes yet.  In this case, the source scoped id must
        // be the original id
        if (originalScodeId == null)
          originalScodeId = sourceScopedId;
        
        // add the new, updated mapping to account for the move
        renameMap.put(destinationScopedId, originalScodeId);
      }
    }

    private void readObject(java.io.ObjectInputStream in) 
      throws IOException, ClassNotFoundException 
    {
      throw new InvalidObjectException("proxy required");
    }
    
    private Object writeReplace() 
    {
      return new SerializationProxy(this);
    }
    
    private static class SerializationProxy implements Serializable 
    {
      SerializationProxy(ChangesForView changesForView) 
      {
        _componentChangesForView = 
          new ArrayList<QualifiedComponentChange>(changesForView._componentChangesForView);
        
        if (changesForView._renameChanges==Collections.EMPTY_LIST) 
          _rw = false;
        else 
          _rw = true;
      }
      
      private Object readResolve() 
      {
        ChangesForView changesForView = new ChangesForView(_rw);
        for (QualifiedComponentChange qualifiedChange : _componentChangesForView)
        {
          changesForView.addChange(qualifiedChange);
        }
        
        return changesForView;
      }
      
      private final List<QualifiedComponentChange> _componentChangesForView;
      private final boolean _rw;
      
      private static final long serialVersionUID = 1L;
    }


    private final Queue<QualifiedComponentChange> _componentChangesForView;
    private final List<MoveChildComponentChange> _renameChanges;
    
    // map of original scopedIds to Map of attribute names and their new values.  This allows
    // us to apply all of attribute changes efficiently
    private final ConcurrentMap<String, ConcurrentMap<String, ComponentChange>> _attrChanges = 
      new ConcurrentHashMap<String, ConcurrentMap<String, ComponentChange>>();
    
    // map of current scoped ids to original scoped ids.  This enables us to correctly update
    // the attributes for the original scoped ids even after the component has moved
    private final ConcurrentMap<String, String> _renameMap = 
      new ConcurrentHashMap<String, String>();

    private static final long serialVersionUID = 1L;
  }
  
  private static final ChangesForView _EMPTY_CHANGES = new ChangesForView(false);
    
  private static class QualifiedComponentChange implements Serializable
  {
    public QualifiedComponentChange(String targetComponentScopedId,
                                    String targetComponentLogicalScopedId,
                                    ComponentChange componentChange)
    {
      // NO-TRANS : Class is private and inner, no translated message required
      if (targetComponentScopedId == null || componentChange == null)
        throw new IllegalArgumentException("Target component scoped id and " +
                                           "component change is required");
      
      _targetComponentScopedId = targetComponentScopedId;
      _targetComponentLogicalScopedId = (targetComponentScopedId.equals(targetComponentLogicalScopedId)) ? null :
                                                    targetComponentLogicalScopedId;
      _componentChange = componentChange;
    }
    
    public String getTargetComponentScopedId()
    {
      return _targetComponentScopedId;
    }
    
    public String getTargetComponentLogicalScopedId()
    {
      return _targetComponentLogicalScopedId != null ? _targetComponentLogicalScopedId : _targetComponentScopedId;
    }

    public ComponentChange getComponentChange()
    {
      return _componentChange;
    }
    
    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;
      
      if (!(o instanceof QualifiedComponentChange))
        return false;
      
      QualifiedComponentChange other = (QualifiedComponentChange)o;
      
      return getTargetComponentLogicalScopedId().equals(other.getTargetComponentLogicalScopedId()) &&
             _componentChange.equals(other._componentChange);
    }
    
    @Override
    public int hashCode()
    {
      return getTargetComponentLogicalScopedId().hashCode() + 37 * _componentChange.hashCode();
    }
        
    @Override
    public String toString()
    {
      return super.toString() + "[target=" + _targetComponentScopedId + " logical_target=" + getTargetComponentLogicalScopedId() +
              " change=" + _componentChange + "]";
    }

    private final String _targetComponentScopedId;
    private final String _targetComponentLogicalScopedId;
    private final ComponentChange _componentChange;

    private static final long serialVersionUID = 1L;
  }
  
  private static final String _COMPONENT_CHANGES_MAP_FOR_SESSION_KEY =
    "org.apache.myfaces.trinidadinternal.ComponentChangesMapForSession";
      
  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SessionChangeManager.class);
}
