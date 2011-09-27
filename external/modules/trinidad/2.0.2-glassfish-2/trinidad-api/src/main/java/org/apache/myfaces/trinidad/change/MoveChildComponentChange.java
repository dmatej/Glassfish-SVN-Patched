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

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ComponentUtils;

import org.w3c.dom.Node;


/**
 * Change specialization for moving a child from one container to another.
 * MoveChildComponent should be registered on a parent component that is
 * common to the child being moved and the container component at destination.
 * In other words, while calling addComponentChange() or addDocumentChange()
 * methods on the ChangeManager to add a MoveChildComponentChange, the common
 * parent component instance must be passed as an argument. The add() utility
 * method in this class can be alternatively used to conveniently register the
 * change against the common parent. While applying this change, if a child with
 * the same id as the movable child were to be already present in the destination 
 * container, the move operation is aborted.
 * @see #add(FacesContext, ChangeManager)
 * @see ChangeManager#addComponentChange(FacesContext, UIComponent, ComponentChange)
 * @see ChangeManager#addDocumentChange(FacesContext, UIComponent, DocumentChange)
 */
public final class MoveChildComponentChange 
  extends ComponentChange
  implements DocumentChange
{
  /**
   * Constructs a MoveChildComponentChange. The child will be appended to the 
   * list of children of the destinationContainer.
   * @param movableChild The child component to be moved.
   * @param destinationContainer The destination component into which the child 
   * component is to be moved.
   * @throws IllegalArgumentException If movableChild or destinationContainer
   * is null
   */
  public MoveChildComponentChange(
    UIComponent movableChild,
    UIComponent destinationContainer)
  {
    this(movableChild, destinationContainer, null);
  }
  
  /**
   * Constructs a MoveChildComponentChange. The child will be inserted to the 
   * list of children of the destinationContainer, before the supplied 
   * insertBeforecomponent. If the supplied insertBeforeComponent is null, the 
   * child will be appended to the list of children of the destinationContainer.
   * If the insertBeforeComponent is non-null, and if it were not to be found
   * while applying this change, the movableChild will not be moved.
   * @param movableChild The child component to be moved.
   * @param destinationContainer The destination component into which the child 
   * component is to be moved.
   * @param insertBeforeComponent The component before which the moved child is
   * to be inserted. This can be null, in which case the movableChild is
   * appended.
   * @throws IllegalArgumentException If movableChild or destinationContainer
   * is null, or if a parent component common to movableChild and 
   * destinationContainer could not be found.
   */
  public MoveChildComponentChange(
    UIComponent movableChild,
    UIComponent destinationContainer, 
    UIComponent insertBeforeComponent)
  {
    if (movableChild == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("MOVABLE_CHILD_REQUIRED"));

    if (destinationContainer == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("DESTINATION_CONTAINER_REQUIRED"));
    
    UIComponent viewRoot = FacesContext.getCurrentInstance().getViewRoot();
    
    String sourceAbsoluteLogicalScopedId = ComponentUtils.getLogicalScopedIdForComponent(movableChild, viewRoot);
    
    String destinationContainerLogicalPrefix = _getScopedIdPrefix(destinationContainer,
                                          ComponentUtils.getLogicalScopedIdForComponent(destinationContainer, viewRoot));
    
    String movableChildId = movableChild.getId();
    
    String destinationAbsoluteLogicalScopedId = (destinationContainerLogicalPrefix != null)
                                          ? new StringBuilder(destinationContainerLogicalPrefix).
                                            append(NamingContainer.SEPARATOR_CHAR).
                                            append(movableChildId).toString()
                                          : movableChildId;

    // Get the common parent
    _commonParent = 
      _getClosestCommonParentUIXComponent(movableChild, destinationContainer);
    
    if (_commonParent == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("COMMON_PARENT_NOT_FOUND"));
    
    // Get the scoped id's for all participants
    _movableChildScopedId = 
      ComponentUtils.getScopedIdForComponent(movableChild, _commonParent);
    _sourceParentScopedId = 
      ComponentUtils.getScopedIdForComponent(movableChild.getParent(), 
                                            _commonParent);
    _destinationContainerScopedId = 
      ComponentUtils.getScopedIdForComponent(destinationContainer, _commonParent);
          
    _commonParentScopedId = 
      ComponentUtils.getScopedIdForComponent(_commonParent, viewRoot);
    if (_movableChildScopedId == null || 
        _sourceParentScopedId == null || 
        _destinationContainerScopedId == null ||
        _commonParentScopedId == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("MOVE_PARTICIPANTS_WITHOUT_ID"));

    // calculate the absolute scoped ids for the source and destination so that we can
    // handle remapping scoped ids in the SessionChangeManager    
    String commonParentPrefix = _getScopedIdPrefix(_commonParent, _commonParentScopedId);
      
    _sourceAbsoluteScopedId = (commonParentPrefix != null)
                                 ? new StringBuilder(commonParentPrefix).
                                           append(NamingContainer.SEPARATOR_CHAR).
                                           append(_movableChildScopedId).toString()
                                 : _movableChildScopedId;
    
    _sourceAbsoluteLogicalScopedId = _sourceAbsoluteScopedId.equals(sourceAbsoluteLogicalScopedId) ? null : 
                                                                                          sourceAbsoluteLogicalScopedId; 
    
    // calculate the absolute scoped id of the destination
    String destinationContainerPrefix = _getScopedIdPrefix(destinationContainer,
                                                           _destinationContainerScopedId);
    
    StringBuilder destinationScopedIdBuilder = new StringBuilder();
    
    if (commonParentPrefix != null)
    {
      destinationScopedIdBuilder.append(commonParentPrefix).append(NamingContainer.SEPARATOR_CHAR);
    }
    
    if (destinationContainerPrefix != null)
    {
      destinationScopedIdBuilder.append(destinationContainerPrefix).append(NamingContainer.SEPARATOR_CHAR);
    }
    
    _destinationAbsoluteScopedId = destinationScopedIdBuilder.append(movableChildId).toString();
    
    _destinationAbsoluteLogicalScopedId = _destinationAbsoluteScopedId.equals(destinationAbsoluteLogicalScopedId) ? null :
                                              destinationAbsoluteLogicalScopedId;

    // For insertBeforeComponent, we do not care to obtain scoped id.
    _insertBeforeId = (insertBeforeComponent == null) ? 
      null:insertBeforeComponent.getId();
  }
  
  private String _getScopedIdPrefix(UIComponent component, String scopedId)
  {
    if (component instanceof NamingContainer)
      return scopedId;
    else
    {
      // remove the component's id from the end
      int separatorIndex = scopedId.lastIndexOf(NamingContainer.SEPARATOR_CHAR);
      
      if (separatorIndex >= 0)
        return scopedId.substring(0, separatorIndex);
      else
      {
        // component was at top level
        return null;
      }
    }
  }
  
  /**
   * Convenience method to add this MoveChildComponentChange to the supplied
   * ChangeManager. The change will be registered against a parent component
   * that is common to the child being moved and the container component at
   * destination.
   * @param facesContext The FacesContext instance for the current request
   * @param changeManager The ChangeManager instance on which this
   * MoveChildComponentChange is to be added.
   * @return The common parent component against which this 
   * MoveChildComponentChange was registered.
   */
  public UIComponent add(
    FacesContext facesContext, 
    ChangeManager changeManager) 
  {
    UIComponent commonParent = _commonParent;

    if (commonParent == null)
      commonParent = 
        facesContext.getViewRoot().findComponent(_commonParentScopedId);
    if (commonParent == null)
    {
      _LOG.warning("COMMON_PARENT_NOT_FOUND", _commonParentScopedId);
      return null;
    }
    
    // Register a move change against the common parent
    changeManager.addComponentChange(facesContext, commonParent, this);
    
    // We dont need to keep the common parent anymore
    _commonParent = null;
    
    return commonParent;
  }
   
  /**
   * Apply this change to the specified component.
   * @param changeTargetComponent The component that is a common parent to the 
   * movable child and the destination container.
   * @throws IllegalArgumentException If the supplied changeTargetComponent
   * is null.
   */
  @Override
  public void changeComponent(UIComponent changeTargetComponent)
  {
    if (changeTargetComponent == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("COMPONENT_REQUIRED"));
    
    // 1. Check for destination container component 
    UIComponent destinationContainer = 
      changeTargetComponent.findComponent(_destinationContainerScopedId);
    if(destinationContainer == null)
    {
      _LOG.warning("DESTINATION_CONTAINER_NOT_FOUND", 
                   _destinationContainerScopedId);
      return;
    }
    
    // 2. Find movableChild, gather the possible duplicates (theoritically only three) and keep a
    //  single copy among them.
    //  Duplicates are possible because 
    //  a) taghandlers re-create the component that was in the jspx file in
    //    their original location, no matter whether it was moved/removed due to 
    //    aplication of a different ComponentChange. Such components could now 
    //    be considered duplicates, because they are newly created from their vanilla definition
    //    in the jspx document, and would not have any further ComponentChanges applied on them. 
    //    There should be one such duplicate.
    //  b) Apart from adding the MoveComponentChange, we expect developers to apply the change in 
    //    order to reflect in the same request cycle (i.e. developers call 
    //    ComponentChange.changeComponent()). Consequently, the component tree contains a moved 
    //    child at the destination. Such components must be preserved, because they have 
    //    incorporated any subsequent ComponentChanges on them. There should be one such moved
    //    component.
    //  c) We would have moved/added components due to previous customization an earlier application 
    //    of ComponentChange, that could still be in the view tree. There should be one such zombie/ 
    //    duplicate.
    UIComponent sourceParent = 
      changeTargetComponent.findComponent(_sourceParentScopedId);
    
    UIComponent foundChild = 
      changeTargetComponent.findComponent(_movableChildScopedId);

    // To flag if a child was already found in a destination container (maybe due to previous move)    
    boolean isChildIdAtDestination = false;
    
    UIComponent movableChild = null;
    int movableChildIndex = 0;
    UIComponent movedChild = null;
    int movedChildIndex = 0;
    UIComponent duplicateChild = null;
    int duplicateChildIndex = 0;
    UIComponent duplicateChildParent = null;

    while (foundChild != null)
    {
      // 2.a. If the parent matches, this could be the component that JSF-Runtime re-created
      //  and added because it is in the physical document
      if (foundChild.getParent().equals(sourceParent))
      {
        movableChild = foundChild;
        movableChildIndex = sourceParent.getChildren().indexOf(movableChild);
      }
      // 2.b.a. We could possibly find the child at its destination, because apart from
      //  adding the change, the change was applied in previous request, and the move
      //  could have been within the same naming container umbrella. In this case
      //  we do not want to move anything and the movable child is considered as a 
      //  duplicate and candidate for removal.
      else if (foundChild.getParent().equals(destinationContainer))
      {
        isChildIdAtDestination = true;
        movedChild = foundChild;
        movedChildIndex = destinationContainer.getChildren().indexOf(movedChild);
      }
      // 2.c. Possible dup from subsequent MoveChildComponentChange in the sequence of multiple
      //  moves of the component in this same request. For example, if the move is from A->B->C,
      //  and if we are currently dealing with move from A->B, the component that was added at
      //  position C (in addition to adding the move change to changemanager) will now be dup.
      else
      {
        duplicateChild = foundChild;
        duplicateChildIndex = foundChild.getParent().getChildren().indexOf(foundChild);
        duplicateChildParent = foundChild.getParent();
      }

      // Invariably, remove the found component from the tree. We remove the
      //  movableChild also, otherwise, findComponent blind loops on this same 
      //  component if movableChild and duplicates are within same immediate
      //  NamingContainer.
      foundChild.getParent().getChildren().remove(foundChild);

      // Try and find the next potential copy of the component to move
      foundChild = changeTargetComponent.findComponent(_movableChildScopedId);
    }
    
    //  We need to re-attach the dup for now, the dupes will be eliminated gradually while applying
    //  the successive move change involving the same component.
    if (duplicateChild != null)
    {
      duplicateChildParent.getChildren().add(duplicateChildIndex, duplicateChild);
    }

    // Can't do anything without a movable child.    
    if(movableChild == null)
    {
      _LOG.warning("MOVABLE_CHILD_NOT_FOUND", _movableChildScopedId);
      // Reverse any damage that we might have caused, and exit
      if (movedChild != null)
      {
        destinationContainer.getChildren().add(movedChildIndex, movedChild);
      }
      return;
    }
    
    // 2.b.b. Similar to situation in step #2.b.a, but here the move is across different naming 
    //  containers, we could not catch this earlier.
    if (!isChildIdAtDestination)
    {
      String movableChildId = movableChild.getId();
      for (UIComponent childComponent:destinationContainer.getChildren())
      {
        if (movableChildId.equals(childComponent.getId()))
        {
          isChildIdAtDestination = true;
          movedChild = childComponent;
          // Temporarily remove this child, we might add it back in step #3 below.
          movedChild.getParent().getChildren().remove(movedChild);
          break;
        }
      }
    }

    // 3. Check whether the destination container has a child with same id.
    if (isChildIdAtDestination)
    {
      _LOG.warning("MOVABLE_CHILD_SAME_ID_FOUND", _movableChildScopedId);

      // Component type matches, this means the child is already at destination. We have removed all
      //  duplicates, and have nothing more to do in this case
      if ( (movableChild.getFamily().equals(movedChild.getFamily())) &&
             (movableChild.getRendererType().equals(movedChild.getRendererType())) )
      {
        // Add back the moved child that we removed earlier.
        destinationContainer.getChildren().add(movedChildIndex, movedChild);
      }
      else
      {
        // Duplicate child by id, but not of the same component type - a condition we cannot handle.
        // Reverse any damage that we might have caused and exit
        sourceParent.getChildren().add(movableChildIndex, movableChild);
      }
      return;
    }

    // We are now dealing with case where there were no duplicates, and a proper point-to-point
    //  move should happen. Reattach the moveable child, so that move happens atomically at the end.
    sourceParent.getChildren().add(movableChildIndex, movableChild);
    
    // 4. See if we can find the insertBeforeComponent among the destinationContainer's children
    int insertIndex = -1;
    if (_insertBeforeId != null)
    {
      for (UIComponent childComponent:destinationContainer.getChildren())
      {
        if (_insertBeforeId.equals(childComponent.getId()))
        {
          insertIndex = 
            destinationContainer.getChildren().indexOf(childComponent);
          break;
        }
      }
  
      // insertBeforeId was specified, but we cannot find the insertBefore component. Exit.
      if (insertIndex == -1)
      {
        _LOG.warning("INSERT_BEFORE_NOT_FOUND", _insertBeforeId);
        return;
      }
    }
    
    // 5. Atomically move the child
    if (insertIndex == -1)
      destinationContainer.getChildren().add(movableChild);
    else
      destinationContainer.getChildren().add(insertIndex, movableChild);
  }
  
  /**
   * Given the DOM Node representing a Component, apply any necessary
   * DOM changes. The node passed will be the Node that is a common parent for
   * the movable child and the destination container.
   * There is a limitation with the document change, that the movable child 
   * Node, destination container Node, and the common parent Node have to belong
   * to the same document.
   * @param changeTargetNode DOM Node that is a common parent for the movable
   * child and the destination container.
   * @throws IllegalArgumentException If changeTargeNode were to be null.
   */
  public void changeDocument(Node changeTargetNode)
  {
    if (changeTargetNode == null)
      throw new IllegalArgumentException(_LOG.getMessage("NO_NODE_SPECIFIED"));

    // Move involves four steps.
    // 1. Finding the child node, the source of move
    Node movableChildNode = 
      ChangeUtils.__findNodeByScopedId(changeTargetNode, 
                                       _movableChildScopedId, 
                                       Integer.MAX_VALUE);
    
    if(movableChildNode == null)
    {
      _LOG.warning("MOVABLE_CHILD_NOT_FOUND", _movableChildScopedId);
      return;
    }
    
    // 2. Finding the destination container node
    Node destinationContainerNode = 
      ChangeUtils.__findNodeByScopedId(changeTargetNode, 
                                       _destinationContainerScopedId, 
                                       Integer.MAX_VALUE);

    
    if(destinationContainerNode == null)
    {
      _LOG.warning("DESTINATION_CONTAINER_NOT_FOUND", 
                   _destinationContainerScopedId);
      return;
    }
    
    //3. Finding the neighbor at the destination
    Node insertBeforeNode = (_insertBeforeId == null) ? 
      null:ChangeUtils.__findNodeByScopedId(destinationContainerNode, 
                                            _insertBeforeId, 
                                            1);
    // insertBeforeId was specified, but corresponding component is missing.
    //  Abort the move.
    if(_insertBeforeId != null && insertBeforeNode == null)
    {
      _LOG.warning("INSERT_BEFORE_NOT_FOUND", _insertBeforeId);
      return;
    }

    //4. Atomically move the child.
    destinationContainerNode.insertBefore(movableChildNode, insertBeforeNode);
  }

  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   * @return true Since moving of components should force the document to reload
   */
  public boolean getForcesDocumentReload()
  {
    return true;
  }
  
  /**
   * Returns the first UIXComponent common parent of two components in a
   * subtree.
   * @param firstComponent The first UIComponent instance
   * @param secondComponent The second UIComponent instance
   * @return UIComponent The closest common parent of the two supplied 
   * components.
   */
  private static UIComponent _getClosestCommonParentUIXComponent(
    UIComponent firstComponent,
    UIComponent secondComponent) 
  {
    if (firstComponent == null || secondComponent == null)
      return null;

    // Calculate the depth of each node.
    int firstDepth = _computeDepth(firstComponent);
    int secondDepth = _computeDepth(secondComponent);
           
    // Move the deeper of the two components to its ancestor at the same depth
    // as the shallower.
    if (secondDepth > firstDepth)
    {
      secondComponent = _getAncestor(secondComponent, secondDepth - firstDepth);
    }
    else if(secondDepth < firstDepth)
    {
      firstComponent = _getAncestor(firstComponent, firstDepth - secondDepth);
    }

    // Crawl up until we find the shared ancestor.
    while (firstComponent != null && (firstComponent != secondComponent))
    {
      firstComponent = firstComponent.getParent();
      secondComponent = secondComponent.getParent();
    }

    // Crawl up to first UIXComponent shared parent, since only UIXComponents 
    // have tags that apply changes.
    UIComponent sharedRoot = firstComponent;

    while ((sharedRoot != null) && !(sharedRoot instanceof UIXComponent))
      sharedRoot = sharedRoot.getParent();
          
    return sharedRoot;
  }
  
  /**
   * Returns the absolute scopedId of the source component
   */
  public String getSourceScopedId()
  {
    return _sourceAbsoluteScopedId;
  }

    
  /**
   * Returns the absolute scopedId of the source component at its destination
   */
  public String getDestinationScopedId()
  {
    return _destinationAbsoluteScopedId;
  }
  
  
  /**
   * Returns the absolute logical scopedId of the source component
   */
  public String getSourceLogicalScopedId()
  {
    return (_sourceAbsoluteLogicalScopedId == null) ? _sourceAbsoluteScopedId : _sourceAbsoluteLogicalScopedId;
  }
  
  /**
   * Returns the absolute logical scopedId of the source component at its destination
   */
  public String getDestinationLogicalScopedId()
  {
    return (_destinationAbsoluteLogicalScopedId == null) ? _destinationAbsoluteScopedId : _destinationAbsoluteLogicalScopedId;
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    
    if (!(o instanceof MoveChildComponentChange))
      return false;
    
    MoveChildComponentChange other = (MoveChildComponentChange)o;
    
    return getSourceLogicalScopedId().equals(other.getSourceLogicalScopedId()) &&
           getDestinationLogicalScopedId().equals(other.getDestinationLogicalScopedId()) &&
           _equalsOrNull(_insertBeforeId, other._insertBeforeId);
  }
  
  @Override
  public int hashCode()
  {
    int hashCode = getSourceLogicalScopedId().hashCode() + 37 * getDestinationLogicalScopedId().hashCode();
    if (_insertBeforeId != null)
    {
      hashCode = hashCode + 1369 * _insertBeforeId.hashCode();
    }
    return hashCode;
  }
      
  @Override
  public String toString()
  {
    return super.toString() + "[logical_source=" + getSourceLogicalScopedId() + " logical_destination=" + getDestinationLogicalScopedId() +
            " absolute source=" + getSourceScopedId() + " absolute destination" + getDestinationScopedId() +" insert_before=" + _insertBeforeId + "]";
  }
  
  /**
   * Returns the depth of a UIComponent in the tree. 
   * @param comp the UIComponent whose depth has to be calculated
   * @return the depth of the passed in UIComponent
   */
  private static int _computeDepth(UIComponent comp) 
  {
    int i = 0;
    while((comp = comp.getParent()) != null) 
    {
      i++;
    }
    return i;
  }

  /**
   * Returns the nth ancestor of the passed in component.
   * @param component The UIComponent whose nth ancestor has to be found
   * @param level Indicates how many levels to go up from the component
   * @return The nth ancestor of the component
   */
  private static UIComponent _getAncestor(UIComponent component, int level) 
  {
    assert(level >= 0);
    
    while(level > 0)
    {
      component = component.getParent();
      level--;
    }
    return component;
  }
  
  private boolean _equalsOrNull(Object obj1, Object obj2)
  {
    return (obj1 == null) ? (obj2 == null) : obj1.equals(obj2);
  }
  
  private transient UIComponent _commonParent;

  private final String _movableChildScopedId;
  private final String _sourceParentScopedId;
  private final String _destinationContainerScopedId;
  private final String _commonParentScopedId;
  private final String _insertBeforeId;
  private final String _sourceAbsoluteScopedId;
  private final String _destinationAbsoluteScopedId;
  private final String _sourceAbsoluteLogicalScopedId;
  private final String _destinationAbsoluteLogicalScopedId;
  private static final long serialVersionUID = 1L;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    MoveChildComponentChange.class);
}
