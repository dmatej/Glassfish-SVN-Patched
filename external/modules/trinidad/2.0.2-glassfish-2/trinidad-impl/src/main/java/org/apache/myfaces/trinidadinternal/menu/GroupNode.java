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
package org.apache.myfaces.trinidadinternal.menu;

import java.lang.reflect.Array;

import java.util.Iterator;
import java.util.List;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Code specific to a Menu Model's GroupNode.
 * @deprecated Use org.apache.myfaces.trinidad.menu.GroupNode instead of this one.
 */

@Deprecated
public class GroupNode extends MenuNode
{
  /**
    * Constructs a GroupNode
    */
  public GroupNode()
  {
    super();
  }

  /**
    * Called by the Default ActionListener
    * when a menu node is clicked/selected.
    *
    * @return String outcome or viewId used
    *         during a POST for navigation.
    */
  @Override
  public String doAction()
  {
    // Call the doAction method of my idref node
    return getRefNode().doAction();
  }

  /**
    * Get the Destination URL of a page for a
    * GET.
    *
    * @return String URL of a page.
    */
  @Override
  public String getDestination()
  {
    // Call the getDestination method of my idref node
    return getRefNode().getDestination();
  }

  /**
    * Sets the idref of the node.
    *
    * The value of this attribute is an "id" of another node
    * This tells the pointing node where to obtain its viewId and
    * takes precedence (and will replace) the pointing nodes viewId,
    * if one exists.  This should point to a node of the same style,
    * e.g. actionNode points to actionNode.
    *
    * @param idref - String name pointing to the "id" of another node
    */
  public void setIdRef(String idref)
  {
    _idref = idref;

    // Create a list of idref's for easier access
    if (_idref != null)
      _makeIdRefList (idref);
  }

  /**
    * Get the node whose id matches this node's
    * idref attribute value.
    *
    * @return the MenuNode whose id matches this
    *         node's idref attribute value.
    */
  @Override
  public MenuNode getRefNode()
  {
    MenuNode refNode = null;

    // create one if it does not exist
    // should not happen, but can't hurt
    if (_idrefList == null)
    {
      String idref = getIdRef();
      _makeIdRefList(idref);
    }

    // Get idrefList
    String[] idrefList = _getIdRefList();

    // get group node's children
    List<MenuNode> children = getChildren();

    // Traverse the list. Do the following:
    //    o get Node from Model's hashMap of nodes and ids
    //    o check attributes (rendered, disabled, readOnly)
    //    o if they are ok, return the node
    for (int i=0; i < Array.getLength(idrefList); i++)
    {
      Iterator<MenuNode> childIter = children.iterator();

      // All node "id" attribute values had the node's
      // system hashcode id appended to the id when
      // placed in the model's idNodeMap.
      //
      // Each id in the idreflist of a group node does
      // NOT have this node sys id appended it to it
      // and needs to or we won't find the group's
      // ref node.
      //
      // Since group nodes can only point to one of
      // its children, we iterate through them, get
      // their sys id and append it to idref until
      // we find a match (or not).
      while (childIter.hasNext())
      {
        MenuNode childNode = childIter.next();
        String modelId = childNode.getModelId();

        // Need to append mode's sys id here to create a
        // unique id.
        String refNodeId = idrefList[i] + modelId;

        refNode = (MenuNode) getRootModel().getNode(refNodeId);

        // if nothing found, move on to the next child
        if (refNode != null)
         break;
      }

      if (refNode == null)
        continue;

      // Check the attributes of the found node
      if (   !refNode.getRendered()
          ||  refNode.getDisabled()
          ||  refNode.getReadOnly()
          || !refNode.getVisible()
         )
      {
        refNode = null;
        continue;
      }

      // Ok, we have a valid RefNode
      break;
    }

    // If no valid node is found,
    // log an error
    if (refNode == null)
    {
        _LOG.severe("GroupNode " + getLabel() + " refers to no valid node.\n");
        return null;
    }

    return refNode;
  }

  /**
    * Get the id of the node referred to by
    * the idref attribute of this node.
    *
    * @return String id of the node referred
    *         to by the idref attribure of
    *         this node.
    */
  public String getIdRef()
  {
    return _idref;
  }
  
  public String[] getIdRefListProperty()
  {
    return _idrefList;
  }

  @Override
  protected MenuNode getThreadSafeCopy()
  {
    return new ImmutableGroupNode(this);
  }
  
  /* =============================================================
   * Private methods
   * =============================================================*/

  /**
    * _getIdRefList. gets the list of idrefs for this node.
    *
    * @return String[] list of idrefs for this node.
    */
  private String[] _getIdRefList()
  {
    return _idrefList;
  }

  /**
    * Make a list of idref entries from the nodes String
    * of idref's.
    *
    * This should only be called from the node's setIdRef
    * method.  So if it is called more than once (highly
    * unlikely), simply empty out the previous contents.
    *
    * @param entries - String of String entries
    *
    */
  private void _makeIdRefList (String entries)
  {
    _idrefList = entries.trim().split("\\s+");
  }

  private String   _idref     = null;
  private String[] _idrefList = null;

  private final static TrinidadLogger _LOG =
       TrinidadLogger.createTrinidadLogger(GroupNode.class);
}
