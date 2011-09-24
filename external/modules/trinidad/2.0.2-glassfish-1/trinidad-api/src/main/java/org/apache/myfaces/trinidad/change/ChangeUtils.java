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

import java.util.List;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;


/**
 * Utility functions for use by Changes.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ChangeUtils.java#0 $) $Date: 10-nov-2005.19:09:58 $
 */
final class ChangeUtils 
{
  private ChangeUtils()
  {
  }

  /**
   * Given a parent component and the identifier for the child, looks up among
   *  the children for a child with the specified identifier and returns.
   * Returns null if there were to be no such child
   * @param parent the parent UIComponent
   * @param childId the 'id' identifier value of child to be searched in the parent's 
   *        children.
   */
  @SuppressWarnings("unchecked")
  public static UIComponent getChildForId(UIComponent parent, String childId)
  {
    return getChildForId(parent, childId, "id");
  }
  
  /**
   * Given a parent component and the identifier value for the child, looks up among
   * the children for a child with the specified identifier and returns.
   * Returns null if there were to be no such child
   * @param parent the parent UIComponent
   * @param childId the identifier value of child to be searched in the parent's 
   *        children.
   * @param identifier the identifier type 
   */
  @SuppressWarnings("unchecked")
  public static UIComponent getChildForId(
    UIComponent parent, 
    String childId,
    String identifier)
  {
    if (parent == null)
      return null;

    int numChildren = parent.getChildCount();
    if (numChildren == 0)
      return null;

    List<UIComponent> children = parent.getChildren();
    
    for (int i=0; i<numChildren; i++)
    {
      UIComponent child = children.get(i);
      Object attrVal = child.getAttributes().get(identifier);
      
      if ( childId.equals(attrVal) )
        return child;
    }
    return null;
  }
  
  /**
   * Given a parent component and the identifier for the child, looks up among
   * the children for a child with the specified identifier and returns the index
   * of the child
   * Returns -1 if there were to be no such child
   * @param parent
   * @param childId the identifier of child to be searched in the parent's 
   * children
   */
  @SuppressWarnings("unchecked")
  public static int getChildIndexForId(UIComponent parent, String childId)
  {
    if (parent == null)
      throw new NullPointerException(_LOG.getMessage(
        "PARENT_CANNOT_BE_NULL"));

    int numChildren = parent.getChildCount();
    if (numChildren == 0)
      return -1;

    List<UIComponent> children = parent.getChildren();      
    UIComponent child;    
    for (int i=0; i<numChildren; i++)
    {
      child = children.get(i);
      if ( childId.equals(child.getId()) )
        return i;
    }
    return -1;
  }
  
  /**
   * Search the supplied Node and its descendants for an Element Node with the 
   * scopedTargetId.
   * @param baseNode The base Node of the subtree relative to which the target
   * Node is to be found.
   * @param scopedTargetId The scoped id of the target node which is to be found. 
   * This id should be relative from the base Node of the search, with 
   * NamingContainer.SEPARATOR_CHAR being the separator for fragments. If the
   * targetId starts with a NamingContainer.SEPARATOR_CHAR, it is considered
   * as an absolute id, and the owner Document will be the base Node of search.
   * Examples of scopedTargetId values: 'foo:bar:baz'/':foo:bar:baz'/'foo'.
   * @param searchDepth The integer which indicates till how many levels deeper 
   * from the baseNode, the search has to be performed.
   * @return The target Node with the given scopedTargetId if found within the
   * permitted searchDepth, else null 
   */
  static Node __findNodeByScopedId(
    Node baseNode,
    String scopedTargetId,
    int searchDepth)
  {
    if (baseNode == null || 
        scopedTargetId == null || 
        scopedTargetId.length() == 0)
      return null;
     
    // Check if we have received an absolute id.
    if (NamingContainer.SEPARATOR_CHAR == scopedTargetId.charAt(0))
    {
      // If so directly deal with the owner Document.
      if (baseNode.getNodeType() != Node.DOCUMENT_NODE)
        baseNode = baseNode.getOwnerDocument();

      // Remove leading ':'
      scopedTargetId = scopedTargetId.substring(1);
    }

    // 'foo:bar:baz' -> ['foo'],['bar'},['baz']
    String[] idFrags = 
      scopedTargetId.split(String.valueOf(NamingContainer.SEPARATOR_CHAR));
    
    return _traceNodeByIdPath(baseNode, idFrags, 0, searchDepth);
  }
  
  /**
   * Given a node representing a component, returns the named facet's Element.
   * @param componentNode The node to search for a facet contained in it.
   * @param facetName The name of the facet to search for.
   * @return
   */
  static Element __getFacetElement(
    Node componentNode,
    String facetName)
  {
    assert componentNode != null;
    assert (facetName != null) && (facetName.length() > 0);
    
    Node currChild = componentNode.getFirstChild();
    
    while (currChild != null)
    {
      // check for local name match
      if ("facet".equals(currChild.getLocalName()))
      {
        // check for namespace match
        if (__JSF_CORE_NAMESPACE.equals(currChild.getNamespaceURI()))
        {
          NamedNodeMap attributes = currChild.getAttributes();

          if (facetName.equals(attributes.getNamedItem("name").getNodeValue()))
          {
            return (Element)currChild;
          }
        }
      }

      currChild = currChild.getNextSibling();
    }
    
    return null;
  }

  /**
   * Removes all of the children from the parent Node.
   * @param parentNode 
   */
  static void __removeAllChildren(Node parentNode)
  {
    Node nukeChild = parentNode.getFirstChild();
    
    while (nukeChild != null)
    {
      parentNode.removeChild(nukeChild);
      nukeChild = parentNode.getFirstChild();
    }
  }

  /**
   * Traces for the targetNode recursively in the subtree rooted by baseNode.
   * Trace path is also controlled by the id path fragments and the fragIndex. 
   * Trace stops when the permitted searchDepth is reached, or when the trace 
   * faled.
   * @returns The traget node of the search. Returns null if the target Node is
   * not to be found, or if we have exceeded the search depth.
   */
  private static Node _traceNodeByIdPath(
    Node baseNode,
    String[] idFrags,
    int fragIndex,
    int searchDepth)
  {
    if ((baseNode.getNodeType() == Node.ELEMENT_NODE) && 
        idFrags[fragIndex].equals(((Element)baseNode).getAttribute(_ID_ATTRIB_NAME)))
    {
      if (idFrags.length == fragIndex + 1)
      {
        // This is the node for the last of the id fragments, so we found the 
        // target now.
        return baseNode;
      }
      else 
      {
        // This is the intermediate node matching the path, start looking for 
        //  nodes with id's matching rest of fragments.
        fragIndex++;
      }
    }

    // Check child Nodes
    if (searchDepth > 0)
    {
      searchDepth--;

      Node currChild  = baseNode.getFirstChild();
     
      while (currChild != null)
      {
        if (Node.ELEMENT_NODE == currChild.getNodeType())
        {
          Node targetNode = _traceNodeByIdPath(currChild,
                                               idFrags,
                                               fragIndex,
                                               searchDepth);
          if (targetNode != null)
            return targetNode;
        }

        currChild = currChild.getNextSibling();
      }
    }
    
    // We are past the permitted search depth, or we searched the entire subtree 
    // in vain, abort.
    return null;
  }

  static final String __JSF_CORE_NAMESPACE = "http://java.sun.com/jsf/core";
  private static final String _ID_ATTRIB_NAME = "id";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ChangeUtils.class);
}
