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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Change specialization for re-ordering of children.
 * While applying this Change, the specified order of children is  restored.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ReorderChildrenComponentChange.java#0 $) $Date: 10-nov-2005.19:10:01 $
 */
public class ReorderChildrenComponentChange extends ComponentChange
                                            implements DocumentChange
{
  /**
   * Constructs a ReorderChange with the given List of identifiers for children.
   * @param childIds An in-order collection (List) of Ids (as java.lang.String) 
   *         of child components.
   *        This List implementation should be of type java.io.Serializable in
   *         order to be persisted.
   *        If no identifier was passed, it would be assumed that the list 
   *          consists of the Ids. 
   * @throws IllegalArgumentException if supplied childIds were to be null.
   */
  public ReorderChildrenComponentChange(
    List<String> childIds
    )
  {
    this(childIds, "id");
  }
  
  /**
   * Constructs a ReorderChange with the given List of identifiers for children.
   * @param childIds An in-order collection (List) of Ids (as java.lang.String) 
   *         of child components.
   *        This List implementation should be of type java.io.Serializable in
   *         order to be persisted.
   * @param identifier Determines the type of identifiers which the List consists of.
   * @throws IllegalArgumentException if supplied childIds were to be null or supplied 
   *          identifier was to be null or emtpy string.
   */
  public ReorderChildrenComponentChange(
    List<String> childIds,
    String identifier
    )
  {
    if (childIds == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_CONSTRUCT_REORDERCHANGE_WITH_NULL_ID"));
    
    if (identifier == null || "".equals(identifier))
      throw new IllegalArgumentException(_LOG.getMessage(
        "IDENTIFIER_TYPE_CANNOT_BE_NULL"));
    
    // make serializable copy of list        
    _childIds = Collections.unmodifiableList(new ArrayList<String>(childIds));
    
    _identifier = identifier;
  }
  
  /**
   * Returns an unmodifiable List of the identifiers for the  children.
   */
  public List<String> getChildIds()
  {
    return _childIds;
  }
  
  /**
   * Returns the identifier type.
   */
  public final String getIdentifier()
  {
    return _identifier;
  }
  
  /**
   * {@inheritDoc}
   * In case children were to be removed between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of a
   *  RemoveChildrenChange, such children are not re-instated.
   * In case children were to be added between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of an 
   *  AddChildChange, such children are appended to the end of the list in
   *  preserving the order in which they were added (that is they appear at 
   *  the end).
   */
  @SuppressWarnings("unchecked")
  @Override
  public void changeComponent(UIComponent uiComponent)
  {
    int childCount = uiComponent.getChildCount();
    if (childCount == 0)
      return;
 
    // build order map of of current Nodes, keyed by id
    Map<String, UIComponent> childrenMap = new LinkedHashMap<String, UIComponent>();
    
    List<UIComponent> children = uiComponent.getChildren();
    
    int fakeIndex = 0;
    for(UIComponent child : children)
    {
      String attrValue = (String)child.getAttributes().get(_identifier);
      
      // create a dummy key to maintain order of children whose identifier 
      // does not exist
      if (attrValue == null) 
      {
        attrValue = Integer.valueOf(fakeIndex++).toString();
      }
      childrenMap.put(attrValue, child);
    }

    // remove the children so that we can add them back in
    children.clear();

    //
    // put children back in, in order
    //
    for(String currReorderID : _childIds)
    {
      UIComponent currChild = childrenMap.remove(currReorderID);
      
      if (currChild != null)
      {
        children.add(currChild);
      }
    }
    
    // add in all of the rest of the children in
    // relative order they originally appeared
    children.addAll(childrenMap.values());
  }

  /**
   * {@inheritDoc}
   * In case children were to be removed between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of a
   *  RemoveChildrenChange, such children are not re-instated.
   * In case children were to be added between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of an 
   *  AddChildChange, such children are appended to the end of the list in
   *  preserving the order in which they were added (that is they appear at 
   *  the end).
   */
  public void changeDocument(
    Node componentNode)
  {
    // build order map of of current Nodes, keyed by id
    LinkedHashMap<String, Node> currChildrenMap = new LinkedHashMap<String, Node>(13);
        
    Node currChild = componentNode.getFirstChild();
    
    int fakeIndex = 0;
    while (currChild != null)
    {
      NamedNodeMap attributes = currChild.getAttributes();
              
      String currKey = null;
      if (attributes != null)
      {
        Node idAttr = attributes.getNamedItem(_identifier);
        
        if (idAttr != null)
        {
          currKey = idAttr.getNodeValue();
        }
      }
      
      // create a dummy key to maintain order of non-ided children
      if (currKey == null)
      {
        // =-= bts What about insignificant whitespace?
        currKey = Integer.valueOf(fakeIndex++).toString();
      }

      currChildrenMap.put(currKey, currChild);
      
      // remove the children so that we can add them back in
      componentNode.removeChild(currChild);
      
      // next node is first node again
      currChild = componentNode.getFirstChild();
    }

    //
    // put children back in, in order
    //
    for(String currReorderID : _childIds)
    {
      currChild = currChildrenMap.remove(currReorderID);
      if (currChild != null)
      {
        componentNode.appendChild(currChild);
      }
    }
    
    // add in all of the rest of the children in
    // relative order they originally appeared
    for(Map.Entry<String, Node> entry : currChildrenMap.entrySet())
    {
      componentNode.appendChild(entry.getValue());
    } 
  }
  
  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  public boolean getForcesDocumentReload()
  {
    return false;
  }

  private final List<String> _childIds;
  private final String _identifier;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ReorderChildrenComponentChange.class);
  private static final long serialVersionUID = 1L;
}
