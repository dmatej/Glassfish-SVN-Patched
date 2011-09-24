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

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Change specialization for adding a child component using document mark up.
 * While applying this Change, the child component is created and added to
 * the document.
 */
public class AddChildDocumentChange extends AddComponentDocumentChange 
{
  /**
   * Constructs an AddChildDocumentChange with the specified child component 
   *  mark up.
   * @param componentFragment DOM mark up for child component to be appended.
   * @throws IllegalArgumentException if componentFragment is <code>null</code>
   */
  public AddChildDocumentChange(DocumentFragment componentFragment)
  {
    this(null, componentFragment);
  }
  
  /**
   * Constructs an AddChildDocumentChange with the specified child component 
   *  mark up and the identifier of the neighbour. If the neighbour were not to 
   *  be found while applying this Change, the child is appended to the end of 
   *  the list of children.
   * @param insertBeforeId The identifier of the sibling before which this new 
   *         child is to be inserted or <code>null</code> to append the child
   * @param componentFragment DOM mark up for child component to be inserted.
   * @throws IllegalArgumentException if componentFragment is <code>null</code>
   */
  public AddChildDocumentChange(
    String insertBeforeId,
    DocumentFragment componentFragment)
  {
    super(componentFragment);
    
    _insertBeforeId = insertBeforeId;
  }
  
  /**
   * Returns the identifier of the sibling before which this new child needs to
   *  be inserted.
   */
  public String getInsertBeforeId()
  {
    return _insertBeforeId;
  }
  
  /**
   * Given the DOM Node representing a Component, apply any necessary
   * DOM changes.
   */
  public void changeDocument(Node componentNode)
  {
    if (componentNode == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_NODE_SPECIFIED"));
    
    // get the fragement, imported into the target document
    DocumentFragment targetFragment = 
      getImportedComponentFragment(componentNode);
    
    // assume that we'll be appending
    Node insertBeforeNode = null;
    
    // search children for the _insertBefore id
    if (_insertBeforeId != null)
    {
      String insertBeforeID = _insertBeforeId;
      
      Node currChild = componentNode.getFirstChild();
      Node idAttr;
      
      while (currChild != null)
      {
        NamedNodeMap attributes = currChild.getAttributes();
        
        if (attributes != null)
        {
          idAttr = attributes.getNamedItem("id");
          if (idAttr != null && insertBeforeID.equals(idAttr.getNodeValue()))
          {
            insertBeforeNode = currChild;
            break;
          }
        }
        currChild = currChild.getNextSibling();
      }
    }    
    
    // insert our DocumentFragment in the correct position
    componentNode.insertBefore(targetFragment, insertBeforeNode);
  }
  
  private final String _insertBeforeId;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    AddChildDocumentChange.class);
}
