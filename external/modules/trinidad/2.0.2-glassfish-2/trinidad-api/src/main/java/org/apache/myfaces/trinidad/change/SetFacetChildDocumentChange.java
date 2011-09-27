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

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Change specialization for adding a child component to a facet using document
 * mark up. While applying this Change, the child component is created and added to
 * the document.  If the facet doesn't exist, it will be created.  If the facet
 * does exist, all of its content will be removed and the new content added.
 */
public class SetFacetChildDocumentChange extends AddComponentDocumentChange 
{  
  /**
   * Constructs an AddFacetDocumentChange with the specified child component mark up and
   *  the name of the facet.
   * @param facetName Name of facet to create the child component in
   * @param fragment DOM mark up for child component to be inserted.
   * @throws IllegalArgumentException if facetName or componentFragment is
   *         <code>null</code>
   */
  public SetFacetChildDocumentChange(
    String facetName,
    DocumentFragment fragment)
  {
    super(fragment);
    
    if ((facetName == null) || (facetName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "FACET_NAME_MUST_SPECIFIED"));
      
    _facetName = facetName;
  }
  
  /**
   * Returns the identifier of the sibling before which this new child needs to
   *  be inserted.
   */
  public String getFacetName()
  {
    return _facetName;
  }
  
  /**
   * Given the DOM Node representing a Component, apply any necessary
   * DOM changes.
   * While applying this Change, the child component is created and added to
   * the document.  If the facet doesn't exist, it will be created.  If the facet
   * does exist, all of its content will be removed and the new content added.
   */
  public void changeDocument(Node componentNode)
  {
    if (componentNode == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_NODE_SPECIFIED"));
    
    // get the fragement, imported into the target document
    DocumentFragment targetFragment = getImportedComponentFragment(componentNode);
    
    Element facetElement = ChangeUtils.__getFacetElement(componentNode, _facetName);
    
    if (facetElement != null)
    {
      // remove any current children
      ChangeUtils.__removeAllChildren(facetElement);
    }
    else
    {
      Document targetDocument = componentNode.getOwnerDocument();
      
      facetElement = targetDocument.createElementNS(_JSF_CORE_NAMESPACE, "f:facet");
      
      // set the xmlns for the prefix to make sure that "f:" is the
      // prefix for faces
      // =-= bts TODO In theory, this could cause problems if the
      // added component used the prefix "f:" for something other than
      // the JSF core
      facetElement.setAttributeNS(_XMLNS_NAMESPACE, "xmlns:f",
                                  _JSF_CORE_NAMESPACE);
      
      facetElement.setAttribute(_FACET_ATTRIBUTE_NAME, _facetName);

      componentNode.appendChild(facetElement);
    }
    
    // add our new facet content
    facetElement.appendChild(targetFragment);
  }

  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  @Override
  public boolean getForcesDocumentReload()
  {
    return false;
  }

  private static final String _JSF_CORE_NAMESPACE = "http://java.sun.com/jsf/core";
  private static final String _XMLNS_NAMESPACE = "http://www.w3.org/2000/xmlns/";
  private static final String _FACET_ATTRIBUTE_NAME = "name";

  private final String _facetName;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SetFacetChildDocumentChange.class);
}
