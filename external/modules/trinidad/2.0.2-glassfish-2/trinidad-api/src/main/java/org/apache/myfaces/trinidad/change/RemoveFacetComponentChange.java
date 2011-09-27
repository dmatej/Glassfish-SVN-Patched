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

import java.util.Map;

import javax.faces.component.UIComponent;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Change specialization for removal of a facet.
 * While applying this Change, if there were to be a facet with the specified
 *  name, it will be removed.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/RemoveFacetComponentChange.java#0 $) $Date: 10-nov-2005.19:10:01 $
 */
public class RemoveFacetComponentChange extends ComponentChange
                                        implements DocumentChange
{
  /**
   * Constructs a RemoveFacetChange with the specified name of the facet.
   * @param facetName The name of facet that needs to be removed.
   * @throws IllegalArgumentException if specified facetName is 
   *          <code>null</code>.
   */
  public RemoveFacetComponentChange(String facetName)
  {
    if ((facetName == null) || (facetName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_CONSTRUCT_REMOVEFACETCHANGE_WITH_NULL_FACETNAME"));
    _facetName = facetName;
  }
  
  /**
   * Returns the name of facet that needs to be removed.
   */
  public String getFacetName()
  {
    return _facetName;
  }
  
  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("unchecked")
  @Override
  public void changeComponent(UIComponent uiComponent)
  {
    Map<String, UIComponent> facets = uiComponent.getFacets();
    facets.remove(_facetName);
  }

  /**
   * {@inheritDoc}
   */
  public void changeDocument(Node componentNode)
  {
    Element facetElement = ChangeUtils.__getFacetElement(componentNode, _facetName);
    
    if (facetElement != null)
    {
      componentNode.removeChild(facetElement);
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

  private final String _facetName;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RemoveFacetComponentChange.class);
  private static final long serialVersionUID = 1L;
}
