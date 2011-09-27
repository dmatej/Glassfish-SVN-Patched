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

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Change specialization for adding a facet.
 * While applying this Change, the specified component is re-created and added as a facet. If there 
 * were to be a facet by the specified name already, the new facet will not be added.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/SetFacetChildComponentChange.java#0 $) $Date: 10-nov-2005.19:10:02 $
 */
public class SetFacetChildComponentChange extends AddComponentChange 
{
  /**
   * Constructs an AddFacetChange with the specified facet name and the 
   *  corresponding component.
   * @param facetName The name by which the component is to be added as a facet.
   * @param facetComponent The component that is to be added as a facet.
   * @throws IllegalArgumentException if specified facetName or the 
   *          facetComponent were to be null.
   */
  public SetFacetChildComponentChange(
    String facetName,
    UIComponent facetComponent)
  {
    super(facetComponent);
    
    if ((facetName == null) || (facetName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_CONSTRUCT_ADDFACETCHANGE_WITH_NULL_FACETNAME_FACETCOMPONENT"));

    _facetName = facetName;
  }
  
  /**
   * Returns the name by which the facet is to be added while applying this
   * Change.
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
    UIComponent facetComponent = getComponent();
    
    if (facetComponent == null)
      return;
    
    // Do not replace a facet if it already exists.
    if (uiComponent.getFacets().get(_facetName) != null)
    {
      _LOG.info("FACET_ALREADY_PRESENT", _facetName);
      return;
    }

    uiComponent.getFacets().put(_facetName, facetComponent);

  }
  
  private final String _facetName;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SetFacetChildComponentChange.class);

  private static final long serialVersionUID = 1L;
}
