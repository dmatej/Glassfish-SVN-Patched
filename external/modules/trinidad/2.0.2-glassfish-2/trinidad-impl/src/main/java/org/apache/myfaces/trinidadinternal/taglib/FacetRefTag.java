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
package org.apache.myfaces.trinidadinternal.taglib;

import javax.faces.component.UIComponent;
import javax.faces.webapp.FacetTag;
import javax.faces.webapp.UIComponentClassicTagBase;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.Tag;

import org.apache.myfaces.trinidad.component.UIXComponentRef;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.webapp.TrinidadTagSupport;

/**
 * Use this tag to copy children or facets from a declarative component
 * on to a component within the corresponding definition.
 */
public class FacetRefTag extends TrinidadTagSupport
{
  public FacetRefTag()
  {
  }

  private String _facet;
  public void setFacetName(String f)
  {
    _facet = f;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(pageContext);
    if (tag == null)
    {
      _LOG.warning("FACETREF_MUST_INSIDE_UICOMPONENT");
      return SKIP_BODY;
    }

    UIComponent component = tag.getComponentInstance();
    UIComponent region = _getRegionComponent(component);
    if (region == null)
    {
      _LOG.warning("CANNOT_FIND_PARENT_COMPONENTREF");
      return SKIP_BODY;
    }
    if (_facet != null)
    {
      UIComponent child = region.getFacet(_facet);
      if (child != null)
      {
        _addChild(component, child);
        // alert the region component that we moved one of its facets.
        // This is so that it can restore the facet, during the next request,
        // just in time to prevent the jsf jsp tag framework from
        // getting confused:
        ComponentRefTag.addRelocatedFacet(region, _facet, child);
      }
    }
    else
    {
      _LOG.warning("FACETNAME_REQUIRED");
    }

    return SKIP_BODY;
  }

  @Override
  public void release()
  {
    super.release();
    _facet = null;
  }

  // adds the given child to the given parent as either a facet or a
  // direct child.
  @SuppressWarnings("unchecked")
  private void _addChild(UIComponent parent, UIComponent child)
  {
    String facet = _getParentFacetName();
    if (facet != null)
    {
      parent.getFacets().put(facet, child);
    }
    else
    {
      parent.getChildren().add(child);
    }
  }

  /**
   * Checks to see if this tag is inside an <f:facet> tag.
   * If so, it returns the facet name on the <f:facet> tag.
   * Otherwise, it returns null.
   */
  private String _getParentFacetName()
  {
    Tag parent = getParent();
    if (parent instanceof FacetTag)
    {
        return (((FacetTag) parent).getName());
    }
    return null;
  }

  /**
   * Finds the UIXComponentRef component that is an ancestor of this component.
   * @param comp the search starts with this component's parent.
   * @return null if the UIXComponentRef cannot be found.
   */
  private UIComponent _getRegionComponent(UIComponent comp)
  {
    // the search must start from the given component's parent
    // because we want to support passing the children from one region
    // into a nested child region.
    do
    {
      comp = comp.getParent();
    }
    while((comp != null) && (!(comp instanceof UIXComponentRef)));
    return comp;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FacetRefTag.class);
  private static final long serialVersionUID = 1L;
}
