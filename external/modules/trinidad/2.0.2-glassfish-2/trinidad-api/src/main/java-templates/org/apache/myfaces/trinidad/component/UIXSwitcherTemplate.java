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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;

import java.util.Collections;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;


/**
 * Base class for the switcher componnet.
 * <p>
 * @version $Name:  $ ($Revision: 1125570 $) $Date: 2011-05-20 14:10:50 -0700 (Fri, 20 May 2011) $
 */
abstract public class UIXSwitcherTemplate extends UIXComponentBase implements FlattenedComponent
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public String getFacetName();
/**/  abstract public String getDefaultFacet();

  /**
   * Processes the selected switcher facet
   */
  public <S> boolean processFlattenedChildren(
    final FacesContext          context,
    ComponentProcessingContext  cpContext,
    final ComponentProcessor<S> childProcessor,
    final S                     callbackContext
    ) throws IOException
  {
    setupVisitingContext(context);

    boolean abort;

    try
    {
      UIComponent facet = _getFacet();

      if (facet != null)
      {
        setupChildrenVisitingContext(context);

        try
        {
          abort = UIXComponent.processFlattenedChildren(context,
                                                        cpContext,
                                                        childProcessor,
                                                        facet,
                                                        callbackContext);
        }
        finally
        {
          tearDownChildrenVisitingContext(context);
        }
      }
      else
      {
        abort = false;
      }
    }
    finally
    {
      tearDownChildrenVisitingContext(context);
    }

    return abort;
  }

  /**
   * Returns <code>true</code> if this FlattenedComponent is currently flattening its children
   * @param context FacesContext
   * @return <code>true</code> if this FlattenedComponent is currently flattening its children
   */
  public boolean isFlatteningChildren(FacesContext context)
  {
    return true;
  }

  /**
   * Only render the currently active facet.
   */
  @Override
  public void encodeChildren(FacesContext context)
    throws IOException
  {
    UIComponent facet = _getFacet();
    if (facet != null)
    {
      facet.encodeAll(context);
    }
  }

  /**
   * Override to return true.
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected Iterator<UIComponent> getRenderedFacetsAndChildren(FacesContext facesContext)
  {
    UIComponent facet = _getFacet();
    if (facet == null)
    {
      return Collections.<UIComponent>emptyList().iterator();
    }
    else
    {
      return Collections.singleton(facet).iterator();
    }
  }

  private UIComponent _getFacet()
  {
    if (!isRendered())
      return null;

    String facetName = getFacetName();
    if (facetName != null)
    {
      UIComponent facet = getFacet(facetName);
      if (facet != null)
        return facet;
    }

    String defaultFacet = getDefaultFacet();
    if (defaultFacet != null)
      return getFacet(defaultFacet);

    return null;
  }
}


