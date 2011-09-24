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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;


/**
 * Base class for the group componnet.
 * <p>
 * @version $Name:  $ ($Revision: 429530 $) $Date: 2006-08-07 18:44:54 -0600 (Mon, 07 Aug 2006) $
 */
abstract public class UIXGroupTemplate extends UIXComponentBase implements FlattenedComponent

{
  /**
   * Overridden to return true.
   * @return true because the children are rendered by this component
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Sets up the grouping context and processes all of the
   * UIXGroup's children
   */
  public <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessingContext cpContext,
    ComponentProcessor<S> childProcessor,
    S callBackContext
    ) throws IOException
  {
    cpContext.pushGroup();

    try
    {
      setupVisitingContext(context);

      try
      {
        setupChildrenVisitingContext(context);

        try
        {
          // bump up the group depth and render all of the children
          return UIXComponent.processFlattenedChildren(context,
                                                       cpContext,
                                                       childProcessor,
                                                       this.getChildren(),
                                                       callBackContext);
        }
        finally
        {
          tearDownChildrenVisitingContext(context);
        }
      }
      finally
      {
        tearDownVisitingContext(context);
      }
    }
    finally
    {
      cpContext.popGroup();
    }
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
   * Renders the children in their raw form.
   * There is no Renderer for this component because it has no
   * visual representation or any sort of layout for its children.
   * @param context the FacesContext
   * @throws IOException if there is an error encoding the children
   */
  @Override
  public void encodeChildren(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    if (getChildCount() > 0)
    {
      for(UIComponent child : (List<UIComponent>)getChildren())
      {
        child.encodeAll(context);
      }
    }
  }
}


