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

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.PartialPageContext;

/**
 * Base class for UIXColumns.
 * The facets of this component are not processed during decode,validate and
 * update.
 */
public abstract class UIXColumnTemplate extends UIXComponentBase
{
  /**
   * Does not process the facets of this column. Only this column and
   * its children are processed.
   */
  @Override
  public void processDecodes(FacesContext context)
  {
    if (!isRendered())
      return;

    // Process all the children of this component:
    new ChildLoop.Decode().runAlways(context, this);

    // Process this component itself
    decode(context);
  }

  /**
   * Does not process the facets of this column. Only this column and
   * its children are processed.
   */
  @Override
  public void processValidators(FacesContext context)
  {
    if (!isRendered())
      return;

    // Process all the children of this component
    new ChildLoop.Validate().runAlways(context, this);
  }

  /**
   * Does not process the facets of this column. Only this column and
   * its children are processed.
   */
  @Override
  public void processUpdates(FacesContext context)
  {
    if (!isRendered())
      return;

    // Process all the children of this component
    new ChildLoop.Update().runAlways(context, this);
  }
  
  /**
   * When the column is being PPR-ed, we have to PPR the entire table
   * Note that this will work for the nested columns too because the parent column's 
   * setPartialTarget() will in turn delegate to the table
   */
  @Override
  protected void setPartialTarget(FacesContext facesContext,
    PartialPageContext partialContext)
  {
    UIXComponent.addPartialTarget(facesContext, partialContext, getParent());
  }
}
