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
package org.apache.myfaces.trinidad.component.html;

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.context.ComponentContextManager;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.SuspendedContextChanges;


public abstract class HtmlHeadTemplate
  extends UIXComponentBase
{
  @Override
  public boolean visitTree(
    VisitContext  visitContext,
    VisitCallback callback)
  {
    ComponentContextManager ctxMgr = RequestContext.getCurrentInstance()
      .getComponentContextManager();
    FacesContext facesContext = visitContext.getFacesContext();

    // Suspend any current component context during a visit tree for re-entrant
    // component tree processing
    SuspendedContextChanges suspendedChanges = ctxMgr.suspend(facesContext);

    try
    {
      return super.visitTree(visitContext, callback);
    }
    finally
    {
      ctxMgr.resume(facesContext, suspendedChanges);
    }
  }

  @Override
  public boolean invokeOnComponent(
    FacesContext    facesContext,
    String          clientId,
    ContextCallback callback
    ) throws FacesException
  {
    ComponentContextManager ctxMgr = RequestContext.getCurrentInstance()
      .getComponentContextManager();

    // Suspend any current component context during an invoke on component call for re-entrant
    // component tree processing
    SuspendedContextChanges suspendedChanges = ctxMgr.suspend(facesContext);

    try
    {
      return super.invokeOnComponent(facesContext, clientId, callback);
    }
    finally
    {
      ctxMgr.resume(facesContext, suspendedChanges);
    }
  }
}