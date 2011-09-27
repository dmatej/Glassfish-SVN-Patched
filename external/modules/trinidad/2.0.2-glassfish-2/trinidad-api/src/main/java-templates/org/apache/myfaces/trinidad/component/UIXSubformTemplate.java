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

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.component.NamingContainer;
import javax.faces.context.FacesContext;

import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

/**
 * Base class for Subform component.
 * <p>
 * @version $Name:  $ ($Revision: 1024069 $) $Date: 2010-10-18 17:07:35 -0700 (Mon, 18 Oct 2010) $
 */
abstract public class UIXSubformTemplate extends UIXComponentBase
                                        implements NamingContainer
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public boolean isSubmitted();
/**/  abstract public void setSubmitted(boolean submitted);
/**/  abstract public boolean isDefault();

  @Override
  public void queueEvent(FacesEvent event)
  {
    // If the event is being queued for anything *after* APPLY_REQUEST_VALUES,
    // then this subform is active.
    if (PhaseId.APPLY_REQUEST_VALUES.compareTo(event.getPhaseId()) < 0)
    {
      _storeSomethingSubmitted(FacesContext.getCurrentInstance());
      setSubmitted(true);
    }

    super.queueEvent(event);
  }

  @Override
  public void processDecodes(FacesContext context)
  {
    setSubmitted(false);
    super.processDecodes(context);
  }

  @Override
  public void processValidators(FacesContext context)
  {
    boolean submitted = isSubmitted();

    if (!submitted && isDefault() && !_isSomethingSubmitted(context))
    {
	  submitted = true;
      setSubmitted(true);
    }

    if (submitted)
      super.processValidators(context);
  }

  @Override
  public void processUpdates(FacesContext context)
  {
    if (isSubmitted())
      super.processUpdates(context);
  }




  @Override
  public boolean invokeOnComponent(FacesContext context,
                                   String clientId,
                                   ContextCallback callback)
    throws FacesException
  {
    // optimize case where clientId isn't in NamingContainer
    return invokeOnNamingContainerComponent(context, clientId, callback);
  }

  @SuppressWarnings("unchecked")
  static private void _storeSomethingSubmitted(FacesContext context)
  {
    context.getExternalContext().getRequestMap().put(_SOMETHING_SUBMITTED,
                                                     Boolean.TRUE);
  }

  static private boolean _isSomethingSubmitted(FacesContext context)
  {
    return Boolean.TRUE.equals(context.getExternalContext().
                               getRequestMap().get(_SOMETHING_SUBMITTED));
  }

  /**
   * Sets whether the subform was submitted on this request
   *
   * @param submitted  the new submitted value
   */
  final public void setSubmitted(boolean submitted)
  {
    String clientId = getClientId();
    FacesContext.getCurrentInstance().getExternalContext().getRequestMap().put(
                                                     _SUBMITTED_PREFIX + clientId,
                                                     submitted ? Boolean.TRUE : Boolean.FALSE);
  }

  /**
   * Gets whether the subform was submitted on this request
   *
   * @return  the new submitted value
   */
  final public boolean isSubmitted()
  {
    String clientId = getClientId();
    Object submitted = FacesContext.getCurrentInstance().getExternalContext().getRequestMap().get(
                                                                     _SUBMITTED_PREFIX + clientId);
    return ComponentUtils.resolveBoolean(submitted, false);
  }


  static private final String _SOMETHING_SUBMITTED =
    "org.apache.myfaces.trinidad.component.UIXSubformSubmitted";
  static private final String _SUBMITTED_PREFIX =
    "org.apache.myfaces.trinidad.component.UIXSubform.";

 /**
  * @deprecated submitted is request scope, and therefore will not be saved on the faces bean as a property
  */
  @Deprecated
  static public final PropertyKey SUBMITTED_KEY =
    TYPE.registerKey("submitted", Boolean.class, Boolean.FALSE, PropertyKey.CAP_NOT_BOUND | PropertyKey.CAP_TRANSIENT);
}
