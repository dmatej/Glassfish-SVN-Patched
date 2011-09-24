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

import java.io.Serializable;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Proxy class representing the state of the UIComponent.
 * 'state' here means the state as served by saveState() method in interface
 * javax.faces.component.StateHolder.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ChangeComponentProxy.java#0 $) $Date: 10-nov-2005.19:09:57 $
 * @todo =-=pu: saveState() just saves the state of this component, what if we
 *    were to represent a component subtree in ChangeComponentProxy. We would
 *    need to do things similar to UIComponent.processSaveState() does with the
 *    exception that we disregard the 'transient' attribute in the algorithm.
 */
class ChangeComponentProxy implements Serializable
{
  /**
   * Constructs an ChangeComponentProxy with the specified UIComponent instance.
   * @throws IllegalArgumentException if specified uiComponent were to be null.
   */
  public ChangeComponentProxy(
    FacesContext facesContext,
    UIComponent uiComponent)
  {
    if (uiComponent == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_CONSTRUCT_CHANGECOMPONENTPROXY_WITH_NULL_UICOMPONENT"));
    _class = uiComponent.getClass();
    _className = _class.getName();
    _state = uiComponent.saveState(facesContext);
  }

  /**
   * Creates a new UIComponent, corresponding to this proxy, restores its state
   *  and returns the same. Returns 'null' if this process fails for any reason.
   */
  public UIComponent createComponent()
  {
    UIComponent uic = null;
    Class<? extends UIComponent> clazz = _getComponentClass();
    if (clazz == null)
    {
      // An error must have already been logged in _getComponentClass();
      return null;
    }

    try
    {
      uic = clazz.newInstance();
      uic.restoreState(FacesContext.getCurrentInstance(), _state);
    }
    catch (InstantiationException ie)
    {
      _LOG.warning("ERR_CREATE_NEW_COMPONENT_INSTANCE", clazz.getName());
      _LOG.warning(ie);
    }
    catch (IllegalAccessException iae)
    {
      _LOG.warning("ERR_CREATE_NEW_COMPONENT_INSTANCE", clazz.getName());
      _LOG.warning(iae);
    }
    return uic;
  }

  @SuppressWarnings("unchecked")
  private Class<? extends UIComponent> _getComponentClass()
  {
    Class<? extends UIComponent> clazz = _class;
    if (clazz == null)
    {
      try
      {
        ClassLoader cl = Thread.currentThread().getContextClassLoader();
        clazz = (Class<? extends UIComponent>)cl.loadClass(_className);
        _class = clazz;
      }
      catch (ClassNotFoundException e)
      {
        _LOG.severe(e);
      }
    }

    return clazz;
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ChangeComponentProxy.class);

  // FindBugs claims this as "Transient field that isn't set
  // by deserialization", but _getComponentClass() lazily restores it
  private transient Class<? extends UIComponent> _class;
  private String _className;
  private Object _state;

  private static final long serialVersionUID = 1L;
}
