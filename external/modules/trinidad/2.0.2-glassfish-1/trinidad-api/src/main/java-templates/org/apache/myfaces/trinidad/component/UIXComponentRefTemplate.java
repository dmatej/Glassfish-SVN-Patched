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

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Base class for referencing declarative components.
 *
 */
public abstract class UIXComponentRefTemplate extends UIXComponentBase
                                              implements NamingContainer
{
/**/  public abstract String getVar();



  @Override
  public boolean invokeOnComponent(FacesContext context,
                                   String clientId,
                                   ContextCallback callback)
    throws FacesException
  {
    // optimize case where clientId isn't in NamingContainer
    return invokeOnNamingContainerComponent(context, clientId, callback);
  }

  @Override
  public void queueEvent(FacesEvent event)
  {
    // we want to wrap up
    // the event so we can execute it in the correct context (with the correct
    // pageDefinition):
    event = new WrapperEvent(this, event);
    super.queueEvent(event);
  }

  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    if (event instanceof WrapperEvent)
    {
      WrapperEvent wrapper = (WrapperEvent) event;
      final FacesEvent wrapped = wrapper.getEvent();
      Runnable runner = new Runnable()
      {
        public void run()
        {
          wrapped.getComponent().broadcast(wrapped);
        }
      };
      FacesContext context = FacesContext.getCurrentInstance();
      _processPhase(context, runner);
    }
    else
    {
      super.broadcast(event);
    }
  }

  @Override
  public void processDecodes(final FacesContext context)
  {
    Runnable runner = new Runnable()
    {
      public void run()
      {
        UIXComponentRefTemplate.super.processDecodes(context);
      }
    };
    _processPhase(context, runner);
  }

  @Override
  public void processValidators(final FacesContext context)
  {
    Runnable runner = new Runnable()
    {
      public void run()
      {
        UIXComponentRefTemplate.super.processValidators(context);
      }
    };
    _processPhase(context, runner);
  }

  @Override
  public void processUpdates(final FacesContext context)
  {
    Runnable runner = new Runnable()
    {
      public void run()
      {
        UIXComponentRefTemplate.super.processUpdates(context);
      }
    };
    _processPhase(context, runner);
  }

  @Override
  public void encodeBegin(FacesContext context) throws IOException
  {
    _setupEL(context);
    super.encodeBegin(context);
  }

  @Override
  public void encodeEnd(final FacesContext context) throws IOException
  {
    super.encodeEnd(context);
    _resetEL(context);
  }

  private void _setupEL(FacesContext context)
  {
    UIXComponentRef region = _getParent();
    // in the JSP VE we try to display a regiondef page. so we can't
    // blow up if there isn't a parent region component.
    if (region != null)
    {
      Object newValue = region.getValue();
      _oldValue =
        TableUtils.setupELVariable(context, _BINDINGS_VAR, newValue);

      String var = _getVar();
      _oldVar =
        TableUtils.setupELVariable(context, var, new AttrMap());
    }
  }

  private void _resetEL(FacesContext context)
  {
    TableUtils.setupELVariable(context, _BINDINGS_VAR, _oldValue);
    String var = _getVar();
    TableUtils.setupELVariable(context, var, _oldVar);

    _oldValue = Boolean.FALSE; // GC
    _oldVar = null; // GC
  }

  private void _processPhase(FacesContext context, Runnable runner)
  {
    assert context != null : "FacesContext is null";

    if (!isRendered())
      return;

    _setupEL(context);

    try
    {
      runner.run();
    }
    finally
    {
      _resetEL(context);
    }
  }

  private UIXComponentRef _getParent()
  {
    for(UIComponent test = this; test != null; test = test.getParent())
    {
      if (test instanceof UIXComponentRef)
        return (UIXComponentRef) test;
    }

    _LOG.severe("NO_PARENT_COMPONENTREF_FOUND");
    return null;
  }

  private String _getVar()
  {
    if (_var == null)
    {
      _var = getVar();
      if (_var == null)
      {
        _LOG.fine("var not set");
        _var = "null"; // initialize so that we don't keep checking.
      }
    }
    return _var;
  }

  private final class AttrMap extends AbstractMap<Object, Object>
  {
    @Override
    public Object get(final Object key)
    {
      FacesContext context = FacesContext.getCurrentInstance();

      assert _oldValue != Boolean.FALSE : "EL variables have not been setup";

      // when evaluating an attribute value on the parent, we need to reset the
      // EL variables, evaluate the attribute value and then restore the EL variables:
      Object currentValue = TableUtils.setupELVariable(context, _BINDINGS_VAR, _oldValue);
      String var = _getVar();
      Object currentVar = TableUtils.setupELVariable(context, var, _oldVar);

      UIXComponentRef region =_getParent();
      Object result = region.getAttributes().get(key);

      TableUtils.setupELVariable(context, _BINDINGS_VAR, currentValue);
      TableUtils.setupELVariable(context, var, currentVar);
      return result;
    }

    @Override
    public Set<Map.Entry<Object, Object>> entrySet()
    {
      return Collections.emptySet();
    }
  }

  // Local backing for the actual Var
  private transient String _var = null;
  private transient Object _oldValue = Boolean.FALSE;
  private transient Object _oldVar = null;
  private static final String _BINDINGS_VAR = "bindings";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXComponentRefTemplate.class);
}
