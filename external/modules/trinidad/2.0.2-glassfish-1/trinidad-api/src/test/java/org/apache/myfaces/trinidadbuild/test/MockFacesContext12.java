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
package org.apache.myfaces.trinidadbuild.test;

import java.util.Collection;


import java.util.HashMap;
import java.util.Map;

import javax.el.ELContext;

import javax.faces.application.Application;
import javax.faces.application.ApplicationWrapper;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.PartialResponseWriter;
import javax.faces.context.PartialViewContext;
import javax.faces.event.PhaseId;
import javax.faces.event.SystemEvent;
import javax.faces.lifecycle.Lifecycle;

import org.apache.shale.test.mock.MockFacesContext;
import org.apache.shale.test.mock.MockApplication12;


public class MockFacesContext12 extends MockFacesContext
{
  public MockFacesContext12(ExternalContext ec,
                            Lifecycle   lifecycle,
                            Application application)
  {
    super(ec, lifecycle);
    elContext = createELContext(application);
    elContext.putContext(FacesContext.class, this);
    _app = application;
  }

  public MockFacesContext12(Application application)
  {
    elContext = createELContext(application);
    elContext.putContext(FacesContext.class, this);
    _app = application;
  }
  
  public PhaseId getCurrentPhaseId() 
  {
    return _currentPhaseId;
  }
  
  public void setCurrentPhaseId(PhaseId phaseId)
  {
    this._currentPhaseId = phaseId;
  }

  public ELContext getELContext()
  {
    return elContext;
  }

  protected MockELContext createELContext(Application application)
  {
    return new MockELContext(application);
  }
  
  public Map<Object,Object> getAttributes()
  {
    return _attrs;
  }
  
  public PartialViewContext getPartialViewContext()
  {
    return _mockPartialContext;
  }
  
  public Application getApplication()
  {
    return new ApplicationWrapper()
    {
      public Application getWrapped()  
      {
        return _app;
      }
      public void publishEvent(FacesContext context,
                               Class<? extends SystemEvent> systemEventClass,
                               Class<?> sourceBaseType,
                               Object source)
      {
        // do nothing
      }
      
      public void publishEvent(FacesContext context,
                               Class<? extends SystemEvent> systemEventClass,
                               Object source)
      {
        //do nothing
      }
    };
  }

  protected MockELContext elContext;
  
  private PhaseId _currentPhaseId;

  private final PartialViewContext _mockPartialContext = new MockPartialViewContext();
  private final Map<Object, Object> _attrs = new HashMap<Object, Object>();
  private Application _app;
  
  private static class MockPartialViewContext extends PartialViewContext
  {
    public Collection<String> getExecuteIds()
    {
      throw new UnsupportedOperationException();
    }
    
    public Collection<String> getRenderIds()
    {
      throw new UnsupportedOperationException();
    }
    
    public PartialResponseWriter getPartialResponseWriter()
    {
      throw new UnsupportedOperationException();
    }
    
    public boolean isAjaxRequest()
    {
      return false;
    }
    
    public boolean isPartialRequest()
    {
      return false;
    }
    
    public boolean isExecuteAll()
    {
      throw new UnsupportedOperationException();
    }
    
    public boolean isRenderAll()
    {
      throw new UnsupportedOperationException();
    }
    
    public void setRenderAll(boolean renderAll)
    {
      throw new UnsupportedOperationException();
    }
    
    public void setPartialRequest(boolean isPartialRequest)
    {
      throw new UnsupportedOperationException();
    }
    
    public void release()
    {
      throw new UnsupportedOperationException();
    }
    
    public void processPartial(PhaseId phaseId)
    {
      throw new UnsupportedOperationException();
    }
  }
}