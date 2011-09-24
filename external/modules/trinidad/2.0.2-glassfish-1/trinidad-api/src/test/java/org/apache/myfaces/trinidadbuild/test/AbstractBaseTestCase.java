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

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.shale.test.jmock.AbstractJmockJsfTestCase;
import org.jmock.Mock;

public abstract class AbstractBaseTestCase extends AbstractJmockJsfTestCase
{

  public AbstractBaseTestCase(String name)
  {
    super(name);
  }
  
  protected void setFacesContext(FacesContext context)
  {
    FacesTestCase.TestFacesContext.setCurrentInstance(context);
  }

  protected Mock buildMockUIComponent()
  {
    return buildMockUIComponent(1);
  }

  protected Mock buildMockUIComponent(
    int iterations
    )
  {
    return buildMockUIComponent(iterations, new String[] {"label"});
  }

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    // Set up a JSF 1.2 FacesContext
    FacesContext oldFacesContext = facesContext;
    UIViewRoot oldViewRoot = oldFacesContext.getViewRoot();
    oldFacesContext.release();
    facesContext = new MockFacesContext12(externalContext,
                                          lifecycle,
                                          application);
    facesContext.setViewRoot(oldViewRoot);
    facesContext.setApplication(application);
  }

  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
  }

  /**
   * Builds a MockUIComponent with attributes setup for the requested number of
   * test iterations.
   */
  protected Mock buildMockUIComponent(
    int iterations,
    String attributeNames[]
     )
  {
    int i;
    Mock c = mock(UIComponent.class);
    Map<String, Object> attrs = new HashMap<String, Object>();
    for (i = 0; i < attributeNames.length; i++)
      attrs.put(attributeNames[i], attributeNames[i]);
    for (i = 0; i < iterations; i++)
    {
      c.stubs().method("getAttributes").will(returnValue(attrs));
      c.stubs().method("getId").will(returnValue("mockId"));
    }

    return c;
  }
}