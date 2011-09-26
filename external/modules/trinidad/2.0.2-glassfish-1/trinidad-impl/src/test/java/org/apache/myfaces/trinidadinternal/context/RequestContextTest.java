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
package org.apache.myfaces.trinidadinternal.context;

import junit.framework.Test;
import junit.framework.TestSuite;


import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroupLayout;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class RequestContextTest extends FacesTestCase
{
  public RequestContextTest(
    String testName)
  {
    super(testName);
  }

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
  }
  
  @Override
  protected void tearDown() throws Exception
  {
    super.tearDown();
  }

  public static Test suite()
  {
    return new TestSuite(RequestContextTest.class);
  }
  

  public void testPageResolver()
  {
    RequestContext context = _createContext();
    try
    {
      assertTrue(context.getPageResolver() instanceof TestPageResolver);
    }
    finally
    {
      context.release();
    }
  }

  public void testPageFlowScopeProvider()
  {
    RequestContext context = _createContext();
    try
    {
      assertTrue(context.getPageFlowScopeProvider()
                   instanceof TestPageFlowScopeProvider);
    }
    finally
    {
      context.release();
    }
  }

  @SuppressWarnings("unchecked")
  public void testComponentState() throws Exception
  {
    RequestContext context = _createContext();

    try
    {
      CorePanelGroupLayout cpgl = new CorePanelGroupLayout();
      cpgl.setLayout("horizontal");
      HtmlOutputText hot = new HtmlOutputText();
      hot.setValue("foo");
      CoreOutputText cot = new CoreOutputText();
      cot.setValue("bar");

      cpgl.getChildren().add(hot);
      cpgl.getChildren().add(cot);

      Object state = context.saveComponent(cpgl);
      UIComponent restored = context.restoreComponent(state);

      assertTrue(restored instanceof CorePanelGroupLayout);
      assertEquals(restored.getChildCount(), 2);
      assertEquals(restored.getAttributes().get("layout"), "horizontal");
      UIComponent childOne = (UIComponent) restored.getChildren().get(0);
      assertTrue(childOne instanceof HtmlOutputText);
      assertEquals(childOne.getAttributes().get("value"), "foo");

      UIComponent childTwo = (UIComponent) restored.getChildren().get(1);
      assertTrue(childTwo instanceof CoreOutputText);
      assertEquals(childTwo.getAttributes().get("value"), "bar");
    }
    finally
    {
      context.release();
    }
  }

  private RequestContext _createContext()
  {
    // =-=AEW Would be better to create it with a mock context so we
    // can test parsing
    return (new RequestContextFactoryImpl()).createContext(null);
  }
}
