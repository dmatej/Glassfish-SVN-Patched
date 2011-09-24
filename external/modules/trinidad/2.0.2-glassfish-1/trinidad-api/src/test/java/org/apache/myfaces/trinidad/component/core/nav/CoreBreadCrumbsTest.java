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
package org.apache.myfaces.trinidad.component.core.nav;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

import org.jmock.Mock;

/**
 * Unit tests for CoreBreadCrumbs.
 *
 */
public class CoreBreadCrumbsTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreBreadCrumbsTest.
   *
   * @param testName  the unit test name
   */
  public CoreBreadCrumbsTest(
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
    return new TestSuite(CoreBreadCrumbsTest.class);
  }
  
  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    assertEquals(true, component.isRendered());
    assertEquals("horizontal", component.getOrientation());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "orientation",
                                "horizontal",
                                "vertical");
  }


  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    doTestApplyRequestValues(component);

    component = new CoreBreadCrumbs();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreBreadCrumbs component = new CoreBreadCrumbs();
    doTestRenderResponse(component);
  }

  protected Mock createMockUIComponent()
  {
    // the BreadCrumbs is allowed to call getId and setId on its child
    Mock mock = super.createMockUIComponent();
    
    mock.stubs().method("getId");
    mock.stubs().method("setId");
        
    return mock;
  }
}
