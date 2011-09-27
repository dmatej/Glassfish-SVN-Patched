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
package org.apache.myfaces.trinidad.component.core.layout;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CorePanelAccordion
 *
 */

public class CorePanelAccordionTest extends UIComponentTestCase
{
  /**
   * Creates a new CorePanelAccordion.
   *
   * @param testName  the unit test name
   */
  public CorePanelAccordionTest(String testName)
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
    return new TestSuite(CorePanelAccordionTest.class);
  }

  /**
   * Tests the Initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelAccordion component = new CorePanelAccordion();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the values set for the component attributes.
   */
  public void testgetAttributeValues()
  {
    CorePanelAccordion component = new CorePanelAccordion();
    assertEquals("org.apache.myfaces.trinidad.Panel",component.getFamily());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   *
   */
  public void testAttributeTransparency()
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestAttributeTransparency(component,
                                "rendered",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the Apply Request Values Phase
   */
  public void testApplyRequestValues()
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestApplyRequestValues(component);
    component= new CorePanelAccordion();
    component.setRendered(false);
    doTestApplyRequestValues(component);
    component= new CorePanelAccordion();
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestInvokeApplication(component, null);
  }
  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CorePanelAccordion component= new CorePanelAccordion();
    doTestRenderResponse(component);
  }
}

