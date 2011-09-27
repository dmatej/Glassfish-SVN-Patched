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

import javax.faces.component.UIComponentBase;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

import org.jmock.Mock;

/**
 * Unit tests for UIXProcess
 *
 */
public class UIXProcessTest extends UIComponentTestCase
{
  /**
   * Creates a new UIXProcessTest.
   *
   * @param testName  the unit test name
   */
  public UIXProcessTest(
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
    return new TestSuite(UIXProcessTest.class);
  }

  public static void main(String[] args)
  {
    TestRunner.run(UIXProcessTest.class);
  }
  
  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXProcess component = new UIXProcess();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    UIXProcess component = new UIXProcess();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);

  }




  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    UIXProcess component = new UIXProcess();
    doTestProcessValidations(component);
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    UIXProcess component = new UIXProcess();
    doTestApplyRequestValues(component);

    component = new UIXProcess();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    UIXProcess component = new UIXProcess();
    doTestUpdateModelValues(component);
  }


  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    UIXProcess component = new UIXProcess();
    doTestRenderResponse(component);
  }
  
  protected Mock createMockUIComponent()
  {
    // the UIXProcess is allowed to call getId and setId on its child
    Mock mock = super.createMockUIComponent();
    
    mock.stubs().method("getId");
    mock.stubs().method("setId");
        
    return mock;
  }

}
