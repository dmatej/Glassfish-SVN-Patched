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

import javax.faces.component.UIComponent;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for UIXShowDetail.
 *
 */
public class UIXShowDetailTest extends UIComponentTestCase
{
  /**
   * Creates a new UIXShowDetailTest.
   *
   * @param testName  the unit test name
   */
  public UIXShowDetailTest(
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
    return new TestSuite(UIXShowDetailTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXShowDetail hideShow = createHideShow();
    assertEquals(false, hideShow.isDisclosed());
    assertEquals(true, hideShow.isRendered());
    assertEquals(false, hideShow.isImmediate());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   *
   * @todo remaining attributes
   */
  public void testAttributeTransparency()
  {
    UIXShowDetail component = createHideShow();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    // remaining attributes here
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    doTestApplyRequestValues(createHideShow());
    
    UIXShowDetail disclosed = createHideShow();
    disclosed.setDisclosed(true);
    doTestApplyRequestValues(disclosed);

    disclosed = createHideShow();

    disclosed.setDisclosed(true);
    disclosed.setRendered(false);
    doTestApplyRequestValues(disclosed);

    disclosed = createHideShow();
    disclosed.setDisclosed(false);
    disclosed.setRendered(false);
    doTestApplyRequestValues(disclosed);
  }


  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    doTestProcessValidations(createHideShow());
    UIXShowDetail disclosed = createHideShow();
    disclosed.setDisclosed(true);
    doTestProcessValidations(disclosed);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    doTestUpdateModelValues(createHideShow());
    UIXShowDetail disclosed = createHideShow();
    disclosed.setDisclosed(true);
    doTestUpdateModelValues(disclosed);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    UIXShowDetail component = createHideShow();

    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    doTestRenderResponse(createHideShow());
  }

  public static void main(String[] args)
  {
    TestRunner.run(UIXShowDetailTest.class);
  }
  
  @Override
  protected boolean willChildrenBeProcessed(UIComponent component)
  {
    if (!component.isRendered())
      return false;

    return ((UIXShowDetail) component).isDisclosed();
  }

  protected UIXShowDetail createHideShow()
  {
    return new UIXShowDetail();
  }

}
