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
package org.apache.myfaces.trinidad.component.core.data;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CoreColumn.
 *
 */
public class CoreColumnTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreColumnTest.
   *
   * @param testName  the unit test name
   */
  public CoreColumnTest(
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
    return new TestSuite(CoreColumnTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreColumn component = new CoreColumn();
    assertTrue(component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreColumn component = new CoreColumn();

    doTestAttributeTransparency(component, "separateRows",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "headerNoWrap",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "noWrap",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    CoreColumn component = new CoreColumn();
    doTestFacetTransparency(component, CoreColumn.HEADER_FACET);
    doTestFacetTransparency(component, CoreColumn.FOOTER_FACET);
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    CoreColumn component = new CoreColumn();
    doTestApplyRequestValues(component);

    component = new CoreColumn();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreColumn component = new CoreColumn();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreColumn component = new CoreColumn();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreColumn component = new CoreColumn();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreColumn component = new CoreColumn();
    doTestRenderResponse(component);
  }

  public static void main(String[] args)
  {
    TestRunner.run(CoreColumnTest.class);
  }

}
