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
package org.apache.myfaces.trinidad.component.html;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for HtmlFrameBorderLayout.
 *
 */
public class HtmlFrameBorderLayoutTest extends UIComponentTestCase
{
  /**
   * Creates a new HtmlFrameBorderLayoutTest.
   *
   * @param testName  the unit test name
   */
  public HtmlFrameBorderLayoutTest(
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
    return new TestSuite(HtmlFrameBorderLayoutTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestAttributeTransparency(component, "width",
                                "50%", "100%");
    doTestAttributeTransparency(component, "height",
                                "25%", "75%");
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();

    doTestFacetTransparency(component, HtmlFrameBorderLayout.CENTER_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.TOP_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.BOTTOM_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.LEFT_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.RIGHT_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.START_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.END_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.INNER_LEFT_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.INNER_RIGHT_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.INNER_START_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.INNER_END_FACET);
    doTestFacetTransparency(component, HtmlFrameBorderLayout.ALTERNATE_CONTENT_FACET);
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    HtmlFrameBorderLayout component = new HtmlFrameBorderLayout();
    doTestRenderResponse(component);
  }
}
