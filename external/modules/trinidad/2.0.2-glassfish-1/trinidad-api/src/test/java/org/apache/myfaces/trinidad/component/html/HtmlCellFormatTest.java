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
 * Unit tests for HtmlCellFormat.
 *
 */
public class HtmlCellFormatTest extends UIComponentTestCase
{
  /**
   * Creates a new HtmlCellFormatTest.
   *
   * @param testName  the unit test name
   */
  public HtmlCellFormatTest(
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
    return new TestSuite(HtmlCellFormatTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    assertTrue(component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestAttributeTransparency(component, "columnSpan",
                                new Integer(1), new Integer(2));
    doTestAttributeTransparency(component, "halign",
                                "top", "bottom");
    doTestAttributeTransparency(component, "header",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "headers",
                                "h1 h2", "h3 h4");
    doTestAttributeTransparency(component, "height",
                                "50%", "100%");
    doTestAttributeTransparency(component, "rowSpan",
                                new Integer(3), new Integer(4));
    doTestAttributeTransparency(component, "shortText",
                                "foo", "bar");
    doTestAttributeTransparency(component, "valign",
                                "left", "right");
    doTestAttributeTransparency(component, "width",
                                "25%", "75%");
    doTestAttributeTransparency(component, "wrappingDisabled",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    // no facets yet
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    HtmlCellFormat component = new HtmlCellFormat();
    doTestRenderResponse(component);
  }
}
