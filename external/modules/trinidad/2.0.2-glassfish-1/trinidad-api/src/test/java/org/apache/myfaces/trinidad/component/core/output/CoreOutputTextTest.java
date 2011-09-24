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
package org.apache.myfaces.trinidad.component.core.output;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CoreOutputText.
 *
 */
public class CoreOutputTextTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreOutputTextTest.
   *
   * @param testName  the unit test name
   */
  public CoreOutputTextTest(
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
    return new TestSuite(CoreOutputTextTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   * @todo deal with accessKey attr
   */
  public void testInitialAttributeValues()
  {
    CoreOutputText component = new CoreOutputText();
    assertEquals(true, component.isRendered());
    assertNull(component.getDescription());
    assertNull(component.getValue());
    assertEquals(0, component.getTruncateAt());

  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   * @todo deal with accessKey, truncateAt attr
   */
  public void testAttributeTransparency()
  {
    CoreOutputText component = new CoreOutputText();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "value",
                                "foo", "bar");
    doTestAttributeTransparency(component, "description",
                                "foo", "bar");
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
    CoreOutputText component = new CoreOutputText();
    // we don't use renderers for decode for this component:
    // Or, we didn't.  But see ADFFACES-42
    //    setRendererUsed(false);
    doTestApplyRequestValues(component);
    // setRendererUsed(true);

    component = new CoreOutputText();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreOutputText component = new CoreOutputText();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreOutputText component = new CoreOutputText();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreOutputText component = new CoreOutputText();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreOutputText component = new CoreOutputText();
    doTestRenderResponse(component);
  }
}
