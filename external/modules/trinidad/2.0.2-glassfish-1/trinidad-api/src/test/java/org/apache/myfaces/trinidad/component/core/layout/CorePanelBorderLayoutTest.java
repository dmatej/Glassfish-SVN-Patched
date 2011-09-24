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

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.component.UIXPanel;

/**
 * Unit tests for CorePanelBorderLayout.
 *
 */
public class CorePanelBorderLayoutTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelBorderLayoutTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelBorderLayoutTest(
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
    return new TestSuite(CorePanelBorderLayoutTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelBorderLayout component = new CorePanelBorderLayout();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CorePanelBorderLayout component = new CorePanelBorderLayout();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    CorePanelBorderLayout component = new CorePanelBorderLayout();

    doTestFacetTransparency(component, CorePanelBorderLayout.TOP_FACET);
    doTestFacetTransparency(component, CorePanelBorderLayout.BOTTOM_FACET);
    doTestFacetTransparency(component, CorePanelBorderLayout.LEFT_FACET);
    doTestFacetTransparency(component, CorePanelBorderLayout.RIGHT_FACET);
    doTestFacetTransparency(component, CorePanelBorderLayout.START_FACET);
    doTestFacetTransparency(component, CorePanelBorderLayout.END_FACET);
  }
  
  @Override
  protected UIXPanel createTestComponent()
  {
    return new CorePanelBorderLayout();
  }
  
  public static void main(String[] args)
  {
    TestRunner.run(CorePanelBorderLayoutTest.class);
  }
}
