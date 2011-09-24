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

import org.apache.myfaces.trinidad.component.UIXPanel;

/**
 * Unit tests for CorePanelHeader.
 *
 */
public class CorePanelHeaderTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelHeaderTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelHeaderTest(
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
    return new TestSuite(CorePanelHeaderTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   * @todo deal with size attr
   */
  public void testInitialAttributeValues()
  {
    CorePanelHeader component = new CorePanelHeader();

    assertTrue(component.isRendered());
    assertNull(component.getIcon());
    assertNull(component.getText());
    assertEquals("none", component.getMessageType());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   * @todo deal with size attr
   */
  public void testAttributeTransparency()
  {
    CorePanelHeader component = new CorePanelHeader();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "messageType",
                                "error", "warning");
    doTestAttributeTransparency(component, "text",
                                "foo", "bar");
    doTestAttributeTransparency(component, "icon",
                                "foo", "bar");
  }

  @Override
  protected UIXPanel createTestComponent()
  {
    return new CorePanelHeader();
  }
}
