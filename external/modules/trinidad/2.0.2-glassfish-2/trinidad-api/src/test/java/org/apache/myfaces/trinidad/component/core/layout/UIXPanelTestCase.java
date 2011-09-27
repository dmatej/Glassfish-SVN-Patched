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

import org.apache.myfaces.trinidad.component.UIComponentTestCase;
import org.apache.myfaces.trinidad.component.UIXPanel;

/**
 * Unit tests for UIXPanel Components
 *
 */
public abstract class UIXPanelTestCase extends UIComponentTestCase
{
  /**
   * Creates a new UIXPanelTestCase.
   *
   * @param testName  the unit test name
   */
  protected UIXPanelTestCase(
    String testName)
  {
    super(testName);
  }

  protected abstract UIXPanel createTestComponent();

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    UIXPanel component = createTestComponent();
    doTestApplyRequestValues(component);

    component = createTestComponent();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    UIXPanel component = createTestComponent();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    UIXPanel component = createTestComponent();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    UIXPanel component = createTestComponent();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    UIXPanel component = createTestComponent();
    doTestRenderResponse(component);
  }

}
