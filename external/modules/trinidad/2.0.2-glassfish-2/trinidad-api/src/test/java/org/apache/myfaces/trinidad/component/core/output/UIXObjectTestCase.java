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

import org.apache.myfaces.trinidad.component.UIComponentTestCase;
import org.apache.myfaces.trinidad.component.UIXObject;

/**
 * Unit tests for UIXObject components
 *
 */
public abstract class UIXObjectTestCase extends UIComponentTestCase
{
  /**
   * Creates a new UIXObjectTest
   *
   * @param testName  the unit test name
   */
  protected UIXObjectTestCase(
    String testName)
  {
    super(testName);
  }

  protected abstract UIXObject createTestComponent();

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    // for decode we don't use renderers:
    // =-=AEW But now, we do... see ADFFACES-42
    //    setRendererUsed(false);
    UIXObject component = createTestComponent();
    doTestApplyRequestValues(component);
    //    setRendererUsed(true);

    component = createTestComponent();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    UIXObject component = createTestComponent();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    UIXObject component = createTestComponent();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    UIXObject component = createTestComponent();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    UIXObject component = createTestComponent();
    doTestRenderResponse(component);
  }
}
