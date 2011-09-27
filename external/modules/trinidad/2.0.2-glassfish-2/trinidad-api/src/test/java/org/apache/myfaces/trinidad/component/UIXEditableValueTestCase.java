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

import javax.faces.component.EditableValueHolder;

import org.apache.myfaces.trinidad.component.UIXComponent;

/**
 * Base class for unit tests for UIXEditableValue components.
 *
 */
abstract public class UIXEditableValueTestCase extends UIComponentTestCase
{
  /**
   * Creates a UIXEditableValueTestCase.
   *
   * @param testName  the unit test name
   */
  public UIXEditableValueTestCase(
    String testName)
  {
    super(testName);
  }

  @SuppressWarnings("cast")
  public void testInterfaces()
  {
    UIXEditableValue component = createEditableValue();
    assertTrue(component instanceof EditableValueHolder);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXEditableValue component = createEditableValue();
    assertEquals(false, component.isTransient());
    assertEquals(true, component.isValid());
    assertEquals(true, component.isRendered());
    assertEquals(null, component.getValue());
    assertEquals(null, component.getLocalValue());
    assertEquals(null, component.getSubmittedValue());
    assertEquals(false, component.isLocalValueSet());
  }

  /**
   * Tests that the constants stored on the component class definition
   * are identical to the instances retrieved dynamically from the component
   * storage type.
   */
  public void testTypeKeyInstances()
  {
    UIXEditableValue component = createEditableValue();
    assertSame(component.getFacesBean().getType().findKey("value"),
               UIXEditableValue.VALUE_KEY);
  }

  /**
   * Tests the transparency of component attributes.
   *
   * @todo remaining attributes
   */
  public void testAttributeTransparency()
  {
    //UIXEditableValue component = createEditableValue();
    // remaining attributes here
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    doTestApplyRequestValues(createEditableValue());

    UIXEditableValue component = createEditableValue();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    doTestProcessValidations(createEditableValue());
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    doTestUpdateModelValues(createEditableValue());
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    UIXEditableValue component = createEditableValue();

    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    doTestRenderResponse(createEditableValue());
  }

  protected final UIXComponent createComponent()
  {
	return createEditableValue();
  }

  abstract protected UIXEditableValue createEditableValue();
}
