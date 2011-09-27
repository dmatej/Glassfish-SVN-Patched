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
package org.apache.myfaces.trinidad.component.core;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CoreForm.
 *
 */
public class CoreFormTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreFormTest.
   *
   * @param testName  the unit test name
   */
  public CoreFormTest(
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
    return new TestSuite(CoreFormTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreForm form = new CoreForm();
    assertEquals(false, form.isUsesUpload());
    assertEquals(false, form.isTransient());
    assertEquals(false, form.isSubmitted());
    assertEquals(true, form.isRendered());
  }

  /**
   * Tests that the constants stored on the component class definition
   * are identical to the instances retrieved dynamically from the component
   * storage type.
   */
  public void testTypeKeyInstances()
  {
    CoreForm form = new CoreForm();
    assertSame(form.getFacesBean().getType(),
               CoreForm.TYPE);
    assertSame(form.getFacesBean().getType().findKey("usesUpload"),
               CoreForm.USES_UPLOAD_KEY);
  }

  /**
   * Tests the transparency of component attributes.
   *
   * @todo remaining attributes
   */
  public void testAttributeTransparency()
  {
    CoreForm component = new CoreForm();

    doTestAttributeTransparency(component, "usesUpload",
                                Boolean.TRUE, Boolean.FALSE);
    // remaining attributes here
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    TestForm testForm = new TestForm(true);
    doTestApplyRequestValues(testForm);

    testForm = new TestForm(true);
    testForm.setRendered(false);
    doTestApplyRequestValues(testForm);

    testForm = new TestForm(false);
    doTestApplyRequestValues(testForm);

    testForm = new TestForm(false);
    testForm.setRendered(false);
    doTestApplyRequestValues(testForm);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreForm form;
    form = new CoreForm();
    form.setSubmitted(false);
    doTestProcessValidations(form);

    form = new CoreForm();
    form.setSubmitted(true);
    doTestProcessValidations(form);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreForm form;
    form = new CoreForm();
    form.setSubmitted(false);
    doTestUpdateModelValues(form);

    form = new CoreForm();
    form.setSubmitted(true);
    doTestUpdateModelValues(form);
  }


  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreForm component = new CoreForm();

    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    doTestRenderResponse(new CoreForm());
  }

  // Subclass that will hardcode whether the form is getting
  // submitted
  static private class TestForm extends CoreForm
  {
    public TestForm(boolean willBeSubmitted)
    {
      _willBeSubmitted = willBeSubmitted;
    }

    public boolean getWillBeSubmitted()
    {
      return _willBeSubmitted;
    }

    @Override
    public void decode(FacesContext context)
    {
      super.decode(context);
      setSubmitted(getWillBeSubmitted());
    }

    private final boolean _willBeSubmitted;
  }

  @Override
  protected boolean willChildrenBeProcessed(UIComponent component)
  {
    if (!component.isRendered())
      return false;

    if (component instanceof TestForm)
      return ((TestForm) component).getWillBeSubmitted();
    else
      return ((CoreForm) component).isSubmitted();
  }
}

