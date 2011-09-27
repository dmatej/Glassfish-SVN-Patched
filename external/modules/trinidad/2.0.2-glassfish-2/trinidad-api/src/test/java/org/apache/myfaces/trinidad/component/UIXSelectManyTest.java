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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.faces.validator.Validator;

import junit.framework.Test;
import junit.framework.TestSuite;
/**
 * Unit tests for UIXSelectMany.
 *
 */
public class UIXSelectManyTest extends UIXEditableValueTestCase
{
  /**
   * Creates a new UIXSelectManyTest.
   *
   * @param testName  the unit test name
   */
  public UIXSelectManyTest(
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
    return new TestSuite(UIXSelectManyTest.class);
  }

  /**
   * Test a few generic properties of the attribute Map.
   * (These tests really should be at a higher level...)
   */
  @SuppressWarnings("unchecked")
  public void testAttributeMap()
  {
    UIXSelectMany component = createSelectMany();
    assertEquals(null, component.getValueBinding("validators"));
    Map<String, Object> attributes = component.getAttributes();
    Validator[] validators = (Validator[]) attributes.get("validators");
    assertEquals(0, validators.length);

    component.addValidator(new javax.faces.validator.LengthValidator());
    validators = (Validator[]) attributes.get("validators");
    assertEquals(1, validators.length);
    assertTrue(validators[0] instanceof javax.faces.validator.LengthValidator);
  }

  public void testCompareValues()
  {
    UIXSelectMany component = createSelectMany();

    assertFalse(component.compareValues(null, null));
    assertFalse(component.compareValues(null, new Object[0]));
    assertFalse(component.compareValues(null, new ArrayList<String>()));
    assertTrue(component.compareValues(null, new Object[1]));
    assertFalse(component.compareValues(new int[]{1, 2}, new int[]{2, 1}));
    assertTrue(component.compareValues(new int[]{1, 2, 3}, new int[]{2, 1}));

    ArrayList<String> one;
    ArrayList<String> two;

    one = new ArrayList<String>();
    one.add("foo");
    one.add("bar");

    two = new ArrayList<String>();
    two.add("bar");
    two.add("foo");

    assertFalse(component.compareValues(one, two));
    // Make sure compareValues didn't destroy either list
    assertEquals(2, one.size());
    assertEquals(2, two.size());

    one.add("baz");
    assertTrue(component.compareValues(one, two));
  }

  @Override
  public void testProcessValidations()
  {
    String[] submittedValue = new String[] {"foo", "bar"};
    List<String> convertedValue = Arrays.asList(submittedValue);

    doTestProcessValidations(createEditableValue(), submittedValue, convertedValue);
  }

  @Override
  protected UIXEditableValue createEditableValue()
  {
    return createSelectMany();
  }

  protected UIXSelectMany createSelectMany()
  {
	return new UIXSelectMany();
  }

}
