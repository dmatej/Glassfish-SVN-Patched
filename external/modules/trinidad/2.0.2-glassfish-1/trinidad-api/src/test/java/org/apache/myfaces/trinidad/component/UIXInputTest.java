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

import javax.el.ValueExpression;
import javax.faces.el.ValueBinding;

import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests for UIXInput
 *
 */
public class UIXInputTest extends UIComponentTestCase
{
  /**
   * @param testName  the unit test name
   */
  public UIXInputTest(
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
    return new TestSuite(UIXInputTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXInput input = new UIXInput();
    assertFalse(input.isLocalValueSet());
    assertNull(input.getSubmittedValue());
    assertNull(input.getLocalValue());
    assertNull(input.getValue());
  }


  public void testValueExpressions()
  {
    Mock mockExpression = mock(ValueExpression.class);
    ValueExpression expr = (ValueExpression) mockExpression.proxy();
    mockExpression.expects(atLeastOnce()).method("getValue").will(returnValue("socks"));
    mockExpression.expects(atLeastOnce()).method("isLiteralText").will(returnValue(Boolean.TRUE));

    UIXInput input = new UIXInput();
    input.setValueExpression("value", expr);
    assertEquals("socks", input.getValue());
    input.setValue("shoes");
    assertEquals("shoes", input.getValue());
    input.setValueExpression("value", null);
    assertEquals("shoes", input.getValue());    
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    UIXInput input = new UIXInput();
    doTestAttributeTransparency(input, "localValueSet", Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(input, "value", "foo", "fum");
    doTestAttributeTransparency(input, "submittedValue", "bar", "baz");
  }

  /**
   * Tests to make sure the setValue(), isLocalValueSet() contract is
   * preserved.
   */
  public void testLocalValue()
  {
    UIXInput input = new UIXInput();
    assertFalse(input.isLocalValueSet());
    assertNull(input.getLocalValue());
    assertNull(input.getValue());
    
    // setup a valueBinding
    Mock mockBinding = mock(ValueBinding.class);
    ValueBinding binding = (ValueBinding) mockBinding.proxy();
    mockBinding.expects(atLeastOnce()).method("getValue").will(returnValue(Boolean.TRUE));//  setupGetValue(Boolean.TRUE);
    
    input.setValueBinding("value", binding);
    assertFalse(input.isLocalValueSet());
    assertNull(input.getLocalValue());
    assertEquals(Boolean.TRUE, input.getValue());
    
    // now set a local value
    input.setValue("foo");
    assertTrue(input.isLocalValueSet());
    assertEquals("foo", input.getLocalValue());
    assertEquals("foo", input.getValue());
  }

  public static void main(String[] args)
  {
    TestRunner.run(UIXInputTest.class);
  }

}
