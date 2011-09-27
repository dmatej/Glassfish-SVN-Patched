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
package org.apache.myfaces.trinidad.validator;

import javax.faces.validator.ValidatorException;
import javax.faces.component.UIComponent;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.validator.RegExpValidator;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.jmock.Mock;

/**
 * Unit tests for RegExpValidator
 *
 */
public class RegExpValidatorTest extends ValidatorTestCase
{
  public RegExpValidatorTest(String testName)
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
    return new TestSuite(RegExpValidatorTest.class);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ValidatorException  when test fails
   */
  public void testNull() throws ValidatorException
  {
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    RegExpValidator validator = new RegExpValidator();

    doTestNull(facesContext, wrapper, validator);
  }

  /**
   * Test when context is set to null
   */
  public void testNullContext()
  {
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    RegExpValidator validator = new RegExpValidator();

    doTestNullContext(wrapper, validator);
  }

  public void testNullComponent()
  {
    RegExpValidator validator = new RegExpValidator();

    doTestNullComponent(facesContext, validator);
  }

  /**
   * Tests that non String objects throw a ValidationException.
   */
  public void testNotString()
  {
    doTestIsNotString(new RegExpValidator());
  }

  /**
   * Test that pattern when not set throws null pointer exceptin
   */
  public void testPatternNotSet()
  {
    // since the pattern has not been set it will be null
    // let us push some arbitary value
    Mock mock = mock(UIComponent.class); 
    UIComponent component = (UIComponent) mock.proxy();

    try
    {
      RegExpValidator validator = new RegExpValidator();
      validator.validate(facesContext, component, "someValue");
      // test fails if it is here

      fail("Expected Null pointer exception");
    }
    catch (NullPointerException npe)
    {
      // suppress it - this is as expected
    }
    mock.verify();
  }

  public void testWithPattern()
  {
    Mock mock = mock(UIComponent.class); 
    UIComponent component = (UIComponent) mock.proxy();

    RegExpValidator validator = new RegExpValidator();
    validator.setPattern("[1-9]|[1-9][0-9]|[1-9][0-9][0-9]");
    validator.validate(facesContext, component, "15");

    mock.verify();
  }

  /**
   * Test that pattern when set to "" should fail validation
   */
  public void testBlankValueOnPattern()
  {
    // some very basic sanity test
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    try
    {
      RegExpValidator validator = new RegExpValidator();
      String value = "999999";
      validator.setPattern("");
      validator.validate(facesContext, component, value);
      fail("Expected ValidatorException");
    }
    catch (ValidatorException ve)
    {
      // if exception then fine.
    }

    mock.verify();
  }

  /**
   * Simple test case which is expected to pass
   * @todo need to add many test cases - add string to the values and
   *       patterns to patterns array.
   *
   */
  public void testSanitySuccess()
  {
    //some very basic sanity test
    //
    RegExpValidator validator = new RegExpValidator();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    String values[]   = {"9123456","9x"};
    String patterns[] = {"[0-9]*","[9][x]"};
    for (int i = 0; i < values.length ; i++)
    {
      validator.setPattern(patterns[i]);
      doTestValidate(validator, facesContext, wrapper, values[i]);
    }
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testStateHolderSaveRestore()
  {
    RegExpValidator validator = new RegExpValidator();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    validator.setPattern("TestSaveRestore");
    validator.setMessageDetailNoMatch("\"{0}\" in \"{1}\" failed!! {4}");
    RegExpValidator restoreValidator = new  RegExpValidator();

    doTestStateHolderSaveRestore(validator, restoreValidator,
                                 facesContext, wrapper);
  }

  /**
   * Test for equality of validators
   */
  public void testIsEqual()
  {
    RegExpValidator validator = new RegExpValidator();
    RegExpValidator otherValidator = new RegExpValidator();
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    validator.setPattern("[0-9]");
    validator.setMessageDetailNoMatch("\"{0}\" in \"{1}\" failed!! {4}");
    otherValidator.setPattern("[0-9]");
    otherValidator.setMessageDetailNoMatch("\"{0}\" in \"{1}\" failed!! {4}");
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    otherValidator.setPattern(null);
    doTestEquals(validator, otherValidator, false);
    assertEquals(false, (validator.hashCode() == otherValidator.hashCode()));
  }

  public void testCustomMessageIsSet()
  {
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    RegExpValidator validator = new RegExpValidator();

    validator.setPattern("[0-9]*");
    validator.setMessageDetailNoMatch("\"{0}\" in \"{1}\" failed!! {4}");
    //some very basic sanity test

    try
    {
      validator.validate(facesContext, component, "9123456");
    }
    catch (ValidatorException ve)
    {
      String msg = ve.getFacesMessage().getDetail();
      assertEquals(msg, "\"four\" in \"label\" failed!! [0-9]*");
    }
  }

}
  //////////////////////////////////////////////////////////////////////////////
  //                             MOCK OBJECTS
  // 1. Get a MockControl for the interface we would like to simulate
  // 2. get the MockObject from MockControl
  // 3. specify the behaviour of the Mock Object (record state)
  // 4. activate the MockObject via the control  (replay state)
  //
  //////////////////////////////////////////////////////////////////////////////