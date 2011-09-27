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

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.jmock.Mock;

/**
 * Unit tests for LongRangeValidator
 *
 */
public class LongRangeValidatorTest extends ValidatorTestCase
{
  public LongRangeValidatorTest(String testName)
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
    return new TestSuite(LongRangeValidatorTest.class);
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
    LongRangeValidator validator = new LongRangeValidator();

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
    LongRangeValidator validator = new LongRangeValidator();

    doTestNullContext(wrapper, validator);
  }

  public void testNullComponent()
  {
    LongRangeValidator validator = new LongRangeValidator();

    doTestNullComponent(facesContext, validator);
  }

  public void testTooLarge()
  {
    // since the pattern has not been set it will be null
    // let us push some arbitary value
    Mock mock = mock(UIComponent.class); 
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    try
    {
      LongRangeValidator validator = new LongRangeValidator();
      validator.setMaximum(100);
      validator.validate(facesContext, component, 1000);
      // test fails if it is here

      fail("Expected Null pointer exception");
    }
    catch (ValidatorException ve)
    {
      // suppress it - this is as expected
    }
    mock.verify();
  }

  public void testWrongType()
  {
    // since the pattern has not been set it will be null
    // let us push some arbitary value
    Mock mock = mock(UIComponent.class); 
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    try
    {
      LongRangeValidator validator = new LongRangeValidator();
      validator.setMaximum(2);
      validator.validate(facesContext, component, "thisShouldFail");
      // test fails if it is here

      fail("Expected Null pointer exception");
    }
    catch (ValidatorException ve)
    {
      // suppress it - this is as expected
    }
    mock.verify();
  }

  public void testExactFailure()
  {
    // some very basic sanity test
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    try
    {
      LongRangeValidator validator = new LongRangeValidator();
      long value = 20;
      validator.setMinimum(2);
      validator.setMaximum(2);
      validator.validate(facesContext, component, value);
      fail("Expected ValidatorException for exact");
    }
    catch (ValidatorException ve)
    {
      // if exception then fine.
    }

    mock.verify();
  }

  public void testSanitySuccess()
  {
    //some very basic sanity test
    //
    LongRangeValidator validator = new LongRangeValidator();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    Long values[]   = {200l,500l};
    validator.setMinimum(2);
    for (int i = 0; i < values.length ; i++)
    {
      doTestValidate(validator, facesContext, wrapper, values[i]);
    }
  }

  public void testStringBasedValues()
  {
    LongRangeValidator validator = new LongRangeValidator();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    String values[]   = {"200","500"};
    validator.setMinimum(199);
    for (int i = 0; i < values.length ; i++)
    {
      doTestValidate(validator, facesContext, wrapper, values[i]);
    }
  }

  public void testStateHolderSaveRestore()
  {
    LongRangeValidator validator = new LongRangeValidator();
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    validator.setMaximum(5);
    validator.setMessageDetailMaximum("Validation failed");
    LongRangeValidator restoreValidator = new  LongRangeValidator();

    doTestStateHolderSaveRestore(validator, restoreValidator,
                                 facesContext, wrapper);
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