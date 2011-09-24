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

import org.apache.myfaces.trinidad.validator.ByteLengthValidator;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Unit tests for ByteLengthValidator
 *
 */
public class ByteLengthValidatorTest extends ValidatorTestCase
{
  public ByteLengthValidatorTest(String testName)
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
    return new TestSuite(ByteLengthValidatorTest.class);
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
    ByteLengthValidator validator = new ByteLengthValidator();
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
    ByteLengthValidator validator = new ByteLengthValidator();
    doTestNullContext(wrapper, validator);
  }
 /**
  * Check when component whose value is null is passed to the validator.
  * Should result in exception.
  */
  public void testNullComponent()
  {
    ByteLengthValidator validator = new ByteLengthValidator();
    doTestNullComponent(facesContext, validator);
  }

  /**
   * Tests that non String objects throw a ValidationException.
   */
  public void testNotString()
  {
    doTestIsNotString(new ByteLengthValidator());
  }

  /**
   * Test that basic test passes
   */
  public void testSanitySuccess()
  {

    ByteLengthValidator validator = new ByteLengthValidator();
    //some very basic sanity test
    String values[]    = {"four"};
    String encodings[] = {"ISO-8859-1"};
    int maxBytes[]     = {4};

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    for (int i = 0; i < values.length ; i++)
    {
      validator.setEncoding(encodings[i]);
      validator.setMaximum(maxBytes[i]);
      doTestValidate(validator, facesContext, wrapper, values[i]);
    }
  }

  public void testDefaultEncodingWorks()
  {
    
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    ByteLengthValidator validator = new ByteLengthValidator();
    String value = "four";
    validator.setMaximum(4);
    doTestValidate(validator, facesContext, wrapper, value);
  }

  /**
   * Test that StateHolder implementation for the validator works fine.
   */
  public void testStateHolderSaveRestore()
  {
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    ByteLengthValidator validator = new ByteLengthValidator();
    validator.setEncoding("ISO-8859-1");
    validator.setMaximum(4);
    validator.setMessageDetailMaximum("Testing state holder?");
    ByteLengthValidator restoreValidator = new  ByteLengthValidator();

    doTestStateHolderSaveRestore(validator, restoreValidator,
                                 facesContext, wrapper);
  }

  /**
   * Test for equals function of Validator
   */
  public void testEquals()
  {
    ByteLengthValidator validator = new ByteLengthValidator();
    //1
    validator.setEncoding("ISO-8859-1");
    validator.setMaximum(100);
    validator.setMessageDetailMaximum("MaxMessage");

    ByteLengthValidator otherValidator = new ByteLengthValidator();
    otherValidator.setEncoding("ISO-8859-1");
    otherValidator.setMaximum(100);
    otherValidator.setMessageDetailMaximum("MaxMessage");
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    //2
    otherValidator.setMaximum(150);
    validator.setMaximum(150);
    validator.setMessageDetailMaximum("MaxMessage1");
    otherValidator.setMessageDetailMaximum("MaxMessage1");
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    //3
    otherValidator.setMaximum(200);
    otherValidator.setMessageDetailMaximum("MaxMessage");
    doTestEquals(validator, otherValidator, false);
    assertEquals(false, (validator.hashCode() == otherValidator.hashCode()));
  }

  public void testCustomMessageIsSet()
  {
    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    ByteLengthValidator validator = new ByteLengthValidator();
    int maxBytes[]     = {3};
    validator.setMessageDetailMaximum("\"{1}\"" + _IS_GREATER
                                      + maxBytes[0]);

    //some very basic sanity test
    String values[]    = {"four"};
    String encodings[] = {"ISO-8859-1"};
    String expected = "\"" + values[0] + "\"" + _IS_GREATER + maxBytes[0];
    try
    {
      validator.setMaximum(maxBytes[0]);
      validator.setEncoding(encodings[0]);
      validator.validate(facesContext, component, values[0]);
    }
    catch (ValidatorException ve)
    {
      String msg = ve.getFacesMessage().getDetail();
      assertEquals(msg, expected);
    }
  }
  private static final String _IS_GREATER
    = " exceeds maximum allowed length of ";
}