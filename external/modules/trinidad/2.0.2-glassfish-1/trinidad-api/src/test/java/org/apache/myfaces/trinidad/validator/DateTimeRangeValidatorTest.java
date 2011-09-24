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

import java.util.Date;

import javax.faces.convert.DateTimeConverter;
import javax.faces.validator.ValidatorException;
import javax.faces.component.UIComponent;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockApplication;
import org.jmock.Mock;

/**
 * Unit tests for DateTimeRangeValidator.
 *
 */
public class DateTimeRangeValidatorTest extends ValidatorTestCase
{
  /**
   * Creates a new DateTimeRangeValidatorTest.
   *
   * @param testName  the unit test name
   */
  public DateTimeRangeValidatorTest(
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
    return new TestSuite(DateTimeRangeValidatorTest.class);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ValidatorException  when test fails
   */
  public void testNull() throws ValidatorException
  {
    DateTimeRangeValidator validator = new DateTimeRangeValidator();

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
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
    DateTimeRangeValidator validator = new DateTimeRangeValidator();

    doTestNullContext(wrapper, validator);
  }

  /**
   * Test null value for component.
   */
  public void testNullComponent()
  {
    DateTimeRangeValidator validator = new DateTimeRangeValidator();

    doTestNullComponent(facesContext, validator);
  }

  /**
   * Tests that non Date objects throw a ValidationException.
   */
  public void testNonDate()
  {
    DateTimeRangeValidator validator = new DateTimeRangeValidator();

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    
    mock.stubs().method("getId").will(returnValue("test"));
    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, "not-a-date");
      fail("ValidatorException not thrown");
    }
    catch (IllegalArgumentException iae)
    {
      // pass
    }
    finally
    {
      setFacesContext(null);
    }
    mock.verify();
  }

  /**
   * Tests that dates before the maximum date are valid.
   *
   * @throws ValidatorException  when test fails
   */
  public void testBeforeMaximumDate() throws ValidatorException
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMaximum(new Date(millis));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();

    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, new Date(millis - 1));
    }
    finally
    {
      setFacesContext(null);
    }
    mock.verify();
  }

  protected void setMockCreateConverter(MockApplication app)
  {
    DateTimeConverter dtc = new DateTimeConverter();
    app.addConverter(DateTimeConverter.CONVERTER_ID, dtc.getClass().getName());
  }

  /**
   * Tests that dates after the maximum date cause a ValidationException.
   */
  public void testAfterMaximumDate()
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMaximum(new Date(millis));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    setMockCreateConverter(application);
    try
    {
      setFacesContext(facesContext);
      validator.setMessageDetailMaximum("max set");
      validator.validate(facesContext, component, new Date(millis + 1));
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass
      String msg = e.getFacesMessage().getDetail();
      assertEquals(msg, "max set");
    }
    finally
    {
      setFacesContext(null);
    }

    mock.verify();
  }

  /**
   * Tests that dates after the minimum date are valid.
   *
   * @throws ValidatorException  when test fails
   */
  public void testAfterMinimumDate() throws ValidatorException
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMinimum(new Date(millis));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();

    validator.validate(facesContext, component, new Date(millis + 1));

    mock.verify();
  }

  /**
   * Tests that dates before the minimum date cause a ValidationException.
   */
  public void testBeforeMinimumDate()
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMinimum(new Date(millis));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    
    setMockCreateConverter(application);

    try
    {
      setFacesContext(facesContext);
      validator.setMessageDetailMinimum("min set");
      validator.validate(facesContext, component, new Date(millis - 1));
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass
      String msg = e.getFacesMessage().getDetail();
      assertEquals(msg, "min set");
    }
    finally
    {
      setFacesContext(null);
    }

    mock.verify();
  }

  /**
   * Tests that dates within the date range are valid.
   *
   * @throws ValidatorException  when test fails
   */
  public void testWithinDateRange() throws ValidatorException
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMinimum(new Date(millis));
    validator.setMaximum(new Date(millis + 2));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();

    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, new Date(millis + 1));
    }
    finally
    {
      setFacesContext(null);
    }

    mock.verify();
  }

  /**
   * Tests that dates before the date range cause a ValidationException.
   */
  public void testBeforeDateRange()
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMinimum(new Date(millis));
    validator.setMaximum(new Date(millis + 10));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    setMockCreateConverter(application);

    try
    {
      setFacesContext(facesContext);
      validator.setMessageDetailMinimum("min");
      validator.validate(facesContext, component, new Date(millis - 1));
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass

    }
    finally
    {
      setFacesContext(null);
    }

    mock.verify();
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testAfterDateRange()
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator validator = new DateTimeRangeValidator();
    validator.setMinimum(new Date(millis));
    validator.setMaximum(new Date(millis + 10));

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);

    setMockCreateConverter(application);

    try
    {
      setFacesContext(facesContext);
      validator.setMessageDetailNotInRange("not in range is set");
      validator.validate(facesContext, component, new Date(millis + 20));
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      //first pass
      String msg = e.getFacesMessage().getDetail();
      assertEquals(msg, "not in range is set");
    }
    finally
    {
      setFacesContext(null);
    }

    mock.verify();
  }


  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testStateHolderSaveRestore()
  {
    long millis = System.currentTimeMillis();
    DateTimeRangeValidator originalValidator = new DateTimeRangeValidator();
    originalValidator.setMinimum(new Date(millis));
    originalValidator.setMaximum(new Date(millis + 10));

    originalValidator.setMessageDetailMinimum("min");
    originalValidator.setMessageDetailMaximum("max");
    originalValidator.setMessageDetailNotInRange("not in range");

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    DateTimeRangeValidator restoredValidator = new DateTimeRangeValidator();

    doTestStateHolderSaveRestore(originalValidator, restoredValidator,
                                 facesContext, wrapper);
  }

}