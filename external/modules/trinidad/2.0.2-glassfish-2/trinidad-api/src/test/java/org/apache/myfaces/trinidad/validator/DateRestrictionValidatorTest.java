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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.faces.application.FacesMessage;
import javax.faces.convert.DateTimeConverter;
import javax.faces.validator.ValidatorException;
import javax.faces.component.UIComponent;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockApplication;
import org.jmock.Mock;

/**
 * Unit tests for DateRestrictionValidator.
 *
 */
public class DateRestrictionValidatorTest extends ValidatorTestCase
{
  /**
   * Creates a new DateTimeRangeValidatorTest.
   *
   * @param testName  the unit test name
   */
  public DateRestrictionValidatorTest(
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
    return new TestSuite(DateRestrictionValidatorTest.class);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ValidatorException  when test fails
   */
  public void testNull() throws ValidatorException
  {
    DateRestrictionValidator validator = new DateRestrictionValidator();

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
    DateRestrictionValidator validator = new DateRestrictionValidator();

    doTestNullContext(wrapper, validator);
  }

  /**
   * Test null value for component.
   */
  public void testNullComponent()
  {
    DateRestrictionValidator validator = new DateRestrictionValidator();

    doTestNullComponent(facesContext, validator);
  }

  /**
   * Tests that non Date objects throw a ValidationException.
   */
  public void testNonDate()
  {
    DateRestrictionValidator validator = new DateRestrictionValidator();

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

  protected void setMockCreateConverter(MockApplication app)
  {
    DateTimeConverter dtc = new DateTimeConverter();
    app.addConverter(DateTimeConverter.CONVERTER_ID, dtc.getClass().getName());
  }
  
  /**
   * Tests that invalid day, like Fri or Sun cause a ValidationException.
   */
  public void testInvalidDaysOfWeek()
  {
    DateRestrictionValidator validator = new DateRestrictionValidator();
    validator.setInvalidDaysOfWeek(new String[] {"mon", "Sun"});

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    
    SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
    Date dateToValidate = null;
    
    try
    {
      dateToValidate = sdf.parse("30.10.2006");
    }
    catch (ParseException e1)
    {
    }

    setMockCreateConverter(application);
    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, dateToValidate);
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass
//      String msg = e.getFacesMessage().getDetail();
//      assertEquals(msg, "max set");
      
      FacesMessage msg = e.getFacesMessage();
      assertNotNull(msg);
      
    }
    finally
    {
      setFacesContext(null);
    }
  }

  /**
   * Tests that invalid day, like Fri or Sun cause a ValidationException.
   */
  public void testInvalidMonth()
  {
    
    DateRestrictionValidator validator = new DateRestrictionValidator();
    validator.setInvalidMonths(new String[] {"Jan", "Oct"});

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    
    SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
    Date dateToValidate = null;
    
    try
    {
      dateToValidate = sdf.parse("30.10.2006");
    }
    catch (ParseException e1)
    {
    }

    setMockCreateConverter(application);
    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, dateToValidate);
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass
//      String msg = e.getFacesMessage().getDetail();
//      assertEquals(msg, "max set");
      
      FacesMessage msg = e.getFacesMessage();
      assertNotNull(msg);
      
    }
    finally
    {
      setFacesContext(null);
    }
  }

  /**
   * Tests that invalid day, like Fri or Sun cause a ValidationException.
   */
  public void testInvalidDays()
  {
    
    DateRestrictionValidator validator = new DateRestrictionValidator();
    TestDateListProvider tdlp = new TestDateListProvider();
    validator.setInvalidDays(tdlp);

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    setMockLabelForComponent(wrapper);
    
    SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
    Date dateToValidate = null;
    
    try
    {
      dateToValidate = sdf.parse("15.05.2007");
    }
    catch (ParseException e1)
    {
    }

    setMockCreateConverter(application);
    try
    {
      setFacesContext(facesContext);
      validator.validate(facesContext, component, dateToValidate);
      fail("ValidatorException not thrown");
    }
    catch (ValidatorException e)
    {
      // pass
//      String msg = e.getFacesMessage().getDetail();
//      assertEquals(msg, "max set");
      
      FacesMessage msg = e.getFacesMessage();
      assertNotNull(msg);
      
    }
    finally
    {
      setFacesContext(null);
    }
  }

  public void testStateHolderSaveRestore()
  {
    DateRestrictionValidator originalValidator = new DateRestrictionValidator();
    
    originalValidator.setInvalidDaysOfWeek(new String[]{"MO", "DI"});
    originalValidator.setInvalidMonths(new String[]{"MO", "DI"});

    originalValidator.setMessageDetailInvalidDays("min");
    originalValidator.setMessageDetailInvalidDaysOfWeek("max");
    originalValidator.setMessageDetailInvalidMonths("not in range");

    Mock mock = buildMockUIComponent();
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    DateRestrictionValidator restoredValidator = new DateRestrictionValidator();

    doTestStateHolderSaveRestore(originalValidator, restoredValidator,
                                 facesContext, wrapper);
  }

}