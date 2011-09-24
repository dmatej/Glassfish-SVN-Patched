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
package org.apache.myfaces.trinidad.convert;

import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidadbuild.test.AbstractBaseTestCase;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockFacesContext;
import org.jmock.Mock;

/**
 * Base class for unit tests of Converters
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/convert/ConverterTestCase.java#0 $) $Date: 17-oct-2005.16:28:54 $
 *
 */
public abstract class ConverterTestCase extends AbstractBaseTestCase
{
  public ConverterTestCase(String testName)
  {
    super(testName);
  }

  /**
   * This test performs action on the method
   * javax.faces.convert.Converter.getAsObject(FacesContext, UIComponent, String)
   * and
   * javax.faces.convert.Converter.getAsString(FacesContext, UIComponent, Object)
   * for method getAsObject() should return a value of null while getAsString()
   * should return a empty string.
   * @throws ValidatorException  when test fails
   */
  protected void doTestNull(
    MockFacesContext context,
    MockUIComponentWrapper wrapper,
    Converter converter
    ) throws ConverterException
  {
    Object obj = converter.getAsObject(context, wrapper.getUIComponent(), null);
    assertEquals(null, obj);
    String str = converter.getAsString(context, wrapper.getUIComponent(), null);
    assertEquals("",str);
    wrapper.getMock().verify();
  }

  /**
   * If contex or component = null then should throw NullPointerException
   */
  protected void doTestNullContext(
    MockUIComponentWrapper wrapper,
    Converter converter) throws NullPointerException
  {
    try
    {
      converter.getAsObject(null, wrapper.getUIComponent(), "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
    try
    {
      converter.getAsString(null, wrapper.getUIComponent(), "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  /**
   * If contex or component = null then should throw NullPointerException
   */
  protected void doTestNullComponent(MockFacesContext context,
    Converter converter ) throws NullPointerException
  {
    try
    {
      converter.getAsObject(context, null, "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
    try
    {
      converter.getAsString(context, null, "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  protected void doTestBlankValue(Converter converter)
  {
    MockFacesContext context = new MockFacesContext();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    Object value = converter.getAsObject(context, component,"");
    assertEquals(null, value);
  }

  /**
   * Test the validity of method
   * javax.faces.convert.Converter.getAsObject(FacesContext, UIComponent, String)
   *
   * @param converter converter which is to be tested
   * @param context MockFaces context
   * @param component MockFaces component
   * @throws javax.faces.convert.ConvertException
   */
  protected void doTestGetAsObject(
    Converter converter,
    MockFacesContext context,
    MockUIComponentWrapper wrapper,
    String value,
    Object expectedValue
    )  throws ConverterException
  {
    Object conv = converter.getAsObject(context, wrapper.getUIComponent(), value);
    assertEquals(expectedValue, conv);

    wrapper.getMock().verify();
  }


  /**
   * Test the validity of call on the method
   * javax.faces.convert.Converter.getAsString(FacesContext, UIComponent, Object)
   * @param converter converter which is to be tested
   * @param context MockFaces context
   * @param component MockFaces component
   * @throws javax.faces.convert.ConvertException
   */
  protected void doTestGetAsString(
    Converter converter,
    MockFacesContext context,
    MockUIComponentWrapper wrapper,
    Object value,
    String expectedValue
    )  throws ConverterException
  {
    Object conv = converter.getAsString(context, wrapper.getUIComponent(), value);
    assertEquals(conv, expectedValue);
    wrapper.getMock().verify();
  }


  /**
   * Test for equality or mismatch of converters
   * Converter's that do not  implement equals should override to check
   * for it equality.
   * @param thisConverter
   * @param otherConverter
   * @param isEqual - Identifies whether the comparison for equality of
   *        converters or mismatch of converters
   */
  protected void doTestEquals(
    Converter thisConverter,
    Converter otherConverter,
    boolean isEqual)
  {
    assertEquals(isEqual, thisConverter.equals(otherConverter));
    assertEquals(isEqual, (thisConverter.hashCode() == otherConverter.hashCode()));
  }

  /**
   * Test to check for Validators which implements the StateHolder interface
   * @param thisValidator  Source converter
   * @param otherValidator The converter in which the state will be restored to
   * @param context MockFaces context
   * @param component MockFaces Component
   */
  protected void doTestStateHolderSaveRestore(
    Converter thisConverter,
    Converter otherConverter,
    MockFacesContext context,
    MockUIComponentWrapper wrapper
    )
  {
    Object state = ((StateHolder)thisConverter).saveState(context);

    ((StateHolder)otherConverter).restoreState(context, state);
    // do all actions of save and restore
    doTestEquals(thisConverter, otherConverter, true);
    wrapper.getMock().verify();
  }


  /**
   * Compares two object relying on its equality.
   * If two objects are null returns true
   * @param o1
   * @param o2
   * @return
   */
  protected boolean equals(
    Object o1,
    Object o2
    )
  {
    return ( o1 == o2 || (o1 != null && o1.equals(o2)));
  }
}