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

import java.util.Locale;

import javax.faces.component.StateHolder;
import javax.faces.convert.Converter;
import javax.faces.convert.NumberConverter;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockFacesContext;

/**
 * Test JSF NumberConverter
 * @version $Name: $ ($version: $) $Date: 16-aug-2005.15:12:23 $
 */
public class JsfNumberConverterTest extends NumberConverterTestCase
{
  public JsfNumberConverterTest(String name)
  {
    super(name);
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
    return new TestSuite(JsfNumberConverterTest.class);
  }
  
  @Override
  protected NumberConverter getNumberConverter()
  {
    return new NumberConverter();
  }

  @Override
  protected void doTestStateHolderSaveRestore(
    Converter conv1,
    Converter conv2,
    MockFacesContext context,
    MockUIComponentWrapper wrapper
    )
  {
    Object state = ((StateHolder)conv1).saveState(context);

    ((StateHolder)conv2).restoreState(context, state);

    boolean isEqual =  _isConvertersEqual((NumberConverter)conv1,
                                           (NumberConverter)conv2);

    assertEquals(true, isEqual);
    wrapper.getMock().verify();
  }

  // This is a bad equals comparison. But this is max we can do.
  private boolean _isConvertersEqual(
    NumberConverter conv1,
    NumberConverter conv2)
  {
    return
    (
      conv1.getMaxFractionDigits() == conv2.getMaxFractionDigits()  &&
      conv1.getMaxIntegerDigits()  == conv2.getMaxIntegerDigits()   &&
      conv1.getMinFractionDigits() ==  conv2.getMinFractionDigits() &&
      conv1.getMinIntegerDigits()  ==  conv2.getMinIntegerDigits()  &&
      conv1.isTransient() == conv2.isTransient() &&
      conv1.isGroupingUsed() ==  conv2.isGroupingUsed() &&
      conv1.isIntegerOnly()  == conv2.isIntegerOnly()   &&
      equals(conv1.getType(), conv2.getType()) &&
      equals(conv1.getLocale(), conv2.getLocale()) &&
      equals(conv1.getCurrencyCode(), conv2.getCurrencyCode()) &&
      equals(conv1.getCurrencySymbol(), conv2.getCurrencySymbol()) &&
      equals(conv1.getPattern(), conv2.getPattern())
    );
  }

  @Override
  protected void doTestEquals(
    Converter conv1,
    Converter conv2,
    boolean expectedOutCome
    )
  {
    NumberConverter nConv1, nConv2;
    nConv1 = (NumberConverter) conv1;
    nConv2 = (NumberConverter) conv2;
    boolean isEqual =  _isConvertersEqual(nConv1, nConv2);
    assertEquals(expectedOutCome, isEqual);
  }

  @Override
   protected void doTestStrictNess(
    MockFacesContext context,
    MockUIComponentWrapper wrapper,
    Locale locale,
    String inputValue)
  {
     NumberConverter converter = getNumberConverter();
     converter.setLocale(locale);
     Object obj = converter.getAsObject(context, wrapper.getUIComponent(), inputValue);
     // JSF Converter is lenient - so it will get parsed to a valid object
     assertEquals(true, (obj != null));
  };

}