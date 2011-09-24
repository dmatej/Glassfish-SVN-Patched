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

import java.text.DecimalFormatSymbols;

import java.util.Locale;

import javax.faces.convert.ConverterException;
import javax.faces.convert.NumberConverter;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;

import org.apache.myfaces.trinidad.context.MockRequestContext;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.myfaces.trinidadbuild.test.MockFacesContext12;
import org.apache.shale.test.mock.MockFacesContext;
import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test Trinidad NumberConverter
 * @version $Name: $ ($version: $) $Date: 16-aug-2005.15:12:23 $
 */
public class TrinidadNumberConverterTest extends NumberConverterTestCase
{
  public TrinidadNumberConverterTest(String name)
  {
    super(name);
  }

  @Override
  protected NumberConverter getNumberConverter()
  {
    return new org.apache.myfaces.trinidad.convert.NumberConverter();
  }

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    _mafct = new MockRequestContext();
    _mafct.setDecimalSeparator('.');
    _mafct.setNumberGroupingSeparator(',');
    _mafct.setCurrencyCode(null);
  }

  @Override
  protected void tearDown() throws Exception
  {
    
    // RequestContext uses a thread local variable to hold itself and has a
    // check in it. So you need to release, since all instances for tests
    // are created on the same thread by Junit.
    _mafct.release();
    _mafct = null;
    super.tearDown();
  }

  public static Test suite()
  {
    return new TestSuite(TrinidadNumberConverterTest.class);
  }
  
  @Override
  public void testCurrencyCodeIsHonoured()
  {
     DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
    _mafct.setDecimalSeparator(symbols.getMonetaryDecimalSeparator());
    _mafct.setNumberGroupingSeparator(symbols.getGroupingSeparator());
    super.testCurrencyCodeIsHonoured();
  }

//  @Override
//  public void testFranceLocale()
//  {
//    DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.FRANCE);
//    _mafct.setDecimalSeparator(symbols.getMonetaryDecimalSeparator());
//    _mafct.setNumberGroupingSeparator(symbols.getGroupingSeparator());
//  
//    super.testFranceLocale();
//  }
  

  public void testValueSetInRequestContextIsHonoured()
  {
    //ugly ?
    _mafct.release();
    _mafct = null;
    _mafct = new MockRequestContext();
    _mafct.setDecimalSeparator('*');
    _mafct.setNumberGroupingSeparator('!');
    _mafct.setCurrencyCode(null);
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();

    NumberConverter conv = getNumberConverter();

    conv.setLocale(Locale.US);
    Number inputValue =  new Double(8989.789);
    String out = conv.getAsString(facesContext, component, inputValue);
    assertEquals("8!989*789", out);

    mock.verify();
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
     context.getViewRoot().setLocale(locale);
     try
     {
       // ADF Converter is not lenient.
       converter.getAsObject(context, wrapper.getUIComponent(), inputValue);
       fail("Expected converter exception");
     }
     catch (ConverterException ce)
     {
       ; // We expected a exception to occur
     }
  }

  public void testCustomMessageIsSet()
  {
    String[] failingValues = {"222.22.2", "3,",       "23.3.3",   "10e.04"};
    String[] types         = {"number",   "percent",  "currency", "pattern"};
    String[] customMessage = {"number", "percent",    "currency", "pattern"};

    for (int i = 0; i < failingValues.length ; i++)
    {
      MockFacesContext12 context  = new MockFacesContext12(externalContext,
                                                           lifecycle,
                                                           application);

      try
      {
        Mock mock = buildMockUIComponent(3);
        UIComponent component = (UIComponent) mock.proxy();
        MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
        
        
        org.apache.myfaces.trinidad.convert.NumberConverter converter =
          new org.apache.myfaces.trinidad.convert.NumberConverter();
        
        UIViewRoot root = facesContext.getViewRoot();
        root.setLocale(Locale.US);

        for (int j = 0; j < 3; j++)
        {
          context.setViewRoot(root);
        }
        
        try
        {
          // ADF Converter is not lenient.
          converter.setMessageDetailConvertNumber(customMessage[0]);
          converter.setMessageDetailConvertPercent(customMessage[1]);
          converter.setMessageDetailConvertCurrency(customMessage[2]);
          converter.setMessageDetailConvertPattern(customMessage[3]);
          
          if ("pattern".equals(types[i]))
            converter.setPattern("##.000");
          else
            converter.setType(types[i]);

          Object obj = converter.getAsObject(context, component, failingValues[i]);
          fail("Expected converter exception");
        }
        catch (ConverterException ce)
        {
          // We expected a exception to occur
          assertEquals(ce.getFacesMessage().getDetail(), customMessage[i]);
        }
      }
      finally
      {
        context.release();
      }
    }
  }

  private MockRequestContext _mafct;

  //checkForSettingsInRequestContext - dec sep, currencyCode, NumGrpSptr
}
