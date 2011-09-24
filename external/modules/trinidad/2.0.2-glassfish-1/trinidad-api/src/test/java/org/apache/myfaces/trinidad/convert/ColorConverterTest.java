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

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.convert.ColorConverter;
import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Unit tests for ColorConverter
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/convert/ColorConverterTest.java#1 $) $Date: 16-aug-2005.15:12:23 $
 */
public class ColorConverterTest extends ConverterTestCase
{
  public ColorConverterTest(String testName)
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
    return new TestSuite(ColorConverterTest.class);
  }
  
  /**
   * Tests that null returns immediately.
   *
   * @throws ConverterException  when test fails
   */
  public void testNull() throws ConverterException
  {
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    ColorConverter converter  = new ColorConverter();

    doTestNull(facesContext, wrapper, converter);
  }

  /**
   * Test when context is set to null
   */
  public void testNullContext()
  {
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    ColorConverter converter  = new ColorConverter();

    doTestNullContext(wrapper, converter);
  }

  public void testNullComponent()
  {
    ColorConverter converter  = new ColorConverter();

    doTestNullComponent(facesContext, converter);
  }

  public void testEmptyValueConversion()
  {
    super.doTestBlankValue(new ColorConverter());
  }

  /**
   * Test for equality of converters
   */
  public void testEquals()
  {
    ColorConverter converter  = new ColorConverter();
    ColorConverter otherConverter = new ColorConverter();
    doTestEquals(converter, otherConverter, true);

    String[] patterns = {"rrr,ggg,bbb", "rrr-ggg-bbb"};
    String[] otherPatterns = {"rrr,ggg,bbb", "rrr-ggg-bbb"};
    converter.setPatterns(patterns);
    converter.setMessageDetailConvert("Test message detail");
    otherConverter.setMessageDetailConvert("Test message detail");
    otherConverter.setPatterns(otherPatterns);
    doTestEquals(converter, otherConverter, true);

    String[] newPattern = {"#RRGGBB", "RR.GG.BB"};
    otherConverter.setPatterns(newPattern);
    doTestEquals(converter, otherConverter, false);

    // check by modifiying the tranisent to be differnt
    // patterns are same
    otherConverter.setPatterns(otherPatterns);
    otherConverter.setTransient(true);
    doTestEquals(converter, otherConverter, false);


    // transient same, patterns same, but allowsTransparent diff
    otherConverter.setTransient(false);
    otherConverter.setTransparentAllowed(true);
    doTestEquals(converter, otherConverter, false);
  }

  public void testDefaultColorPatternWorks()
  {
    ColorConverter converter  = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);
    String value = "#FFFFFF";
    Color expectedColor = new Color(255,255,255);
    doTestGetAsObject(converter, facesContext, wrapper, value, expectedColor);
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testStateHolderSaveRestore()
  {
    ColorConverter converter = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    String[] patterns = {"#RRGGBB","RR.GG.BB"};
    converter.setPatterns(patterns);
    converter.setMessageDetailConvert("Works fine");
    ColorConverter restoreConverter = new  ColorConverter();

    doTestStateHolderSaveRestore(converter, restoreConverter,
        facesContext, wrapper);
    mock.verify();
  }
  /**
   * Test ColorConverte's getAsObject(FacesContext, UIComponent, String) method
   * works fine.
   */
  public void testGetAsObjectConversion()
  {
    ColorConverter converter = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    String[] patterns = { "#RRGGBB",
                          "RR.GG.BB",
                          "RRGGBB",
                          "RR GG BB",
                          "rrr-ggg-bbb",
                          "rrr ggg bbb",
                          "r-g-b",
                          };

    String values[]   = { "#FF0204",
                          "FF0206",
                          "FF FF FF",
                          "0-0-0",
                          "105 105 105",
                        };

   Color[] matchColors = {  new Color(255,2,4),
                            new Color(255,2,6),
                            new Color(255,255,255),
                            new Color(0,0,0),
                            new Color(105,105,105)
                         };

    converter.setPatterns(patterns);
    for (int i = 0; i < values.length; i++)
    {
      doTestGetAsObject(converter, facesContext, wrapper, values[i], matchColors[i]);
    }
    mock.verify();
  }

  /**
   * Test Color conveters getAsString(FacesContext, UIComponent, Object) method
   * works fine
   */
  public void testGetAsString()
  {
    ColorConverter converter  = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);

    Color[] colors = {  new Color(255,2,4),
                        new Color(255,2,6),
                        new Color(255,255,255),
                        new Color(0,0,0),
                        new Color(105,105,105),
                      };

    List<String[]> patternsHoloder = new ArrayList<String[]>();
    patternsHoloder.add(new String[]{"#RRGGBB", "RRGGBB"});
    patternsHoloder.add(new String[]{"RR.GG.BB", "#RRGGBB" });
    patternsHoloder.add(new String[]{"RRGGBB", "r-g-b"});
    patternsHoloder.add(new String[]{"RR GG BB", "rrr ggg bbb"});
    patternsHoloder.add(new String[]{"rrr-ggg-bbb", "rrr ggg bbb" });

    String matchValues[]   = {  "#FF0204",
                                "FF.02.06",
                                "FFFFFF",
                                "00 00 00",
                                "105-105-105",
                             };

    for (int i = 0; i < patternsHoloder.size(); i++)
    {
      String[] patterns = patternsHoloder.get(i);
      converter.setPatterns(patterns);
      doTestGetAsString(converter, facesContext, wrapper,
                                    colors[i], matchValues[i] );
    }
    mock.verify();
  }

  /**
   * Test that try to set null value for patterns throw IllegalArgumentException
   */
  public void testNullValueForPatterns()
  {
    ColorConverter converter = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    
    try
    {
      converter.setPatterns(null);
      fail("Expected IllegalArgumentException for null value of patterns");
    }
    catch (IllegalArgumentException ex)
    {
      // expected fine
    }
    mock.verify();
  }

  public void testGetAsObjectIllegalValue()
  {
    ColorConverter converter = new ColorConverter();
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();
    mock.stubs().method("getId").will(returnValue("test"));
    //component.setupGetId("test");
    try
    {
      converter.getAsString(facesContext, component, new Integer(1));
      fail("Expected a converter exception");
    } catch (IllegalArgumentException ex)
    {
      // expected
    }
    mock.verify();
  }
}