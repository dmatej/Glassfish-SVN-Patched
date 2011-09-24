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
import javax.faces.convert.Converter;
import javax.faces.convert.DateTimeConverter;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockFacesContext;

public class JsfDateTimeConverterTest extends DateTimeConverterTestCase
{
  public JsfDateTimeConverterTest(String name)
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
    return new TestSuite(JsfDateTimeConverterTest.class);
  }
  
  @Override
  protected void doTestStateHolderSaveRestore(
    Converter conv1,
    Converter conv2,
    MockFacesContext context,
    MockUIComponentWrapper wrapper
    )
  {
    DateTimeConverter dtConv1, dtConv2;
    dtConv1 = (DateTimeConverter)conv1;
    dtConv2 = (DateTimeConverter)conv2;

    Object state = ((StateHolder)conv1).saveState(context);

    ((StateHolder)conv2).restoreState(context, state);

    boolean isEqual =  _isConvertersEqual(dtConv1, dtConv2);

    assertEquals(true, isEqual);
    wrapper.getMock().verify();
  }

  @Override
  protected void doTestEquals(
    Converter conv1,
    Converter conv2,
    boolean expectedOutCome
    )
  {
    DateTimeConverter dtConv1, dtConv2;
    dtConv1 = (DateTimeConverter)conv1;
    dtConv2 = (DateTimeConverter)conv2;
    boolean isEqual =  _isConvertersEqual(dtConv1, dtConv2);
    assertEquals(expectedOutCome, isEqual);
  }

  @Override
  protected DateTimeConverter getDateTimeConverter()
  {
    return new DateTimeConverter();
  }


  @Override
  protected void setSecondaryPattern(
    DateTimeConverter converter,
    String secondaryPattern
    )
  {
    // Do nothing there is nothing called secondary pattern in JSF world DTC
    return;
  }

  private boolean _equals(
    Object o1,
    Object o2
    )
  {
    return ( o1 == o2 || (o1 != null && o1.equals(o2)));
  }

  private boolean _isConvertersEqual(
    DateTimeConverter conv1,
    DateTimeConverter conv2
    )
  {
    return
      ( conv1.isTransient() == conv2.isTransient()
         && _equals(conv1.getDateStyle(), conv2.getDateStyle())
         && _equals(conv1.getLocale(), conv2.getLocale())
         && _equals(conv1.getPattern(), conv2.getPattern())
         && _equals(conv1.getTimeStyle(), conv2.getTimeStyle())
         && _equals(conv1.getTimeZone(), conv2.getTimeZone())
         && _equals(conv1.getType(), conv2.getType())
      );
  }

}