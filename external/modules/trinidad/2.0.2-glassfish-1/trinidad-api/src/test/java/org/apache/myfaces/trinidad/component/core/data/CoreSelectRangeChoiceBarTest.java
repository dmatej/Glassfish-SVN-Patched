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
package org.apache.myfaces.trinidad.component.core.data;

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.RangeChangeListener;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests for CoreSelectRangeChoiceBar
 *
 */
public class CoreSelectRangeChoiceBarTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreSelectRangeChoiceBarTest.
   *
   * @param testName  the unit test name
   */
  public CoreSelectRangeChoiceBarTest(
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
    return new TestSuite(CoreSelectRangeChoiceBarTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    assertEquals(true, component.isRendered());
    assertEquals(25, component.getRows());
    assertEquals(0, component.getFirst());
    assertEquals(false, component.isShowAll());
    //assertEquals(-1, component.getMaximum());    
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "showAll", Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "immediate",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "rows",
                                new Integer(1), new Integer(5));
    doTestAttributeTransparency(component, "first",
                                new Integer(1), new Integer(5));                                 
  }


  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    doTestApplyRequestValues(component);

    component = new CoreSelectRangeChoiceBar();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    doTestUpdateModelValues(component);
  }


  private class MockRangeChangeListener implements RangeChangeListener
  {
    public List<RangeChangeEvent> list()
    {
      return _list;
    }

    public void processRangeChange(RangeChangeEvent event)
    {
      _list.add(event);
    }

    private List<RangeChangeEvent> _list = new ArrayList<RangeChangeEvent>();
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    MockRangeChangeListener listener = new MockRangeChangeListener();

    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    component.addRangeChangeListener(listener);
    RangeChangeEvent event = new RangeChangeEvent(component, 0, 9, 10, 19);

    doTestInvokeApplication(component, event);

    assertEquals(listener.list().size(), 1);
    assertEquals(listener.list().get(0), event);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreSelectRangeChoiceBar component = new CoreSelectRangeChoiceBar();
    doTestRenderResponse(component);
  }
  
  public static void main(String[] args)
  {
    TestRunner.run(CoreSelectRangeChoiceBarTest.class);

  }  
}
