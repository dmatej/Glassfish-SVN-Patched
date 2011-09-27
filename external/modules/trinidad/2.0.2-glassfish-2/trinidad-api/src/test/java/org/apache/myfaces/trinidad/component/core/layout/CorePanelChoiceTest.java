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
package org.apache.myfaces.trinidad.component.core.layout;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CorePanelChoice
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/component/core/layout/CoreShowOneChoiceTest.java#1 $) $Date: 16-aug-2005.15:12:21 $
 */

public class CorePanelChoiceTest extends UIComponentTestCase
{
  /**
   * Creates a new CorePanelChoice.
   *
   * @param testName  the unit test name
   */
   public CorePanelChoiceTest(String testName)
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
     return new TestSuite(CorePanelChoiceTest.class);
   }

  /**
   * Tests the Initial values for the component attributes.
   */
   public void testInitialAttributeValues()
   {
     CorePanelChoice component = new CorePanelChoice();
     assertEquals(true, component.isRendered());
     assertNull(component.getLabel());
     assertEquals("start",component.getPosition());
     assertEquals("center", component.getAlignment());
   }

  /**
   * Tests the values set for the component attributes.
   */
   public void testgetAttributeValues()
   {
     CorePanelChoice component = new CorePanelChoice();
     assertEquals("org.apache.myfaces.trinidad.ShowOne",component.getFamily());
   }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
   public void testAttributeTransparency()
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestAttributeTransparency(component, "rendered",
                                    Boolean.TRUE, Boolean.FALSE);
     doTestAttributeTransparency(component, "label", "foo", "bar");
     doTestAttributeTransparency(component, "position", "start", "top");
     doTestAttributeTransparency(component, "alignment", "center", "bottom");
   }

  /**
   * Tests the Apply Request Values Phase
   */
   public void testApplyRequestValues()
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestApplyRequestValues(component);
     component= new CorePanelChoice();
     component.setRendered(false);
     doTestApplyRequestValues(component);
   }

  /**
   * Tests the process-validations lifecycle phase.
   */
   public void testProcessValidations()
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestProcessValidations(component);
   }

  /**
   * Tests the update-model-values lifecycle phase.
   */
   public void testUpdateModelValues()
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestUpdateModelValues(component);
   }

  /**
   * Tests the invoke-application lifecycle phase.
   */
   public void testInvokeApplication()
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestInvokeApplication(component, null);
   }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
   public void testRenderResponse() throws IOException
   {
     CorePanelChoice component= new CorePanelChoice();
     doTestRenderResponse(component);
   }

  }

