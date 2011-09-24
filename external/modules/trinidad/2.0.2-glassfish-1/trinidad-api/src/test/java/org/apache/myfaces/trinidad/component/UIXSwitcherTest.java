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
package org.apache.myfaces.trinidad.component;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;
import org.jmock.Mock;


/**
 * Unit tests for UIXSwitcher.
 *
 */
public class UIXSwitcherTest extends UIComponentTestCase
{
  /**
   * Creates a new UIXSwitcherTest.
   *
   * @param testName  the unit test name
   */
  public UIXSwitcherTest(
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
    return new TestSuite(UIXSwitcherTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXSwitcher switcher = new UIXSwitcher();

    assertNull(switcher.getFacetName());
    assertNull(switcher.getDefaultFacet());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    UIXSwitcher component = new UIXSwitcher();

    doTestAttributeTransparency(component, "defaultFacet",
                                "foo", "notFoo");
    doTestAttributeTransparency(component, "facetName",
                                "bar", "notBar");
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    UIXSwitcher switcher = new UIXSwitcher();
    switcher.setFacetName("foo");
    doTestApplyRequestValues(switcher);

    switcher = new UIXSwitcher();
    switcher.setFacetName("foo");
    switcher.setRendered(false);
    doTestApplyRequestValues(switcher);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void doTestApplyRequestValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {

    Mock mockUIComponent = createMockUIComponent();
    UIComponent fooChild = (UIComponent) mockUIComponent.proxy();
    
    // JavaServer Faces 1.0 Specification, section 2.2.2
    // During the apply-request-values phase,
    // only the processDecodes lifecycle method may be called.
    if (component.isRendered()){
      mockUIComponent.stubs().method("processDecodes").with(eq(facesContext));
    }

    // These children will never get called
    Mock mockBarChild = createMockUIComponent();
    UIComponent barChild = (UIComponent) mockBarChild.proxy();
    
    Mock mockOrdinaryChild = createMockUIComponent();
    UIComponent ordinaryChild = (UIComponent) mockOrdinaryChild.proxy();

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    root.getChildren().add(component);
    component.getFacets().put("foo", fooChild);
    component.getFacets().put("bar", barChild);
    component.getChildren().add(ordinaryChild);
    root.processDecodes(context);

    mockUIComponent.verify();
    mockBarChild.verify();
    mockOrdinaryChild.verify();
  }


  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    UIXSwitcher switcher = new UIXSwitcher();
    switcher.setFacetName("foo");
    doTestProcessValidations(switcher);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void doTestProcessValidations(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    
    Mock mockUIComponent = createMockUIComponent();//mock(UIComponent.class);
    UIComponent fooChild = (UIComponent) mockUIComponent.proxy();
    
    // JavaServer Faces 1.0 Specification, section 2.2.2
    // During the apply-request-values phase,
    // only the processDecodes lifecycle method may be called.
    mockUIComponent.expects(once()).method("processValidators");

    // These children will never get called
    Mock mockBarChild = createMockUIComponent();
    UIComponent barChild = (UIComponent) mockBarChild.proxy();
    
    Mock mockOrdinaryChild = createMockUIComponent();
    UIComponent ordinaryChild = (UIComponent) mockOrdinaryChild.proxy(); 

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    root.getChildren().add(component);
    component.getFacets().put("foo", fooChild);
    component.getFacets().put("bar", barChild);
    component.getChildren().add(ordinaryChild);
    root.processValidators(context);

    mockUIComponent.verify();
    mockBarChild.verify();
    mockOrdinaryChild.verify();
  }



  /**
   * Tests the updatem-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    UIXSwitcher switcher = new UIXSwitcher();
    switcher.setFacetName("foo");
    doTestUpdateModelValues(switcher);
  }


  @SuppressWarnings("unchecked")
  @Override
  protected void doTestUpdateModelValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    Mock mockUIComponent = createMockUIComponent();
    UIComponent fooChild = (UIComponent) mockUIComponent.proxy();
    // JavaServer Faces 1.0 Specification, section 2.2.2
    // During the apply-request-values phase,
    // only the processDecodes lifecycle method may be called.
    mockUIComponent.expects(once()).method("processUpdates");

    // These children will never get called
    Mock mockBarChild = createMockUIComponent();
    UIComponent barChild = (UIComponent) mockBarChild.proxy();
    
    Mock mockOrdinaryChild = createMockUIComponent();
    UIComponent ordinaryChild = (UIComponent) mockOrdinaryChild.proxy();

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    root.getChildren().add(component);
    component.getFacets().put("foo", fooChild);
    component.getFacets().put("bar", barChild);
    component.getChildren().add(ordinaryChild);
    root.processUpdates(context);

    mockUIComponent.verify();
    mockBarChild.verify();
    mockOrdinaryChild.verify();
  }

  public static void main(String[] args)
  {
    TestRunner.run(UIXSwitcherTest.class);
  }

  @Override
  protected boolean isRendererUsed()
  {
    return false;
  }
}
