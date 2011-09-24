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
package org.apache.myfaces.trinidad.util;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXInput;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.UIXTable;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class FindRelativeComponentTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(FindRelativeComponentTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public FindRelativeComponentTest(
    String testName)
  {
    super(testName);
  }

  static private class TestNamingContainer extends UIXPanel
                                           implements NamingContainer
  {

  }

  
  // Test sibling components
  @SuppressWarnings("unchecked")
  public void testSiblingButtons()
  {

      UIXCommand button1 = new UIXCommand();
      button1.setId("commandButton1");
      UIXCommand button2 = new UIXCommand();
      button2.setId("commandButton2");
      UIXPanel rootPanel = new UIXPanel();
      rootPanel.getChildren().add(button1);
      rootPanel.getChildren().add(button2);
      
      UIComponent cmp = 
          ComponentUtils.findRelativeComponent(button2, "commandButton1");
      // old and new are the same
      assertEquals(cmp, button1);
      
      cmp = ComponentUtils.findRelativeComponent(button2, "::::::commandButton1");
      // old and new are the same
      assertEquals(cmp, button1);

    
  }
    
  // Test sibling components where one is a table and one is a button
  @SuppressWarnings("unchecked")
  public void testSiblingWithTable()
  {
      // panel
      //    table1
      //       tableChild
      //    button1 (table1 & button1 peers)
      UIXCommand button1 = new UIXCommand();
      button1.setId("commandButton1");
  
      UIXTable table1 = new UIXTable();
      table1.setId("table1");
      UIXPanel rootPanel = new UIXPanel();
      rootPanel.getChildren().add(button1);
      rootPanel.getChildren().add(table1);
      UIXPanel tableChild = new UIXPanel();
      tableChild.setId("tableChildId");
      table1.getChildren().add(tableChild);
      
      
      UIComponent cmp =
        ComponentUtils.findRelativeComponent(table1,"::commandButton1");
       // new & old (because  in old it starts from the viewRoot)
      assertEquals(button1, cmp);
      
      cmp = ComponentUtils.findRelativeComponent(table1, "commandButton1");
      // old & new are the same
      // uses findComponent rules if it doesn't start with multiple colons
      // - if this UIComponent is a NamingContainer it will serve as the basis.
      assertEquals(null, cmp);
      
    cmp = ComponentUtils.findRelativeComponent(button1, "table1");
    assertEquals(table1, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(button1, "tableChildId");
    assertEquals(null, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(button1, "table1:tableChildId");
    assertEquals(tableChild, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(table1, "tableChildId");
    assertEquals(tableChild, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(tableChild, "table1");
    assertEquals(table1, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(tableChild, ":commandButton1");
    assertEquals(button1, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(tableChild, ":::commandButton1");
    assertEquals(button1, cmp);
    
    cmp = ComponentUtils.findRelativeComponent(tableChild, "::commandButton1");   
    assertEquals(button1, cmp); // new way
    //assertEquals(null, cmp); // old way
    

  }
    


  @SuppressWarnings("unchecked")
  public void testRelativeSearch()
  {
    /*<f:subview id="ncRoot">
     *  <commandButton1>
     *  <commandButton2>
     *     <f:subview id="nc1">
     *     <tr:inputText id="inputA" pT="::commandButton1"/>
           <tr:panelGroupLayout>
             <tr:inputText 
             id="input1"
             partialTriggers="::commandButton1"/>
           </tr:panelGroupLayout>
          </f:subview>
       </f:subview>
     */

    // set up component hierarchy
    UIForm form = new UIForm();
    TestNamingContainer ncRoot = new TestNamingContainer(); ncRoot.setId("ncRoot");
    UIXCommand button1 = new UIXCommand();
    button1.setId("commandButton1");
    UIXCommand button2 = new UIXCommand();
    button2.setId("commandButton2");
    
    form.getChildren().add(ncRoot);
    ncRoot.getChildren().add(button1);
    ncRoot.getChildren().add(button2);
      
    TestNamingContainer nc = new TestNamingContainer(); nc.setId("nc1");
    UIXInput inputA = new UIXInput(); inputA.setId("inputA");
    UIXPanel panel = new UIXPanel(); panel.setId("panel1");
    UIXInput input1 = new UIXInput(); input1.setId("input1");  
    ncRoot.getChildren().add(nc);
    nc.getChildren().add(inputA);
    nc.getChildren().add(panel);
    panel.getChildren().add(input1);
      
    // input1's parent is panel. panel's parent is nc1 (::) goes there. ::: goes to ncRoot
    // in old way. New way pops OUT of nc1 with '::'.
    UIComponent cmp =
      ComponentUtils.findRelativeComponent(input1,":::commandButton1");
    assertEquals(button1, cmp); // old way
    // assertEquals(null, cmp); // new way (popped too far), so the code looks the old way
    
    cmp = ComponentUtils.findRelativeComponent(input1, "::::ncRoot:commandButton1");
    assertEquals(button1, cmp); // old way & new way
    
    cmp = ComponentUtils.findRelativeComponent(input1, ":::ncRoot:commandButton1");
    assertEquals(button1, cmp); // old way
    
    
    // inputA's parent is nc1. ::  will get you there. : will pop you out. /old way
    // :: will pop you out of nc1
    cmp = ComponentUtils.findRelativeComponent(inputA, ":::commandButton1");
    assertEquals(button1, cmp); // old way
    //assertEquals(null, cmp);   // new way
    
    cmp = ComponentUtils.findRelativeComponent(inputA, "::ncRoot:commandButton1");
    //assertEquals(null, cmp); // old way
    assertEquals(button1, cmp);   // new way    
 
  }


}


