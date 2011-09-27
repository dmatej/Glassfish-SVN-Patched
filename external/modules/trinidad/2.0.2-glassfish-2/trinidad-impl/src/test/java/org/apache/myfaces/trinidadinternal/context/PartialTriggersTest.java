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
package org.apache.myfaces.trinidadinternal.context;

import java.util.Set;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.component.UIViewRoot;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXInput;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class PartialTriggersTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(PartialTriggersTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public PartialTriggersTest(
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
    RequestContext context = _createContext();
    try
    {
      UIXCommand button1 = new UIXCommand();
      button1.setId("commandButton1");
      UIXCommand button2 = new UIXCommand();
      button2.setId("commandButton2");
      UIXPanel rootPanel = new UIXPanel();
      rootPanel.getChildren().add(button1);
      rootPanel.getChildren().add(button2);
      
      String[] triggers = {"commandButton1"};
      // add partialTriggers on button2
      context.addPartialTriggerListeners(button2, triggers);
      // now make sure the partialTargets is what we expect
      // in this case, the partial target source is button1,
      // and the targets are button2.
      Set<UIComponent> set = context.getPartialTargets(button1);
      assertTrue(set.contains(button2));
    }
    finally
    {
      context.release();
    }
    
  }
    
  // Test sibling components where one is a table and one is a button
  @SuppressWarnings("unchecked")
  public void testSiblingWithTable()
  {
    RequestContext context = _createContext();
    try
    {
      UIXCommand button1 = new UIXCommand();
      button1.setId("commandButton1");
  
      UIXTable table1 = new UIXTable();
      table1.setId("table1");
      UIXPanel rootPanel = new UIXPanel();
      rootPanel.getChildren().add(button1);
      rootPanel.getChildren().add(table1);
      
      String[] triggers = {"::commandButton1"};
      // add partialTriggers on button2
      context.addPartialTriggerListeners(table1, triggers);
      // now make sure the partialTargets is what we expect
      Set<UIComponent> set = context.getPartialTargets(button1);
      assertTrue(set.contains(table1));
      
      // the 'old' way worked like this, test backward-compatibility
      String[] triggersOld = {"commandButton1"};
      // add partialTriggers on button2
      context.addPartialTriggerListeners(table1, triggersOld);
      // now make sure the partialTargets is what we expect
      set = context.getPartialTargets(button1);
      assertTrue(set.contains(table1));
      
    }
    finally
    {
      context.release();
    }
    
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
             id="inputText1"
             partialTriggers="::commandButton1"/>
           </tr:panelGroupLayout>
          </f:subview>
       </f:subview>
     */
    RequestContext context = _createContext();
    try
    {
      // set up component hierarchy
      UIViewRoot viewRoot = new UIViewRoot();
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
      UIXInput input = new UIXInput(); input.setId("input1");
      ncRoot.getChildren().add(nc);
      nc.getChildren().add(inputA);
      nc.getChildren().add(panel);
      panel.getChildren().add(input);
      
      // test input partialTriggers="::commandButton1"
      String[] triggers = {"::commandButton1"};
      context.addPartialTriggerListeners(input, triggers);
      Set<UIComponent> set = context.getPartialTargets(button1);
      assertTrue(set.contains(input));
      
      // the 'old' way worked like this, test backward-compatibility
      String[] triggersOld = {":::commandButton1"};
      context.addPartialTriggerListeners(input, triggersOld);
      set = context.getPartialTargets(button1);
      assertTrue(set.contains(input));
      
      String[] triggersA = {"::commandButton1"};
      context.addPartialTriggerListeners(inputA, triggersA);
      set = context.getPartialTargets(button1);
      assertTrue(set.contains(inputA));
      // test old way
      String[] triggersAOld = {":::commandButton1"};
      context.addPartialTriggerListeners(inputA, triggersAOld);
      set = context.getPartialTargets(button1);
      assertTrue(set.contains(inputA));
      
      
    }
    finally
    {
      context.release();
    }
    

  }


  private RequestContext _createContext()
  {
    // =-=AEW Would be better to create it with a mock context so we
    // can test parsing
    return (new RequestContextFactoryImpl()).createContext(null);
  }
}
