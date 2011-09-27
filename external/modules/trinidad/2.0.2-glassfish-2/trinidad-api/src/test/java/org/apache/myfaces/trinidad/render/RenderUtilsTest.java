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
package org.apache.myfaces.trinidad.render;


import javax.faces.component.NamingContainer;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIXForm;
import org.apache.myfaces.trinidad.component.UIXInput;
import org.apache.myfaces.trinidad.component.UIXPanel;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class RenderUtilsTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(RenderUtilsTest.class);
  }

  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public RenderUtilsTest(
    String testName)
  {
    super(testName);
  }

  static private class TestNamingContainer extends UIXPanel
                                           implements NamingContainer
  {
    @Override
    public String getClientId(FacesContext context)
    {
      // NOTE - client ids cannot be cached because the generated
      // value has to be dynamically calculated in some cases (UIData)

      String clientId = getId() + "_Client";
      return clientId;
    }
    
    public String getContainerClientId(FacesContext p1)
    {
      return getId();
    }
    
    protected Renderer getRenderer(FacesContext context)
    {
      return null;
    }
  }
  
  static private class TestUIXPanel extends UIXPanel
  {

    
    protected Renderer getRenderer(FacesContext context)
    {
      return null;
    }
  }


  // Test sibling components where one is a table and one is a button
  @SuppressWarnings("unchecked")
  public void testButtonAndNamingContainerSiblings()
  {
    FacesContext context = FacesContext.getCurrentInstance();

      // rootPanel
      //     button1
      //     table1
      TestUIXPanel button1 = new TestUIXPanel();
      button1.setId("commandButton1");
      TestNamingContainer table1 = new TestNamingContainer();
      table1.setId("table1");
      TestUIXPanel rootPanel = new TestUIXPanel();
      rootPanel.setId("rootPanel");
      rootPanel.getChildren().add(button1);
      rootPanel.getChildren().add(table1);
      TestUIXPanel tableChild = new TestUIXPanel();
      tableChild.setId("tableChildId");
      table1.getChildren().add(tableChild);
    
    String relativeId =
      RenderUtils.getRelativeId(context, button1, "table1");
    assertEquals("table1", relativeId);
    
    relativeId =
      RenderUtils.getRelativeId(context, button1, ":table1");
    assertEquals("table1", relativeId);
    
    // new way would find nothing, so we'd have to get something logical
    relativeId =
      RenderUtils.getRelativeId(context, table1, "someRandomId");
    assertEquals("table1_Client:someRandomId", relativeId);
    
    relativeId =
      RenderUtils.getRelativeId(context, table1, ":commandButton1");
    assertEquals("commandButton1", relativeId);

    // to get to the commandButton from the table, you need to pop out of the
    // table
    relativeId =
      RenderUtils.getRelativeId(context, table1, "::commandButton1");
    assertEquals("commandButton1", relativeId);
    
    // backward compatibility test -- this was the old syntax for siblings to the table.
    // this should be found by looking at the nc's parent from findRelativeComponent
    relativeId =
      RenderUtils.getRelativeId(context, table1, "commandButton1");
    assertEquals("commandButton1", relativeId);
       
    // backward compatibility test -- this was the old syntax for children to the table.
    relativeId =
      RenderUtils.getRelativeId(context, table1, "table1:tableChildId");
    assertEquals("table1:tableChildId", relativeId);
     // this is the new syntax for children to the table
    relativeId =
      RenderUtils.getRelativeId(context, table1, "tableChildId");
    assertEquals("table1:tableChildId", relativeId);

  }




  @SuppressWarnings("unchecked")
  public void testRelativeSearch()
  {
    FacesContext context = FacesContext.getCurrentInstance();


      // set up component hierarchy
      UIXForm form = new UIXForm(); form.setId("formId");
      TestNamingContainer ncRoot = new TestNamingContainer(); ncRoot.setId("ncRoot");
      TestUIXPanel button1 = new TestUIXPanel();
      button1.setId("button1");
      TestUIXPanel button2 = new TestUIXPanel();
      button2.setId("button2");
      TestUIXPanel rootButton = new TestUIXPanel();
      rootButton.setId("rootButton");

      form.getChildren().add(ncRoot);
      form.getChildren().add(rootButton);
      ncRoot.getChildren().add(button1);
      ncRoot.getChildren().add(button2);

      TestNamingContainer nc1 = new TestNamingContainer();
      nc1.setId("nc1");

          
      UIXInput inputA = new UIXInput(); inputA.setId("inputA");
      UIXPanel panel1 = new UIXPanel(); panel1.setId("panel1");
      UIXInput input1 = new UIXInput(); input1.setId("input1");
      ncRoot.getChildren().add(nc1);
      nc1.getChildren().add(inputA);
      nc1.getChildren().add(panel1);
      panel1.getChildren().add(input1);
      
    /*<f:subview id="ncRoot">
     *  <commandButton1>
     *  <commandButton2>
     *   <f:subview id="nc1">
     *     <tr:inputText id="inputA" pT="::commandButton1"/>
           <tr:panelGroupLayout>
             <tr:inputText
             id="input1"
             for="::commandButton1"/>
           </tr:panelGroupLayout>
          </f:subview>
       </f:subview>
       rootButton
     */
      
    String relativeId =
      RenderUtils.getRelativeId(context, input1, "::button1");
    // new way should pop OUT of ONE naming container and will find it
    assertEquals("ncRoot:button1", relativeId);

    
    relativeId =
      RenderUtils.getRelativeId(context, input1, ":::button1");
    // new way should pop OUT of TWO naming containers and will find not find it
    // since it is in ncRoot and the base is now the view root.
    // so it goes to the old findRelativeComponent, and this will find it.
    assertEquals("ncRoot:button1", relativeId);


    relativeId =
      RenderUtils.getRelativeId(context, input1, "randomPeer");
    // randomPeer doesn't exist, so new way won't find it.
    // uses code that doesn't need to find the component to return this:
    assertEquals("nc1_Client:randomPeer", relativeId);
    
    relativeId =
      RenderUtils.getRelativeId(context, input1, "::randomPeer");
    // randomPeer doesn't exist, so new way won't find it.
    // uses code that doesn't need to find the component to return this:
    assertEquals("ncRoot_Client:randomPeer", relativeId);
 
    // rootButton is child of form and sibling to ncRoot. It's 2 nc up from input1
    relativeId =
      RenderUtils.getRelativeId(context, input1, ":::rootButton");
    // new way should pop OUT of both NC with ::: and will find it
    assertEquals("rootButton", relativeId);

 
    // rootButton is child of form and sibling to ncRoot. It's 2 nc up from input1
    relativeId =
      RenderUtils.getRelativeId(context, input1, "::rootButton");
    // new way should pop OUT of one NC with ::, so it can't find it
    // the 'old' findRelativeComponent can't find it either.
    // so it returns what the old getRelativeId would have returned
    // old way goes up to the current naming container nc1
    // old 'new' way will return nc1_Root:rootButton
    assertEquals("ncRoot_Client:rootButton", relativeId);

    
    // rootButton is child of form and sibling to ncRoot. It's 2 nc up from input1
    relativeId =
      RenderUtils.getRelativeId(context, input1, "::::rootButton");
    // new way should pop OUT of ALL NCs and will find it.
    assertEquals("rootButton", relativeId);
    

    
    relativeId =
      RenderUtils.getRelativeId(context, input1, "::::button1");
    // new way should return this
    assertEquals("button1", relativeId);
    
    relativeId =
      RenderUtils.getRelativeId(context, input1, ":::::button1");
    assertEquals("button1", relativeId);
  }



}
