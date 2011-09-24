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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.faces.component.UIData;
import javax.faces.component.UIForm;
import javax.faces.component.UIInput;
import javax.faces.component.UINamingContainer;
import javax.faces.component.UIOutput;
import javax.faces.component.UIPanel;
import javax.faces.component.UIViewRoot;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class ComponentReferenceTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(ComponentReferenceTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public ComponentReferenceTest(
    String testName)
  {
    super(testName);
  }
  
  public void testVerySimpleFinder()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIInput input1 = new UIInput(); input1.setId("input1");

    // build the Tree...
    root.getChildren().add(input1);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input1);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();

    assertEquals(input1, referencedComp);
  }

  @SuppressWarnings("unchecked")
  public void testFailoverOnCustomFacet() throws IOException, ClassNotFoundException
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIPanel panel = new UIPanel(); panel.setId("panel");
    UIInput input = new UIInput(); input.setId("input1");

    // build the Tree...
    panel.getFacets().put("fancyFacet", input);
    nc3.getChildren().add(panel);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();
    assertEquals(input, referencedComp);
    
    // find it again!
    assertEquals(input, uiRef.getComponent());

    // fake the failover
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(baos);

    oos.writeObject(uiRef);

    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
    ObjectInputStream ois = new ObjectInputStream(bais);

    uiRef = (ComponentReference<UIInput>) ois.readObject();

    referencedComp = uiRef.getComponent();
    assertEquals(input, referencedComp);

  }

  
  @SuppressWarnings("unchecked")
  public void testFailover() throws IOException, ClassNotFoundException
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIInput input1 = new UIInput(); input1.setId("input1");

    // build the Tree...
    root.getChildren().add(input1);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input1);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();
    assertEquals(input1, referencedComp);
    
    // fake the failover
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(baos);

    oos.writeObject(uiRef);
    oos.flush();

    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
    ObjectInputStream ois = new ObjectInputStream(bais);

    uiRef = (ComponentReference<UIInput>) ois.readObject();

    referencedComp = uiRef.getComponent();
    assertEquals(input1, referencedComp);
  }

  public void testEmptyViewRootOnGetComponent()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");

    // build the tree
    root.getChildren().add(nc1);
    nc1.getChildren().add(nc2);
    nc2.getChildren().add(nc3);

    // Get the ComponentReference util
    ComponentReference<UINamingContainer> uiRef = ComponentReference.newUIComponentReference(nc3);

    // find the component...
    UINamingContainer referencedComp = uiRef.getComponent();

    assertEquals(nc3, referencedComp);

    // clear the ViewRoot
    root.getChildren().clear();

    // now, the getComponent should return NULL
    assertNull(uiRef.getComponent());
  }

  public void testNoComponentWithoutAnId()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIInput input1 = new UIInput();

    // build the Tree...
    root.getChildren().add(input1);

    ComponentReference ref = ComponentReference.newUIComponentReference(input1);

    // Get the ComponentReference util
    try
    {
      ref.getComponent();
      
      // find the component...
      fail("IllegalStateException expected");
    }
    catch (IllegalStateException e)
    {
      // suppress it - this is as expected
    }
  }

  public void testViewRoot()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");

    // Get the ComponentReference util
    ComponentReference<UIViewRoot> uiRef = ComponentReference.newUIComponentReference(root);

    // find the component...
    UIViewRoot referencedComp = uiRef.getComponent();
    assertEquals(root, referencedComp);
  }

  public void testNamingContainerViewRoot()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");

    // build the tree
    root.getChildren().add(nc1);
    nc1.getChildren().add(nc2);
    nc2.getChildren().add(nc3);

    // Get the ComponentReference util
    ComponentReference<UINamingContainer> uiRef = ComponentReference.newUIComponentReference(nc3);

    // find the component...
    UINamingContainer referencedComp = uiRef.getComponent();

    assertEquals(nc3, referencedComp);
  }

  public void testMovingInsideNamingContainer()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");

    // build the tree
    root.getChildren().add(nc1);
    nc1.getChildren().add(nc2);
    nc2.getChildren().add(nc3);

    // Get the ComponentReference util
    ComponentReference<UINamingContainer> uiRef = ComponentReference.newUIComponentReference(nc3);

    // find the component...
    UINamingContainer referencedComp = uiRef.getComponent();
    assertEquals(nc3, referencedComp);

    // let's move the NC3 component one level up;
    nc2.getChildren().remove(nc3);
    nc1.getChildren().add(nc3);

    // and we can not find the component...
    referencedComp = uiRef.getComponent();
    assertNull(referencedComp);
  }

  public void testDeferredMovingInsideNamingContainer()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");

    // almost build the tree
    nc1.getChildren().add(nc2);
    nc2.getChildren().add(nc3);

    // Get the ComponentReference util, this will be a deferred component reference since the
    // component wasn't attached
    ComponentReference<UINamingContainer> uiRef = ComponentReference.newUIComponentReference(nc3);

    // now finish building the component tree
    root.getChildren().add(nc1);

    // find the component...
    UINamingContainer referencedComp = uiRef.getComponent();
    assertEquals(nc3, referencedComp);

    // let's move the NC3 component one level up;
    nc2.getChildren().remove(nc3);
    nc1.getChildren().add(nc3);

    // and we can not find the component...
    referencedComp = uiRef.getComponent();
    assertNull(referencedComp);
  }

  public void testComponentNotInTree()
  {
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");

    // build the tree
    nc1.getChildren().add(nc2);
    nc2.getChildren().add(nc3);

    // Get the ComponentReference util
    ComponentReference ref = ComponentReference.newUIComponentReference(nc3);

    try
    {
      // find the component...
      ref.getComponent();
      
      fail("IllegalStateException expected");
    }
    catch (IllegalStateException e)
    {
      // suppress it - this is as expected
    }
  }

  public void testFindInPanelComponents()
  {
    UIViewRoot root = facesContext.getViewRoot(); root.setId("root");
    UIPanel panel1 = new UIPanel(); panel1.setId("panel1");
    UIPanel panel2 = new UIPanel(); panel2.setId("panel2");
    UIPanel panel3 = new UIPanel(); panel3.setId("panel3");
    UIOutput txt = new UIOutput(); txt.setId("txt");

    // build the Tree...
    root.getChildren().add(panel1);
    panel1.getChildren().add(panel2);
    panel2.getChildren().add(panel3);
    panel3.getChildren().add(txt);

    // Get the ComponentReference util
    ComponentReference<UIOutput> uiRef = ComponentReference.newUIComponentReference(txt);

    // find the component...
    UIOutput referencedComp = uiRef.getComponent();

    assertEquals(txt, referencedComp);
  }

  public void testCustomFacet()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIPanel panel = new UIPanel(); panel.setId("panel");
    UIInput input = new UIInput(); input.setId("input1");

    // build the Tree...
    panel.getFacets().put("fancyFacet", input);
    nc3.getChildren().add(panel);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();
    assertEquals(input, referencedComp);
    
    // find it again!
    assertEquals(input, uiRef.getComponent());
  }

  public void testMoreFacets()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIPanel panel = new UIPanel(); panel.setId("panel");
    UIInput input = new UIInput(); input.setId("input1");

    // build the Tree...
    panel.getFacets().put("f1", new UIOutput());
    panel.getFacets().put("f2", new UIOutput());
    panel.getFacets().put("f3", new UIOutput());
    panel.getFacets().put("f4", new UIOutput());
    panel.getFacets().put("f5", new UIOutput());

    // add the important facet
    panel.getFacets().put("f0", input);

    nc3.getChildren().add(panel);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();
    assertEquals(input, referencedComp);
    
    // find it again!
    assertEquals(input, uiRef.getComponent());
  }

  public void testCustomFacetWithFind()
  {
    UIViewRoot root = new UIViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIPanel panel = new UIPanel(); panel.setId("panel");
    UIInput input = new UIInput(); input.setId("input1");

    // build the Tree...
    panel.getFacets().put("fancyFacet", input);
    panel.getChildren().add(input);
    nc3.getChildren().add(panel);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);
    
    facesContext.setViewRoot(root);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();
    assertEquals(input, referencedComp);

    // find it again!
    assertEquals(input, uiRef.getComponent());

  }
  
  public void testUIDataFooterFacet()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIData table = new UIData(); table.setId("table1");
    UIInput input = new UIInput(); input.setId("input1");

    // build the Tree...
    table.setFooter(input);
    nc3.getChildren().add(table);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();

    assertEquals(input, referencedComp);
  }

  public void testIndex()
  {
    UIViewRoot root = facesContext.getViewRoot();
    root.setId("root");
    UIForm form = new UIForm(); form.setId("form");
    UINamingContainer nc1 = new UINamingContainer(); nc1.setId("nc1");
    UINamingContainer nc2 = new UINamingContainer(); nc2.setId("nc2");
    UINamingContainer nc3 = new UINamingContainer(); nc3.setId("nc3");
    UIInput input1 = new UIInput(); input1.setId("input1");

    // build the Tree...
    nc3.getChildren().add(input1);
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(new UIOutput());
    nc2.getChildren().add(nc3);
    nc1.getChildren().add(nc2);
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(new UIOutput());
    form.getChildren().add(nc1);
    root.getChildren().add(new UIOutput());
    root.getChildren().add(form);

    // Get the ComponentReference util
    ComponentReference<UIInput> uiRef = ComponentReference.newUIComponentReference(input1);

    // find the component...
    UIInput referencedComp = uiRef.getComponent();

    assertEquals(input1, referencedComp);
  }
}