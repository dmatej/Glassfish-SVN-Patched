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

import javax.faces.component.NamingContainer;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

import org.apache.myfaces.trinidad.component.UIXPanel;

public class ClientIdCachingTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(ClientIdCachingTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public ClientIdCachingTest(
    String testName)
  {
    super(testName);
  }

  static private class TestPanel extends UIXPanel
  {
    protected Renderer getRenderer(FacesContext context)
    {
      return null;
    }
  }

  static private class TestNamingContainer extends TestPanel
                                           implements NamingContainer
  {
  }

  // Test nested NamingContainer callbacks
  @SuppressWarnings("unchecked")
  public void testBasic()
  {
    TestNamingContainer a = new TestNamingContainer(); a.setId("a");
    TestNamingContainer b = new TestNamingContainer(); b.setId("b");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    TestPanel e = new TestPanel(); e.setId("e");
    TestPanel g = new TestPanel(); g.setId("g");
    a.getChildren().add(b);
    b.getChildren().add(d);
    b.getChildren().add(g);
    d.getChildren().add(e);
    
    FacesContext context = FacesContext.getCurrentInstance();
    assertEquals("a:b:d:e", e.getClientId(context));
  }
  
  public void testSetId()
  {
    TestNamingContainer a = new TestNamingContainer(); a.setId("a");
    TestNamingContainer b = new TestNamingContainer(); b.setId("b");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    TestPanel e = new TestPanel(); e.setId("e");
    TestPanel g = new TestPanel(); g.setId("g");
    a.getChildren().add(b);
    b.getChildren().add(d);
    b.getChildren().add(g);
    d.getChildren().add(e);

    // prime    
    FacesContext context = FacesContext.getCurrentInstance();
    assertEquals("a:b:d:e", e.getClientId(context));

    // set the component's id using accessor
    e.setId("ePrime");
    assertEquals("a:b:d:ePrime", e.getClientId(context));
    
    // set the component's id using attribute map
    e.getAttributes().put("id", "eDoublePrime");
    assertEquals("a:b:d:eDoublePrime", e.getClientId(context));
    

    // set an ancsestor's id using accessor
    b.setId("bPrime");
    assertEquals("a:bPrime:d:eDoublePrime", e.getClientId(context));
    
    // set the component's id using attribute map
    b.getAttributes().put("id", "bDoublePrime");
    assertEquals("a:bDoublePrime:d:eDoublePrime", e.getClientId(context));
  }

  public void testMoving()
  {
    TestNamingContainer a = new TestNamingContainer(); a.setId("a");
    TestNamingContainer b = new TestNamingContainer(); b.setId("b");
    TestNamingContainer c = new TestNamingContainer(); c.setId("c");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    TestPanel e = new TestPanel(); e.setId("e");
    TestPanel f = new TestPanel(); f.setId("f");
    TestPanel g = new TestPanel(); g.setId("g");
    a.getChildren().add(b);
    a.getChildren().add(c);
    b.getChildren().add(d);
    b.getChildren().add(g);
    d.getChildren().add(e);
    d.getChildren().add(f);

    // prime    
    FacesContext context = FacesContext.getCurrentInstance();
    assertEquals("a:b:d:e", e.getClientId(context));

    // move within same NamingContainer--no clientId change
    f.getChildren().add(e);
    assertEquals("a:b:d:e", e.getClientId(context));
    
    // move between NamingContainers
    g.getChildren().add(e);
    assertEquals("a:b:e", e.getClientId(context)); 
  }
}
