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
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.component.UIViewRoot;

import javax.faces.context.FacesContext;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class FindComponentTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(FindComponentTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public FindComponentTest(
    String testName)
  {
    super(testName);
  }

  static private class TestNamingContainer extends UIXPanel
                                           implements NamingContainer
  {
    @Override
    public UIComponent findComponent(String expr)
    {
      addToTrace(getId());
      addToTrace(expr);
      return super.findComponent(expr);
    }

    public static void clearTrace()
    {
      _trace = new StringBuffer();
    }

    // Append to the current trace log (or clear if null)
    public static void addToTrace(String text)
    {
      _trace.append('/');
      _trace.append(text);
    }

    // Retrieve the current trace log
    public static String getTrace()
    {
      return _trace.toString();
    }

    // Accumulated trace log
    private static StringBuffer _trace = new StringBuffer();
  }

  
  // Test nested NamingContainer callbacks
  @SuppressWarnings("unchecked")
  public void testNested()
  {
    TestNamingContainer a = new TestNamingContainer(); a.setId("a");
    TestNamingContainer b = new TestNamingContainer(); b.setId("b");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    UIXPanel e = new UIXPanel(); e.setId("e");
    UIXPanel g = new UIXPanel(); g.setId("g");
    a.getChildren().add(b);
    b.getChildren().add(d);
    b.getChildren().add(g);
    d.getChildren().add(e);
    
    TestNamingContainer.clearTrace();
    assertTrue(a == a.findComponent("a"));
    assertEquals("/a/a", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(a == a.findComponent(":a"));
    assertEquals("/a/:a", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(b == a.findComponent("b"));
    assertEquals("/a/b", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(b == a.findComponent(":b"));
    assertEquals("/a/:b", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(d == a.findComponent("b:d"));
    assertEquals("/a/b:d/b/d", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(d == a.findComponent(":b:d"));
    assertEquals("/a/:b:d/b/d", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(e == a.findComponent("b:d:e"));
    assertEquals("/a/b:d:e/b/d:e/d/e", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(e == a.findComponent(":b:d:e"));
    assertEquals("/a/:b:d:e/b/d:e/d/e", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(g == a.findComponent("b:g"));
    assertEquals("/a/b:g/b/g", TestNamingContainer.getTrace());
    
    TestNamingContainer.clearTrace();
    assertTrue(g == a.findComponent(":b:g"));
    assertEquals("/a/:b:g/b/g", TestNamingContainer.getTrace());
  }


  @SuppressWarnings("unchecked")
  public void testRelativeSearch()
  {
    // Set up a component hierarchy as follows (component ids in quotes):
    // "a" - UIViewRoot at head of hierarchy
    // "a" has children "b" and "c"
    // "b" has children "d" and "g"
    // "d" has children "e" and "f"
    // "c" has children "h" and "i"
    // Components "b" and "d" implement NamingContainer
    UIViewRoot a = new UIViewRoot(); a.setId("a");
    UIForm b = new UIForm(); b.setId("b");
    UIXPanel c = new UIXPanel(); c.setId("c");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    UIXPanel e = new UIXPanel(); e.setId("e");
    UIXPanel f = new UIXPanel(); f.setId("f");
    UIXPanel g = new UIXPanel(); g.setId("g");
    UIXPanel h = new UIXPanel(); h.setId("h");
    UIXPanel i = new UIXPanel(); i.setId("i");
 
    a.getChildren().add(b);
    a.getChildren().add(c);
    b.getChildren().add(d);
    b.getChildren().add(g);
    c.getChildren().add(h);
    c.getChildren().add(i);
    d.getChildren().add(e);
    d.getChildren().add(f);
    
    // Positive relative searches from "a"
    assertTrue(a == a.findComponent("a"));
    assertTrue(b == a.findComponent("b"));
    assertTrue(c == a.findComponent("c"));
    assertTrue(d == a.findComponent("b:d"));
    assertTrue(e == a.findComponent("b:d:e"));
    assertTrue(f == a.findComponent("b:d:f"));
    assertTrue(g == a.findComponent("b:g"));
    assertTrue(h == a.findComponent("h"));
    assertTrue(i == a.findComponent("i"));
    
    // Negative relative searches from "a"
    assertNull(a.findComponent("d"));
    assertNull(a.findComponent("e"));
    assertNull(a.findComponent("f"));
    assertNull(a.findComponent("g"));
    
    // Positive relative searches from "b"
    assertTrue(b == b.findComponent("b"));
    assertTrue(d == b.findComponent("d"));
    assertTrue(e == b.findComponent("d:e"));
    assertTrue(f == b.findComponent("d:f"));
    assertTrue(g == b.findComponent("g"));
    
    // Negative relative searches from "b"
    assertNull(b.findComponent("a"));
    assertNull(b.findComponent("c"));
    assertNull(b.findComponent("e"));
    assertNull(b.findComponent("f"));
    assertNull(b.findComponent("h"));
    assertNull(b.findComponent("i"));
    
    // Positive relative searches from "c"
    assertTrue(a == c.findComponent("a"));
    assertTrue(b == c.findComponent("b"));
    assertTrue(c == c.findComponent("c"));
    assertTrue(d == c.findComponent("b:d"));
    assertTrue(e == c.findComponent("b:d:e"));
    assertTrue(f == c.findComponent("b:d:f"));
    assertTrue(g == c.findComponent("b:g"));
    assertTrue(h == c.findComponent("h"));
    assertTrue(i == c.findComponent("i"));
    
    // Negative relative searches from "c"
    assertNull(c.findComponent("d"));
    assertNull(c.findComponent("e"));
    assertNull(c.findComponent("f"));
    assertNull(c.findComponent("g"));
    
    // Positive relative searches from "d"
    assertTrue(d == d.findComponent("d"));
    assertTrue(e == d.findComponent("e"));
    assertTrue(f == d.findComponent("f"));
    
    // Negative relative searches from "d"
    assertNull(d.findComponent("a"));
    assertNull(d.findComponent("b"));
    assertNull(d.findComponent("c"));
    assertNull(d.findComponent("g"));
    assertNull(d.findComponent("h"));
    assertNull(d.findComponent("i"));
    
    // Positive relative searches from "e"
    assertTrue(d == e.findComponent("d"));
    assertTrue(e == e.findComponent("e"));
    assertTrue(f == e.findComponent("f"));
    
    // Negative relative searches from "e"
    assertNull(e.findComponent("a"));
    assertNull(e.findComponent("b"));
    assertNull(e.findComponent("c"));
    assertNull(e.findComponent("g"));
    assertNull(e.findComponent("h"));
    assertNull(e.findComponent("i"));
    
    // Positive relative searches from "f"
    assertTrue(d == f.findComponent("d"));
    assertTrue(e == f.findComponent("e"));
    assertTrue(f == f.findComponent("f"));
    
    // Negative relative searches from "f"
    assertNull(f.findComponent("a"));
    assertNull(f.findComponent("b"));
    assertNull(f.findComponent("c"));
    assertNull(f.findComponent("g"));
    assertNull(f.findComponent("h"));
    assertNull(f.findComponent("i"));
    
    // Positive relative searches from "g"
    assertTrue(b == g.findComponent("b"));
    assertTrue(d == g.findComponent("d"));
    assertTrue(e == g.findComponent("d:e"));
    assertTrue(f == g.findComponent("d:f"));
    assertTrue(g == g.findComponent("g"));
    
    // Negative relative searches from "g"
    assertNull(g.findComponent("a"));
    assertNull(g.findComponent("c"));
    assertNull(g.findComponent("e"));
    assertNull(g.findComponent("f"));
    assertNull(g.findComponent("h"));
    assertNull(g.findComponent("i"));
    
    // Positive relative searches from "h"
    assertTrue(a == h.findComponent("a"));
    assertTrue(b == h.findComponent("b"));
    assertTrue(c == h.findComponent("c"));
    assertTrue(d == h.findComponent("b:d"));
    assertTrue(e == h.findComponent("b:d:e"));
    assertTrue(f == h.findComponent("b:d:f"));
    assertTrue(g == h.findComponent("b:g"));
    assertTrue(h == h.findComponent("h"));
    assertTrue(i == h.findComponent("i"));
    
    // Negative relative searches from "h"
    assertNull(h.findComponent("d"));
    assertNull(h.findComponent("e"));
    assertNull(h.findComponent("f"));
    assertNull(h.findComponent("g"));
    
    // Positive relative searches from "i"
    assertTrue(a == i.findComponent("a"));
    assertTrue(b == i.findComponent("b"));
    assertTrue(c == i.findComponent("c"));
    assertTrue(d == i.findComponent("b:d"));
    assertTrue(e == i.findComponent("b:d:e"));
    assertTrue(f == i.findComponent("b:d:f"));
    assertTrue(g == i.findComponent("b:g"));
    assertTrue(h == i.findComponent("h"));
    assertTrue(i == i.findComponent("i"));
    
    // Negative relative searches from "i"
    assertNull(i.findComponent("d"));
    assertNull(i.findComponent("e"));
    assertNull(i.findComponent("f"));
    assertNull(i.findComponent("g"));
    
    // Absolute searches from "a"
    assertTrue(a == a.findComponent(":a"));
    assertTrue(b == a.findComponent(":b"));
    assertTrue(c == a.findComponent(":c"));
    assertTrue(d == a.findComponent(":b:d"));
    assertTrue(e == a.findComponent(":b:d:e"));
    assertTrue(f == a.findComponent(":b:d:f"));
    assertTrue(g == a.findComponent(":b:g"));
    assertTrue(h == a.findComponent(":h"));
    assertTrue(i == a.findComponent(":i"));
    
    // Absolute searches from "b"
    assertTrue(a == b.findComponent(":a"));
    assertTrue(b == b.findComponent(":b"));
    assertTrue(c == b.findComponent(":c"));
    assertTrue(d == b.findComponent(":b:d"));
    assertTrue(e == b.findComponent(":b:d:e"));
    assertTrue(f == b.findComponent(":b:d:f"));
    assertTrue(g == b.findComponent(":b:g"));
    assertTrue(h == b.findComponent(":h"));
    assertTrue(i == b.findComponent(":i"));
    
    // Absolute searches from "c"
    assertTrue(a == c.findComponent(":a"));
    assertTrue(b == c.findComponent(":b"));
    assertTrue(c == c.findComponent(":c"));
    assertTrue(d == c.findComponent(":b:d"));
    assertTrue(e == c.findComponent(":b:d:e"));
    assertTrue(f == c.findComponent(":b:d:f"));
    assertTrue(g == c.findComponent(":b:g"));
    assertTrue(h == c.findComponent(":h"));
    assertTrue(i == c.findComponent(":i"));
    
    // Absolute searches from "d"
    assertTrue(a == d.findComponent(":a"));
    assertTrue(b == d.findComponent(":b"));
    assertTrue(c == d.findComponent(":c"));
    assertTrue(d == d.findComponent(":b:d"));
    assertTrue(e == d.findComponent(":b:d:e"));
    assertTrue(f == d.findComponent(":b:d:f"));
    assertTrue(g == d.findComponent(":b:g"));
    assertTrue(h == d.findComponent(":h"));
    assertTrue(i == d.findComponent(":i"));
    
    // Absolute searches from "e"
    assertTrue(a == e.findComponent(":a"));
    assertTrue(b == e.findComponent(":b"));
    assertTrue(c == e.findComponent(":c"));
    assertTrue(d == e.findComponent(":b:d"));
    assertTrue(e == e.findComponent(":b:d:e"));
    assertTrue(f == e.findComponent(":b:d:f"));
    assertTrue(g == e.findComponent(":b:g"));
    assertTrue(h == e.findComponent(":h"));
    assertTrue(i == e.findComponent(":i"));
    
    // Absolute searches from "f"
    assertTrue(a == f.findComponent(":a"));
    assertTrue(b == f.findComponent(":b"));
    assertTrue(c == f.findComponent(":c"));
    assertTrue(d == f.findComponent(":b:d"));
    assertTrue(e == f.findComponent(":b:d:e"));
    assertTrue(f == f.findComponent(":b:d:f"));
    assertTrue(g == f.findComponent(":b:g"));
    assertTrue(h == f.findComponent(":h"));
    assertTrue(i == f.findComponent(":i"));
    
    // Absolute searches from "g"
    assertTrue(a == g.findComponent(":a"));
    assertTrue(b == g.findComponent(":b"));
    assertTrue(c == g.findComponent(":c"));
    assertTrue(d == g.findComponent(":b:d"));
    assertTrue(e == g.findComponent(":b:d:e"));
    assertTrue(f == g.findComponent(":b:d:f"));
    assertTrue(g == g.findComponent(":b:g"));
    assertTrue(h == g.findComponent(":h"));
    assertTrue(i == g.findComponent(":i"));
    
    // Absolute searches from "h"
    assertTrue(a == h.findComponent(":a"));
    assertTrue(b == h.findComponent(":b"));
    assertTrue(c == h.findComponent(":c"));
    assertTrue(d == h.findComponent(":b:d"));
    assertTrue(e == h.findComponent(":b:d:e"));
    assertTrue(f == h.findComponent(":b:d:f"));
    assertTrue(g == h.findComponent(":b:g"));
    assertTrue(h == h.findComponent(":h"));
    assertTrue(i == h.findComponent(":i"));
    
    // Absolute searches from "i"
    assertTrue(a == i.findComponent(":a"));
    assertTrue(b == i.findComponent(":b"));
    assertTrue(c == i.findComponent(":c"));
    assertTrue(d == i.findComponent(":b:d"));
    assertTrue(e == i.findComponent(":b:d:e"));
    assertTrue(f == i.findComponent(":b:d:f"));
    assertTrue(g == i.findComponent(":b:g"));
    assertTrue(h == i.findComponent(":h"));
    assertTrue(i == i.findComponent(":i"));
  }

  @SuppressWarnings("unchecked")
  public void testAbsoluteSearch()
  {
    // Set up a component hierarchy as follows (component ids in quotes):
    // "a" - UIViewRoot at head of hierarchy
    // "a" has children "b" and "c"
    // "b" has children "d" and "g"
    // "d" has children "e" and "f"
    // "c" has children "h" and "i"
    // Components "b" and "d" implement NamingContainer
    UIViewRoot a = new UIViewRoot(); a.setId("a");
    UIForm b = new UIForm(); b.setId("b");
    UIXPanel c = new UIXPanel(); c.setId("c");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    UIXPanel e = new UIXPanel(); e.setId("e");
    UIXPanel f = new UIXPanel(); f.setId("f");
    UIXPanel g = new UIXPanel(); g.setId("g");
    UIXPanel h = new UIXPanel(); h.setId("h");
    UIXPanel i = new UIXPanel(); i.setId("i");
    a.getChildren().add(b);
    a.getChildren().add(c);
    b.getChildren().add(d);
    b.getChildren().add(g);
    c.getChildren().add(h);
    c.getChildren().add(i);
    d.getChildren().add(e);
    d.getChildren().add(f);
        
    // Absolute searches from "a"
    assertTrue(a == a.findComponent(":a"));
    assertTrue(b == a.findComponent(":b"));
    assertTrue(c == a.findComponent(":c"));
    assertTrue(d == a.findComponent(":b:d"));
    assertTrue(e == a.findComponent(":b:d:e"));
    assertTrue(f == a.findComponent(":b:d:f"));
    assertTrue(g == a.findComponent(":b:g"));
    assertTrue(h == a.findComponent(":h"));
    assertTrue(i == a.findComponent(":i"));
    
    // Absolute searches from "b"
    assertTrue(a == b.findComponent(":a"));
    assertTrue(b == b.findComponent(":b"));
    assertTrue(c == b.findComponent(":c"));
    assertTrue(d == b.findComponent(":b:d"));
    assertTrue(e == b.findComponent(":b:d:e"));
    assertTrue(f == b.findComponent(":b:d:f"));
    assertTrue(g == b.findComponent(":b:g"));
    assertTrue(h == b.findComponent(":h"));
    assertTrue(i == b.findComponent(":i"));
    
    // Absolute searches from "c"
    assertTrue(a == c.findComponent(":a"));
    assertTrue(b == c.findComponent(":b"));
    assertTrue(c == c.findComponent(":c"));
    assertTrue(d == c.findComponent(":b:d"));
    assertTrue(e == c.findComponent(":b:d:e"));
    assertTrue(f == c.findComponent(":b:d:f"));
    assertTrue(g == c.findComponent(":b:g"));
    assertTrue(h == c.findComponent(":h"));
    assertTrue(i == c.findComponent(":i"));
    
    // Absolute searches from "d"
    assertTrue(a == d.findComponent(":a"));
    assertTrue(b == d.findComponent(":b"));
    assertTrue(c == d.findComponent(":c"));
    assertTrue(d == d.findComponent(":b:d"));
    assertTrue(e == d.findComponent(":b:d:e"));
    assertTrue(f == d.findComponent(":b:d:f"));
    assertTrue(g == d.findComponent(":b:g"));
    assertTrue(h == d.findComponent(":h"));
    assertTrue(i == d.findComponent(":i"));
    
    // Absolute searches from "e"
    assertTrue(a == e.findComponent(":a"));
    assertTrue(b == e.findComponent(":b"));
    assertTrue(c == e.findComponent(":c"));
    assertTrue(d == e.findComponent(":b:d"));
    assertTrue(e == e.findComponent(":b:d:e"));
    assertTrue(f == e.findComponent(":b:d:f"));
    assertTrue(g == e.findComponent(":b:g"));
    assertTrue(h == e.findComponent(":h"));
    assertTrue(i == e.findComponent(":i"));
    
    // Absolute searches from "f"
    assertTrue(a == f.findComponent(":a"));
    assertTrue(b == f.findComponent(":b"));
    assertTrue(c == f.findComponent(":c"));
    assertTrue(d == f.findComponent(":b:d"));
    assertTrue(e == f.findComponent(":b:d:e"));
    assertTrue(f == f.findComponent(":b:d:f"));
    assertTrue(g == f.findComponent(":b:g"));
    assertTrue(h == f.findComponent(":h"));
    assertTrue(i == f.findComponent(":i"));
    
    // Absolute searches from "g"
    assertTrue(a == g.findComponent(":a"));
    assertTrue(b == g.findComponent(":b"));
    assertTrue(c == g.findComponent(":c"));
    assertTrue(d == g.findComponent(":b:d"));
    assertTrue(e == g.findComponent(":b:d:e"));
    assertTrue(f == g.findComponent(":b:d:f"));
    assertTrue(g == g.findComponent(":b:g"));
    assertTrue(h == g.findComponent(":h"));
    assertTrue(i == g.findComponent(":i"));
    
    // Absolute searches from "h"
    assertTrue(a == h.findComponent(":a"));
    assertTrue(b == h.findComponent(":b"));
    assertTrue(c == h.findComponent(":c"));
    assertTrue(d == h.findComponent(":b:d"));
    assertTrue(e == h.findComponent(":b:d:e"));
    assertTrue(f == h.findComponent(":b:d:f"));
    assertTrue(g == h.findComponent(":b:g"));
    assertTrue(h == h.findComponent(":h"));
    assertTrue(i == h.findComponent(":i"));
    
    // Absolute searches from "i"
    assertTrue(a == i.findComponent(":a"));
    assertTrue(b == i.findComponent(":b"));
    assertTrue(c == i.findComponent(":c"));
    assertTrue(d == i.findComponent(":b:d"));
    assertTrue(e == i.findComponent(":b:d:e"));
    assertTrue(f == i.findComponent(":b:d:f"));
    assertTrue(g == i.findComponent(":b:g"));
    assertTrue(h == i.findComponent(":h"));
    assertTrue(i == i.findComponent(":i"));
  }
    
  @SuppressWarnings("unchecked")
  public void testExceptions()
  {
    // Set up a component hierarchy as follows (component ids in quotes):
    // "a" - UIViewRoot at head of hierarchy
    // "a" has children "b" and "c"
    // "b" has children "d" and "g"
    // "d" has children "e" and "f"
    // "c" has children "h" and "i"
    // Components "b" and "d" implement NamingContainer
    UIViewRoot a = new UIViewRoot(); a.setId("a");
    UIForm b = new UIForm(); b.setId("b");
    UIXPanel c = new UIXPanel(); c.setId("c");
    TestNamingContainer d = new TestNamingContainer(); d.setId("d");
    UIXPanel e = new UIXPanel(); e.setId("e");
    UIXPanel f = new UIXPanel(); f.setId("f");
    UIXPanel g = new UIXPanel(); g.setId("g");
    UIXPanel h = new UIXPanel(); h.setId("h");
    UIXPanel i = new UIXPanel(); i.setId("i");
    a.getChildren().add(b);
    a.getChildren().add(c);
    b.getChildren().add(d);
    b.getChildren().add(g);
    c.getChildren().add(h);
    c.getChildren().add(i);
    d.getChildren().add(e);
    d.getChildren().add(f);

    // Cases that should throw exceptions
    try
    {
      a.findComponent(null);
      fail("Should have thrown NullPointerException");
    }
    catch (NullPointerException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent("a:c:h");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent("a:c:i");
      fail("Should have thrown IllegalArgumentException");
    } catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent(":a:c:h");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent(":a:c:i");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent("c:h");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent("c:i");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent(":c:h");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
    
    try
    {
      a.findComponent(":c:i");
      fail("Should have thrown IllegalArgumentException");
    }
    catch (IllegalArgumentException ex)
    {
      ; // Expected result
    }
  }
}
