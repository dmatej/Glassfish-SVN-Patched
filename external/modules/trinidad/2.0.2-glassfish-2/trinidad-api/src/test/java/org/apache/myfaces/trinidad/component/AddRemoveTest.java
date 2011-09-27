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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIXPanel;

public class AddRemoveTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(AddRemoveTest.class);
  }
  
  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public AddRemoveTest(
    String testName)
  {
    super(testName);
  }

  @SuppressWarnings("unchecked")
  public void testAddChild()
  {
    UIXPanel a = new UIXPanel();
    UIXPanel b = new UIXPanel();
    UIXPanel c = new UIXPanel();
    UIXPanel d = new UIXPanel();
    UIXPanel e = new UIXPanel();

    a.getChildren().add(b);
    a.getChildren().add(c);
    a.getFacets().put("dChild", d);

    assertEquals(b.getParent(), a);
    assertEquals(c.getParent(), a);
    assertEquals(d.getParent(), a);
    assertEquals(a.getChildren().get(0), b);
    assertEquals(a.getChildren().get(1), c);

    // Remove item 0, which had better be "b"
    a.getChildren().remove(0);
    assertEquals(b.getParent(), null);
    assertEquals(c.getParent(), a);
    assertEquals(a.getChildren().get(0), c);

    // Now add "d" to "e", and verify it's in "e", but not in "d"
    e.getChildren().add(d);
    assertEquals(null, a.getFacets().get("dChild"));
    assertEquals(e, d.getParent());
    
    // Now move "c" over to "e" too
    e.getChildren().add(c);
    assertEquals(0, a.getChildren().size());
    assertEquals(2, e.getChildren().size());
    assertEquals(e, c.getParent());

    //Make sure the children are in the right order under "e" 
    assertEquals(e.getChildren().get(0), d);
    assertEquals(e.getChildren().get(1), c);
  }

  @SuppressWarnings("unchecked")
  public void testAddFacet()
  {
    UIXPanel a = new UIXPanel();
    UIXPanel b = new UIXPanel();
    UIXPanel c = new UIXPanel();
    UIXPanel d = new UIXPanel();
    UIXPanel e = new UIXPanel();

    a.getChildren().add(b);
    a.getChildren().add(c);
    a.getFacets().put("dChild", d);

    // Now add "d" to "e", and verify it's in "e", but not in "d"
    e.getFacets().put("otherChild", d);
    assertEquals(null, a.getFacets().get("dChild"));
    assertEquals(e, d.getParent());
    
    // Now move "c" over to "e" too, and make sure it's gone from a.
    e.getFacets().put("otherOtherChild", c);
    assertEquals(e, c.getParent());
    assertEquals(1, a.getChildren().size());
    assertEquals(b, a.getChildren().get(0));
  }

  @SuppressWarnings("unchecked")
  public void testMoveFromFacetToChild()
  {
    UIXPanel a = new UIXPanel();
    UIXPanel b = new UIXPanel();
    a.getChildren().add(b);

    a.getFacets().put("child", b);
    assertEquals(a, b.getParent());
    assertEquals(0, a.getChildren().size());
    assertEquals(b, a.getFacets().get("child"));
  }

  @SuppressWarnings("unchecked")
  public void testMoveFromChildToFacet()
  {
    UIXPanel a = new UIXPanel();
    UIXPanel b = new UIXPanel();
    a.getFacets().put("child", b);


    a.getChildren().add(b);
    assertEquals(a, b.getParent());
    assertEquals(1, a.getChildren().size());
    assertEquals(b, a.getChildren().get(0));
    assertEquals(null, a.getFacets().get("child"));
  }
}
