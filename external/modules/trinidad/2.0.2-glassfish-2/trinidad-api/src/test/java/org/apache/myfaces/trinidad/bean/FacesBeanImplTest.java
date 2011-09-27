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
package org.apache.myfaces.trinidad.bean;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import java.util.Iterator;

import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.PropertyKey;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.context.MockRequestContext;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;

public class FacesBeanImplTest extends FacesTestCase
{
  public static final Test suite()
  {
    return new TestSuite(FacesBeanImplTest.class);
  }

  public static void main(String[] args) throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }

  public FacesBeanImplTest(
    String testName)
  {
    super(testName);
  }


  private MockRequestContext _mafct;

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    _mafct = new MockRequestContext();
  }

  @Override
  protected void tearDown() throws Exception
  {
    _mafct.release();
    _mafct = null;
    super.tearDown();
  }

  public void testInitialValues()
  {
    TestBean bean = new TestBean();
    assertNull(bean.getFirst());
    assertNull(bean.getSecond());
  }

  public void testSubclass()
  {
    SubTypeBean bean = new SubTypeBean();
    assertNull(bean.getFirst());
    assertNull(bean.getSecond());
    assertNull(bean.getSub());
    bean.setSub("sub");
    assertEquals("sub", bean.getSub());
  }

  public void testSetValues()
  {
    TestBean bean = new TestBean();
    bean.setFirst("first");
    bean.setSecond("second");
    assertEquals(bean.getFirst(), "first");
    assertEquals(bean.getSecond(), "second");

    bean.setFirst(null);
    bean.setSecond(null);

    assertNull(bean.getFirst());
    assertNull(bean.getSecond());

    bean.setSecond("newSecond");
    assertEquals(bean.getSecond(), "newSecond");
    assertEquals(bean.getProperty(TestBean.SECOND_KEY), "newSecond");
    assertEquals(bean.getLocalProperty(TestBean.SECOND_KEY), "newSecond");

    bean.setProperty(TestBean.FIRST_KEY, "newFirst");
    assertEquals(bean.getFirst(), "newFirst");
  }

  public void testAliases()
  {
    TestBean bean = new TestBean();
    bean.setFirstAlias("alias");
    assertEquals("alias", bean.getFirst());
    assertEquals("alias", bean.getFirstAlias());

    bean = new TestBean();
    bean.setFirst("alias2");
    assertEquals("alias2", bean.getFirst());
    assertEquals("alias2", bean.getFirstAlias());
  }

  public void testAnonymousKeys()
  {
    // Create an anonymous key
    PropertyKey thirdKey = new PropertyKey("third");
    TestBean bean = new TestBean();
    bean.setFirst("first");
    bean.setSecond("second");
    bean.setProperty(thirdKey, "third");
    assertEquals(bean.getFirst(), "first");
    assertEquals(bean.getSecond(), "second");
    assertEquals(bean.getProperty(thirdKey), "third");

    PropertyKey extraInstance = new PropertyKey("third");
    assertEquals(bean.getProperty(thirdKey),
                 bean.getProperty(extraInstance));
  }


  public void testBindingNotAllowed()
  {
    TestBean bean = new TestBean();
    try
    {
      bean.setValueBinding(TestBean.CANT_BE_BOUND_KEY,
                           new TestValueBinding());
      fail();
    }
    catch (IllegalArgumentException e)
    {
    }
  }



  public void testBindings()
  {
    TestBean bean = new TestBean();
    TestValueBinding vb1 = new TestValueBinding();
    vb1.setValue(null, "vbFirst");
    bean.setValueBinding(TestBean.FIRST_KEY, vb1);

    assertSame(bean.getValueBinding(TestBean.FIRST_KEY), vb1);

    assertEquals("vbFirst", bean.getFirst());

    bean.setFirst("first");
    assertEquals("first", bean.getFirst());

    bean.setFirst(null);
    assertEquals("vbFirst", bean.getFirst());
  }

  public void testSets()
  {
    TestBean bean = new TestBean();
    assertTrue(bean.keySet().isEmpty());
    assertTrue(bean.bindingKeySet().isEmpty());

    bean.setFirst("first");
    bean.setSecond("second");

    assertEquals(2, bean.keySet().size());

    bean.setSecond("newSecond");

    assertEquals(2, bean.keySet().size());

    bean.setSecond(null);

    // This test is somewhat dubious...
    assertEquals(1, bean.keySet().size());

    // Create an anonymous key
    PropertyKey thirdKey = new PropertyKey("third");

    bean.setValueBinding(TestBean.FIRST_KEY, new TestValueBinding());
    assertEquals(1, bean.bindingKeySet().size());

    bean.setValueBinding(TestBean.FIRST_KEY, new TestValueBinding());
    assertEquals(1, bean.bindingKeySet().size());

    bean.setValueBinding(thirdKey, new TestValueBinding());
    assertEquals(2, bean.bindingKeySet().size());

    assertTrue(bean.bindingKeySet().contains(thirdKey));
    assertTrue(bean.bindingKeySet().contains(TestBean.FIRST_KEY));
    assertTrue(!bean.bindingKeySet().contains(TestBean.SECOND_KEY));
  }

  public void testLists()
  {
    TestBean bean = new TestBean();
    Iterator<Object> iterator = bean.items();
    assertTrue(!iterator.hasNext());
    Integer[] array = bean.getItems();
    assertNotNull(array);
    assertEquals(0, array.length);

    bean.addItem(new Integer(1));
    assertEquals(1, bean.getItems().length);

    bean.addItem(new Integer(2));
    assertEquals(2, bean.getItems().length);

    array = bean.getItems();
    assertEquals(array[0], new Integer(1));
    assertEquals(array[1], new Integer(2));

    // Verify that this is a *list*, not a Set, so adding the
    // same value twice works as expected
    bean.addItem(new Integer(2));
    assertEquals(3, bean.getItems().length);
    bean.removeItem(new Integer(2));

    iterator = bean.items();
    assertEquals(new Integer(1), iterator.next());
    assertEquals(new Integer(2), iterator.next());
    assertTrue(!iterator.hasNext());

    assertTrue(bean.containsEntry(TestBean.ITEMS_KEY, Number.class));
    assertTrue(bean.containsEntry(TestBean.ITEMS_KEY, Integer.class));
    assertTrue(!bean.containsEntry(TestBean.ITEMS_KEY, Long.class));

    bean.removeItem(new Integer(1));
    bean.removeItem(new Integer(2));

    iterator = bean.items();
    assertTrue(!iterator.hasNext());
    array = bean.getItems();
    assertNotNull(array);
    assertEquals(0, array.length);

    // List items cannot be set, bound, or retrieved
    try
    {
      bean.setProperty(TestBean.ITEMS_KEY, "Shouldn't work");
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }


    try
    {
      bean.getProperty(TestBean.ITEMS_KEY);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.getLocalProperty(TestBean.ITEMS_KEY);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.setValueBinding(TestBean.ITEMS_KEY, new TestValueBinding());
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    // Meanwhile, you can't use the list APIs for non-list keys
    try
    {
      bean.addEntry(TestBean.FIRST_KEY, null);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.addEntry(TestBean.FIRST_KEY, null);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.removeEntry(TestBean.FIRST_KEY, null);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.getEntries(TestBean.FIRST_KEY, Object.class);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }

    try
    {
      bean.entries(TestBean.FIRST_KEY);
      fail();
    }
    catch (IllegalArgumentException iae)
    {
      // expected
    }
  }

  public void testTypeLocked()
  {
    try
    {
      TestBean.TYPE.registerKey("ShouldntWork");
      fail();
    }
    catch (IllegalStateException ise)
    {
    }
  }

  public void testAddAll()
  {
    TestBean bean = new TestBean();
    bean.setFirst("first");
    TestValueBinding binding = new TestValueBinding();
    binding.setValue(null, "FirstBinding");
    bean.setValueBinding(TestBean.FIRST_KEY, binding);
    bean.setSecond("second");
    bean.setProperty(new PropertyKey("sub"), "subValue");
    bean.addItem(new Integer(1));
    bean.addItem(new Integer(2));

    SubTypeBean sub = new SubTypeBean();
    sub.setSecond("third");
    sub.addAll(bean);

    assertEquals("first", sub.getFirst());
    assertEquals("second", sub.getSecond());
    assertEquals("subValue", sub.getSub());
    assertNotNull(sub.getValueBinding(TestBean.FIRST_KEY));
    assertEquals("FirstBinding",
                 sub.getValueBinding(TestBean.FIRST_KEY).getValue(null));
    Integer[] items = sub.getItems();
    assertNotNull(items);
    assertEquals(2, items.length);
    assertEquals(new Integer(1), items[0]);
    assertEquals(new Integer(2), items[1]);

    TestBean andBackAgain = new TestBean();
    andBackAgain.addAll(sub);
    assertEquals("subValue",
                 andBackAgain.getLocalProperty(new PropertyKey("sub")));
  }

  public void testStateSaveAndRestore()
  {
    // Build a bean
    SubTypeBean bean = new SubTypeBean();
    TestValueBinding vb1 = new TestValueBinding();
    vb1.setValue(null, "vbFirst");
    bean.setValueBinding(SubTypeBean.FIRST_KEY, vb1);
    bean.setSecond("second");
    bean.setTransient("Won't be there");
    bean.setSub("sub");
    bean.addItem(new Integer(1));
    bean.addItem(new Integer(2));

    SillyStateHolder silly = new SillyStateHolder();
    bean.setProperty(SubTypeBean.SILLY_KEY, silly);

    assertEquals("0", silly.toString());

    // Save its state
    Object savedState = bean.saveState(null);
    assertNotNull(savedState);

    try
    {
      savedState = _copyObjectThroughSerialization(savedState);
    }
    catch (Exception e)
    {
      e.printStackTrace();
      fail();
    }

    // Verify that our "silly" object has had its state saved
    assertEquals("1", silly.toString());

    // Build a new bean, and restore its state
    SubTypeBean newBean = new SubTypeBean();
    newBean.restoreState(null, savedState);

    // Verify it looks like the old bean
    assertEquals("vbFirst", newBean.getFirst());
    assertNull(newBean.getLocalProperty(SubTypeBean.FIRST_KEY));
    assertEquals("second", newBean.getSecond());
    assertNull(newBean.getValueBinding(SubTypeBean.SECOND_KEY));
    assertEquals("sub", newBean.getSub());

    // Verify that our "silly" object has had its state restored
    assertEquals("2", newBean.getProperty(SubTypeBean.SILLY_KEY).toString());

    Integer[] array = newBean.getItems();
    assertEquals(2, array.length);
    assertEquals(new Integer(1), array[0]);
    assertEquals(new Integer(2), array[1]);


    // Make sure the transient value is now null
    assertNull(newBean.getTransient());

    // Make sure the value binding looks the same, but is
    // not actually the same instance
    ValueBinding vb = newBean.getValueBinding(SubTypeBean.FIRST_KEY);
    assertTrue(vb instanceof TestValueBinding);
    assertTrue(vb != vb1);
    assertEquals(vb.getValue(null), "vbFirst");

    // Now change the value binding, and verify the original
    // bean is unchanged
    vb.setValue(null, "changedVB");
    assertEquals("changedVB", newBean.getFirst());
    assertEquals("vbFirst", bean.getFirst());

    // Now, verify that if we mark the initial state and save, that we get
    // a non-null value
    newBean.markInitialState();
    assertNull(newBean.saveState(null));

    // Now, we'll set a value, so we should get a non-null state

    // Our current delta support *does not* keep track of the original value.
    // If it does, add this test
    // String oldFirst = newBean.getFirst();
    newBean.setFirst("foo");
    assertNotNull(newBean.saveState(null));

    // Our current delta support *does not* keep track of the original value.
    // If it does, add this test
//    newBean.setFirst(oldFirst);
//    assertNull(newBean.saveState(null));
  }

  static private Object _copyObjectThroughSerialization(Object o)
    throws IOException, ClassNotFoundException
  {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(bos);
    oos.writeObject(o);
    oos.close();

    byte[] byteArray = bos.toByteArray();
    ByteArrayInputStream bis = new ByteArrayInputStream(byteArray);
    ObjectInputStream ois = new ObjectInputStream(bis);

    return ois.readObject();
  }

  // -= Simon Lessard =-
  // Never read locally as of 2006-08-09
  //private PropertyKey _thirdKey;
}
