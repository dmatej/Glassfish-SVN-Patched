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
package org.apache.myfaces.trinidad.model;

import java.beans.IntrospectionException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.shale.test.base.AbstractJsfTestCase;

/**
 * There is a hardcoded dependency between this test and the SortableModel implementation that 
 * happens to be used by ModelUtils.toCollectionModel().
 */
public class RowKeySetTreeImplTest extends AbstractJsfTestCase
{
  public RowKeySetTreeImplTest(String name)
  {
    super(name);
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
    return new TestSuite(RowKeySetTreeImplTest.class);
  }

  public void testInitialState() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    _testNotInSet(pathSet, _0, _011, _2, _20);
  }

  public void testIterator() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    _add(pathSet, _0, _011, _20);

    Iterator<Object> iter = pathSet.iterator();
    // there is no requirement that the paths come out in the following
    // order. However, I want to make writing this test easy, so
    // I am taking advantage of the order:
    assertEquals(_20, iter.next());
    assertEquals(_0, iter.next());
    assertEquals(_011, iter.next());
    assertFalse(iter.hasNext());
  }

  public void testInitialAddAll() throws IntrospectionException
  {
    RowKeySet pathSet = new RowKeySetTreeImpl(true);
    TreeModel model = ChildPropertyTreeModelTest.createModel();
    pathSet.setCollectionModel(model);

    _testInSet(pathSet, _0, _011, _2, _20);
  }

  public void testAdd() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();

    assertTrue(pathSet.add(_0));
    assertTrue("contained", pathSet.contains(_0));
    _testNotInSet(pathSet, _011, _2, _20);

    assertFalse(pathSet.add(_0));
  }

  public void testRemove() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    pathSet.add(_2);
    assertFalse(pathSet.remove(_0));
    pathSet.add(_0);
    assertTrue(pathSet.remove(_0));
    _testNotInSet(pathSet, _0, _011, _20);
    assertTrue("is contained", pathSet.contains(_2));
  }

  public void testAddAll() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    TreeModel model = pathSet.getTreeModel();

    model.setRowKey(_2);
    pathSet.addAll();

    _testNotInSet(pathSet, _0, _011);
    _testInSet(pathSet, _2, _20);
  }

  public void testAddAllCollection() throws IntrospectionException
  {
    RowKeySet set1 = new TestPathSet();
    RowKeySet set2 = new TestPathSet();
    _add(set1, _011, _2, _20);
    set2.add(_0);

    set2.addAll(set1);
    _testInSet(set1, _011, _2, _20);
    _testNotInSet(set1, _0);
    _testInSet(set2, _0, _011, _2, _20);

    // test with set1 having default=true:
    set1 = new TestPathSet(true);
    set2 = new TestPathSet();
    set1.remove(_2);
    set1.remove(_011);
    set2.add(_2);

    set2.addAll(set1);
    _testInSet(set2, _0, _2, _20);
    _testNotInSet(set2, _011);

    // test with set1 having default=true on subtree:
    set1 = new TestPathSet();
    set2 = new TestPathSet();
    set1.getCollectionModel().setRowKey(_2);
    set1.addAll();

    set2.addAll(set1);
    _testInSet(set2, _2, _20);
    _testNotInSet(set2, _0, _011);
  }

  public void testRemoveAllCollection() throws IntrospectionException
  {
    RowKeySet set1 = new TestPathSet();
    RowKeySet set2 = new TestPathSet();
    _add(set1, _011, _2, _20);
    _add(set2, _0, _2, _20);

    set2.removeAll(set1);
    _testInSet(set1, _011, _2, _20);
    _testNotInSet(set1, _0);
    _testInSet(set2, _0);
    _testNotInSet(set2, _011, _2, _20);

    // test with set1 having default=true:
    set1 = new TestPathSet(true);
    set2 = new TestPathSet();
    set1.remove(_2);
    _add(set2, _011, _2);

    set2.removeAll(set1);
    _testInSet(set2, _2);
    _testNotInSet(set2, _011, _20);

    // test with set1 having default=true on subtree:
    set1 = new TestPathSet();
    set2 = new TestPathSet();
    set1.getCollectionModel().setRowKey(_2);
    set1.addAll();
    _add(set2, _20, _011);

    set2.removeAll(set1);
    _testInSet(set2, _011);
    _testNotInSet(set2, _0, _2, _20);
  }

  public void testClone() throws IntrospectionException, CloneNotSupportedException
  {
    RowKeySet pathSet = new RowKeySetTreeImpl();
    TreeModel model = ChildPropertyTreeModelTest.createModel();
    pathSet.setCollectionModel(model);
    model.setRowKey(_2);
    pathSet.addAll();
    pathSet.add(_011);

    RowKeySet clone = pathSet.clone();
    // mutate clone:
    clone.remove(_2);

    // make sure original has not changed:
    assertFalse(pathSet.contains(_0));
    _testInSet(pathSet, _011, _2, _20);

    // verify clone:
    _testNotInSet(clone, _0, _2);
    _testInSet(clone, _011, _20);
  }

  public void testSize() throws IntrospectionException
  {
    RowKeySet set = new TestPathSet();
    assertEquals("size", 0, set.size());
    _add(set, _2, _20, _0, _011);
    assertEquals("size", 4, set.size());

    set = new RowKeySetTreeImpl(true);
    TreeModel model = ChildPropertyTreeModelTest.createModel();
    set.setCollectionModel(model);
    assertEquals("addAll:size", 14, set.size());

    set.remove(_011);
    assertEquals("addAll:size", 13, set.size());

    model.setRowKey(_011);
    set.removeAll();
    assertEquals("addAll:size", 10, set.size());

  }

  public void testClear() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    _add(pathSet, _2, _20, _0, _011);

    pathSet.clear();

    _testNotInSet(pathSet, _0, _011, _2, _20);
  }

  public void testRemoveAll() throws IntrospectionException
  {
    TestPathSet pathSet = new TestPathSet();
    TreeModel model = pathSet.getTreeModel();
    _add(pathSet, _2, _20, _0, _011);

    model.setRowKey(_2);
    pathSet.removeAll();

    _testInSet(pathSet, _0, _011);
    _testNotInSet(pathSet, _2, _20);
  }

  public void testSerialization()
    throws IOException, ClassNotFoundException, IntrospectionException
  {
    TreeModel model = ChildPropertyTreeModelTest.createModel();
    final byte[] bytes;
    {
      RowKeySet pathSet = new RowKeySetTreeImpl();
      pathSet.setCollectionModel(model);
      pathSet.add(_2);
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);
      out.writeObject(pathSet);
      out.close();
      bytes = bos.toByteArray();
    }

    // we're doing a test here to warn the developer if the
    // serialized size changes significantly. If the size changes are
    // expected, then change the expected range for these tests:
    assertTrue(bytes.length < 1000);
    assertTrue(bytes.length > 615);

    ObjectInputStream in =
      new ObjectInputStream(new ByteArrayInputStream(bytes));
    RowKeySet pathSet = (RowKeySet) in.readObject();
    in.close();
    pathSet.setCollectionModel(model);

    // test state:
    _testNotInSet(pathSet, _0, _011, _20);
    assertTrue("is contained", pathSet.contains(_2));
  }

  private void _testInSet(RowKeySet set, Object ... keys)
  {
    for(Object key:keys)
    {
      assertTrue("must contain key:"+key, set.contains(key));
    }
  }

  private void _testNotInSet(RowKeySet set, Object ... keys)
  {
    for(Object key:keys)
    {
      assertFalse("must not contain key:"+key, set.contains(key));
    }
  }

  private void _add(RowKeySet set, Object ... keys)
  {
    for(Object key:keys)
    {
      set.add(key);
    }
  }

  private static List<Object> _createPath(Object ... rowKeys)
  {
    return Collections.unmodifiableList(Arrays.asList(rowKeys));
  }

  private static final class TestPathSet extends RowKeySetTreeImpl
  {
    public TestPathSet(boolean def) throws IntrospectionException
    {
      super(def);
      _model = ChildPropertyTreeModelTest.createModel();
      setCollectionModel(_model);
    }

    public TestPathSet() throws IntrospectionException
    {
      this(false);
    }

    public TreeModel getTreeModel()
    {
      return _model;
    }

    private final TreeModel _model;
  }

  private static final List<Object> _0 = _createPath(0);
  private static final List<Object> _011 = _createPath(0, 1, 1);
  private static final List<Object> _2 = _createPath(2);
  private static final List<Object> _20 = _createPath(2, 0);
}
