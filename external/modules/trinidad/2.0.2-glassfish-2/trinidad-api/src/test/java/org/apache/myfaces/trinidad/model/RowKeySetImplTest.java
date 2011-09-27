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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.util.Set;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;
import org.apache.shale.test.base.AbstractJsfTestCase;

/**
 * Test for the RowKeySetImpl class.
 * 
 * There is a hardcoded dependency between this test and the SortableModel implementation that 
 * happens to be used by ModelUtils.toCollectionModel().
 * 
 */
public final class RowKeySetImplTest extends AbstractJsfTestCase
{
  public RowKeySetImplTest(String testName)
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
    return new TestSuite(RowKeySetImplTest.class);
  }
  
  public void testInitialyEmpty()
  {
    RowKeySetImpl set = _createKeySet(false);
    // make sure that set is empty:
    _testAll(set, false);
  }

  public void testInitialAddAll()
  {
    RowKeySetImpl set = _createKeySet(true);
    // make sure that everything is added:
    _testAll(set, true);
  }

  public void testAddAll()
  {
    RowKeySetImpl set = _createKeySet(false);
    _initModel(set);
    set.addAll();
    _testAll(set, true);
  }

  public void testAddRemoveAllWithSimpleSet()
  {
    RowKeySetImpl set1 = _createKeySet(false);
    RowKeySetImpl set2 = _createKeySet(false);

    Object k1 = _getKey(0);
    Object k2 = _getKey(1);
    Object k3 = _getKey(2);

    set1.add(k1);
    set1.add(k2);
    set2.add(k2);
    set2.add(k3);

    set1.addAll(set2);
    assertEquals("size", 3, set1.size());
    assertEquals("size", 2, set2.size());

    assertTrue(set1.contains(k1));
    assertTrue(set1.contains(k2));
    assertTrue(set1.contains(k3));

    assertFalse(set2.contains(k1));
    assertTrue(set2.contains(k2));
    assertTrue(set2.contains(k3));

    set1.removeAll(set2);
    assertEquals("size", 1, set1.size());
    assertTrue(set1.contains(k1));
    assertFalse(set1.contains(k2));
    assertFalse(set1.contains(k3));
  }

  public void testAddRemoveAllWithInfiniteSet()
    throws CloneNotSupportedException
  {
    Object k1 = _getKey(0);
    Object k2 = _getKey(1);
    Object k3 = _getKey(2);

    List<Object> infinite = new AbstractList<Object>()
    {
      @Override
      public int size()
      {
        return Integer.MAX_VALUE;
      }

      @Override
      public Object get(int index)
      {
        // code in javax.faces.model.ListDataModel always fetches index 0:
        if (index == 0)
          return Boolean.TRUE;

        throw new AssertionFailedError("must not fetch all data");
      }
    };

    RowKeySetImpl set1 = _createKeySet(true);
    set1.setCollectionModel(ModelUtils.toCollectionModel(infinite));
    set1.remove(k3);

    RowKeySetImpl set2 = _createKeySet(false);
    set2.addAll(set1);
    assertTrue(set2.contains(k1));
    assertTrue(set2.contains(k2));
    assertFalse(set2.contains(k3));

    // now test removeAll:
    set2.add(k3);
    set2.removeAll(set1);
    assertFalse(set2.contains(k1));
    assertFalse(set2.contains(k2));
    assertTrue(set2.contains(k3));
    assertEquals("size", 1, set2.size());

    // now test with both sets being infinite:
    set1 = _createKeySet(true);
    set1.setCollectionModel(ModelUtils.toCollectionModel(infinite));
    set1.remove(k1);
    set1.remove(k2);

    set2 = _createKeySet(true);
    set2.setCollectionModel(ModelUtils.toCollectionModel(infinite));
    set2.remove(k2);
    set2.remove(k3);

    RowKeySetImpl set3 = set2.clone(); // save for later

    // test addAll:
    set2.addAll(set1);
    assertTrue(set2.contains(k1));
    assertFalse(set2.contains(k2));
    assertTrue(set2.contains(k3));

    // test removeAll:
    set3.removeAll(set1);
    assertTrue(set3.contains(k1));
    assertFalse(set3.contains(k2));
    assertFalse(set3.contains(k3));
    assertEquals("size", 1, set3.size());
  }

  public void testClear()
  {
    RowKeySetImpl set = _createKeySet(false);
    _initModel(set);
    set.clear();
    _testAll(set, false);
  }

  public void testAddRemove()
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);
    boolean state = true;
    for(int i=0; i<endOfTrueRange; i++)
    {
      // all of the items in this range should have been previously
      // added:
      set.getCollectionModel().setRowKey(_getKey(i));
      assertTrue("item is selected", set.isContained());
      if (state)
        set.add();
      else
        set.remove();
      // make sure that the selection changed correctly:
      assertEquals("selection changed", state, set.isContained());
      // we are going to alternate between selecting and unselecting:
      state = !state;
    }
    for(int i=endOfTrueRange; i<_TEST_SIZE; i++)
    {
      // all of the items in this range should have been previously
      // removed:
      set.getCollectionModel().setRowKey(_getKey(i));
      assertFalse("item not selected", set.isContained());
      if (state)
        set.add();
      else
        set.remove();
      assertEquals("selection changed", state, set.isContained());
      state = !state;
    }
  }

  private void _testRange(Set<Object> keySet, int start, int end,
                          boolean expected, boolean newValue)
  {
    for(int i=start; i<end; i++)
    {
      Object rowKey = _getKey(i);
      assertEquals("current state", expected, keySet.contains(rowKey));
      if (newValue)
      {
        assertEquals("was set changed?", !expected, keySet.add(rowKey));
      }
      else
      {
        assertEquals("was set changed?", expected, keySet.remove(rowKey));
      }
      assertEquals("new state", newValue, keySet.contains(rowKey));
    }
  }

  public void testAddRemoveNoModel()
  {
    RowKeySetImpl keySet = _createKeySet(false);
    keySet.setCollectionModel(null);

    int size = 10;
    _testRange(keySet, 0, size, false, true);
    _testRange(keySet, size/2, size, true, true);
    _testRange(keySet, size/2, size, true, false);
    _testRange(keySet, size, size*2, false, false);
  }

  public void testInvert()
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);

    // the item at this index was previously selected:
    int index = endOfTrueRange - 1;
    // make sure that the item is unselected after inverting:
    set.getCollectionModel().setRowKey(_getKey(index));
    assertFalse("item is unselected", set.invert());
    assertFalse("item is unselected", set.isContained());

    // the item at this index was previously unselected:
    index = endOfTrueRange + 1;
    // make sure that the item is selected after inverting:
    set.getCollectionModel().setRowKey(_getKey(index));
    assertTrue("item is selected", set.invert());
    assertTrue("item is selected", set.isContained());
  }

  public void testInvertAll()
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);
    set.invertAll();
    // after inverting, all the items in this range should be unselected:
    _testAll(set, false, 0, endOfTrueRange);
    // after inverting, all the items in this range should be selected:
    _testAll(set, true, endOfTrueRange, _TEST_SIZE);
  }

  public void testSerialization() throws IOException, ClassNotFoundException
  {
    final byte[] bytes;
    final int endOfTrueRange;
    {
      RowKeySetImpl set = _createKeySet(false);
      endOfTrueRange = _initModel(set);

      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);
      // no need to worry about testing to make sure the model isn't
      // serialized, because the Entry class isn't Serializable.
      out.writeObject(set);
      out.close();

      bytes = bos.toByteArray();
      // test to make sure that the serialized size is reasonable:
     assertTrue(bytes.length <= 200);
     assertTrue(bytes.length >= 80);
    }

    ObjectInputStream in =
      new ObjectInputStream(new ByteArrayInputStream(bytes));
    RowKeySetImpl set = (RowKeySetImpl)in.readObject();
    assertNull("transient model", set.getCollectionModel());
    set.setCollectionModel(_MODEL);
    in.close();

    _testAll(set, true, 0, endOfTrueRange);
    _testAll(set, false, endOfTrueRange, _TEST_SIZE);
  }

  public void testGetRowKeyIterator()
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);
    Iterator<Object> selections = set.iterator();
    _testSelectionIterator(set.getCollectionModel(), selections,
                           endOfTrueRange, 0, endOfTrueRange);
  }

  public void testInvertAllRowKeyIterator()
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);
    set.invertAll();
    Iterator<Object> selections = set.iterator();
    _testSelectionIterator(set.getCollectionModel(), selections,
                           _TEST_SIZE - endOfTrueRange,
                           endOfTrueRange, _TEST_SIZE);
  }

  public void testClone() throws CloneNotSupportedException
  {
    RowKeySetImpl set = _createKeySet(false);
    int endOfTrueRange = _initModel(set);

    RowKeySetImpl clone = set.clone();
    _testAll(clone, true, 0, endOfTrueRange);
    _testAll(clone, false, endOfTrueRange, _TEST_SIZE);

    // modify the original. no change should be observed on the clone:
    CollectionModel model = set.getCollectionModel();
    model.setRowIndex(0);
    set.remove();
    assertFalse(set.isContained());
    assertTrue(clone.isContained());

    // modify the clone. no change should be observed on the original:
    model.setRowIndex(endOfTrueRange);
    clone.add();
    assertFalse(set.isContained());
    assertTrue(clone.isContained());
  }

  private void _testSelectionIterator(CollectionModel table,
                                      Iterator<Object> selections, int size,
                                      int rangeStart, int rangeEnd)
  {
    List<Object> selectedList = new ArrayList<Object>(size);
    for(;selections.hasNext();)
    {
      selectedList.add(selections.next());
    }

    int sz = selectedList.size();
    // make sure that we have the correct number of selected items:
    assertEquals("number of selected items", size, sz);

    for(int i=0; i<sz; i++)
    {
      Object rowKey = selectedList.get(i);
      table.setRowKey(rowKey);
      Entry val = (Entry) table.getRowData();

      // make sure this item was in the selected range:
      assertTrue("item is in selected range", val.index < rangeEnd);
      assertTrue("item is in selected range", val.index >= rangeStart);

      // make sure we haven't seen this value before:
      assertFalse("no repeats", val.used);
      // mark that this value was seen:
      val.used = true;
    }
  }


  private static CollectionModel _createCollectionModel(int testSize)
  {
    List<Object> lst = new ArrayList<Object>(testSize);
    for(int i=0; i < testSize; i++)
    {
      lst.add(new Entry(i));
    }
    return ModelUtils.toCollectionModel(lst);
  }

  private RowKeySetImpl _createKeySet(boolean defState)
  {
    RowKeySetImpl set = new RowKeySetImpl(defState);
    set.setCollectionModel(_MODEL);
    return set;
  }

  // this class is not Serializable, so that if the model is
  // accidentally serialized, then we will catch that error here:
  private static final class Entry
  {
    public final int index;
    public boolean used = false;
    public Entry(int index)
    {
      this.index = index;
    }
  }

  // modify the selection so that the first half are selected, and the
  // remaining half are not:
  private int _initModel(RowKeySetImpl set)
  {
    int endOfTrueRange = _TEST_SIZE / 2;
    for(int i=0; i<endOfTrueRange; i++)
    {
      set.getCollectionModel().setRowKey(_getKey(i));
      set.add();
    }
    for(int i=endOfTrueRange; i<_TEST_SIZE; i++)
    {
      set.getCollectionModel().setRowKey(_getKey(i));
      set.remove();
    }
    return endOfTrueRange;
  }

  private void _testAll(RowKeySetImpl set, boolean expected)
  {
    _testAll(set, expected, 0, _TEST_SIZE);
  }

  private void _testAll(RowKeySetImpl set, boolean expected,
                        int start, int end)
  {
    for(int i=start; i<end; i++)
    {
      set.getCollectionModel().setRowKey(_getKey(i));
      boolean isSet = set.isContained();
      assertEquals("is item selected?", expected, isSet);
    }
  }

  private static Object _getKey(int index)
  {
    return index;
  }

//  private static int _getIndex(String key)
//  {
//    return Integer.parseInt(key);
//  }

  private static final int _TEST_SIZE = 11;
  private static final CollectionModel _MODEL = _createCollectionModel(_TEST_SIZE);
}
