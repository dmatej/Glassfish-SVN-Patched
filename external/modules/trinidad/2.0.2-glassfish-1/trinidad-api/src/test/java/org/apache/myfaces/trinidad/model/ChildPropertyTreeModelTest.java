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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.shale.test.base.AbstractJsfTestCase;

public class ChildPropertyTreeModelTest extends AbstractJsfTestCase
{
  public ChildPropertyTreeModelTest(String name)
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
    return new TestSuite(ChildPropertyTreeModelTest.class);
  }
  
  public void testInitialState() throws IntrospectionException
  {
    TreeModel model = createModel();
    _testTree(model, _ROOTS);
  }

  private void _followPath(TreeModel model, int[] path)
  {
    model.setRowKey(null);
    int lastIndex = path.length - 1;
    for(int i=0; i<lastIndex; i++)
    {
      model.setRowIndex(path[i]);
      model.enterContainer();
    }
    model.setRowIndex(path[lastIndex]);
  }
  
  public void testGetSetRowKey() throws IntrospectionException
  {
    TreeModel model = createModel();
    
    Bean root = _ROOTS.get(0);
    Bean sub = root.getKids().get(1);

    int[] testPath = {0, 1};
    _followPath(model, testPath);
    Object key = model.getRowKey();
    model.setRowKey(null);
    _testTree(model, _ROOTS);

    model.setRowKey(key);
    _testTree(model, sub);
  }
  
  public void testGetContainerRowKey() throws IntrospectionException
  {
    TreeModel model = createModel();
    int[] testPath1 = {0, 1};
    _followPath(model, testPath1);
    Object path1 = model.getRowKey();

    int[] testPath2 = {0, 1, 1};
    _followPath(model, testPath2);
    Object path2 = model.getRowKey();
        
    model.setRowKey(null);
    assertEquals("getContainerRowKey(key)", path1, model.getContainerRowKey(path2));
  }

  public void testGetDepth() throws IntrospectionException
  {
    TreeModel model = createModel();
    int[] testPath1 = {0, 1};
    _followPath(model, testPath1);
    assertEquals("getDepth", testPath1.length - 1, model.getDepth());
    Object path1 = model.getRowKey();

    int[] testPath2 = {0, 1, 1};
    _followPath(model, testPath2);
    assertEquals("getDepth", testPath2.length - 1, model.getDepth());
    Object path2 = model.getRowKey();
        
    model.setRowKey(null);
    assertEquals("getDepth(key)", testPath1.length - 1, model.getDepth(path1));
    assertEquals("getDepth(key)", testPath2.length - 1, model.getDepth(path2));
  }

  /**
   * Tests getRowData, isContainer, enterContainer, exitContainer, getRowCount,
   * setRowIndex, getRowIndex, getContainerRowKey
   */
  private void _testTree(TreeModel model, List<Bean> data)
  {
    int sz = data.size();
    assertEquals("rowCount", sz, model.getRowCount());
    assertEquals("initial rowIndex", -1, model.getRowIndex());

    if (sz > 0)
    {
      int oldIndex = model.getRowIndex();
      for(int i=0; i<sz; i++)
      {
        Bean child = data.get(i);
        model.setRowIndex(i);
        assertEquals("rowIndex before enterContainer", i, model.getRowIndex());
        _testTree(model, child);
        assertEquals("rowIndex after exitContainer", i, model.getRowIndex());
      }
      model.setRowIndex(oldIndex);
    }
  }

  /**
   * Tests getRowData, isContainer, enterContainer, exitContainer, getRowCount,
   * setRowIndex, getRowIndex, getContainerRowKey
   */
  private void _testTree(TreeModel model, Bean bean)
  {
    assertEquals("rowData", bean, model.getRowData());
    
    List<Bean> kids = bean.getKids();
    boolean hasChildren = (kids != null);
    assertEquals("isContainer", hasChildren, model.isContainer());
    
    if (hasChildren)
    {
      Object parentKey = model.getRowKey();
      model.enterContainer();
      assertEquals("getContainerRowKey", parentKey, model.getContainerRowKey());
      
      _testTree(model, kids);
      model.exitContainer();
      assertEquals("rowData after exit", bean, model.getRowData());
      assertEquals("isContainer after exit", hasChildren, model.isContainer());
    }
  }

  public static TreeModel createModel() throws IntrospectionException
  {
    TreeModel model = new ChildPropertyTreeModel(_ROOTS, "kids");
    return model;
  }
  
  private static final List<Bean> _ROOTS = _createTree();
  
  private static List<Bean> _createTree()
  {
    List<Bean> roots = new ArrayList<Bean>(3);

    Bean root, sub, subsub;

    roots.add(root = new Bean()); // path = 0
      root.addKid(new Bean());    // path = 0,0
      root.addKid(sub = new Bean()); // path = 0,1
        sub.addKid(new Bean());
        sub.addKid(subsub = new Bean()); // path=0,1,1
          subsub.addKid(new Bean());
          subsub.addKid(new Bean());
          subsub.addKid(new Bean());
        sub.addKid(new Bean());
  
        
    roots.add(new Bean()); // path = 1

    roots.add(root = new Bean());
      root.addKid(sub = new Bean()); // path = 2,0
        sub.addKid(new Bean());
        sub.addKid(new Bean());

    return Collections.unmodifiableList(roots);
  }
  
  
  public static final class Bean
  {
    public List<Bean> getKids()
    {
      return _kids;
    }
    
    protected void addKid(Bean kid)
    {
      if (_kids == null)
      {
        _kids = new ArrayList<Bean>(3);
      }

      _kids.add(kid);
    }
    
    private List<Bean> _kids = null;
  }
}
