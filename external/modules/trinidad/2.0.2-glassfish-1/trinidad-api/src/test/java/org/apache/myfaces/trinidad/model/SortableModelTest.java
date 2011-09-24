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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.faces.model.DataModel;
import javax.faces.model.ListDataModel;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.model.SortableModel;
import org.apache.shale.test.base.AbstractJsfTestCase;

/**
 */
public class SortableModelTest extends AbstractJsfTestCase
{
  public SortableModelTest(String name)
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
    return new TestSuite(SortableModelTest.class);
  }
  
  public void testInitialSort()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _testInitialSort(sModel);
  }

  private void _testInitialSort(SortableModel sModel)
  {
    sModel.setRowIndex(0);
    assertTrue(sModel.getRowData() == _bean1);
    sModel.setRowIndex(1);
    assertTrue(sModel.getRowData() == _bean2);
    sModel.setRowIndex(2);
    assertTrue(sModel.getRowData() == _bean3);
  }
  
  public void testRowCount()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    assertTrue(sModel.getRowCount() == dModel.getRowCount());    
    _sort(sModel, "name", true);    
    assertTrue(sModel.getRowCount() == dModel.getRowCount());    
  }

  public void testSortableProperties()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    assertTrue(sModel.isSortable("age"));
    assertTrue(sModel.isSortable("name"));
    assertFalse(sModel.isSortable("object"));
  }

  public void testSortAscending()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _sort(sModel, "name", true);    


//    for (int i=0; i<sModel.getRowCount(); i++)
//    {
//      sModel.setRowIndex(i);
//      System.out.println("i:"+i+" data:"+sModel.getRowData());
//    }

    sModel.setRowIndex(0);
    assertTrue(sModel.getRowData() == _bean2);
    sModel.setRowIndex(1);
    assertTrue(sModel.getRowData() == _bean1);
    sModel.setRowIndex(2);
    assertTrue(sModel.getRowData() == _bean3);
  }
  
  public void testUnsort()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _sort(sModel, "name", true);    

    sModel.setSortCriteria(null);
    _testInitialSort(sModel);
  }
  
  public void testSortDescending()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _sort(sModel, "age", false);    
    sModel.setRowIndex(0);
    assertTrue(sModel.getRowData() == _bean2);
    sModel.setRowIndex(1);
    assertTrue(sModel.getRowData() == _bean3);
    sModel.setRowIndex(2);
    assertTrue(sModel.getRowData() == _bean1);
  }
  

  // Test sorting an empty model - see bug 4258884
  public void testEmptyTable()
  {
    DataModel emptyModel = new ListDataModel(new ArrayList<Bean>());
    SortableModel sModel = new SortableModel(emptyModel);
    _sort(sModel, "age", false);    
  }

  public void testGetRowIndex()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _sort(sModel, "age", true);    

    dModel.setRowIndex(2); //Zach
    assertTrue(sModel.getRowIndex() == 1);
    dModel.setRowIndex(0); //Tracy
    assertTrue(sModel.getRowIndex() == 0);
    dModel.setRowIndex(1); //Anne
    assertTrue(sModel.getRowIndex() == 2);
  }

  public void testSetRowIndex()
  {
    DataModel dModel = _createTestDataModel();
    SortableModel sModel = new SortableModel(dModel);
    _sort(sModel, "age", true);    

    sModel.setRowIndex(1); //Zach
    assertTrue(dModel.getRowIndex() == 2);
    sModel.setRowIndex(0); //Tracy
    assertTrue(dModel.getRowIndex() == 0);
    sModel.setRowIndex(2); //Anne
    assertTrue(dModel.getRowIndex() == 1);
  }
  
  private DataModel _createTestDataModel()
  {
    List<Bean> list = new ArrayList<Bean>(3);
    list.add(_bean1);
    list.add(_bean2);
    list.add(_bean3);
    return new ListDataModel(Collections.unmodifiableList(list));
  }

  private void _sort(CollectionModel model, 
                     String property, boolean isAscending)
  {
    SortCriterion criterion = new SortCriterion(property, isAscending);
    model.setSortCriteria(Collections.singletonList(criterion));
  }
  
  private final Bean _bean1 = new Bean(10, "Tracy");
  private final Bean _bean2 = new Bean(15, "Anne");
  private final Bean _bean3 = new Bean(12, "Zach");
  
  public static final class Bean
  {
    public Bean(int age, String name)
    {
      _age = age;
      _name = name;
      _obj = new Object();
    }
    
    public int getAge()
    {
      return _age;
    }
    
    public String getName()
    {
      return _name;
    }
    
    // this is not sortable
    public Object getObject()
    {
      return _obj;
    }
    
    @Override
    public String toString()
    {
      return "name:"+_name+" age:"+_age;
    }
    
    private final int _age;
    private final String _name;
    private final Object _obj;
  }
}
