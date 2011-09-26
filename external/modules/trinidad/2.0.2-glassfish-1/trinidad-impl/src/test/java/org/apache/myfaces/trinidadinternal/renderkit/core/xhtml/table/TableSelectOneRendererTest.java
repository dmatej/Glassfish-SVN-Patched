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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.event.FacesEvent;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.context.MockRequestContext;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;

import org.apache.myfaces.trinidadbuild.test.FacesTestCase;
import org.jmock.Mock;


/**
 */
public class TableSelectOneRendererTest extends FacesTestCase
{

  public TableSelectOneRendererTest(String testName)
  {
    super(testName);
  }

  @Override
  protected void setUp() throws Exception
  {
    super.setUp();
    _mockRequestContext = new MockRequestContext();
  }

  @Override
  protected void tearDown() throws Exception
  {
    _mockRequestContext.release();
    super.tearDown();
  }

  public static Test suite()
  {
    return new TestSuite(TableSelectOneRendererTest.class);
  }

  /**
   * if there are no selection parameters on the request, then the current
   * selectedIndex should not change:
   */
  public void testDecodeNothing()
  {

    CoreTable table = _createComponent();
    _doDecode(table, -1);
    _testSelection(table, _INIT_SELECTION);
  }

  /**
   * if there is a new selectedIndex on the request, then it must be set on
   * the component:
   */
  public void testDecodeSelected()
  {
    final int selectedIndex = 4;

    TestTable table = (TestTable) _createComponent();
    _doDecode(table, selectedIndex);

    SelectionEvent event = (SelectionEvent) table.event;

    assertNotNull(event);

    RowKeySet unselect = event.getRemovedSet();
    RowKeySet select = event.getAddedSet();
    int oldIndex = table.getRowIndex();

    table.setRowIndex(_INIT_SELECTION);
    assertTrue(unselect.isContained());
    assertFalse(select.isContained());

    table.setRowIndex(selectedIndex);
    assertFalse(unselect.isContained());
    assertTrue(select.isContained());

    table.setRowIndex(oldIndex);
    _testSelection(table, selectedIndex);
  }

  private CoreTable _createComponent()
  {
    String[] data = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};
    CoreTable table = new TestTable();
    table.setId(_TABLE_ID);
    table.setValue(data);
    table.setRowIndex(_INIT_SELECTION);
    table.getSelectedRowKeys().add();
    table.setRowIndex(-1);
    table.setRowSelection("single");
    return table;
  }

  private void _testSelection(CoreTable table,
                              int expectedSelectedIndex)
  {
    table.setRowIndex(expectedSelectedIndex);
    RowKeySet state = table.getSelectedRowKeys();
    assertTrue("row is selected", state.isContained());
    Iterator<Object> selection = state.iterator();
    // make sure there is exactly one selected item:
    assertTrue("has one selected item", selection.hasNext());
    selection.next();
    assertFalse("has one selected item", selection.hasNext());
  }

  private void _doDecode(CoreTable table,
                         int selectedIndex)
  {
    //this.facesContext.setViewRoot(new UIViewRoot());
    Mock mockRenderKit = getMockRenderKitWrapper().getMock();
    TableSelectOneRenderer renderer = new TableSelectOneRenderer(CoreTable.TYPE);
    mockRenderKit.expects(atLeastOnce()).method("getRenderer").will(returnValue(renderer));


    if (selectedIndex >= 0)
    {
      int oldIndex = table.getRowIndex();
      table.setRowIndex(selectedIndex);
      String selectedParam = table.getCurrencyString();
      table.setRowIndex(oldIndex);

      Map<String, String> requestParams = new HashMap<String, String>(2);
      String selectionParam =
        TableSelectOneRenderer.__getSelectionParameterName(facesContext, table);
      requestParams.put(selectionParam, selectedParam);
      externalContext.setRequestParameterMap(requestParams);
    }
    else
    {
      externalContext.setRequestParameterMap(Collections.EMPTY_MAP);
    }


    renderer.decode(facesContext, table);

    mockRenderKit.verify();

  }

  private static class TestTable extends CoreTable
  {
    public TestTable()
    {
      super();
      setSelectedRowKeys(new RowKeySetImpl());
      setDisclosedRowKeys(new RowKeySetImpl());
    }

    @Override
    public void queueEvent(FacesEvent event)
    {
      this.event = event;
    }

    public FacesEvent event = null;
  }


  private MockRequestContext _mockRequestContext;
  private static final String _TABLE_ID = "table1";
  private static final int _INIT_SELECTION = 3;
}
