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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.PropertyNotFoundException;
import javax.faces.el.ValueBinding;
import javax.faces.render.Renderer;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestSuite;

import junit.textui.TestRunner;

import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.SortableModel;

/**
 * Unit tests for UIXTable
 *
 */
public class UIXTableTest extends UIComponentTestCase
{
  /**
   * @param testName  the unit test name
   */
  public UIXTableTest(
    String testName)
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
    return new TestSuite(UIXTableTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    UIXTable table = _createTable();
    assertEquals(25, table.getRows());
    assertEquals(0, table.getFirst());
    assertFalse(table.isImmediate());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    UIXTable table = _createTable();
    doTestAttributeTransparency(table, "var", "row", "emp");
    doTestAttributeTransparency(table, "value", "row", "emp");
    doTestAttributeTransparency(table, "first", new Integer(0), new Integer(1));
    doTestAttributeTransparency(table, "immediate", Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(table, "rows", new Integer(30), new Integer(10));
  }

  public void testModelMethods()
  {
    TestTable table = _createTable();
    SortableModel model = table.model;
    int sz = model.getRowCount();
    assertEquals(sz, table.getRowCount());
    for(int i=0; i<sz; i++)
    {
      table.setRowIndex(i);
      assertEquals(i, model.getRowIndex());
      assertEquals(model.getRowKey(), table.getRowKey());
      assertEquals(model.isRowAvailable(), table.isRowAvailable());
      assertEquals(model.getRowData(), table.getRowData());
    }
    table.setRowIndex(-1);
    assertEquals(-1, model.getRowIndex());
    assertEquals(model.getRowKey(), table.getRowKey());
    assertEquals(model.isRowAvailable(), table.isRowAvailable());
  }

  public void testProcessDecodes()
  {
    TestTable table = _createTable();
    table.setRows(10);
    table.setFirst(3);
    doTestApplyRequestValues(table);
    _testColumnChild(table, table.column1Child.getDecodesRowData());
    _testColumnChild(table, table.column2Child.getDecodesRowData());

    assertEquals(1, table.column1Header.getDecodesRowData().size());
    assertEquals(1, table.column2Header.getDecodesRowData().size());

    List<Object> detailData = table.detailStamp.getDecodesRowData();
    _testDetailStamp(table, detailData);

  }

  public void testProcessValidators()
  {
    TestTable table = _createTable();
    table.setRows(10);
    table.setFirst(3);
    doTestProcessValidations(table);
    _testColumnChild(table, table.column1Child.getValidatesRowData());
    _testColumnChild(table, table.column2Child.getValidatesRowData());

    assertEquals(1, table.column1Header.getValidatesRowData().size());
    assertEquals(1, table.column2Header.getValidatesRowData().size());

    List<Object> detailData = table.detailStamp.getValidatesRowData();
    _testDetailStamp(table, detailData);

  }

  public void testProcessUpdates()
  {
    TestTable table = _createTable();
    table.setRows(10);
    table.setFirst(3);
    doTestUpdateModelValues(table);
    _testColumnChild(table, table.column1Child.getUpdatesRowData());
    _testColumnChild(table, table.column2Child.getUpdatesRowData());

    assertEquals(1, table.column1Header.getUpdatesRowData().size());
    assertEquals(1, table.column2Header.getUpdatesRowData().size());

    List<Object> detailData = table.detailStamp.getUpdatesRowData();
    _testDetailStamp(table, detailData);

  }

  public void testEditableValueHolderChildren()
  {
    TestTable table = _createTable();
    UIXInput testComp = table.column1Child;
    UIXInput detailStamp = table.detailStamp;

    // initialize:
    table.setRowIndex(0);
    _setEVHData(testComp, "Foo", true);
    _setEVHData(detailStamp, "Foo-ds", false);

    table.setRowIndex(1);
    _setEVHData(testComp, "Bar", false);
    _setEVHData(detailStamp, "Bar-ds", true);

    // now test:
    table.setRowIndex(0);
    _testEVHData(testComp, "Foo", true);
    _testEVHData(detailStamp, "Foo-ds", false);

    table.setRowIndex(1);
    _testEVHData(testComp, "Bar", false);
    _testEVHData(detailStamp, "Bar-ds", true);

  }


  public void testSaveRestoreState()
  {
    final Object state;
    {
      TestTable table = _createTable();

      UIXInput testComp = table.column1Child;

      // initialize:
      table.setRowIndex(0);
      _setEVHData(testComp, "Foo", true);

      table.setRowIndex(1);
      _setEVHData(testComp, "Bar", false);

      // note that we did not set rowIndex back to -1.
      // processSaveState should store the stamp data even though rowIndex was
      // not changed.

      state = table.processSaveState(facesContext);
    }

    TestTable table = _createTable();
    UIXInput testComp = table.column1Child;
    table.processRestoreState(facesContext, state);

    // now test:
    table.setRowIndex(0);
    _testEVHData(testComp, "Foo", true);

    table.setRowIndex(1);
    _testEVHData(testComp, "Bar", false);

  }

  /**
   * make sure that the model is not executed at invalid or unnecessary times.
   * valueBindings cannot be called during restoreState.
   * Also table model must not be executed if rendered="false".
   * However, saveState is called even if rendered="false" on a component.
   * Therefore, saveState should not call getValue() on the table.
   */
  public void testSaveRestoreStateGetValue()
  {
    // make sure that getValue() is not called during restoreState:
    DoNotCallBinding doNotCall = new DoNotCallBinding();
    doNotCall.doNotCall = true;
    final Object state;
    {
      TestTable table = _createTable(false);
      table.setValue(null); // instead use the valueBinding below:
      table.setValueBinding("value", doNotCall);
      state = table.processSaveState(facesContext);
    }

    TestTable table = _createTable(false);
    table.setValue(null); // instead use the valueBinding below:
    // this value binding should be restored during processRestoreState;
    // however, set it anyway just to catch any getValue() calls prior to
    // that.
    table.setValueBinding("value", doNotCall);
    table.processRestoreState(facesContext, state);

    assertTrue(table.getValueBinding("value") instanceof DoNotCallBinding);

  }

  @SuppressWarnings("unchecked")
  public void testNotRenderedSaveRestoreState()
  {
    // this test is to make sure no exceptions are thrown during save/restore
    // state of the table. Exceptions were being thrown earlier because
    // model and certain initial state were computed lazily.
    final Object state;
    {
      UIXPanel panel = new UIXPanel();
      TestTable table = _createTable(false);
      table.setRendered(false);
      panel.getChildren().add(table);
      state = panel.processSaveState(facesContext);
    }

    UIXPanel panel = new UIXPanel();
    TestTable table = _createTable(false);
    panel.getChildren().add(table);

    panel.processRestoreState(facesContext, state);

  }

  @SuppressWarnings("unchecked")
  @Override
  protected void doTestUpdateModelValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    root.getChildren().add(component);
    root.processUpdates(context);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void doTestProcessValidations(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    root.getChildren().add(component);
    root.processValidators(context);
  }

  @Override
  public void setCurrentContext(FacesContext fc)
  {
    // prevent removal of facesContext before we are done testing:
    if (fc != null)
      super.setCurrentContext(fc);
  }

  private void _setEVHData(
    EditableValueHolder testComp,
    String value,
    boolean isValid)
  {
    testComp.setSubmittedValue("submitedValue-"+value);
    testComp.setValue("Value-"+value);
    testComp.setValid(isValid);
  }

  private void _testEVHData(
    EditableValueHolder testComp,
    String value,
    boolean isValid)
  {
    assertEquals("submitedValue-"+value, testComp.getSubmittedValue());
    assertEquals("Value-"+value, testComp.getLocalValue());
    assertEquals(isValid, testComp.isValid());
  }

  private void _testDetailStamp(TestTable table, List<Object> detailData)
  {
    assertEquals(1, detailData.size());
    table.setRowIndex(_DISCLOSED_INDEX);
    assertEquals(table.getRowData(), detailData.get(0));
  }

  private void _testColumnChild(TestTable table, List<Object> rowData)
  {
    // make sure that the rowData values that were seen during this phase
    // were the correct values:
    int rows = table.getRows();
    assertEquals(rows, rowData.size());
    int first = table.getFirst();
    for(int i=0; i<rows; i++)
    {
      table.setRowIndex(i+first);
      assertEquals(table.getRowData(), rowData.get(i));
    }
  }

  public static void main(String[] args)
  {
    TestRunner.run(UIXTableTest.class);
//    UIXTableTest test = new UIXTableTest("aasd");
//    test.testSaveRestoreStateGetValue();
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void doTestApplyRequestValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    root.getChildren().add(component);
    root.processDecodes(context);
  }

  @Override
  protected boolean isRendererUsed()
  {
    // we use our own MockRenderer; not the one created by our super class:
    return false;
  }

  private TestTable _createTable()
  {
    return _createTable(true);
  }

  private TestTable _createTable(boolean useModel)
  {
    SortableModel model = useModel ? _createTableData() : null;
    TestTable table = new TestTable(model);
    return table;
  }

  private static SortableModel _createTableData()
  {
    final int sz = 25;
    List<Object> data = new ArrayList<Object>(sz);
    for(int i=0; i<sz; i++)
    {
      data.add(new Integer(i));
    }
    return new SortableModel(ModelUtils.toDataModel(data));
  }

  // must be public static so that it can be state-saved:
  public static final class DoNotCallBinding extends ValueBinding
    implements StateHolder
  {
    @Override
    public Class<?> getType(FacesContext context) throws EvaluationException, PropertyNotFoundException
    {
      throw new AssertionFailedError("This method should not be called");
    }

    @Override
    public Object getValue(FacesContext context) throws EvaluationException, PropertyNotFoundException
    {
      if (doNotCall)
        throw new AssertionFailedError("This method should not be called");

      return _createTableData();
    }

    @Override
    public boolean isReadOnly(FacesContext context) throws EvaluationException, PropertyNotFoundException
    {
      throw new AssertionFailedError("This method should not be called");
    }

    @Override
    public void setValue(FacesContext context, Object value) throws EvaluationException, PropertyNotFoundException
    {
      throw new AssertionFailedError("This method should not be called");
    }

    public Object saveState(FacesContext context)
    {
      return Boolean.FALSE;
    }

    public void restoreState(FacesContext context, Object state)
    {
      // apon restoring state, to not allow getValue() calls:
      doNotCall = true;
    }

    public boolean isTransient()
    {
      return false;
    }

    public void setTransient(boolean newTransientValue)
    {
    }

    public boolean doNotCall = false;
  }

  private static final class TestComponent extends UIXInput
  {
    public TestComponent(String id)
    {
      _decodes   = Collections.emptyList();
      _updates   = Collections.emptyList();
      _validates = Collections.emptyList();
      setId(id);
    }

    @Override
    public void processDecodes(FacesContext fc)
    {
      _decodes = _addRowData(fc, _decodes);
    }

    @Override
    public void processUpdates(FacesContext fc)
    {
      _updates = _addRowData(fc, _updates);
    }

    @Override
    public void processValidators(FacesContext fc)
    {
      _validates = _addRowData(fc, _validates);
    }

    public List<Object> getDecodesRowData()
    {
      return _decodes;
    }

    public List<Object> getValidatesRowData()
    {
      return _validates;
    }

    public List<Object> getUpdatesRowData()
    {
      return _updates;
    }

    @Override
    public String toString()
    {
      return getId();
    }

    @Override
    protected Renderer getRenderer(FacesContext fc)
    {
      // for testing purposes must return null. Otherwise a renderer
      // will be needed to compute the clientID:
      return null;
    }

    private List<Object> _addRowData(FacesContext fc, List<Object> currList)
    {
      if (currList == Collections.emptyList())
        currList = new ArrayList<Object>(10);

      Object rowData = fc.getExternalContext().getRequestMap().get(_VAR);
      currList.add(rowData);
      return currList;
    }

    private List<Object> _decodes;
    private List<Object> _updates;
    private List<Object> _validates;
  }

  private static final String _VAR = "row";

  private static final class TestTable extends UIXTable
  {
    @SuppressWarnings("unchecked")
    public TestTable(SortableModel model)
    {
      super();
      setValue(model);
      this.model = model;
      setVar(_VAR);
      setId("table1");

      UIXColumn column1 = new UIXColumn()
      {
        @Override
        protected Renderer getRenderer(FacesContext fc)
        {
          // for testing purposes must return null. Otherwise a renderer
          // will be needed to compute the clientID:
          return null;
        }
      };
      column1.setId("col1");
      column1Header = new TestComponent("col1Header");
      column1.setHeader(column1Header);
      column1Child = new TestComponent("col1Child");
      column1.getChildren().add(column1Child);

      UIXColumn column2 = new UIXColumn()
      {
        @Override
        protected Renderer getRenderer(FacesContext fc)
        {
          // for testing purposes must return null. Otherwise a renderer
          // will be needed to compute the clientID:
          return null;
        }
      };
      column2.setId("col2");
      column2Header = new TestComponent("col2Header");
      column2.setHeader(column2Header);
      column2Child = new TestComponent("col2Child");
      column2.getChildren().add(column2Child);

      List<UIComponent> kids = getChildren();
      kids.add(column1);
      kids.add(column2);

      detailStamp = new TestComponent("detail");
      setDetailStamp(detailStamp);

      if (model != null)
      {
        setRowIndex(_DISCLOSED_INDEX);
        getDisclosedRowKeys().add();
        setRowIndex(-1);
      }
    }

    @Override
    protected Renderer getRenderer(FacesContext fc)
    {
      // for testing purposes must return null. Otherwise a renderer
      // will be needed to compute the clientID:
      return null;
    }

    public final SortableModel model;
    public final TestComponent column1Header, column1Child,
      column2Header, column2Child, detailStamp;
  }

  private static final int _DISCLOSED_INDEX = 5;
}
