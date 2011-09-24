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


/**
 * A base class which takes a TreeModel. Developers can extend this class and
 * just override the getFocusRowKey() method.
 *
 */
public abstract class BaseMenuModel extends MenuModel
{

  /**
   *
   * @param modelObject the treeModel to use, this object will be passed to
   * {@link ModelUtils#toTreeModel}.
   */
  public BaseMenuModel(Object modelObject)
  {
    _treeModel = ModelUtils.toTreeModel(modelObject);
  }

  /**
   * no-arg constructor needed for managed-bean support.
   * {@link #setWrappedData} must be called soon after constructing this
   * instance.
   */
  protected BaseMenuModel()
  {
  }

  @Override
  public Object getContainerRowKey(Object childKey)
  {
    return _treeModel.getContainerRowKey(childKey);
  }

  @Override
  public void enterContainer()
  {
    _treeModel.enterContainer();
  }

  @Override
  public void exitContainer()
  {
    _treeModel.exitContainer();
  }

  @Override
  public int getRowCount()
  {
    return _treeModel.getRowCount();
  }

  @Override
  public Object getRowData()
  {
    return _treeModel.getRowData();
  }

  @Override
  public int getRowIndex()
  {
    return _treeModel.getRowIndex();
  }

  @Override
  public Object getRowKey()
  {
    return _treeModel.getRowKey();
  }

  @Override
  public Object getWrappedData()
  {
    // since we don't know how to adapt an ordinary POJO into a
    // treeModel, the wrappedData itself is a treeModel. so just return it:
    return _treeModel;
  }

  @Override
  public boolean isContainer()
  {
    return _treeModel.isContainer();
  }

  @Override
  public boolean isRowAvailable()
  {
    return _treeModel.isRowAvailable();
  }

  @Override
  public void setRowIndex(int index)
  {
    _treeModel.setRowIndex(index);
  }

  @Override
  public void setRowKey(Object key)
  {
    _treeModel.setRowKey(key);
  }

  @Override
  public void setWrappedData(Object data)
  {
    // since we don't know how to adapt an ordinary POJO into a
    // treeModel, the ModelUtils call is currently no-op:
    _treeModel = ModelUtils.toTreeModel(data);
  }

  private TreeModel _treeModel = null;
}
