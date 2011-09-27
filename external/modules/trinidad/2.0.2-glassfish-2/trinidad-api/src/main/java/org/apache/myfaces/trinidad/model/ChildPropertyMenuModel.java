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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.model.DataModel;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.CollectionUtils;

/**
 * Creates a MenuModel from a List of beans.
 * To use this class you must have a tree of beans (or Maps).
 * The structure of your tree must be a List (or array) of root beans, and each bean must have a
 * getter method (or Map property) that returns the children List. All elements of your tree must be
 * the same type.
 * <P>
 * Suppose you have a bean called EmpBean that contains the data for a particular employee. Suppose
 * this bean has a method called getDirectReports() that returns a List of EmpBeans which are the
 * direct reports of the employee. Suppose there is a List called "founders" which is the root list
 * of EmpBeans. Now you can construct a MenuModel by calling:
 * <pre>
 * MenuModel model = new ChildPropertyMenuModel(founders, "directReports");
 * </pre>
 * Bean rules will be used to find an appropriate getter method for the "directReports" property.
 * java.util.Maps are also supported instead of beans.
 * <p>
 * Example: Given the following class:
 * <pre>
 * public class Person
 * {
 *    public Person(String name)
 *    {
 *      _name = name;
 *    }
 *
 *    public String getName()
 *    {
 *      return _name;
 *    }
 *
 *    public List getKids()
 *    {
 *      return _kids;
 *    }
 *
 *    private final String _name;
 *    private final List _kids = new ArrayList();
 * }
 * </pre>
 * You can construct a tree by:
 * <pre>
 * Person john = new Person("John Smith");
 * Person kim = new Person("Kim Smith");
 * Person tom = new Person("Tom Smith");
 * Person ira = new Person("Ira Wickrememsinghe");
 * Person mallika = new Person("Mallika Wickremesinghe");
 *
 * john.getKids().add(kim);
 * john.getKids().add(tom);
 * ira.getKids().add(mallika);
 *
 * // create the list of root nodes:
 * List people = new ArrayList();
 * people.add(john);
 * people.add(ira);
 * </pre>
 * Now you can construct a MenuModel by:
 * <pre>
 * MenuModel model = new ChildPropertyMenuModel(people, "kids", focusRowKey);
 * </pre>
 */
public class ChildPropertyMenuModel extends MenuModel
{
  /**
   * Creates a MenuModel
   * @param instance The Collection of root nodes of this tree.
   *                 This can be a List or array of beans (or Maps).
   *                 This instance is first converted into a CollectionModel (see
   *                 {@link ModelUtils#toCollectionModel}).
   * @param childProperty This property will be used to get at the child Lists from each bean (or
   *                      Map). Bean rules will be used to find a getter method that matches this
   *                      property. If each node is a Map, this property will be passed in to the
   *                      Map's get method to get the child List.
   * @param focusRowKey null or the row key in focus
   */
  public ChildPropertyMenuModel(Object instance, String childProperty, Object focusRowKey)
  {
    this();
    setChildProperty(childProperty);
    setWrappedData(instance);
    this._focusRowKey = focusRowKey;
  }

  /**
   * No-arg constructor for use with managed-beans.
   * Must call the {@link #setChildProperty} and
   * {@link #setWrappedData} methods after constructing this instance.
   */
  public ChildPropertyMenuModel()
  {
    Node root = new Node(null);
    _path.add(root);
  }

  /**
   * Gets the rowKey of the current row.
   */
  @Override
  public Object getRowKey()
  {
    final int sz = _path.size() - 1;
    Object lastRowkey = _getRowKey();
    if ((sz == 0) && (lastRowkey == null))
      return null;  // root collection
    
    // have to clone the path here. otherwise, we have to say that
    // this tree model cannot be mutated while accessing the path
    // returned by this method.
    List<Object> path = new ArrayList<Object>(sz+1);
    if (sz > 0)
    {
      for(int i=0; i<sz; i++)
      {
        Node node = _getNode(i);
        path.add(node.childModel.getRowKey());
      }
    }
    path.add(lastRowkey);
    return path;
  }

  /**
   * Selects a new current row. The row that matches the given rowKey is made current.
   * @param rowKey use null to access the root collection 
   */
  @SuppressWarnings("unchecked")
  @Override
  public void setRowKey(Object rowKey)
  {
    Node root = _getNode(0);
    _path.clear();
    _path.add(root);
    
    List<Object> path = (List<Object>) rowKey;
    if ((path == null) || (path.size() == 0))
    {
      setRowIndex(-1);
      return;
    }
      
    int lastIndex = path.size() - 1;
    for(int i=0; i<lastIndex; i++)
    {
      Object pathKey = path.get(i);
      _setRowKey(pathKey);
      enterContainer();
    }
    
    _setRowKey(path.get(lastIndex));
  }

  @SuppressWarnings("unchecked")
  @Override
  public Object getContainerRowKey(Object childKey)
  {
    List<Object> path = (List<Object>) childKey;
    if ((path == null) || (path.size() <= 1))
      return null;
    
    // wrap sublist in a Serializable copy, since sublist usually returns non-Serializable
    // instances
    return CollectionUtils.newSerializableList(path.subList(0, path.size() - 1));
  }

  @Override
  public int getRowCount()
  {
    return _getModel().getRowCount();
  }

  @Override
  public Object getRowData()
  {
    return _getModel().getRowData();
  }

  @Override
  public boolean isRowAvailable()
  {
    return _getModel().isRowAvailable();
  }

  @Override
  public boolean isContainer()
  {
    Object rowData = getRowData();
    Object value = getChildData(rowData);
    
    if (value != null)
    {
      if (value instanceof Collection<?>)
      {
        return !((Collection<?>)value).isEmpty();
      }
      else if (value.getClass().isArray())
      {
        return Array.getLength(value) > 0;
      }
      else if (value instanceof DataModel)
      {
        return ((DataModel)value).getRowCount() > 0;
      }
    }
    
    return value != null;
  }

  @Override
  public void enterContainer()
  {
    Object rowData = getRowData();
    if (rowData == null)
      throw new IllegalStateException(_LOG.getMessage("NULL_ROWDATA"));
    Node node = new Node(rowData);
    _path.add(node);
  }

  @Override
  public void exitContainer()
  {
    int sz = _path.size();
    if (sz > 1)
      _path.remove(sz - 1);
    else
      throw new IllegalStateException(_LOG.getMessage("CANNOT_EXIT_ROOT_CONTAINER"));
  }
  
  /**
   * Gets the instance being wrapped by this MenuModel.
   */
  @Override
  public Object getWrappedData()
  {
    return _wrappedData;
  }

  /**
   * Sets the instance being wrapped by this MenuModel.
   * Calling this method sets the path to empty.
   */
  @Override
  public void setWrappedData(Object data)
  {
    Node root = _getNode(0);
    root.childModel = ModelUtils.toCollectionModel(data);
    setRowKey(null);
    _wrappedData = data;
  }

  /**
   * Gets the property name used to fetch the children.
   */
  public final String getChildProperty()
  {
    return _childProperty;
  }

  /**
   * Sets the property name used to fetch the children.
   */
  public final void setChildProperty(String childProperty)
  {
    _childProperty = childProperty;
  }

  @Override
  public int getRowIndex()
  {
    return _getModel().getRowIndex();
  }

  @Override
  public void setRowIndex(int rowIndex)
  {
    _getModel().setRowIndex(rowIndex);
  }

  @Override
  public boolean isSortable(String property)
  {
    return _getModel().isSortable(property);
  }

  @Override
  public List<SortCriterion> getSortCriteria()
  {
    return _getModel().getSortCriteria();
  }

  @Override
  public void setSortCriteria(List<SortCriterion> criteria)
  {
    _getModel().setSortCriteria(criteria);
  }

  /**
   * Gets the child data for a node. This child data will be converted into a CollectionModel by
   * calling {@link #createChildModel}.
   * @param parentData the node to get the child data from
   * @return the List or array that is the child data.
   * must return null for a leaf node.
   */
  protected Object getChildData(Object parentData)
  {
    String prop = getChildProperty();
    if (prop == null)
      return null;
    
    return SortableModel.__resolveProperty(parentData, prop);
  }

  /**
   * Converts childData into a CollectionModel.
   * This method calls {@link ModelUtils#toCollectionModel}
   * @param childData the data to convert. This can be a List or array.
   */
  protected CollectionModel createChildModel(Object childData)
  {
    CollectionModel model = ModelUtils.toCollectionModel(childData);
    model.setRowIndex(-1);
    return model;
  }

  private Object _getRowKey()
  {
    return _getModel().getRowKey();
  }

  private void _setRowKey(Object key)
  {
    _getModel().setRowKey(key);
  }
  
  private Node _getCurrentNode()
  {
    return _getNode(_path.size() - 1);
  }

  private Node _getNode(int index)
  {
    return _path.get(index);    
  }

  private CollectionModel _getModel()
  {
    Node node = _getCurrentNode();
    CollectionModel model = node.childModel;
  
    if (model == null)
    {
      Object value = getChildData(node.parentData);
      model = createChildModel(value);
      node.childModel = model;
    }
    return model;
  }

  public Object getFocusRowKey()
  {
    return _focusRowKey;
  }

  public void setFocusRowKey(Object focusRowKey)
  {
    _focusRowKey = focusRowKey;
  }

  private static final class Node
  {
    public Node(Object parentData)
    {
      this.parentData = parentData;
    }

    public final Object parentData;
    CollectionModel childModel = null;
  }

  private final List<Node> _path = new ArrayList<Node>(5);
  private String _childProperty;
  private Object _wrappedData;
  private Object _focusRowKey;
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ChildPropertyMenuModel.class);
}
