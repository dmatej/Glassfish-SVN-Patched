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

import java.util.HashMap;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.el.PropertyResolver;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Creates a MenuModel from a TreeModel where nodes in the treeModel contain
 * viewId information.
 * <p>
 * Each node must have either a bean getter method or a Map property
 * that returns a viewId. There are several restrictions on the data:
 * <ul>
 * <li>The nodes in the tree must either be all beans or all maps,
 * but not a mix of beans and maps.
 * <li>The viewId of a node can be null, but if set it must be unique.
 * <li>The tree cannot be mutable.
 * </ul>
 * The getFocusPath method
 * <ul>
 * <li>gets the current viewId by calling
 * FacesContext.getCurrentInstance().getViewRoot().getViewId()
 * <li> compares the current viewId with the viewId's in
 * the nodes of the tree
 * <li>returns the path to the node with the current viewId or null if the current viewId can't be found
 * </ul>
 * <p>
 * Assuming that NavigationTree is a tree of beans with a vieId getter, an
 * example of creating a MenuModel with this class might look like:
 * <pre><code>
 *     CollectionModel collectionModel = ModelUtils.toCollectionModel(new NavigationTree());
 *     TreeModel treeModel = new ChildPropertyTreeModel(collectionModel, "children");
 *     MenuModel menuModel = new ViewIdPropertyMenuModel(treeModel, "viewId");
 * </code></pre>
 */
// TODO - support for mutable trees?
public class ViewIdPropertyMenuModel extends BaseMenuModel
{
  /**
   * No-arg constructor for use with managed-beans.
   * Must call the {@link #setViewIdProperty} and
   * {@link #setWrappedData} methods after constructing this instance.
   */
  public ViewIdPropertyMenuModel()
  {
    super();
    _focusPathMap = new HashMap<Object, Object>();
  }
  /**
   *
   * @param instance a treeModel. This object will be passed to
   * {@link ModelUtils#toTreeModel}
   * @param viewIdProperty the property to use to retrieve a viewId
   * from a node in the tree
   * @throws IntrospectionException
   */
  public ViewIdPropertyMenuModel(Object instance, String viewIdProperty)
    throws IntrospectionException
  {
    super(instance);
    _focusPathMap = new HashMap<Object, Object>();
    setViewIdProperty(viewIdProperty);
    setWrappedData(instance);
  }

  @Override
  public void setWrappedData(Object data)
  {
    super.setWrappedData(data);
    Object oldPath = getRowKey();

    //set the focus path map
    _focusPathMap.clear();
    setRowKey(null);
    FacesContext context = FacesContext.getCurrentInstance();
    _addToMap(context, this, _focusPathMap, getViewIdProperty());
    setRowKey(oldPath);
  }

  /**
   * Returns the rowKey to the current viewId.
   * <p>
   *
   * The getFocusRowKey method
   * <ul>
   * <li>gets the current viewId by calling
   * FacesContext.getCurrentInstance().getViewRoot().getViewId()
   * <li> compares the current viewId with the viewId's in
   * the nodes of the tree
   * <li>returns the rowKey to the node with the current viewId or null
   * if the current viewId can't be found
   * </ul>
   *
   * @return  the rowKey to the node with the current viewId or null if the current viewId can't be found
   */
  @Override
  public Object getFocusRowKey()
  {
    String currentViewId = getCurrentViewId();
    Object focusPath = _focusPathMap.get(currentViewId);
    return focusPath;
  }

  /**
   * Maps the focusPath returned when the viewId is newViewId
   * to the focusPath returned when the viewId is aliasedViewId.
   * This allows view id's not in the treeModel to be mapped
   * to a focusPath.
   * @param newViewId the view id to add a focus path for
   * @param aliasedViewId the view id to use to get the focusPath to use for newViewId
   */
  public void addViewId(
    String newViewId,
    String aliasedViewId
  )
  {
    Object focusPath = _focusPathMap.get(aliasedViewId);
    if (focusPath != null)
    {
      _focusPathMap.put(newViewId, focusPath);
    }
  }

  /**
   * Gets the property to use to retrieve a viewId
   * from a node in the tree
   */
  public String getViewIdProperty()
  {
    return _viewIdProperty;
  }

  /**
   * Sets the property to use to retrieve a viewId
   * from a node in the tree
   */
  public void setViewIdProperty(String viewIdProperty)
  {
    _viewIdProperty = viewIdProperty;
  }

  /**
   * Returns the current viewId.
   * <p>
   *
   *
   * @return  the current viewId or null if the current viewId can't be found
   */

  protected String getCurrentViewId()
  {
    String currentViewId =
                   FacesContext.getCurrentInstance().getViewRoot().getViewId();

    return currentViewId;
  }


  private static void _addToMap(
    FacesContext context,
    TreeModel tree,
    Map<Object, Object> focusPathMap,
    String viewIdProperty
    )
  {
    for ( int i = 0; i < tree.getRowCount(); i++)
    {
      tree.setRowIndex(i);
      if (viewIdProperty != null)
      {
        Object focusPath = tree.getRowKey();
        Object data = tree.getRowData();
        PropertyResolver resolver =
          context.getApplication().getPropertyResolver();
        Object viewIdObject = resolver.getValue(data, viewIdProperty);
        focusPathMap.put(viewIdObject, focusPath);
      }
      else
      {
        _LOG.warning("NULL_VIEWID");
      }

      if (tree.isContainer() && !tree.isContainerEmpty())
      {
        tree.enterContainer();
        _addToMap(context, tree, focusPathMap, viewIdProperty);
        tree.exitContainer();
      }

    }
  }


  private final Map<Object, Object> _focusPathMap;
  private String _viewIdProperty = null;



  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ViewIdPropertyMenuModel.class);
}
