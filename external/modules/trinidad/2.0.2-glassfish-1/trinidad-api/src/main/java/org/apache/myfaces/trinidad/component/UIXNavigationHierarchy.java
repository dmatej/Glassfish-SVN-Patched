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

import org.apache.myfaces.buildtools.maven2.plugin.builder.annotation.JSFComponent;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.MenuModel;
import org.apache.myfaces.trinidad.model.ModelUtils;


/**
 * Base class for the Navigation component.
 * <p>
 * Work on modeling navigation continues and it is very possible that this class
 * will change in a future release.
 */
// TODO these base classes need to be completely refactored
@JSFComponent
abstract public class UIXNavigationHierarchy extends UIXHierarchy
{

  /**
   * Create a Page component with the given render-type
   */
  protected UIXNavigationHierarchy(String rendererType)
  {
    super(rendererType);
  }

  protected UIXNavigationHierarchy()
  {
    this(null);
  }

  @Override
  public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    MenuModel model = ModelUtils.toMenuModel(value);
    model.setRowKey(null);
    return model;
  }



  /**
   * gets the MenuModel that this page is displaying.
   */
  protected MenuModel getMenuModel()
  {
    return (MenuModel)getCollectionModel();
  }

  /**
   * Gets the focus row for the current viewId.
   * @see MenuModel#getFocusRowKey
   * @return the focus rowKey for the current viewId
   */
  @Override
  public Object getFocusRowKey()
  {
    MenuModel model = getMenuModel();
    if (model != null)
    {
      Object currPath  = model.getRowKey();      
      Object focusPath = model.getFocusRowKey();

      // The row key should not change as a result of calling getFocusRowKey()
      assert(((currPath == null) && (model.getRowKey() == null)) ||
             ((currPath != null) && currPath.equals(model.getRowKey())));
      return focusPath;
    }

    return null;
  }

}
