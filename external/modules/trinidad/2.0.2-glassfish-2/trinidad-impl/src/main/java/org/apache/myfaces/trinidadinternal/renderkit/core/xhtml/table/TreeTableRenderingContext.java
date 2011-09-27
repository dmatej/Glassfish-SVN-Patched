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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXColumn;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;

/**
 * Context for caching TreeTable data.
 */
public final class TreeTableRenderingContext extends TableRenderingContext
{

  /**
   * Gets the current TreeTableRenderingContext.
   * Most unfortunately can't call this method getCurrentInstance() because
   * of java inheritance issues.
   */
  public static TreeTableRenderingContext getInstance()
  {
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();
    return (tContext instanceof TreeTableRenderingContext)
      ? (TreeTableRenderingContext) tContext
      : null;
  }

  public TreeTableRenderingContext(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         hGrid)
  {
    super(context, arc, hGrid);

    _hGridBase = (UIXTreeTable) hGrid;

    _pathStamp = _hGridBase.getFacet(XhtmlConstants.PATH_STAMP_CHILD);
    UIComponent nodeStamp = _hGridBase.getNodeStamp();
    if (nodeStamp instanceof UIXColumn)
    {
      _nodeStamp = nodeStamp;
    }
    else
    {
      _LOG.warning("NODESTAMP_FACET_ON_TREETABLE_MISSING_OR_NOT_UIXCOLUMN_TYPE", getTableId());
      _nodeStamp = null;
    }

    _crumbs = TreeUtils.getFocusRowKey(_hGridBase);

    Number spacerWidth = (Number)
      arc.getSkin().getProperty(SkinProperties.AF_TREE_TABLE_SPACER_WIDTH);
    if (spacerWidth != null)
    {
      _spacerWidth = spacerWidth.intValue();
    }
    else
    {
      _spacerWidth = _DEFAULT_SPACER_WIDTH;
    }
    
    _rootNodeRendered = _hGridBase.isRootNodeRendered();
  }

  /**
   * Gets the selection state for this table.
   * This is overwritten in HGridRenderingContext
   */
  @Override
  public RowKeySet getSelectedRowKeys()
  {
    return ((UIXTree) getCollectionComponent()).getSelectedRowKeys();
  }

  /**
   * Gets the focus path.
   */
  public Object getFocusRowKey()
  {
    return _crumbs;
  }

//  /**
//   * adds a row DataObject to the current list of hidden DataObjects.
//   * @param node the DataObject that must be used as the current DataObject
//   * when rendering any hidden form elements on the respective row.
//   */
//  public void addHiddenDataObject(DataObject node)
//  {
//    if (_invisibleNodes==null) _invisibleNodes = new ArrayList(10);
//    _invisibleNodes.add(node);
//  }

//  /**
//   * @param rowIndex the row
//   * @return the table row DataObject at the given rowIndex
//   */
//  public DataObject getHiddenDataObject(int rowIndex)
//  {
//    return (DataObject) _invisibleNodes.get(rowIndex);
//  }

  /**
   * @return the node to use as a stamp for the object hierarchy column
   */
  public UIComponent getTreeNodeStamp()
  {
    return _nodeStamp;
  }

  /**
   * the focus column is not shown if breadcrumbs have been disabled.
   * @return false if no breadcrumbs.
   * @see #getBreadCrumbs()
   */
  public boolean isFocusColumnVisible()
  {
    return (_pathStamp != null);
  }

  /**
   * Get the actual column count, taking selection,  focus
   * and object-hierarchy columns into account.
   */
  @Override
  public int getSpecialColumnCount()
  {
    // we need to add one for the object hierarchy column and possibly another
    // for the focus column
    return super.getSpecialColumnCount() +
      (isFocusColumnVisible() ? 2 : 1);
  }

  /**
   * @return null. HGrids cannot have detail-disclosure.
   */
  @Override
  public UIComponent getDetail()
  {
    return null;
  }

  public UIXTreeTable getUIXTreeTable()
  {
    return _hGridBase;
  }

  public int getSpacerWidth()
  {
    return _spacerWidth;
  }
  
  /**
   * @return if the root node is rendered
   */
  public boolean isRootNodeRendered()
  {
    return _rootNodeRendered;
  }

//  protected boolean computeHasNavigation(UINode table, int rows)
//  {
//    TreeBase comp = (TreeBase) table.getUIComponent();
//    // The treeTable cannot show multiple roots. Therefore use
//    // the navigation bar for navigation of the root collection:
//    if (comp.getDepth(comp.getFocusRowKey()) > 0)
//      return false;
//
//    return super.computeHasNavigation(table, rows);
//  }

  private final UIXTreeTable _hGridBase;
//  private ArrayList _invisibleNodes;
  private final Object _crumbs;
  private final UIComponent _nodeStamp, _pathStamp;
  private final int _spacerWidth;
  private final boolean _rootNodeRendered;

  private static final int _DEFAULT_SPACER_WIDTH = 18;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeTableRenderingContext.class);
}
