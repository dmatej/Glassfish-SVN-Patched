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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.component.UIXPage;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.collection.UINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootUINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.NotBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.OrBoundValue;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/PageRenderer.java#0 $) $Date: 10-nov-2005.18:54:07 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageRenderer extends UINodeRenderer
{
  @Override
  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node)
  {
    return _compositeNode;
  }

  @Override
  protected void renderWithNode(
    UIXRenderingContext context,
    UINode           node,
    UINode           renderingUINode
    ) throws IOException
  {
    UIXPage page = (UIXPage)node.getUIComponent();
    Object focusPath = page.getFocusRowKey();
    PageRendererUtils.setFocusPath(context, focusPath);
    super.renderWithNode(context, node, renderingUINode);
    PageRendererUtils.setFocusPath(context, null);
  }

  private static UINode  _createCompositeUINode()
  {
    // add the tabs
    MarlinBean pageLayout = new MarlinBean(UIConstants.PAGE_LAYOUT_NAME);
    MutableUINode navigationGlobal =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_MENU_BUTTONS_NAME);
    pageLayout.setNamedChild(UIConstants.NAVIGATION_GLOBAL_CHILD, navigationGlobal);

    // add the bar
    MutableUINode menuTabs =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_MENU_TABS_NAME);
    menuTabs.setAttributeValue(UIConstants.LEVEL_ATTR,
                               1);
    pageLayout.setNamedChild(UIConstants.NAVIGATION1_CHILD, menuTabs);
    MutableUINode menuBar =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_MENU_BAR_NAME);
    menuBar.setAttributeValue(UIConstants.LEVEL_ATTR,
                              2);
    pageLayout.setNamedChild(UIConstants.NAVIGATION2_CHILD, menuBar);

    // add the tree

    MutableUINode navigationTree =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_NAVIGATION_TREE_NAME);
    navigationTree.setAttributeValue(UIConstants.START_LEVEL_ATTR,
                              3);

    BoundValue treeRenderedBV = new HasDataBoundValue(
                                  RootBoundValue.getBoundValue(), 3, true);
    navigationTree.setAttributeValue(UIConstants.RENDERED_ATTR, treeRenderedBV);
    // for PPR, an ID is required:
    navigationTree.setAttributeValue(UIConstants.ID_ATTR, new BoundValue()
    {
      public Object getValue(UIXRenderingContext context)
      {
        UINode pageNode = context.getParentContext().getAncestorNode(0);
        UIComponent component = pageNode.getUIComponent();
        return component.getClientId(context.getFacesContext()) +
               org.apache.myfaces.trinidadinternal.renderkit.uix.PageRenderer.TREE_SUFFIX;
      }
    }
                              );


    // add the list
    MutableUINode menuList =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_MENU_LIST_NAME);
    menuList.setAttributeValue(UIConstants.LEVEL_ATTR,
                              3);
    BoundValue listRenderedBV = new HasDataBoundValue(
                                  RootBoundValue.getBoundValue(), 3, false);
    menuList.setAttributeValue(UIConstants.RENDERED_ATTR, listRenderedBV);


    MarlinBean navigation3Flow = new MarlinBean(UIConstants.FLOW_LAYOUT_NAME);
    navigation3Flow.addIndexedChild(menuList);
    navigation3Flow.addIndexedChild(navigationTree);
    navigation3Flow.setAttributeValue(UIConstants.RENDERED_ATTR,
                           new OrBoundValue(treeRenderedBV,listRenderedBV));
    pageLayout.setNamedChild(UIConstants.NAVIGATION3_CHILD, navigation3Flow);



    // add the path
    MutableUINode navigationPath =
                  new BaseMutableUINode(UIConstants.MARLIN_NAMESPACE,
                                        UIConstants.PAGE_NAVIGATION_PATH_NAME);

    BoundValue hasLocation =
          XhtmlLafUtils.createIsRenderedBoundValue(UIConstants.LOCATION_CHILD);
    navigationPath.setAttributeValue(UIConstants.RENDERED_ATTR,
                               new NotBoundValue(hasLocation));


    UINode location = ContextPoppingUINode.getUINode(UIConstants.LOCATION_CHILD);
    MarlinBean locationFlow = new MarlinBean(UIConstants.FLOW_LAYOUT_NAME);
    locationFlow.addIndexedChild(navigationPath);
    locationFlow.addIndexedChild(location);
    pageLayout.setNamedChild(UIConstants.LOCATION_CHILD, locationFlow);

    //_setPoppedChild(pageLayout, UIConstants.LOCATION_CHILD);
    _setPoppedChild(pageLayout, UIConstants.MENU_SWITCH_CHILD);
    _setPoppedChild(pageLayout, UIConstants.ACTIONS_CHILD);
    _setPoppedChild(pageLayout, UIConstants.APP_ABOUT_CHILD);
    _setPoppedChild(pageLayout, UIConstants.APP_COPYRIGHT_CHILD);
    _setPoppedChild(pageLayout, UIConstants.APP_PRIVACY_CHILD);
    _setPoppedChild(pageLayout, UIConstants.BRANDING_CHILD);
    _setPoppedChild(pageLayout, UIConstants.BRANDING_APP_CHILD);
    _setPoppedChild(pageLayout, UIConstants.BRANDING_APP_CONTEXTUAL_CHILD);
    _setPoppedChild(pageLayout, UIConstants.CONTEXT_SWITCHER_CHILD);
    _setPoppedChild(pageLayout, UIConstants.INFO_FOOTNOTE_CHILD);
    _setPoppedChild(pageLayout, UIConstants.INFO_RETURN_CHILD);
    _setPoppedChild(pageLayout, UIConstants.INFO_STATUS_CHILD);
    _setPoppedChild(pageLayout, UIConstants.INFO_SUPPLEMENTAL_CHILD);
    _setPoppedChild(pageLayout, UIConstants.INFO_USER_CHILD);
    _setPoppedChild(pageLayout, UIConstants.MESSAGES_CHILD);
    _setPoppedChild(pageLayout, UIConstants.SEARCH_CHILD);
    UINodeList rootNodeList = RootUINodeList.getNodeList();
    pageLayout.setIndexedNodeList(rootNodeList);

    _setPoppedAttribute(pageLayout, UIConstants.CHROME_TYPE_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ID_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.SHORT_DESC_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.STYLE_CLASS_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.INLINE_STYLE_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_CLICK_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_DOUBLE_CLICK_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_KEY_DOWN_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_KEY_UP_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_KEY_PRESS_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_MOUSE_DOWN_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_MOUSE_MOVE_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_MOUSE_OUT_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_MOUSE_OVER_ATTR);
    _setPoppedAttribute(pageLayout, UIConstants.ON_MOUSE_UP_ATTR);

    return pageLayout;
  }

  private static void _setPoppedAttribute(
    MutableUINode node,
    AttributeKey  attr
    )
  {
    node.setAttributeValue(attr, RootAttributeBoundValue.getBoundValue(attr));
  }

  private static void _setPoppedChild(
    MutableUINode node,
    String        childName
    )
  {
    node.setNamedChild(childName,
                       ContextPoppingUINode.getUINode(childName));
  }
  private static UINode _compositeNode = _createCompositeUINode();

  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private static class HasDataBoundValue implements BoundValue
  {
    public HasDataBoundValue(
      BoundValue componentBV,
      int        startDepth,
      boolean    isDataATree
      )
    {
      _componentBV = componentBV;
      _startDepth  = startDepth;
      _isDataATree = isDataATree;
    }
    public Object getValue(
      UIXRenderingContext context
    )
    {
      UINode node = (UINode)_componentBV.getValue(context);
      UIXPage component = (UIXPage)node.getUIComponent();
      Object oldPath = component.getRowKey();

      boolean isNewPath = PageRendererUtils.setNewPath(context,
                                                       component,
                                                       _startDepth);
      if (!isNewPath)
        return Boolean.FALSE;

      if (component.getRowCount() > 0)
      {
        boolean isTree = false;
        int i = 0;
        while (!isTree && i < component.getRowCount())
        {
          component.setRowIndex(i);
          if (component.isContainer() && !component.isContainerEmpty())
          {
            isTree = true;
          }
          i++;
        }
        component.setRowKey(oldPath);

        return Boolean.valueOf(_isDataATree == isTree);
      }
      component.setRowKey(oldPath);
      return Boolean.FALSE;
    }

    private BoundValue _componentBV;
    private int        _startDepth;
    private boolean    _isDataATree = false;
  }
}
