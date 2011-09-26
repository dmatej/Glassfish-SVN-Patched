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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXPage;

import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.PageRendererUtils;


/**
 * Renderer for navigationTree in a page 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PageNavigationTreeRenderer.java#0 $) $Date: 10-nov-2005.18:55:32 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageNavigationTreeRenderer extends NavigationTreeRenderer
{ 
  @Override
  protected RowKeySet getExpandedRowKeys(UIXHierarchy tree)
  {
    return ((UIXPage)tree).getDisclosedRowKeys();    
  }

  @Override
  protected UIXHierarchy getTree(
    UIXRenderingContext context, 
    UINode           node)
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    UIXHierarchy component = (UIXHierarchy) pageNode.getUIComponent();     
    return component;    
  }
  
  @Override
  protected UINode getStamp(
    UIXRenderingContext context, 
    UINode           node)
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    return getNamedChild(context, pageNode, NODE_STAMP_CHILD);
  }  
  
  
  @Override
  protected boolean setInitialPath(
    UIXRenderingContext context, 
    UINode           node,
    UIXHierarchy         tree)
  {
   
    int startLevel = getIntAttributeValue(context, node, START_LEVEL_ATTR, 0);
    return PageRendererUtils.setNewPath(context, tree, startLevel); 
  }
}
