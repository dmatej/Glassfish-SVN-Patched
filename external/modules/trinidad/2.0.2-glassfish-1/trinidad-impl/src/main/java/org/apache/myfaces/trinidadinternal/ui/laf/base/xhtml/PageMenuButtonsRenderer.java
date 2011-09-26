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

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXPage;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/PageMenuButtonsRenderer.java#0 $) $Date: 10-nov-2005.18:54:06 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageMenuButtonsRenderer extends GlobalButtonBarRenderer
{
  @Override
  protected UIXHierarchy getHierarchyBase(
    UIXRenderingContext context,
    UINode           node
  )
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    return (UIXPage) pageNode.getUIComponent();
  }

  @Override
  protected UINode getStamp(
    UIXRenderingContext context,
    UINode           node
    )
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    return getNamedChild(context, pageNode, NODE_STAMP_CHILD);
  }

  @Override
  protected boolean setNewPath(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy      component
  )
  {
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    return PageRendererUtils.setNewPath(context, component, startDepth);
  }
}
