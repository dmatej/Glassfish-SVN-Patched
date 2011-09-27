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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;

import org.apache.myfaces.trinidadinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.trinidadinternal.ui.composite.RootUINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;

/**
 * Printable version of PageLayout.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PrintablePageLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:34 $
 * 
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PrintablePageLayoutRenderer extends UINodeRenderer
                  implements UIConstants
{
  @Override
  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _INSTANCE;
  }
  
  private static UINode _createCompositeUINode()
  {
    MarlinBean headerContent = new MarlinBean(STACK_LAYOUT_NAME);
    headerContent.setIndexedNodeList(RootUINodeList.getNodeList());    

    MarlinBean noTitleFlowLayout = new MarlinBean(FLOW_LAYOUT_NAME);
    noTitleFlowLayout.addIndexedChild(
      ContextPoppingUINode.getUINode(INFO_STATUS_CHILD));
    noTitleFlowLayout.addIndexedChild(headerContent);

    MarlinBean node = new MarlinBean(STACK_LAYOUT_NAME);
    node.addIndexedChild(ContextPoppingUINode.getUINode(MESSAGES_CHILD));
    node.addIndexedChild(noTitleFlowLayout);

    return node;
  }


  private static final UINode _INSTANCE = _createCompositeUINode();
}
