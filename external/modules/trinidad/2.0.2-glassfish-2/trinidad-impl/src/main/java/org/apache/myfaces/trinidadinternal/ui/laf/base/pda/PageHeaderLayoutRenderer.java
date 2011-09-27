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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeMap;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PageHeaderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:00 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageHeaderLayoutRenderer extends UINodeRenderer
                                      implements UIConstants
{
  private static UINode _createCompositeUINode()
  {
    MarlinBean brandingFlow = new MarlinBean(FLOW_LAYOUT_NAME);

    MarlinBean spacer = new MarlinBean(SPACER_NAME);
    spacer.setAttributeValue(WIDTH_ATTR, "5");
    spacer.setAttributeValue(HEIGHT_ATTR, "1");

    brandingFlow.setNamedChild(SEPARATOR_CHILD, spacer);
    brandingFlow.addIndexedChild(
      ContextPoppingUINode.getUINode(BRANDING_CHILD));
    brandingFlow.addIndexedChild(
      ContextPoppingUINode.getUINode(BRANDING_APP_CHILD));
    brandingFlow.addIndexedChild(
      ContextPoppingUINode.getUINode(BRANDING_APP_CONTEXTUAL_CHILD));

    MarlinBean compositeRoot = new MarlinBean(STACK_LAYOUT_NAME);

    // delegate all of the attributes to the RootAttribtueMap
    compositeRoot.setAttributeMap(RootAttributeMap.getAttributeMap());

    compositeRoot.addIndexedChild(
      ContextPoppingUINode.getUINode(NAVIGATION_GLOBAL_CHILD));
    brandingFlow.addIndexedChild(
      ContextPoppingUINode.getUINode(MENU_SWITCH_CHILD));

    compositeRoot.addIndexedChild(brandingFlow);
    compositeRoot.addIndexedChild(
      ContextPoppingUINode.getUINode(NAVIGATION1_CHILD));
    compositeRoot.addIndexedChild(
      ContextPoppingUINode.getUINode(NAVIGATION2_CHILD));
    compositeRoot.addIndexedChild(
      ContextPoppingUINode.getUINode(SEARCH_CHILD));

    return compositeRoot;
  }

  @Override
  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _INSTANCE;
  }


  private static final UINode _INSTANCE = _createCompositeUINode();
}
