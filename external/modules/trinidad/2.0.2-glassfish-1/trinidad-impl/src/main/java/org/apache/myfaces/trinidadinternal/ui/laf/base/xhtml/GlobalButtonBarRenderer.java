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

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.laf.base.NodeRoleUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/GlobalButtonBarRenderer.java#0 $) $Date: 10-nov-2005.18:53:54 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class GlobalButtonBarRenderer extends RowLayoutRenderer
{
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    CommandNavigationItemRenderer.setNavigationItemRendererType(context,
                                    CommandNavigationItemRenderer.GLOBAL_BUTTON_TYPE);
    renderRelatedLinksBlockStart(context, mapKey("af_menuButtons.BLOCK_TITLE"));
    super.prerender(context, node);
  }


  protected UIXHierarchy getHierarchyBase(
    UIXRenderingContext context,
    UINode           node
  )
  {
    return (UIXHierarchy) node.getUIComponent();
  }


  protected UINode getStamp(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getNamedChild(context, NODE_STAMP_CHILD);
  }


  protected boolean setNewPath(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy    component
  )
  {
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    return ModelRendererUtils.setNewPath(component, startDepth,
                                         ((UIXNavigationLevel)component).getFocusRowKey());
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);
    UIXHierarchy component = getHierarchyBase(context, node);
    if (component != null)
    {
      UINode stamp = getStamp(context, node);
      if(stamp != null)
      {
        // Save the current key
        Object oldPath = component.getRowKey();
        boolean isNewPath = setNewPath(context, node, component);

        if (isNewPath)
        {
          int size = component.getRowCount();
          int rowIndex = component.getRowIndex();
          boolean needsSeparator = false;

          for (int i = 0; i < size; i++)
          {

            component.setRowIndex(i);
            boolean rendered = isRendered(context, stamp);

            if (rendered)
            {

              if (needsSeparator)
                renderBetweenNodes(context, node);
              else
                needsSeparator = true;

              renderStamp(context, stamp, i==rowIndex);
            }

          }

          if (getVisibleIndexedChildCount(context, node) > 0)
            renderBetweenNodes(context, node);

          // Restore the old path
          component.setRowKey(oldPath);
        }
      }
    }

    super.renderContent(context, node);

    // Reset the selected status, which might have been changed on rendering
    LinkUtils.setSelected(context, initialLinkSelectedStatus);
  }

  protected boolean isRendered(
    UIXRenderingContext context,
    UINode           stamp
  )
  {
    return getBooleanAttributeValue(context, stamp, RENDERED_ATTR, true);
  }

 protected void renderStamp(
    UIXRenderingContext context,
    UINode           node,
    boolean          selected
    )throws IOException
  {
    if (selected)
    {
      LinkUtils.setSelected(context, true);
    }

    renderChild(context, node);

    if (selected)
    {
      LinkUtils.setSelected(context, false);
    }
  }

 @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.postrender(context, node);
    renderRelatedLinksBlockEnd(context);
    CommandNavigationItemRenderer.setNavigationItemRendererType(context, null);
  }

 @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderBetweenNodes(context, node);
  }

  protected void renderBetweenNodes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderChild(context, _SPACER);
  }

  @Override
  protected void renderDefaultCellAttributes(
    UIXRenderingContext context,
    UINode           child) throws IOException
  {
    String valign;
    // See bug 2866954
    if ( context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS &&
         context.getAgent().getAgentApplication() ==
               TrinidadAgent.Application.IEXPLORER)
      valign = "top";
    else
      valign = "bottom";

    renderAttribute(context, "valign", valign);
  }

  /**
   * Renders attributes of the current node.
   */
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    // A table nested in a td will not align right
    // on ie 5.0 on a mac even if the td says align="right".
    // The table itself has to have align="right"
    // so adding that here as a work-around for
    // browser bug and fix for bug 2112321 and bug 2112433
    /* =-=AEW Now things get bizarre:  this code introduced bug 2713156.
       But removing this code no longer triggers bug 2112321 or bug 2112433!
    if ( context.getAgent().getAgentOS() == Agent.OS_MACOS &&
         context.getAgent().getAgentApplication() ==
                                                  Agent.APPLICATION_IEXPLORER)
    {
      renderAttribute(context, node, "align", "right");
    }
    */
  }

  /**
   *
   * @param context
   * @return true if the ancestor node is a table
   * (tableLayout or messageComponentLayout)
   */
  @Override
  protected boolean hasTableParent(UIXRenderingContext context)
  {
    UINode ancestor = NodeRoleUtils.getStructuralAncestor(context);

    return (isEqualMarlinName(ancestor, TABLE_LAYOUT_NAME));
  }

  protected String mapKey(String key)
  {
    return key;
  }

  // space to render between each child
  private static final MarlinBean _SPACER = new MarlinBean(SPACER_NAME);
  static
  {
    _SPACER.setAttributeValue(WIDTH_ATTR, "10");
    _SPACER.setAttributeValue(HEIGHT_ATTR, "1");
  }
}
