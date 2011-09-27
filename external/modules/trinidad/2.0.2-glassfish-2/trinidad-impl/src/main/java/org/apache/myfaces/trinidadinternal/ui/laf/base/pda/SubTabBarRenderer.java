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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;

import java.io.IOException;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SubTabBarRenderer extends XhtmlLafRenderer
{
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT ;
  }

  @Override
  protected void renderID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Do not render id for the individual tab bars. The overall 3.0 component
    //  is the showOneTab (a combination of subTabLayout and subTabBar from 2.2)
    //  We will render id once on the top most html element i.e. <span>
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

    // Disable default link style classes - subTabBar items
    // don't need to render the default OraLink style class.
    LinkUtils.startDefaultStyleClassDisabled(context);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Re-enable default link style classes
    LinkUtils.endDefaultStyleClassDisabled(context);
    super.postrender(context, node);
  }

  /**
   * Overrride to render in three passes.
   */
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // save the inital stauts
    boolean actualLinkStatus = LinkUtils.isSelected(context);
    ResponseWriter writer = context.getResponseWriter();

    LinkRenderer.setSaveModelDisabled(context, true);

    Integer selectedIndex = (Integer)SubTabBarUtils.getSelectedIndex(context);

    // save away the selected index
    context.setLocalProperty( _SELECTED_NODE_KEY, selectedIndex);

    // render pass 1 - row with rendered children
    writer.startElement(DIV_ELEMENT, null);
    super.renderContent(context, node);
    writer.endElement(DIV_ELEMENT);
    LinkRenderer.setSaveModelDisabled(context, false);
    // restore it back to original status
    LinkUtils.setSelected(context,actualLinkStatus);
  }

  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {
    UINode currVisChild = node.getIndexedChild(context, currVisChildIndex);
    ResponseWriter writer = context.getResponseWriter();
    // get the selected index
    int selectedIndex = ((Integer)context.getLocalProperty( 0,
                                                            _SELECTED_NODE_KEY,
                                                            ZERO)).intValue();
    boolean isSelected = (currVisChildIndex == selectedIndex);
    //
    // output the link
    //
    try
    {
      context.pushChild(currVisChild, null, currVisChildIndex);
      context.pushRenderedChild(context, currVisChild);
      writer.startElement( SPAN_ELEMENT, null);
      writer.writeAttribute( NOWRAP_ATTRIBUTE,
                             Boolean.TRUE,
                             null);
      renderStyleClassAttribute(context,
                                isSelected ?
                                AF_SHOW_ONE_TAB_SELECTED_STYLE_CLASS : 
                                AF_SHOW_ONE_TAB_STYLE_CLASS);
      renderSpacer( context, 5, 1 );
      // Store the status of Link selection - used in LinkRenderer
      LinkUtils.setSelected(context,isSelected);
      Renderer renderer = context.getRendererManager().getRenderer(
            MARLIN_NAMESPACE, SHOW_ITEM_NAME);
          renderer.render(context, currVisChild);
      //currVisChild.render(context);
      renderSpacer( context, 5, 1 );
      writer.endElement( SPAN_ELEMENT);
    }
    finally
    {
      context.popRenderedChild(context);
      context.popChild();
    }
  }

  // key of the selected node
  private static final Object _SELECTED_NODE_KEY = new Object();
}
