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

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/SubTabBarRenderer.java#0 $) $Date: 10-nov-2005.18:56:18 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SubTabBarRenderer extends HtmlLafRenderer
{
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return TABLE_ELEMENT ;

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

    renderLayoutTableAttributes( context,
                                 ZERO_ATTRIBUTE_VALUE,
                                 ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE);

  }
  

  /**
   *
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    renderRelatedLinksBlockStart(context, "af_panelTabbed.BLOCK_TITLE");
    super.prerender(context, node);

    // Disable default link style classes - subTabBar items
    // don't need to render the default OraLink style class.
    LinkUtils.startDefaultStyleClassDisabled(context);
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(TABLE_ROW_ELEMENT, null); 
    writer.startElement(TABLE_DATA_ELEMENT, null);
    // use a style class instead attribute for width=100%. 
    // This way the user can change this if he wants it centered.
    renderStyleClassAttribute(context, _CELL_START_STYLE);
    //IE seems to need something in the cell in order for styles to take effect. 
    renderHorizontalSpacer(context, "0");
    writer.endElement(TABLE_DATA_ELEMENT);    
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(TABLE_DATA_ELEMENT, null);
    // use a style class instead attribute for width=100%. 
    // This way the user can change this if he wants it centered.
    renderStyleClassAttribute(context, _CELL_END_STYLE);
    //IE seems to need something in the cell in order for styles to take effect. 
    renderHorizontalSpacer(context, "0");    
    writer.endElement(TABLE_DATA_ELEMENT);


    // Re-enable default link style classes
    LinkUtils.endDefaultStyleClassDisabled(context);
    writer.endElement(TABLE_ROW_ELEMENT);
    super.postrender(context, node);
    renderRelatedLinksBlockEnd(context);

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
    // save the initial stauts
    boolean actualLinkStatus = LinkUtils.isSelected(context);

    Integer selectedIndex = (Integer)SubTabBarUtils.getSelectedIndex(context);

    // save away the selected index
    context.setLocalProperty( _SELECTED_NODE_KEY, selectedIndex );

    LinkRenderer.setSaveModelDisabled(context, true);

    super.renderContent(context, node);
    LinkRenderer.setSaveModelDisabled(context, false);
    // restore it to the initial stauts
    LinkUtils.setSelected(context,actualLinkStatus);
  }  
  
  /**
   * Returns the StyleClass to use to render this node.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object o = super.getStyleClass(context, node);
    if (o == null)
    {
      // save away whether this subTabBar is bottom or not
      Object orientation = node.getAttributeValue(context,
                                                  ORIENTATION_ATTR);
      
      if (orientation == null ||
          ORIENTATION_DEFAULT.equals( orientation))
      {
        // if this is default check if subTabLayout set property
        // of which way to render
        orientation = SubTabBarUtils.getOrientation(context);
      }

      if (ORIENTATION_BOTTOM.equals(orientation))
        o = _ORIENTATION_BOTTOM_STYLE;
      else
        o = _ORIENTATION_TOP_STYLE;
    }

    return o;
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
    
    boolean beforeSelected = (nextVisChildIndex == selectedIndex);
    context.setLocalProperty(_BEFORE_SELECTED_KEY, 
                             Boolean.valueOf(beforeSelected));
    context.setLocalProperty(_AFTER_SELECTED_KEY, 
                             Boolean.valueOf(isSelected));

    // I need to get this information into renderBetweenIndexedChildren

    // Store the status of Link selection - used in LinkRenderer
    LinkUtils.setSelected(context,isSelected);

    try
    {

      context.pushChild(currVisChild, null, currVisChildIndex);
      context.pushRenderedChild(context, currVisChild);


      writer.startElement(TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(HEIGHT_ATTRIBUTE,"1",null);
      writer.writeAttribute(NOWRAP_ATTRIBUTE,
                            Boolean.TRUE,
  						              null);

      renderStyleClassAttribute(context,
                                isSelected ?
                                AF_SHOW_ONE_TAB_SELECTED_STYLE_CLASS : 
                                AF_SHOW_ONE_TAB_STYLE_CLASS);

      // Render the child using the SHOW_ITEM_RENDERER directly,
      // instead of letting the UINode (UIComponent) render itself.
      Renderer renderer = context.getRendererManager().getRenderer(
        MARLIN_NAMESPACE, SHOW_ITEM_NAME);
      renderer.render(context, currVisChild);
      //          currVisChild.render(context);

      writer.endElement(TABLE_DATA_ELEMENT);

    }
    finally
    {
      context.popRenderedChild(context);
      context.popChild();
    }


  }
  
  /**
   * Override of renderBetweenIndexedChildren() which renders a separator
   * style on the td. The style will be af|panelTabbed::separator-before-selected,
   * af|panelTabbed::separator-after-selected, or af|panelTabbed::separator.
   */
  @Override
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Boolean beforeSelected = (Boolean)context.getLocalProperty(
                                0, _BEFORE_SELECTED_KEY, Boolean.FALSE);
    Boolean afterSelected = (Boolean)context.getLocalProperty(
                                0, _AFTER_SELECTED_KEY, Boolean.FALSE);
    
    String styleClass = (Boolean.TRUE.equals(afterSelected)) ? 
                         _SEPARATOR_AFTER_SELECTED_STYLE:
                         (Boolean.TRUE.equals(beforeSelected)) ?
                         _SEPARATOR_BEFORE_SELECTED_STYLE :
                         _SEPARATOR_STYLE;

  
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(TABLE_DATA_ELEMENT, null);

    renderStyleClassAttribute(context, styleClass);
    //IE seems to need something in the cell in order for styles to take effect. 
    renderHorizontalSpacer(context, "0");
    writer.endElement(TABLE_DATA_ELEMENT);

  }

  // key of the selected node
  private static final Object _SELECTED_NODE_KEY = new Object();
  private static final Object _BEFORE_SELECTED_KEY = new Object();
  private static final Object _AFTER_SELECTED_KEY = new Object();
  
  // style classes used in this showOneTabs renderer.
  private static final String _SEPARATOR_AFTER_SELECTED_STYLE =
    "af|panelTabbed::separator-after-selected";   
  private static final String _SEPARATOR_BEFORE_SELECTED_STYLE =
    "af|panelTabbed::separator-before-selected";    
  private static final String _SEPARATOR_STYLE =
    "af|panelTabbed::separator";
  private static final String _CELL_START_STYLE =    
    "af|panelTabbed::cell-start";
  private static final String _CELL_END_STYLE =    
    "af|panelTabbed::cell-end";
  private static final String _ORIENTATION_BOTTOM_STYLE =    
    "af|panelTabbed::orientation-bottom";
  private static final String  _ORIENTATION_TOP_STYLE =
    "af|panelTabbed::orientation-top";
    
}
