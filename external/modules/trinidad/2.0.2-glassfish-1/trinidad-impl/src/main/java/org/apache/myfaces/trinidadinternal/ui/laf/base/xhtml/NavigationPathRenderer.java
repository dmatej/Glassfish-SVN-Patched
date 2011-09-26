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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXPage;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * Renders the breadcrumbs UI element. (this doesn't seem to be called. Use the core one instead)
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/NavigationPathRenderer.java#0 $) $Date: 10-nov-2005.18:54:02 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NavigationPathRenderer extends XhtmlLafRenderer
{
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // The breadcrumbs always have a span around it. Then if there are
    // no children, it will still be identified if there is an id on the span
    // for PPR and Visual Editor purposes. See bug # # 2222541.
    return SPAN_ELEMENT;
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

    //UIXNavigationPath component = (UIXNavigationPath) node.getUIComponent();
    UIXPage component = (UIXPage) node.getUIComponent();
    Object  focusRowKey = component.getFocusRowKey();
    int childCount = 0;

    // no kids, no breadcrumbs -- but you still get the span.
    if (focusRowKey != null)
    {
      int minChildCount = 0;
      if ( !renderLastChild(context, node))
        minChildCount = 1;

      childCount = component.getDepth(focusRowKey) + 1;

      if (childCount <= minChildCount )
        return;

    }
    else
      return;


    boolean vertical = _getOrientation(context, node);

    // Grab the separator Icon, which is rendered by the BetweenRenderer
    Icon separatorIcon = context.getIcon(
                                   AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME);

    BetweenRenderer between = new BetweenRenderer(node,
                                                  vertical,
                                                  childCount,
                                                  separatorIcon);

    setRenderingProperty(context, _BETWEEN_RENDERER_KEY, between);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    BetweenRenderer between =
      (BetweenRenderer)getRenderingProperty(context, _BETWEEN_RENDERER_KEY);
    setRenderingProperty(context, _BETWEEN_RENDERER_KEY, null);

    // no kids, no breadcrumbs -- but you still get the span.
    if (between != null)
    {
      boolean vertical = _getOrientation(context, node);

      // Turn off link disabling (which was turned on in BetweenRenderer)
      LinkRenderer.setDisabled(context, false);

      _renderEndOfLink(context, vertical);
    }

    setRenderingProperty(context,
                         _IS_LAST_CHILD_KEY,
                         null);

    super.postrender(context, node);

  }

  @SuppressWarnings("unchecked")
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    UIXPage component = (UIXPage) node.getUIComponent();
    UINode stamp = this.getNamedChild(context, node, NODE_STAMP_CHILD);
    if(stamp == null)
      return;

    // Save the current path
    Object oldPath = component.getRowKey();
    Object focusRowKey = component.getFocusRowKey();
    int startDepth = 0;

    // when you get the focusPath the menuModel may not restore the path
    // properly, so be sure to set the path to the value you want after the
    // call to getFocusPath.
    component.setRowKey(null);
    List<Object> focusPath = Collections.EMPTY_LIST;
    if (focusRowKey != null)
    {
      focusPath = component.getAllAncestorContainerRowKeys(focusRowKey);
      focusPath = new ArrayList<Object>(focusPath);
      focusPath.add(focusRowKey);
    }

    int size = focusPath.size();
    if (focusRowKey != null )
    {
      if ( size > startDepth )
      {
        component.setRowKey(focusPath.get(startDepth));
      }
      else
      {
        // Restore the current path
        component.setRowKey(oldPath);
        return;
      }
    }
    else
    {
      // Restore the current path
      component.setRowKey(oldPath);
      return;
    }


    for (int i = startDepth; i < size; i++)
    {
      component.setRowKey(focusPath.get(i));
      renderStamp(context, node, stamp, (i+1 == size));

    }

    // Restore the current path
    component.setRowKey(oldPath);

  }

  protected void renderStamp(
    UIXRenderingContext context,
    UINode           node,
    UINode           stamp,
    boolean          isLastChild
    ) throws IOException
  {
    // When we have multiple children directly in the BreadCrumbs,
    // we've got an efficient - and robust - technique for picking out
    // which is the "last" child.  In such cases, note that very fact
    // so our poor "BetweenRendered" can relax.

    if ( isLastChild )
    {
      setRenderingProperty(context,
                           _IS_LAST_CHILD_KEY,
                           Boolean.TRUE);
    }

    BetweenRenderer between =
      (BetweenRenderer)getRenderingProperty(context, _BETWEEN_RENDERER_KEY);
    if (between != null)
      between.render(context, node);

    if ( isLastChild && !renderLastChild(context, node))
    {
      // the ui team now wants the last caret to be shown, but not
      // the last link.
      return;
    }
    Map<String, String> originalResourceKeyMap = context.getSkinResourceKeyMap();
    try
    {
      context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);

      stamp.render(context);

      if (isLastChild)
        ((CoreRenderingContext) context).setLinkDisabled(false);
    }
    finally
    {
      context.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

/*
  protected void renderIndexedChild(
    RenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {
    // When we have multiple children directly in the BreadCrumbs,
    // we've got an efficient - and robust - technique for picking out
    // which is the "last" child.  In such cases, note that very fact
    // so our poor "BetweenRendered" can relax.
    boolean isLastChild = false;

    isLastChild = (nextVisChildIndex == NO_CHILD_INDEX);

    if ( isLastChild )
    {
      setRenderingProperty(context,
                           _IS_LAST_CHILD_KEY,
                           Boolean.TRUE);
    }

    BetweenRenderer between =
      (BetweenRenderer)getRenderingProperty(context, _BETWEEN_RENDERER_KEY);
    if (between != null)
      between.render(context, node);

    if ( isLastChild && !renderLastChild(context, node))
    {
      // the ui team now wants the last caret to be shown, but not
      // the last link.
      return;
    }

    super.renderIndexedChild(context,
                             node,
                             currVisChildIndex,
                             prevVisChildIndex,
                             nextVisChildIndex,
                             ithRenderedChild);
  }
  */
  protected boolean renderLastChild(
    UIXRenderingContext context,
    UINode           node)
  {
    return false;
  }

  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return AF_NAVIGATION_PATH_STYLE_CLASS;
  }

  //
  // Renders (almost) everything that goes before the link
  //
  private void _renderStartOfLink(
    UIXRenderingContext context,
    boolean      vertical,
    boolean      isLastChild) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (vertical)
    {
      writer.startElement(DIV_ELEMENT, null);
    }

    // Breadcrumbs wrap within a page depending on the size of the
    // browser window. The wrap occurs to the right of the greater-than
    // symbol that follows after the breadcrumb text string,
    // and not within the text string itself.
    writer.startElement(XhtmlLafConstants.NO_BREAK_ELEMENT, null);
  }


  //
  // Renders everything that goes after the link
  //
  private void _renderEndOfLink(
    UIXRenderingContext context,
    boolean      vertical
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();


    writer.endElement(XhtmlLafConstants.NO_BREAK_ELEMENT);

    if(vertical)
    {
      writer.endElement(DIV_ELEMENT);
    }
  }

  /**
   * gets the orientation attribute from the link. If this attr is not set,
   * defaults to horizontal.
   * @return true if the orientation is vertical
   */
  private static boolean _getOrientation(UIXRenderingContext context,
                                         UINode breadCrumbs)
  {
    Object orientation = breadCrumbs.getAttributeValue(context,
                                                       ORIENTATION_ATTR);
    return (orientation != null) &&
      ORIENTATION_VERTICAL.equals(orientation);
  }


  // # of hard spaces to use in indenting vertical breadcrumbs
  private static final int _INDENT_SPACES = 10;

  //
  // BetweenRenderer is used to render everything that goes "between"
  // links.  Really, it gets called _before_ each link, so we have to specially
  // handle the first time it gets called.
  //
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private class BetweenRenderer implements Renderer
  {
    public BetweenRenderer(
      UINode  breadCrumbs,
      boolean vertical,
      int     totalCrumbs,
      Icon    separatorIcon)
    {
      _vertical     = vertical;
      _totalCrumbs  = totalCrumbs;

      // We stash away the separator Icon so we don't
      // have to look it up repeatedly.
      _separatorIcon = separatorIcon;
    }

    public void render(UIXRenderingContext context, UINode node)
      throws IOException
    {
      ResponseWriter writer = context.getResponseWriter();

      // Special handling for when we're the first or last child
      boolean isLastChild;
      if (_totalCrumbs >= 0)
        isLastChild = ((_renderedCount + 1) >= _totalCrumbs);
      else
        isLastChild = _isLastChild(context);

      boolean isFirstChild = (_renderedCount == 0);

      if (!isFirstChild)
      {
        // render the separator
        if (_separatorIcon != null)
        {
          RenderingContext arc = RenderingContext.getCurrentInstance();
          FacesContext fContext = context.getFacesContext();
          OutputUtils.renderIcon(fContext, arc, _separatorIcon, "", null);
        }

        // and render everything that goes after the previous link.
        // We'll take care of also rendering this content in prerender()
        _renderEndOfLink(context, _vertical);
      }

      _renderStartOfLink(context, _vertical, isLastChild);


      // if breadcrumbs are oriented vertically, then indent the levels
      if(!isFirstChild && _vertical)
      {
     char[] chars = new char[1];
  chars[0] = NBSP_CHAR;

        for(int i = 0; i < _renderedCount * _INDENT_SPACES; i++)
        {
          writer.writeText(chars, 0, 1);
        }
      }

      _renderedCount++;

      if (isLastChild)
        LinkRenderer.setDisabled(context, true);
    }

    private boolean _isLastChild(UIXRenderingContext context)
    {
      // See if BreadCrumbs has taken responsibility for determining
      // this.  This code's here so that our "BetweenRenderer" strategy
      // doesn't break anything - in practice, the other code path will usually
      // produce the same results (albeit a bit more expensively)
      Object o  = getRenderingProperty(context, _IS_LAST_CHILD_KEY);
      return (o != null);
    }

    private final boolean _vertical;
    private int           _totalCrumbs;
    private int           _renderedCount;
    private Icon          _separatorIcon;
  }

  static private final Object _IS_LAST_CHILD_KEY = new Object();
  static private final Object _BETWEEN_RENDERER_KEY = new Object();
  private static final Map<String, String> _RESOURCE_KEY_MAP;
  static
  {
    _RESOURCE_KEY_MAP  =  new HashMap<String, String>();
    
    _RESOURCE_KEY_MAP.put(
      XhtmlLafConstants.LINK_STYLE_CLASS,
      XhtmlLafConstants.AF_NAVIGATION_PATH_STEP_STYLE_CLASS);
    // the selected step is disabled, which is why we map these two styles
    _RESOURCE_KEY_MAP.put(
      XhtmlLafConstants.LINK_DISABLED_STYLE_CLASS,
      XhtmlLafConstants.AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS);
  }
}
