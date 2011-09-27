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
import java.util.HashMap;
import java.util.List;

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;

/**
 * Renders the breadcrumbs UI element.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/BreadCrumbsRenderer.java#0 $) $Date: 10-nov-2005.18:53:13 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BreadCrumbsRenderer extends XhtmlLafRenderer
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

    UIXHierarchy component = getHierarchyBase(context, node);
    int childCount = _getItemCount(context, node, component);

    // no kids, no breadcrumbs -- but you still get the span.
    int minChildCount = 0;
    if ( !renderLastChild(context, node))
      minChildCount = 1;

    if (childCount <= minChildCount )
      return;

    int realChildCount = _getRealItemCount(childCount);


    boolean vertical = _getOrientation(context, node);

    // Grab the separator Icon, which is rendered by the BetweenRenderer
    Icon separatorIcon = context.getIcon(
                                   AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME);

    BetweenRenderer between = new BetweenRenderer(node,
                                                  vertical,
                                                  realChildCount,
                                                  separatorIcon);

    setRenderingProperty(context, _BETWEEN_RENDERER_KEY, between);
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


  protected Object getFocusPath(
    UIXHierarchy    component
  )
  {
    return component.getFocusRowKey();
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    UIXHierarchy component = getHierarchyBase(context, node);
    UINode stamp = getStamp(context, node);

    if(stamp != null)
    {
      // Save the current key
      Object oldPath = component.getRowKey();
      Object focusPath = getFocusPath(component);
      if (focusPath == null)
        return;
      
      List<Object> paths = 
        new ArrayList<Object>(component.getAllAncestorContainerRowKeys(focusPath));
      paths.add(focusPath);
      int size = paths.size();
      
      int childCount = _getItemCount(context, node, component);

      // no kids, no breadcrumbs -- but you still get the span.
      int minChildCount = 0;
      if ( !renderLastChild(context, node))
        minChildCount = 1;

      if (childCount <= minChildCount )
      {
        component.setRowKey(oldPath);
        return;
      }

      // todo - I'm not calculating whether the item is visible or not
      int nextVisChildIndex = 1;

      for (int i = 0; i < size; i++)
      {
        //PageRendererUtils.setNewPath(context, component, i);

        if ( i + 1 == childCount)
          nextVisChildIndex=NO_CHILD_INDEX;
        else
          nextVisChildIndex = i + 1;
        component.setRowKey(paths.get(i));

        renderNode(context, stamp, nextVisChildIndex);
      }

      // Restore the old path
      component.setRowKey(oldPath);
    }

    super.renderContent(context, node);
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

    setLastChildProperty(context, null);


    super.postrender(context, node);

  }

  protected void renderNode(
    UIXRenderingContext context,
    UINode           child,
    int              nextVisChildIndex
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
      setLastChildProperty(context, Boolean.TRUE);
    }

    Renderer between = getBetweenRenderer(context);

    if (between != null)
      between.render(context, child);

    if ( isLastChild && !renderLastChild(context, child))
    {
      // the ui team now wants the last caret to be shown, but not
      // the last link.
      return;
    }
    Map<String, String> originalResourceKeyMap = context.getSkinResourceKeyMap();
    try
    {
      context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      renderChild(context, child);
    
    }
    finally
    {
      context.setSkinResourceKeyMap(originalResourceKeyMap);
    }   
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
    UINode child = node.getIndexedChild(context, currVisChildIndex);
    Map<String, String> originalResourceKeyMap = context.getSkinResourceKeyMap();
    try
    {
      context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      renderNode(context, child, nextVisChildIndex);  
    }
    finally
    {
      context.setSkinResourceKeyMap(originalResourceKeyMap);
    }     
  }
  
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


  protected void setLastChildProperty(
    UIXRenderingContext context,
    Boolean          isLastChild
  )
  {
    setRenderingProperty(context,
                         _IS_LAST_CHILD_KEY,
                         isLastChild);
  }

  protected Renderer getBetweenRenderer(
    UIXRenderingContext context
  )
  {
    return (Renderer)getRenderingProperty(context, _BETWEEN_RENDERER_KEY);
  }

  private int _getItemCount(
    UIXRenderingContext context,
    UINode           node,
    UIXHierarchy    component
    )
  {
    Object focusPath = getFocusPath(component);
    int kids = getVisibleIndexedChildCount(context, node);

    if (focusPath == null)
      return kids;

    return kids + component.getDepth(focusPath) + 1;
  }

  /**
   * @todo see below
   */
  private int _getRealItemCount(
    int              childCount
    )throws IOException
  {
    return childCount;
  }

  /*
   * @todo should this be supported?
   *
  protected int getRealItemCount(
    RenderingContext context,
    UINode           node,
    int              childCount
    )throws IOException
  {
    // If we think we only have one child - then we may very well
    // not really have one child;  instead, we might have a non-structural
    // node.  In that case - and only that case - count all the links and
    // use that!  Otherwise, we'll let the general rendering process figure
    // out when we're on the last link (signaled by realChildCount being
    // negative)
    int realChildCount = -1;

    if (childCount == 1)
    {
      UINode oneChild = getNextRenderedChildNode(context,
                                                 node,
                                                 NO_CHILD_INDEX);
      if (!NodeRoleUtils.isStructuralButNotComposite(
                    oneChild.getNodeRole(context)))
      {
        // =-=AEW Throw away the list of links;  we're really only
        // curious how many there are.
        realChildCount = LinkDataObject.getLinkDataList(context,
                                                    node).getLength();
      }
    }

    return realChildCount;
  }
   */


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
    _RESOURCE_KEY_MAP = new HashMap<String, String>();
    
    _RESOURCE_KEY_MAP.put(
      XhtmlLafConstants.LINK_STYLE_CLASS,
      XhtmlLafConstants.AF_NAVIGATION_PATH_STEP_STYLE_CLASS);
    // the selected step is disabled, which is why we map these two styles
    _RESOURCE_KEY_MAP.put(
      XhtmlLafConstants.LINK_DISABLED_STYLE_CLASS,
      XhtmlLafConstants.AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS); 
  }
  
}
