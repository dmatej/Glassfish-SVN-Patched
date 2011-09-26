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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.text.Bidi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.core.nav.CoreBreadCrumbs;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;


public class BreadCrumbsRenderer extends XhtmlRenderer
{
  public BreadCrumbsRenderer()
  {
    super(CoreBreadCrumbs.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _orientationKey = type.findKey("orientation");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("span", component);
    renderAllAttributes(context, rc, component, bean);
    renderId(context, component);

    int renderedItemCount = _getItemCount((UIXHierarchy)component);
    int minItemCount = _getMinItemCount(rc);

    // no kids, no NavigationPath -- but you still get the span.
    if (renderedItemCount > minItemCount )
    {
      renderContent(context, rc,
                    (UIXHierarchy)component, bean);
    }

    writer.endElement("span");
  }

  /**
   * Gets the stamp to use to render each link
   */
  protected UIComponent getStamp(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     component,
    FacesBean        bean
    )
  {
    UIComponent stamp = component.getFacet("nodeStamp");
    return stamp;
  }

  @SuppressWarnings("unchecked")
  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIXHierarchy     component,
    FacesBean        bean
    ) throws IOException
  {

    boolean isVertical;

    // To reduce the breadcrumb's width, render vertically for narrow-screen
    // PDAs.
    if (supportsNarrowScreen(rc))
    {
      isVertical = true;
    }
    else
    {
      isVertical = _isVertical(bean);
    }

    boolean shouldRenderLastChild = shouldRenderLastChild(rc);
    boolean isLastChild   = false;
    boolean isFirstChild  = true;
    int renderedCount     = 0;
    int nextVisChildIndex = 1;
    UIComponent stamp = getStamp(context, rc, component, bean);
    Icon separatorIcon = rc.getIcon(
                            SkinSelectors.AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME);

    // use the focusKey to stamp out path
    if(stamp != null)
    {
      // Save the current key
      Object oldPath = component.getRowKey();
      Object focusPath = _getFocusRowKey(component);
      if (focusPath == null)
        return;

      List<Object> paths =
        new ArrayList<Object>(component.getAllAncestorContainerRowKeys(focusPath));

      paths.add(focusPath);
      int size = paths.size();

      for (int i = 0; i < size; i++)
      {

        // todo - I'm not calculating whether the item is visible or not
        if ( i + 1 == size)
          nextVisChildIndex=NO_CHILD_INDEX;
        else
          nextVisChildIndex = i + 1;

        isLastChild = (nextVisChildIndex == NO_CHILD_INDEX);
        component.setRowKey(paths.get(i));

        renderNode(context, rc, separatorIcon, stamp, renderedCount,
                   shouldRenderLastChild, isFirstChild, isLastChild, isVertical);
        renderedCount++;
        isFirstChild = false;

      }

      // Restore the old path
      component.setRowKey(oldPath);
    }

    // now render children
    if (hasChildren(component))
    {
      List<UIComponent> children = component.getChildren();
      nextVisChildIndex = getNextRenderedChildIndex(children, -1);
      while (nextVisChildIndex != NO_CHILD_INDEX)
      {
        UIComponent child = children.get(nextVisChildIndex);
        nextVisChildIndex = getNextRenderedChildIndex(children,
                                                      nextVisChildIndex);
        isLastChild = (nextVisChildIndex == NO_CHILD_INDEX);
        renderNode(context, rc, separatorIcon, child, renderedCount,
                   shouldRenderLastChild, isFirstChild, isLastChild, isVertical);
        renderedCount++;
        isFirstChild = false;
      }
    }

  }

  protected boolean hasChildren(
    UIComponent component)
  {
    int childCount = component.getChildCount();
    return childCount > 0;
  }

  protected void renderNode(
    FacesContext     context,
    RenderingContext rc,
    Icon             separatorIcon,
    UIComponent      child,
    int              renderedCount,
    boolean          shouldRenderLastChild,
    boolean          isFirstChild,
    boolean          isLastChild,
    boolean          isVertical
    ) throws IOException
  {

    boolean separatorOnNewRow = shouldRenderSeparatorOnNewLineWhenVertical(rc);

    if (!isLastChild || shouldRenderLastChild)
    {
      renderStartOfLink(context, isVertical);

      ResponseWriter writer = context.getResponseWriter();

      // if oriented vertically, then indent the levels
      if(!isFirstChild && isVertical)
      {
        char[] chars = new char[1];
        chars[0] = XhtmlConstants.NBSP_CHAR;

        int indents = separatorOnNewRow ? renderedCount - 1 : renderedCount;
        int indentSpaces = indents * getNumberOfIndentSpaces(rc);
        for(int i = 0; i < indentSpaces; i++)
        {
          writer.writeText(chars, 0, 1);
        }
      }

      if (rc.isRightToLeft())
      {
        writer.startElement(XhtmlConstants.SPAN_ELEMENT, null);
        writer.writeAttribute(XhtmlConstants.DIR_ATTRIBUTE_VALUE, "rtl", null);
      }

      if (!isFirstChild && (isVertical && separatorOnNewRow) )
      {
        OutputUtils.renderIcon(context, rc, separatorIcon, "", null );
      }

      renderLink(context, rc, child, renderedCount, isLastChild);

      if (!isLastChild && (!isVertical || !separatorOnNewRow) )
      {
        OutputUtils.renderIcon(context, rc, separatorIcon, "", null );
      }

      if (rc.isRightToLeft())
      {
         writer.endElement(XhtmlConstants.SPAN_ELEMENT);
      }

      renderEndOfLink(context, isVertical);

    }
  }

  protected void renderLink(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      child,
    int              renderedCount,
    boolean          isLastChild
    ) throws IOException
  {
    if (isLastChild)
      ((CoreRenderingContext) rc).setLinkDisabled(true);

    boolean isBidi = false;
    String text = toString(child.getAttributes().get(CoreCommandLink.TEXT_KEY.getName()));
    if ((text != null) && (text.length() > 0))
    {
      char[] firstChar = new char[1];
      firstChar[0] = text.charAt(0);
      isBidi = Bidi.requiresBidi(firstChar, 0, 1);
    }

    Map<String, String> originalResourceKeyMap = rc.getSkinResourceKeyMap();
    try
    {
      rc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      if (!isBidi)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement(XhtmlConstants.SPAN_ELEMENT, null);
        writer.writeAttribute(XhtmlConstants.DIR_ATTRIBUTE_VALUE, "ltr",null);
        encodeChild(context, child);
        writer.endElement(XhtmlConstants.SPAN_ELEMENT);
      }
      else
        encodeChild(context, child);

      if (isLastChild)
        ((CoreRenderingContext) rc).setLinkDisabled(false);
    }
    finally
    {
      rc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

  protected boolean shouldRenderLastChild(
    RenderingContext rc
  )
  {
    Object propValue = rc.getSkin().getProperty(
                      SkinProperties.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY);

    return Boolean.TRUE.equals(propValue);
  }

  /**
   * renderStyleAttributes - use the NavigationPath style class as the default
   * styleClass
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    renderStyleAttributes(context, rc, component, bean,
                          SkinSelectors.AF_NAVIGATION_PATH_STYLE_CLASS);
  }

  protected String getOrientation(
    FacesBean bean)
  {
    return toString(bean.getProperty(_orientationKey));
  }


  //
  // Renders (almost) everything that goes before the link
  //
  /**
   * @todo - not rendering style elements - see breadcrumbsrenderer
   * for explanation, do we still need this code?
   */
  protected final void renderStartOfLink(
    FacesContext context,
    boolean      isVertical
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (isVertical)
    {
      writer.startElement("div", null);
    }

    // Breadcrumbs wrap within a page depending on the size of the
    // browser window. The wrap occurs to the right of the greater-than
    // symbol that follows after the breadcrumb text string,
    // and not within the text string itself.
    writer.startElement("nobr", null);
  }

  //
  // Renders everything that goes after the link
  //
  protected final void renderEndOfLink(
    FacesContext context,
    boolean      isVertical
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.endElement("nobr");

    if(isVertical)
    {
      writer.endElement("div");
    }
  }

  private Object _getFocusRowKey(
    UIXHierarchy component
  )
  {
    return component.getFocusRowKey();
  }

  private int _getMinItemCount(
    RenderingContext rc
  )
  {
    int minChildCount = 0;
    boolean shouldRenderLastChild = shouldRenderLastChild(rc);
    if ( !shouldRenderLastChild)
      minChildCount = 1;

    return minChildCount;
  }

  private int _getItemCount(
    UIXHierarchy component
    )
  {
    Object focusPath = _getFocusRowKey(component);
    int kids = getRenderedChildCount(component);

    if (focusPath == null)
      return kids;

    return kids + component.getDepth(focusPath) + 1;
  }

  /**
   * gets the orientation attribute from the link. If this attr is not set,
   * defaults to horizontal.
   * @return true if the orientation is vertical
   */
  private boolean _isVertical(
    FacesBean bean)
  {
    String orientation = getOrientation(bean);
    return XhtmlConstants.ORIENTATION_VERTICAL.equals(orientation);
  }

  protected boolean shouldRenderSeparatorOnNewLineWhenVertical(
    RenderingContext rc
  )
  {
    Object propValue = rc.getSkin().getProperty(
                                  SkinProperties.AF_BREAD_CRUMBS_SEPARATOR_ON_NEW_LINE);
    return Boolean.TRUE.equals(propValue);
  }

  protected int getNumberOfIndentSpaces(
    RenderingContext rc
  )
  {
    // In the case of narrow-screen PDAs, the number of indent spaces is
    // reduced to decrease the overall breadcrumb's width.
    if (supportsNarrowScreen(rc))
    {
      return NARROW_SCREEN_INDENT_SPACES;
    }

    Object propValue = rc.getSkin().getProperty(
                                      SkinProperties.AF_BREAD_CRUMBS_INDENT_SPACES);

    int intValue = _INDENT_SPACES;
    if (propValue != null && propValue instanceof Integer)
    {
      intValue = (Integer) propValue;
      if (intValue < 0)
        intValue = _INDENT_SPACES;
    }

    return intValue;
  }

  private PropertyKey _orientationKey;

  // # of hard spaces to use in indenting vertical breadcrumbs
  private static final int _INDENT_SPACES = 10;

  // # of hard spaces to use in indenting vertical breadcrumbs
  // in the case of narrow-screen PDAs
  private static final int NARROW_SCREEN_INDENT_SPACES = 3;

  private static final Map<String, String> _RESOURCE_KEY_MAP;
  static
  {
    _RESOURCE_KEY_MAP  =  new HashMap<String, String>();

    _RESOURCE_KEY_MAP.put(
      SkinSelectors.LINK_STYLE_CLASS,
      SkinSelectors.AF_NAVIGATION_PATH_STEP_STYLE_CLASS);
    // the selected step is disabled, which is why we map these two styles
    _RESOURCE_KEY_MAP.put(
      SkinSelectors.LINK_DISABLED_STYLE_CLASS,
      SkinSelectors.AF_NAVIGATION_PATH_SELECTED_STEP_STYLE_CLASS);
  }
}
