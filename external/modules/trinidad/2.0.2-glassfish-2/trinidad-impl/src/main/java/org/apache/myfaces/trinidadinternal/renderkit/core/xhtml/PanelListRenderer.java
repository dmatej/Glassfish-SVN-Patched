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

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.ComponentProcessingContext;
import org.apache.myfaces.trinidad.component.ComponentProcessor;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelList;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;


public class PanelListRenderer extends XhtmlRenderer
{
  public PanelListRenderer()
  {
    super(CorePanelList.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _maxColumnsKey = type.findKey("maxColumns");
    _rowsKey = type.findKey("rows");
    _listStyleKey = type.findKey("listStyle");

  }

  /*
   * We want to render the styleClass/inlineStyle attributes as well
   * as our component styleClass.
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
    throws IOException
  {
    renderStyleAttributes(context, rc, component, bean, SkinSelectors.AF_PANEL_LIST_STYLE_CLASS);
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
    Number rowsNumber = getRows(component, bean);
    int rows;
    if (rowsNumber == null)
    {
      rows = Integer.MAX_VALUE;
    }
    else
    {
      rows = rowsNumber.intValue();
    }

    Number maxColumnsNumber = getMaxColumns(component, bean);
    int maxCols;
    if (maxColumnsNumber == null)
    {
      maxCols = _getDefaultColumns(rc);
    }
    else
    {
      maxCols = maxColumnsNumber.intValue();
    }

    String listStyle = getListStyle(component, bean);

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("div", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    ((CoreRenderingContext) rc).setDefaultLinkStyleDisabled(true);

    // Gather information about the bullets and groupings:
    List<UIComponent> children = component.getChildren();
    GroupingCallbackState groupingCallbackState = new GroupingCallbackState(children.size());
    UIXComponent.processFlattenedChildren(
      context,
      _groupingCallback,
      children,
      groupingCallbackState);

    int bulletCount = groupingCallbackState.bulletCount;

    // If there are no kids or no rows or no columns, then no list
    if (bulletCount <= 0 || rows < 1 || maxCols < 1)
    {
      writer.endElement("div");
      ((CoreRenderingContext) rc).setDefaultLinkStyleDisabled(false);
      return;
    }

    boolean usesPossiblyMultipleColumnDom = false;
    if (rows < bulletCount)
    {
      usesPossiblyMultipleColumnDom = true;
      writer.startElement("table", null);
      OutputUtils.renderLayoutTableAttributes(context, rc, "0", "100%");
      writer.startElement("tr", null);
    }

    int bulletsPerColumn = bulletCount;
    int columnCount = (int)Math.ceil((double)bulletCount / (double)rows);

    if (columnCount > maxCols)
    {
      columnCount = maxCols;
    }

    if (columnCount > 1)
    {
      bulletsPerColumn = (int)Math.ceil(((double)bulletCount) / (double) columnCount);
    }

    String columnWidth = IntegerUtils.getString(100 / columnCount) + "%";

    // Encode the columns and bullets:
    EncodingCallbackState encodingCallbackState =
      new EncodingCallbackState(
        groupingCallbackState.groupStates,
        bulletsPerColumn,
        columnWidth,
        listStyle,
        usesPossiblyMultipleColumnDom);
    UIXComponent.encodeFlattenedChildren(
      context,
      _encoderCallback,
      children,
      encodingCallbackState);

    if (usesPossiblyMultipleColumnDom)
    {
      writer.endElement("tr");
      writer.endElement("table");
    }

    writer.endElement("div");
    ((CoreRenderingContext) rc).setDefaultLinkStyleDisabled(false);
  }

  protected Number getRows(
    UIComponent component,
    FacesBean   bean)
  {
    return (Number)bean.getProperty(_rowsKey);
  }

  protected Number getMaxColumns(
    UIComponent component,
    FacesBean   bean)
  {
    return (Number)bean.getProperty(_maxColumnsKey);
  }

  protected String getListStyle(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_listStyleKey));
  }

  private void _renderListStyle(
    FacesContext context,
    String       listStyle
    ) throws IOException
  {
    if (listStyle != null)
    {
      context.getResponseWriter().writeAttribute("style", listStyle, "listStyle");
    }
  }

  /**
   * Get the default columns off of the skin property.
   */
  private int _getDefaultColumns(
    RenderingContext rc)
  {
    // TODO Either document this skin property in skin-selectors.xml and add it to the skin demo
    //      page or put it into the panelList component metadata xml file.
    Integer defaultColumns =
      (Integer)rc.getSkin().getProperty(SkinProperties.AF_PANEL_LIST_DEFAULT_COLUMNS);
    return (defaultColumns != null) ? defaultColumns.intValue() : COLUMNS_DEFAULT;
  }

  /**
   * Enumeration representing the current state of the grouping transitions for this child.
   */
  private enum GroupState
  {
    /** This component is starting a group. */
    START_GROUP,

    /** This component is inside a group. */
    IN_GROUP,

    /** This component is outside of a group. */
    OUTSIDE_GROUP
  };

  /**
   * Context object used to collect information about the grouping of the children to be laid out.
   */
  private static class GroupingCallbackState
  {
    public GroupingCallbackState(
      int initialSize)
    {
      this.groupStates = new ArrayList<GroupState>(initialSize);
    }

    protected final List<GroupState> groupStates;
    protected int                    bulletCount;
  }

  /**
   * Visitor for each of the flattened children in the PanelGroupLayoutRenderer, using the
   * information in the GroupingCallbackState.
   */
  private class GroupingCallback implements ComponentProcessor<GroupingCallbackState>
  {
    public void processComponent(
      FacesContext               facesContext,
      ComponentProcessingContext processContext,
      UIComponent                child,
      GroupingCallbackState      callbackState)
      throws IOException
    {
      // Ungrouped items qualifiy for a leading separator and wrapper element.
      // Starts of groups qualify for a leading separator and wrapper element.
      // In-group items do not qualify for a leading separator nor wrapper element.
      if (processContext.getGroupDepth() == 0)
      {
        // Ungrouped.  This child starts a new bullet of the list.
        callbackState.groupStates.add(GroupState.OUTSIDE_GROUP);
        callbackState.bulletCount++;
      }
      else if (processContext.getStartDepth() > 0)
      {
        // Start of a new grouping.  This child starts a new bullet of the list.
        callbackState.groupStates.add(GroupState.START_GROUP);
        callbackState.bulletCount++;
      }
      else
      {
        // Inside of a grouping. This child is inside of an existing bullet of the list.
        callbackState.groupStates.add(GroupState.IN_GROUP);
      }
    }
  }

  /**
   * State passed to the EncoderCallback.
   */
  private static class EncodingCallbackState
  {
    public EncodingCallbackState(
      List<GroupState> groupStates,
      int              bulletsPerColumn,
      String           columnWidth,
      String           listStyle,
      boolean          usesPossiblyMultipleColumnDom)
    {
      this.groupStates                   = groupStates;
      this.bulletsPerColumn              = bulletsPerColumn;
      this.columnWidth                   = columnWidth;
      this.listStyle                     = listStyle;
      this.usesPossiblyMultipleColumnDom = usesPossiblyMultipleColumnDom;
    }

    protected final List<GroupState> groupStates;
    protected final int              bulletsPerColumn;
    protected final String           columnWidth;
    protected final String           listStyle;
    protected final boolean          usesPossiblyMultipleColumnDom;
    protected int                    visitedChildIndex;
    protected int                    currentColumnIndex;
  }

  /**
   * Visitor for each of the flattened children in the ToolboxRenderer, using the information in the
   * EncoderCallbackState.
   */
  private class EncoderCallback implements ComponentProcessor<EncodingCallbackState>
  {
    public void processComponent(
      FacesContext               facesContext,
      ComponentProcessingContext processContext,
      UIComponent                child,
      EncodingCallbackState      encodingState)
      throws IOException
    {
      ResponseWriter   rw                            = facesContext.getResponseWriter();
      boolean          usesPossiblyMultipleColumnDom = encodingState.usesPossiblyMultipleColumnDom;
      int              visitedChildIndex             = encodingState.visitedChildIndex++;
      int              currentColumnIndex            = encodingState.currentColumnIndex;
      int              bulletsPerColumn              = encodingState.bulletsPerColumn;
      List<GroupState> groupStates                   = encodingState.groupStates;
      GroupState       groupState                    = groupStates.get(visitedChildIndex);
      boolean          isNewLayoutItem               = !GroupState.IN_GROUP.equals(groupState);
      int              childCount                    = groupStates.size();
      boolean          isLayoutItemEnd               = true;

      if (!GroupState.OUTSIDE_GROUP.equals(groupState) && visitedChildIndex < childCount - 1)
      {
        // The current child is a layout end item if any of the following are true:
        // - it is outside of a group
        // - it is inside of a group and the next child is outside of a group
        // - it is inside of a group and the next child is the start of a new group
        GroupState nextGroupState = groupStates.get(1+visitedChildIndex);
        isLayoutItemEnd =
          GroupState.OUTSIDE_GROUP.equals(nextGroupState) ||
          GroupState.START_GROUP.equals(nextGroupState);
      }

      if (currentColumnIndex >= bulletsPerColumn)
      {
        // Reset the column index to zero:
        currentColumnIndex = 0; // the index of this child
      }

      if (isNewLayoutItem)
      {
        if (currentColumnIndex == 0)
        {
          if (usesPossiblyMultipleColumnDom)
          {
            // Do this if all of the following are true:
            // - We have DOM for multiple columns
            // - This child is a new layout item (either ungrouped item or the first item in a group)
            // - This is the first child of the current column
            rw.startElement("td", null);
            rw.writeAttribute("width", encodingState.columnWidth, null);
            rw.writeAttribute("valign", "top", null);
          }

          // Start the UL if this child is the first item of the column:
          rw.startElement("ul", null);
          _renderListStyle(facesContext, encodingState.listStyle);
        }

        // Start the LI if this is the first item of the group or if the item is ungrouped:
        rw.startElement("li", null);
        rw.writeAttribute("value", 1+visitedChildIndex, null); // must start at 1, not zero!
      }

      encodeChild(facesContext, child);

      if (isLayoutItemEnd)
      {
        // End the LI if this is the last item of the group or if the item is ungrouped:
        rw.endElement("li");

        // End the UL if this child is the last item of the column:
        if (currentColumnIndex == bulletsPerColumn - 1 ||
            visitedChildIndex == childCount - 1)
        {
          rw.endElement("ul");

          if (usesPossiblyMultipleColumnDom)
          {
            // Do this if all of the following are true:
            // - We have DOM for multiple columns
            // - This child is an end layout item (either ungrouped item or the last item in a group)
            // - This is the last child of the column (in other words, the index of this child in the
            //   current column is the last index or the child is actually the very last child overall)
            rw.endElement("td");
          }
        }

        // Increment the currentColumnIndex since this was the end of the layout item:
        encodingState.currentColumnIndex = ++currentColumnIndex;
      }
    }
  }

  private final GroupingCallback _groupingCallback = new GroupingCallback();
  private final EncoderCallback  _encoderCallback = new EncoderCallback();

  private  static final int COLUMNS_DEFAULT = 3;

  private PropertyKey _maxColumnsKey;
  private PropertyKey _rowsKey;
  private PropertyKey _listStyleKey;
}
