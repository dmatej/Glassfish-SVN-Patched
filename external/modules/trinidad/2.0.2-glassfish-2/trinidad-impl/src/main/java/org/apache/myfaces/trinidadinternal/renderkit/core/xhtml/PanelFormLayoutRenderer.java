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

import java.awt.Dimension;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.ComponentProcessingContext;
import org.apache.myfaces.trinidad.component.ComponentProcessor;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelFormLayout;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class PanelFormLayoutRenderer extends XhtmlRenderer
{
  public PanelFormLayoutRenderer()
  {
    super(CorePanelFormLayout.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _labelAlignmentKey = type.findKey("labelAlignment");
    _labelWidthKey = type.findKey("labelWidth");
    _fieldWidthKey = type.findKey("fieldWidth");
    _rowsKey = type.findKey("rows");
    _maxColumnsKey = type.findKey("maxColumns");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * This is how we can render both the user defined styleClass and our
   * component style class.
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    renderStyleAttributes(context, rc, component, bean, SkinSelectors.AF_PANEL_FORM_STYLE_CLASS);
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
    throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", component); // the root element
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    int maxColumns = 0;
    Number maxColumnsNumber = _getMaxColumns(component, bean);
    if (maxColumnsNumber != null)
    {
      maxColumns = maxColumnsNumber.intValue();
    }
    else
    {
      maxColumns = _getColumnsDefault();
    }

    int rows = 0;
    if (isPDA(rc))
    {
      maxColumns = 1;
    }
    Number rowsNumber = _getRows(component, bean);
    if (rowsNumber == null)
    {
      rows = Integer.MAX_VALUE;
    }
    else
    {
      rows = rowsNumber.intValue();
      if (rows < 1)
      {
        rows = Integer.MAX_VALUE;
      }
    }

    _encodeChildren(context, rc, component, bean, maxColumns, rows);

    rw.endElement("div"); // the root element
  }

  /**
   * Retrieves whether the parent is a PanelFormLayout and this component should
   * be rendered specifically for it.
   * @param context the Faces context
   * @param component the component being rendered
   * @return true if the parent is a PanelFormLayout
   */
  static boolean __isInPanelFormLayout(
    FacesContext context,
    UIComponent  component)
  {
   Map requestMap = context.getExternalContext().getRequestMap();
   Object formItem = requestMap.get(_PANEL_FORM_LAYOUT_FORM_ITEM_KEY);
   return component.equals(formItem);
  }

  /**
   * Encodes DOM specifically for a child.
   * @param context the FacesContext
   * @param rc the RenderingContext
   * @param component the component to render
   * @param bean the FacesBean of the component to render
   * @param maxColumns
   * @param rows
   * @throws IOException if there is a problem with the encoding
   */
  @SuppressWarnings("unchecked")
  private void _encodeChildren(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    int              maxColumns,
    int              rows
    ) throws IOException
  {
    // We cannot render a nested panelForm with any more than a single column
    // so we must monitor whether we are nested or not:
    int nestLevel = _getNestLevel(context, -1) + 1;

    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    requestMap.put(_PANEL_FORM_LAYOUT_NEST_LEVEL_KEY, nestLevel);

    // Iterate through the childPeers extracting and counting the number of
    // rendered children, also count the rendered children inside of rendered groups:
    List<UIComponent> formChildren = component.getChildren();

    GroupingState renderedGroupingStates = new GroupingState(formChildren.size());

    UIXComponent.processFlattenedChildren(context,
                                          _renderedItemExtractor,
                                          formChildren,
                                          renderedGroupingStates);

    List<GroupState> renderedGroupStates = renderedGroupingStates.groupStates;

    int totalFormItemCount = renderedGroupStates.size();

    UIComponent footerFacetComponent = getFacet(component, "footer");

    // Iterate through the footerPeers extracting the rendered children:
    int totalFooterItemCount = 0;
    List<GroupState> footerGroupStates = null;
    if (footerFacetComponent != null)
    {
      GroupingState footerGroupingStates = new GroupingState(footerFacetComponent.getChildCount());

      UIXComponent.processFlattenedChildren(context,
                                            _renderedItemExtractor,
                                            footerFacetComponent,
                                            footerGroupingStates);
      footerGroupStates = footerGroupingStates.groupStates;
      totalFooterItemCount = footerGroupStates.size();
    }

    // convert the groupStates list into an array of sizes for each non-breakable group
    int[] renderedGroupSizes = new int[totalFormItemCount];
    int renderedGroupsCount = _computeRenderedGroupSizes(renderedGroupStates, renderedGroupSizes);

    // Now that we have the list and counts of rendered form items (and group
    // arrangements), we must figure out how many actual columns and actual rows
    // we really need:
    int actualColumns = maxColumns;
    int actualRows = rows;
    Object labelAlignment;

    // In the case of narrow-screen PDAs, the label and field are rendered
    // vertically to reduce the overall page's width. Thus, labelAlignment
    // is always "top" for narrow-screen PDAs.
    if (supportsNarrowScreen(rc))
    {
      labelAlignment = "top";
    }
    else
    {
      labelAlignment = _getLabelAlignment(component, bean);
    }

    boolean forceSingleColumn = (nestLevel != 0);
    boolean startAlignedLabels = !forceSingleColumn;
    if ("start".equals(labelAlignment))
    {
      startAlignedLabels = true;
    }
    else if ("top".equals(labelAlignment))
    {
      startAlignedLabels = false;
    }
    if ( forceSingleColumn || (totalFormItemCount == 0) )
    {
      // Must use a single column and unlimited rows:
      actualColumns = 1;
      actualRows = Integer.MAX_VALUE;
    }
    else if (actualColumns == 1)
    {
      // Developer wanted to use a single column and unlimited rows:
      actualRows = Integer.MAX_VALUE;
    }
    else
    {
      // We must compute how many rows will fit in the given max number of columns
      // and also see if there are actually fewer columns needed:
      Dimension actualResults = PanelFormLayoutRenderer._computeActualColumnsAndRows(
        actualColumns,
        actualRows,
        totalFormItemCount,
        renderedGroupSizes,
        renderedGroupsCount);
      actualColumns = (int)actualResults.getWidth();
      actualRows = (int)actualResults.getHeight();
    }

    if (actualColumns < 1)
    {
      return;
    }

    // These widths can either be pixels, percentages, or undefined.
    // We must ensure that if using percentages or undefined that we correct them
    // to total up properly.
    String labelWidth = (String)_getLabelWidth(component, bean);
    String fieldWidth = (String)_getFieldWidth(component, bean);

    // Create the DOM for the form:
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", null); // the outer table
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);

    FormWidths effectiveWidths =
      _getFormWidths(startAlignedLabels, labelWidth, fieldWidth, actualColumns);
    rw.writeAttribute("style", "width: " + effectiveWidths.getOverallWidth(), null);

    rw.startElement("tbody", null); // the outer tbody
    rw.startElement("tr", null); // the outer row

    // build up list column and group starts for rendering
    List<LayoutAction> renderedLayoutActions =
                         _computeLayoutActions(actualRows, renderedGroupSizes, renderedGroupStates);

    // Create the form columns:

    ColumnEncodingState columnEncodingState = new ColumnEncodingState(
      rc,
      startAlignedLabels,
      effectiveWidths.getMainLabelWidth(),
      effectiveWidths.getMainFieldWidth(),
      actualColumns,
      actualRows,
      1, // colSpan
      renderedLayoutActions);

    UIXComponent.encodeFlattenedChildren(context,
                                         _formColumnEncoder,
                                         formChildren,
                                         columnEncodingState);

    rw.endElement("tr"); // the outer row

    // Create the column-spanning footer row(s):
    if (totalFooterItemCount > 0)
    {
      rw.startElement("tr", null); // the outer row

      // convert the footer group states into the encoding information for the footer
      int[] footerGroupSizes = new int[totalFooterItemCount];
      _computeRenderedGroupSizes(footerGroupStates, footerGroupSizes);

      List<LayoutAction> footerLayoutActions =
                  _computeLayoutActions(totalFooterItemCount, footerGroupSizes, footerGroupStates);

      ColumnEncodingState footerEncodingState = new ColumnEncodingState(
        rc,
        startAlignedLabels,
        effectiveWidths.getFooterLabelWidth(),
        effectiveWidths.getFooterFieldWidth(),
        1, // column count
        totalFooterItemCount, // row count
        actualColumns, // this is actually colSpan
        footerLayoutActions);

      UIXComponent.encodeFlattenedChild(context, _formColumnEncoder, footerFacetComponent, footerEncodingState);

      rw.endElement("tr"); // the outer row
    }

    // Indicate that we are leaving this level of nesting:
    if (nestLevel == 0)
    {
      // delete the value altogether:
      requestMap.remove(_PANEL_FORM_LAYOUT_NEST_LEVEL_KEY);
    }
    else
    {
      // decrement the value:
      requestMap.put(_PANEL_FORM_LAYOUT_NEST_LEVEL_KEY, nestLevel - 1);
    }

    rw.endElement("tbody"); // the outer tbody
    rw.endElement("table"); // the outer table
  }

  /**
   * Convert the grouping information into an array of sizes for each rendered group and the valid
   * size of the grouping array
   * @param groupStates Grouping information about every child to lay out
   * @param renderedGroupSizes Array of group sizes to fill in.  This array is sized for the
   *                           maximum possible capacity (the number of group items)
   * @return the number of valid entries in the filled in renderedGroupSizes array
   */
  private static int _computeRenderedGroupSizes(
    List<GroupState> groupStates,
    int[]            renderedGroupSizes)
  {
    int totalFormItemCount = renderedGroupSizes.length;

    // build up array of rendered item size and compute the number of row blocks
    int currGroupSize = 0;
    int renderedGroupsCount = 0;

    for (int itemIndex = 0; itemIndex < totalFormItemCount; itemIndex++)
    {
      GroupState currGroupState = groupStates.get(itemIndex);

      if (GroupState.IN_GROUP.equals(currGroupState))
      {
        currGroupSize++;
      }
      else
      {
        assert GroupState.START_GROUP.equals(currGroupState) ||
               GroupState.OUTSIDE_GROUP.equals(currGroupState) ||
               GroupState.IN_GROUP_TRANSITION.equals(currGroupState) : "unexpected groups state";

        // write out the previous group we have been collecting
        if (currGroupSize > 0)
          renderedGroupSizes[renderedGroupsCount++] = currGroupSize;

        currGroupSize = 1;
      }
    }

    // write out the last group
    if (currGroupSize > 0)
      renderedGroupSizes[renderedGroupsCount++] = currGroupSize;

    return renderedGroupsCount;
  }

  /**
   * Given the grouping information and maximum number of rows per column, compute the
   * actual layout of the panelFormLayout
   * @param rowCount Maxim
   * @param renderedGroupSizes Size information for each group of children
   * @param groupStates Grouping state information for child to lay out
   * @return LayoutActions for each child to lay out
   */
  private static List<LayoutAction> _computeLayoutActions(
    int              rowCount,
    int[]            renderedGroupSizes,
    List<GroupState> groupStates)
  {
    int totalFormItemCount = groupStates.size();

    if (totalFormItemCount == 0)
      return Collections.emptyList();

    List<LayoutAction> layoutActions = new ArrayList<LayoutAction>(totalFormItemCount);

    int currRowIndex = 0;
    int renderedGroupIndex = 0;
    int groupEndIndex = renderedGroupSizes[0];
    GroupState lastGroupState = GroupState.OUTSIDE_GROUP;

    for (int i = 0; i < totalFormItemCount; i++)
    {
      // we have finished the current group.  Try the next one
      if (i == groupEndIndex)
      {
        int groupSize = renderedGroupSizes[++renderedGroupIndex];
        groupEndIndex += groupSize;

        // determine when we need to start a new column
        if (currRowIndex + groupSize > rowCount)
          currRowIndex = 0;
      }

      // update group state
      GroupState currGroupState = groupStates.get(i);
      LayoutAction layoutAction;

      if (currRowIndex == 0)
        layoutAction = LayoutAction.NEW_COLUMN;
      else
      {
        if (GroupState.START_GROUP.equals(currGroupState) ||
            GroupState.IN_GROUP_TRANSITION.equals(currGroupState) ||
            (GroupState.OUTSIDE_GROUP.equals(currGroupState) &&
             GroupState.IN_GROUP.equals(lastGroupState)) ||
            (GroupState.OUTSIDE_GROUP.equals(currGroupState) &&
             GroupState.IN_GROUP_TRANSITION.equals(lastGroupState)) ||
            (GroupState.OUTSIDE_GROUP.equals(currGroupState) &&
             GroupState.START_GROUP.equals(lastGroupState)))
          layoutAction = LayoutAction.NEW_GROUP;
        else
          layoutAction = LayoutAction.FLAT;
      }

      layoutActions.add(layoutAction);

      // try next row
      lastGroupState = currGroupState;
      currRowIndex++;
    }

    return layoutActions;
  }

  /**
   * Calculates the number of rows and columns that will be used.
   * @param guessColumns initial estimate at the number of columns
   * @param guessRows initial estimate at the number of rows
   * @param totalFormItemCount the number of items in the form
   * @param renderedGroupSizes array of ints for the sizes of each non-breaking rendered block
   * @param renderedGroupsCount # of valid entries in renderedGroupSizes
   * @return a Dimension where width represents the number of columns and height
   *         represents the number of rows
   */
  private static Dimension _computeActualColumnsAndRows(
    int   guessColumns,
    int   guessRows,
    int   totalFormItemCount,
    int[] renderedGroupSizes,
    int   renderedGroupsCount)
  {
    if (totalFormItemCount == 0)
      return new Dimension(0, 0);

    int actualRows = guessRows;
    int actualColumns = guessColumns;

    // Take a first guess at what the actualRows needs to be though we might
    // need to use a larger value.  For example, if rows = 4, maxColumns = 3,
    // totalFormItemCount = 11, and items 2 through 6 are in a group, then 3
    // columns and 4 rows are not enough!

    // first guess:
    // (note the page developer can specify a rows value higher than is actually
    // needed so we must take that into account)

    // determine number of rows per column needed to display form items, rounding up
    int rowsPerColumn = (totalFormItemCount + actualColumns - 1) / actualColumns;

    if (rowsPerColumn > actualRows)
      actualRows = rowsPerColumn;

    // test the first guess and keep trying more rows until it fits:
    boolean itemsWillFit = false;
    while (!itemsWillFit)
    {
      int currentItemIndex = 0;
      for (int col=0; (col<actualColumns && !itemsWillFit); col++)
      {
        int currentRow = 0;
        while (currentRow < actualRows)
        {
          int itemSize = renderedGroupSizes[currentItemIndex];
          if ( (currentRow == 0) || (currentRow + itemSize <= actualRows) )
          {
            // If the item or group of items is the first one in this column...
            //  - or -
            // If the item or group of items fit...

            // This particular item or group of items fits:
            currentItemIndex++;
            currentRow += itemSize;

            if (currentItemIndex >= renderedGroupsCount)
            {
              // the items fit so we now know how many rows and columns we need:
              actualColumns = 1 + col;
              itemsWillFit = true;
              break;
            }
          }
          else
          {
            break;
          }
        }
      }

      if (!itemsWillFit)
      {
        // didn't fit, so let's try more rows:
        actualRows++;
      }
    }

    return new Dimension(actualColumns, actualRows);
  }

  /**
   * Enumeration for the 3 possible label and field width types: None, Pixel, and Percentage.
   */
  private enum WidthType { NONE, PIXEL, PERCENT }

  /**
   * Calculates information about the effective widths of the main form items,
   * the footer form items, and the overall form.
   * @param startAlignLabels true means start aligned, false is top aligned
   * @param labelWidth the specified label width
   * @param fieldWidth the specified field width
   * @param actualColumns the actual number of columns in the form
   * @return width information for various parts of the form
   */
  private FormWidths _getFormWidths(
    boolean startAlignLabels,
    String  labelWidth,
    String  fieldWidth,
    int     actualColumns)
  {
    WidthType labelWidthType = WidthType.NONE;
    WidthType fieldWidthType = WidthType.NONE;

    if (labelWidth != null)
    {
      //determine if label width is pixel or percent
      int percentCharIndex = labelWidth.indexOf("%");
      labelWidthType = percentCharIndex == -1 ? WidthType.PIXEL : WidthType.PERCENT;
    }

    if (fieldWidth != null)
    {
      //determine if field width is pixel or percent
      int percentCharIndex = fieldWidth.indexOf("%");
      fieldWidthType = percentCharIndex == -1 ? WidthType.PIXEL : WidthType.PERCENT;
    }

    return startAlignLabels ? _getStartAlignedFormWidths(
                                labelWidth,
                                labelWidthType,
                                fieldWidth,
                                fieldWidthType,
                                actualColumns)
                            : _getTopAlignedFormWidths(
                                labelWidth,
                                labelWidthType,
                                fieldWidth,
                                fieldWidthType,
                                actualColumns);
  }

  /**
   * Calculates information about the effective widths of the main form items,
   * the footer form items, and the overall form for start aligned labels.
   * @param labelWidth the specified label width
   * @param labelWidthType the type of label width: None, Pixel, or Percentage
   * @param fieldWidth the specified field width
   * @param fieldWidthType the type of field width: None, Pixel, or Percentage
   * @param actualColumns the actual number of columns in the form
   * @return width information for various parts of the form
   */
  private FormWidths _getStartAlignedFormWidths(
    String    labelWidth,
    WidthType labelWidthType,
    String    fieldWidth,
    WidthType fieldWidthType,
    int       actualColumns)
  {
    String effectiveLabelWidth = null;
    String effectiveFieldWidth = null;
    String effectiveFooterLabelWidth = null;
    String effectiveFooterFieldWidth = null;
    String outerTableWidth = "100%";

    double labelRatio = 1.0;
    double fieldRatio = 1.0;

    //if only one value is a percent (exclusive or) than force the other to the remaining percent
    if (labelWidthType.equals(WidthType.PERCENT) ^ fieldWidthType.equals(WidthType.PERCENT))
    {
      if (labelWidthType.equals(WidthType.PERCENT))
      {
        int percentCharIndex = labelWidth.indexOf("%");
        int percentValue = Integer.valueOf(labelWidth.substring(0, percentCharIndex));
        fieldWidth = (100 - percentValue) + "%";
        fieldWidthType = WidthType.PERCENT;
      }
      else //fieldWidth is percent
      {
        int percentCharIndex = fieldWidth.indexOf("%");
        int percentValue = Integer.valueOf(fieldWidth.substring(0, percentCharIndex));
        labelWidth = (100 - percentValue) + "%";
        labelWidthType = WidthType.PERCENT;
      }
    }

    if (labelWidthType.equals(WidthType.PIXEL))
    {
      // Use the same number of pixels for the column labels as for the footer labels:
      effectiveLabelWidth = labelWidth + "px";
      effectiveFooterLabelWidth = effectiveLabelWidth;
    }

    if (fieldWidthType.equals(WidthType.PIXEL))
    {
      effectiveFieldWidth = fieldWidth + "px";
      effectiveFooterFieldWidth = effectiveFieldWidth;
    }

    if ( labelWidthType.equals(WidthType.PERCENT) && fieldWidthType.equals(WidthType.PERCENT) )
    {
      // We have a percentage for both label and field.

      // Note the percentage ratios (they will be normalized later):
      int labelPercentCharIndex = labelWidth.indexOf("%");
      labelRatio = Double.valueOf(labelWidth.substring(0, labelPercentCharIndex));
      int fieldPercentCharIndex = fieldWidth.indexOf("%");
      fieldRatio = Double.valueOf(fieldWidth.substring(0, fieldPercentCharIndex));

      // BlackBerry(BB) browsers cannot handle width if it is expressed in
      // percentage and the percentage value contains a decimal points like
      // 40.0%. So lets truncate the percentage value from the decimal point
      // for BB browsers. Example, instead of 40.0%, lets render 40%.
      RenderingContext arc = RenderingContext.getCurrentInstance();
      Agent agent = arc.getAgent();

      boolean isBlackBerry =
                 Agent.AGENT_BLACKBERRY.equals(agent.getAgentName());

      // Now normalize the percentages (including the footer label width):
      double ratioTotal = (labelRatio + fieldRatio) / 100;
      double effectiveLabelWidthDouble = labelRatio / ratioTotal;
      double footerLabel = effectiveLabelWidthDouble / actualColumns;
      double footerField = 100 - footerLabel;

      if (isBlackBerry)
      {
        effectiveLabelWidth = (int) effectiveLabelWidthDouble + "%";
        effectiveFieldWidth = (int) (fieldRatio / ratioTotal) + "%";
        effectiveFooterLabelWidth = (int) _roundTo2DecimalPlaces(footerLabel) + "%";
        effectiveFooterFieldWidth = (int) _roundTo2DecimalPlaces(footerField) + "%";
      }
      else
      {
        effectiveLabelWidth = Math.floor(effectiveLabelWidthDouble) + "%";
        effectiveFieldWidth = Math.floor(fieldRatio / ratioTotal) + "%";
        effectiveFooterLabelWidth = _roundTo2DecimalPlaces(footerLabel) + "%";
        effectiveFooterFieldWidth = _roundTo2DecimalPlaces(footerField) + "%";
      }
    }
    else if ( labelWidthType.equals(WidthType.PIXEL) && fieldWidthType.equals(WidthType.PIXEL) )
    {
      // Pixels are used for both labels and fields:
      // This is important to note because the outer table must not be set to
      // a certain number of pixels.
      int labelPixels = Integer.valueOf(labelWidth);
      int fieldPixels = Integer.valueOf(fieldWidth);
      int outerTablePixels = (labelPixels + fieldPixels) * actualColumns;
      outerTableWidth = outerTablePixels + "px";
      effectiveFooterFieldWidth = (outerTablePixels - labelPixels) + "px";
    }
    else
    {
      // Either label or field does not have a width value, so we set table width to auto.
      outerTableWidth = "auto";
    }
    return new FormWidths(
      effectiveLabelWidth,
      effectiveFieldWidth,
      effectiveFooterLabelWidth,
      effectiveFooterFieldWidth,
      outerTableWidth);
  }

  /**
   * Calculates information about the effective widths of the main form items,
   * the footer form items, and the overall form for top aligned labels.
   * @param labelWidth the specified label width
   * @param labelWidthType the type of label width: None, Pixel, or Percentage
   * @param fieldWidth the specified field width
   * @param fieldWidthType the type of field width: None, Pixel, or Percentage
   * @param actualColumns the actual number of columns in the form
   * @return width information for various parts of the form
   */
  private FormWidths _getTopAlignedFormWidths(
    String    labelWidth,
    WidthType labelWidthType,
    String    fieldWidth,
    WidthType fieldWidthType,
    int       actualColumns)
  {
    String effectiveLabelWidth = null;
    String effectiveFieldWidth = null;
    String effectiveFooterLabelWidth = null;
    String effectiveFooterFieldWidth = null;
    String outerTableWidth;

    if (labelWidthType.equals(WidthType.NONE) && fieldWidthType.equals(WidthType.NONE))
    {
      // If no percentages or pixels are used for either labels or fields then we set to auto.
      outerTableWidth = "auto";
    }
    else if (labelWidthType.equals(WidthType.PERCENT) || fieldWidthType.equals(WidthType.PERCENT))
    {
      //If either one is set to a percent, then we use 100%, because with top aligned the only
      //choices are auto, pixel value, or 100%
      outerTableWidth = "100%";
      effectiveLabelWidth = labelWidthType.equals(WidthType.PERCENT) ? "100%" : null;
      effectiveFieldWidth = fieldWidthType.equals(WidthType.PERCENT) ? "100%" : null;
      effectiveFooterLabelWidth = effectiveLabelWidth;
      effectiveFooterFieldWidth = effectiveFieldWidth;
    }
    else
    {
      //either label or field has a pixel (or no) width value. Use the larger value of the two for
      //the table width.

      int labelPixels = 0;
      int fieldPixels = 0;

      if (labelWidthType.equals(WidthType.PIXEL))
      {
        effectiveLabelWidth = labelWidth + "px";
        effectiveFooterLabelWidth = effectiveLabelWidth;
        labelPixels = Integer.valueOf(labelWidth);
      }

      if (fieldWidthType.equals(WidthType.PIXEL))
      {
        effectiveFieldWidth = fieldWidth + "px";
        effectiveFooterFieldWidth = effectiveFieldWidth;
        fieldPixels = Integer.valueOf(fieldWidth);
      }
      //outertable is the larger pixel value x # columns
      int tableSize = Math.max(labelPixels, fieldPixels) * actualColumns;
      outerTableWidth = tableSize + "px";
    }
    return new FormWidths(
      effectiveLabelWidth,
      effectiveFieldWidth,
      effectiveFooterLabelWidth,
      effectiveFooterFieldWidth,
      outerTableWidth);
  }

  private double _roundTo2DecimalPlaces(
    double value)
  {
    return Math.round(value * 100) / 100.0;
  }

  /**
   * Encodes a divider for grouped form items.
   * @param context the FacesContext
   * @param rc the RenderingContext
   * @param startAlignedLabels whether form labels are to the side of the fields
   * @throws IOException if there is a problem with the encoding
   */
  private static void _encodeGroupDivider(
    FacesContext     context,
    RenderingContext rc,
    boolean          startAlignedLabels
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("tr", null);
    rw.startElement("td", null);
    if (startAlignedLabels)
    {
      rw.writeAttribute("colspan", "2", null);
    }
    // =-= mcc I considered using an HR but IE6 always adds its own border around
    //     any background graphics you attempt to put inside.  Firefox 1.5 behaves
    //     as expected.  Using a DIV until we know a way to fix IE6.
    rw.startElement("div", null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_FORM_SEPARATOR_STYLE_CLASS);
    rw.endElement("div");
    rw.endElement("td");
    rw.endElement("tr");
  }

  /**
   * Encodes a form item.
   * @param context the FacesContext
   * @param rc the RenderingContext
   * @param startAlignedLabels whether form labels are to the side of the fields
   * @param item the form item
   * @throws IOException if there is a problem with the encoding
   */
  @SuppressWarnings("unchecked")
  private void _encodeFormItem(
    FacesContext     context,
    RenderingContext rc,
    boolean          startAlignedLabels,
    UIComponent      item
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    boolean isFullRow = _isFullRow(context, item);

    if (isFullRow) // "plays well" with panelForm
    {
      // If a peer wants to play well with panelForm, it must use the proper
      // PanelForm wrapper APIs to ensure proper DOM structure.
      _encodeBeforeLabelTd(context, startAlignedLabels);
      Map<String, String> originalResourceKeyMap = rc.getSkinResourceKeyMap();

      Map<String, String> resourceKeys = (startAlignedLabels)
                                           ? _RESOURCE_KEY_SIDE_BY_SIDE_MAP
                                           : _RESOURCE_KEY_STACKED_MAP;

      rc.setSkinResourceKeyMap(resourceKeys);

      try
      {
        // The following allows the child renderer to know...
        // - whether it is a direct decendant of a panelFormLayout or equivalent
        // - whether labels should be start-aligned

        // gather the current values so we can restore them later:
        Map<String, Object>  requestMap = context.getExternalContext().getRequestMap();
        Object formItemToRestore =
          requestMap.get(_PANEL_FORM_LAYOUT_FORM_ITEM_KEY);
        Object startAlignToRestore =
          requestMap.get(PANEL_FORM_LAYOUT_LABELS_START_ALIGNED_KEY);

        // specify our values and encode the child:
        requestMap.put(
          _PANEL_FORM_LAYOUT_FORM_ITEM_KEY,
          item);
        requestMap.put(
          PANEL_FORM_LAYOUT_LABELS_START_ALIGNED_KEY,
          startAlignedLabels);

        Renderer renderer = _getRenderer(context, item);
        boolean isSimple = _isSimple(item, renderer);

        if(isSimple)
        {
          rw.startElement("tr", null);
          if (startAlignedLabels)
          {
            rw.startElement("td", null); // label cell (empty)
            rw.endElement("td"); // label cell (empty)
          }
          rw.startElement("td", null); // field cell (non-empty)
          renderStyleClass(context, rc,
            SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
        }

        encodeChild(context, item);

        if(isSimple)
        {
          rw.endElement("td"); // field cell (non-empty)
          rw.endElement("tr");
        }

        // restore the old values:
        requestMap.put(_PANEL_FORM_LAYOUT_FORM_ITEM_KEY, formItemToRestore);
        requestMap.put(PANEL_FORM_LAYOUT_LABELS_START_ALIGNED_KEY, startAlignToRestore);
      }
      finally
      {
        rc.setSkinResourceKeyMap(originalResourceKeyMap);
      }
      _encodeAfterFieldTd(context, startAlignedLabels);
    }
    else // does not "play well" with panelForm
    {
      if (startAlignedLabels) // (labels side-by-side with fields)
      {
        rw.startElement("tr", null);

        rw.startElement("td", null); // label cell (empty)
        rw.endElement("td"); // label cell (empty)

        rw.startElement("td", null); // field cell (non-empty)
        renderStyleClass(context, rc,
          SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
        encodeChild(context, item);
        rw.endElement("td"); // field cell (non-empty)

        rw.endElement("tr");
      }
      else // top-aligned (labels stacked above fields)
      {
        rw.startElement("tr", null);

        rw.startElement("td", null); // field cell (non-empty)
        renderStyleClass(context, rc,
          SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
        encodeChild(context, item);
        rw.endElement("td"); // field cell (non-empty)

        rw.endElement("tr");
      }
    }
  }

  /**
   * Encodes DOM before a label's TD.
   * @param context the FacesContext
   * @param startAlignedLabels whether form labels are to the side of the fields
   * @throws IOException if there is a problem with the encoding
   */
  private static void _encodeBeforeLabelTd(
    FacesContext context,
    boolean      startAlignedLabels
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    // startAlignedLabels means (labels side-by-side with fields)
    if (!startAlignedLabels) // top-aligned (labels stacked above fields)
    {
      rw.startElement("tr", null);
      rw.startElement("td", null); // stack cell
    }
  }

  /**
   * Encodes DOM after a field's TD.
   * @param context the FacesContext
   * @param startAlignedLabels whether form labels are to the side of the fields
   * @throws IOException if there is a problem with the encoding
   */
  private static void _encodeAfterFieldTd(
    FacesContext context,
    boolean      startAlignedLabels
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    if (!startAlignedLabels) // top-aligned (labels stacked above fields)
    {
      rw.endElement("td"); // stack cell
      rw.endElement("tr"); // form item row
    }
  }

  /**
   * Class to contain information about the various widths of a form layout.
   */
  private static class FormWidths
  {
    /**
     * Setting constructor.
     * @param mainLabelWidth the width for labels in the main part of the form
     * @param mainFieldWidth the width for fields in the main part of the form
     * @param footerLabelWidth the width for labels in the footer of the form
     * @param footerFieldWidth the width for fields in the footer of the form
     * @param overallWidth the width for the overall form
     */
    FormWidths(
      String mainLabelWidth,
      String mainFieldWidth,
      String footerLabelWidth,
      String footerFieldWidth,
      String overallWidth)
    {
      _mainLabelWidth = mainLabelWidth;
      _mainFieldWidth = mainFieldWidth;
      _footerLabelWidth = footerLabelWidth;
      _footerFieldWidth = footerFieldWidth;
      _overallWidth = overallWidth;
    }

    /**
     * Retrieves the width for labels in the main part of the form.
     * @return the width for labels in the main part of the form
     */
    String getMainLabelWidth()
    {
      return _mainLabelWidth;
    }

    /**
     * Retrieves the width for fields in the main part of the form.
     * @return the width for fields in the main part of the form
     */
    String getMainFieldWidth()
    {
      return _mainFieldWidth;
    }

    /**
     * Retrieves the width for labels in the footer of the form.
     * @return the width for labels in the footer of the form
     */
    String getFooterLabelWidth()
    {
      return _footerLabelWidth;
    }

    /**
     * Retrieves the width for fields in the footer of the form.
     * @return the width for fields in the footer of the form
     */
    String getFooterFieldWidth()
    {
      return _footerFieldWidth;
    }

    /**
     * Retrieves the width for the overall form.
     * @return the width for the overall form
     */
    String getOverallWidth()
    {
      return _overallWidth;
    }

    String _mainLabelWidth;
    String _mainFieldWidth;
    String _footerLabelWidth;
    String _footerFieldWidth;
    String _overallWidth;
  }

  /**
   * Retrieves whether the child renderer can and should render a complete form
   * layout row specifically structured for the panelFormLayout.
   * @return true if the child can and should render a complete form
   *              layout row specifically structured for the panelFormLayout
   */
  private boolean _isFullRow(
    FacesContext context,
    UIComponent  component)
  {
    String rendererType = component.getRendererType();

    if (component instanceof UIXEditableValue)
    {
      return !_UNSUPPORTED_RENDERER_TYPES.contains(rendererType);
    }

    if (UIXPanel.COMPONENT_FAMILY.equals(component.getFamily()))
    {
      if ("org.apache.myfaces.trinidad.LabelAndMessage".equals(rendererType) ||
          "org.apache.myfaces.trinidad.rich.LabelAndMessage".equals(rendererType))
        return true;
    }

    Renderer renderer = _getRenderer(context, component);
    if(renderer == null)
      return false;
    else
      return renderer instanceof LabelAndMessageRenderer;
  }

  /**
   * Retrieves whether the child renderer should be wrapped in a tr/td to ensure that a
   * simple mode component causes no invalid html inside the panelFormLayout.
   * @param component the child component
   * @return true if the child is rendered in simple mode
   */
  private boolean _isSimple(
    UIComponent component,
    Renderer    renderer)
  {
    if (renderer != null && renderer instanceof InputLabelAndMessageRenderer)
    {
      return (((InputLabelAndMessageRenderer)renderer)
        .getSimple(component, getFacesBean(component)));
    }
    else
    {
      return false;
    }
  }

  /**
   * Helper method to return a <code>Renderer</code> instance for a given <code>UIComponent</code>
   * object.
   * @param context the <code>FacesContext</code> for the request we currently processing
   * @param component the child component
   * @return <code>Renderer</code> for the given <code>UIComponent</code>.
   */
  private Renderer _getRenderer(
    FacesContext context,
    UIComponent  component)
  {
    // =-= AEW Might consider altering the following approach of getting the
    //         child renderer in case anyone wants to use renderer decoration.
    String family = component.getFamily();
    String rendererType = component.getRendererType();
    if (rendererType == null)
      return null;
    else
      return context.getRenderKit().getRenderer(family, rendererType);
  }

  /**
   * Get the default number of columns
   * @return the default number of columns
   */
  private int _getColumnsDefault()
  {
    return _COLUMNS_DEFAULT;
  }

  /**
   * Property accessor for "labelWidth".
   * @param bean the FacesBean of the component to render
   * @return the defined "labelWidth" or null if not defined
   */
  private Object _getLabelWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_labelWidthKey);
  }

  /**
   * Property accessor for "labelAlignment".
   * @param bean the FacesBean of the component to render
   * @return the defined "labelAlignment" or null if not defined
   */
  private Object _getLabelAlignment(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_labelAlignmentKey);
  }

  /**
   * Property accessor for "fieldWidth".
   * @param bean the FacesBean of the component to render
   * @return the defined "fieldWidth" or null if not defined
   */
  private Object _getFieldWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_fieldWidthKey);
  }

  /**
   * Property accessor for "rows".
   * @param bean the FacesBean of the component to render
   * @return the defined "rows" or null if not defined
   */
  private Number _getRows(
    UIComponent component,
    FacesBean   bean)
  {
    return (Number)bean.getProperty(_rowsKey);
  }

  /**
   * Property accessor for "maxColumns".
   * @param bean the FacesBean of the component to render
   * @return the defined "maxColumns" or null if not defined
   */
  private Number _getMaxColumns(
    UIComponent component,
    FacesBean   bean)
  {
    return (Number)bean.getProperty(_maxColumnsKey);
  }

  /**
   * Returns the current panelFormLayout nesting level.
   * @param context
   * @param defaultValue
   * @return the current nesting level
   */
  private static int _getNestLevel(
    FacesContext context,
    int          defaultValue)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();

    Object nestLevelObject = requestMap.get(_PANEL_FORM_LAYOUT_NEST_LEVEL_KEY);

    return (nestLevelObject != null)
             ? ((Integer)nestLevelObject).intValue()
             : defaultValue;
  }

  /**
   * Context object used to collect information about the grouping of the children to be
   * laid out.
   */
  private static class GroupingState
  {
    public GroupingState(
      int initialSize)
    {
      this.groupStates = new ArrayList<GroupState>(initialSize);
    }

    protected final List<GroupState> groupStates;
    protected int lastDepth;
  }

  /**
   * Enumeration representing the current state of the grouping transitions for this child.
   */
  private enum GroupState
  {
    /** This component is starting a top-level group */
    START_GROUP,

    /** This component is inside a group */
    IN_GROUP,

    /** This component is on the boundary from one group to another */
    IN_GROUP_TRANSITION,

    /** This component is outside of any group */
    OUTSIDE_GROUP
  };

  /**
   * Enumeration controlling what layout action to perform for a particular child.
   */
  private enum LayoutAction
  {
    /** create a new column for this child */
    NEW_COLUMN,

    /** create a new group for this child */
    NEW_GROUP,

    /** render this child as-is  */
    FLAT
  };

  /**
   * Visitor used to collect information about the rendered flattened children into the
   * GroupingState.  This information is later processed to determine the correct layout.
   */
  private class RenderedItemExtractor
    implements ComponentProcessor<GroupingState>
  {
    public void processComponent(
      FacesContext               context,
      ComponentProcessingContext cpContext,
      UIComponent                currChild,
      GroupingState              groupingState
      ) throws IOException
    {
      int groupDepth = cpContext.getGroupDepth();

      groupingState.groupStates.add((groupDepth == 0)
                        ? GroupState.OUTSIDE_GROUP
                        : (cpContext.getStartDepth() > 0)
                          ? (groupingState.lastDepth == 0)
                            ? GroupState.START_GROUP
                            : GroupState.IN_GROUP_TRANSITION
                          : (groupingState.lastDepth == groupDepth)
                              ? GroupState.IN_GROUP
                              : GroupState.IN_GROUP_TRANSITION);
      groupingState.lastDepth = groupDepth;
    }
  }

  /**
   * State passed to the FormColumnEncoder to render the children of the PanelFormLayout.
   */
  private static class ColumnEncodingState
  {
    public ColumnEncodingState(
      RenderingContext   rc,
      boolean            startAlignedLabels,
      String             effectiveLabelWidth,
      String             effectiveFieldWidth,
      int                columnCount,
      int                rowCount,
      int                colSpan,
      List<LayoutAction> layoutActions)
    {
      this.rc = rc;
      this.startAlignedLabels = startAlignedLabels;
      this.effectiveLabelWidth = effectiveLabelWidth;
      this.effectiveFieldWidth = effectiveFieldWidth;
      this.columnCount = columnCount;
      this.rowCount = rowCount;
      this.colSpan = colSpan;
      this.layoutActions = layoutActions;
    }

    protected final RenderingContext   rc;
    protected final boolean            startAlignedLabels;
    protected final String             effectiveLabelWidth;
    protected final String             effectiveFieldWidth;
    protected final int                columnCount;
    protected final int                rowCount;
    protected final int                colSpan;
    protected final List<LayoutAction> layoutActions;
    protected int                      childIndex;
    protected int                      columnIndex = -1;
  }

  /**
   * Visitor that Renders each of the flattened children in the PanelFormLayoutRenderer, using
   * the information in the ColumnEncodingState.
   */
  private class FormColumnEncoder implements ComponentProcessor<ColumnEncodingState>
  {
    public void processComponent(
      FacesContext               context,
      ComponentProcessingContext cpContext,
      UIComponent                currChild,
      ColumnEncodingState        columnEncodingState
      ) throws IOException
    {
      ResponseWriter rw = context.getResponseWriter();

      final boolean startAlignedLabels = columnEncodingState.startAlignedLabels;

      List<LayoutAction> layoutActions = columnEncodingState.layoutActions;

      LayoutAction currLayoutAction = layoutActions.get(columnEncodingState.childIndex);

      // render the enclosing new column of the panelFormLayout, if necessary
      if (LayoutAction.NEW_COLUMN.equals(currLayoutAction))
      {
        // terminate the previously created column we created if this isn't the first column
        if (columnEncodingState.childIndex > 0)
        {
          _finishColumn(rw);
        }

        // bump up the column index for next time
        final int columnIndex = ++columnEncodingState.columnIndex;
        final int columnCount = columnEncodingState.columnCount;

        final int columnPercentage = 100 / columnCount;

        //String outerColumnWidth = Math.floor(columnPercentage) + "%";
        String outerColumnWidth = columnPercentage + "%";

        rw.startElement("td", null); // the outer column
        renderStyleClass(context,
                         columnEncodingState.rc,
                         SkinSelectors.AF_PANEL_FORM_COLUMN_STYLE_CLASS);

        rw.writeAttribute("colspan", columnEncodingState.colSpan, null);
        if (columnIndex < columnCount - 1) // let the last column take the leftover space
        {
          rw.writeAttribute("width", outerColumnWidth, null);
        }

        rw.startElement("table", null); // the inner table
       OutputUtils.renderLayoutTableAttributes(context, columnEncodingState.rc, "0", "100%");
        rw.startElement("tbody", null); // the inner tbody
        if (startAlignedLabels)
        {
          rw.startElement("tr", null); // the sizing row
          rw.startElement("td", null); // the sizing label cell
          if (columnEncodingState.effectiveLabelWidth != null)
          {
            rw.writeAttribute("style",
                              "width: " + columnEncodingState.effectiveLabelWidth,
                              null);
          }
          rw.endElement("td"); // the sizing label cell
          rw.startElement("td", null); // the sizing field cell
          if (columnEncodingState.effectiveFieldWidth != null)
          {
            rw.writeAttribute("style",
                              "width: " + columnEncodingState.effectiveFieldWidth,
                              null);
          }
          rw.endElement("td"); // the sizing field cell
          rw.endElement("tr"); // the sizing row
        }
      }

      // render the separator, if any
      if (LayoutAction.NEW_GROUP.equals(currLayoutAction))
      {
        PanelFormLayoutRenderer._encodeGroupDivider(context,
                                                    columnEncodingState.rc,
                                                    startAlignedLabels);
      }

      // encode the child
      _encodeFormItem(context, columnEncodingState.rc, startAlignedLabels, currChild);

      // This particular item or group of items fits:
      columnEncodingState.childIndex++;

      // if this is the last child finish its column
      if (columnEncodingState.childIndex == layoutActions.size())
      {
        _finishColumn(rw);
      }
    }

    private void _finishColumn(
      ResponseWriter rw
      ) throws IOException
    {
      rw.endElement("tbody"); // the inner tbody
      rw.endElement("table"); // the inner table
      rw.endElement("td"); // the outer column
    }
  }

  /**
   * Request map key for child renderers to inspect to see if their
   * labels should be stacked above their fields as opposed to the side.
   */
  protected static final String PANEL_FORM_LAYOUT_LABELS_START_ALIGNED_KEY =
    "oracle.adfinternal.PanelFormLayoutLabelsStartAligned";

  private PropertyKey _labelAlignmentKey;
  private PropertyKey _labelWidthKey;
  private PropertyKey _fieldWidthKey;
  private PropertyKey _rowsKey;
  private PropertyKey _maxColumnsKey;
  private final RenderedItemExtractor _renderedItemExtractor = new RenderedItemExtractor();
  private final FormColumnEncoder _formColumnEncoder = new FormColumnEncoder();

  // Overallocate because we basically want everything to miss
  private static final Set<String> _UNSUPPORTED_RENDERER_TYPES;
  static
  {
    _UNSUPPORTED_RENDERER_TYPES = new HashSet<String>(64);
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.Hidden");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.Shuttle");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.rich.Hidden");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.rich.Shuttle");
  }

  /**
   * Request map key for child renderers to inspect to see if their
   * alternative rendering format for being a form item is enabled.
   */
  private static final String _PANEL_FORM_LAYOUT_FORM_ITEM_KEY =
    "org.apache.myfaces.trinidadinternal.PanelFormLayoutFormItem";

  private static final String _PANEL_FORM_LAYOUT_NEST_LEVEL_KEY =
    "org.apache.myfaces.trinidadinternal.PanelFormNestLevel";

  private static final int _COLUMNS_DEFAULT = 3;

  // we need a  resource key map since we are using LabelAndMessageRenderer.
  private static final Map<String, String> _RESOURCE_KEY_SIDE_BY_SIDE_MAP =
                                                                     new HashMap<String, String>();
  private static final Map<String, String> _RESOURCE_KEY_STACKED_MAP =
                                                                     new HashMap<String, String>();

  static
  {
    // style keys.
    // for panelFormLayout, we want a specific af|panelFormLayout style for the label cell,
    // instead of the generic prompt cell style.

    // Start-aligned labels for side-by-side orientation:
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_LABEL_CELL_STYLE_CLASS);
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS);

    // Stacked labels for one-over-the-other orientation:
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_LABEL_STACKED_CELL_STYLE_CLASS);
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS);
  }
}
