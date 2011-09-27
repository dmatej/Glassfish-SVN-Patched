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
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UISelectItem;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.UIXSelectItem;
import org.apache.myfaces.trinidad.component.UIXSelectRange;
import org.apache.myfaces.trinidad.component.core.data.CoreSelectRangeChoiceBar;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.util.Range;


public class SelectRangeChoiceBarRenderer extends XhtmlRenderer
{
  public SelectRangeChoiceBarRenderer()
  {
    this(CoreSelectRangeChoiceBar.TYPE);
  }

  public SelectRangeChoiceBarRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _rowsKey = type.findKey("rows");
    _firstKey = type.findKey("first");
    _immediateKey = type.findKey("immediate");
    _showAllKey = type.findKey("showAll");
    _varKey = type.findKey("var");
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();

    Object event = parameters.get(XhtmlConstants.EVENT_PARAM);

    // get the goto event parameter values and queue a RangeChangeEvent.
    if (XhtmlConstants.GOTO_EVENT.equals(event))
    {
      Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
      String id = clientId == null ? component.getClientId(facesContext) : clientId;
      if (id.equals(source))
      {
        UIXSelectRange choiceBar = (UIXSelectRange)component;
        Object valueParam = parameters.get(XhtmlConstants.VALUE_PARAM);
        RangeChangeEvent rce = _createRangeChangeEvent(choiceBar, valueParam);
        rce.queue();

        if (choiceBar.isImmediate())
          facesContext.renderResponse();

        RequestContext.getCurrentInstance().addPartialTarget(component);
      }
    }
  }

  private RangeChangeEvent _createRangeChangeEvent(
    UIXSelectRange choiceBar,
    Object         valueParam)
  {
    // get the variables needed to calculate oldStart, oldEnd,
    // newStart, newEnd.
    int rowCount = choiceBar.getRowCount();
    int rows = choiceBar.getRows();

    FacesBean bean = getFacesBean(choiceBar);
    boolean isShowAll = getShowAll(choiceBar, bean);

    // calculate oldStart and oldEnd
    int increment = (isShowAll && rowCount > -1) ? rowCount : rows;
    int oldStart = choiceBar.getFirst();
    int oldEnd = oldStart + increment;


    // calculate newStart and newEnd

    // initialize showAll to its default state. We will change
    // this later if the event's value is "all".
    if (isShowAll)
      bean.setProperty(_showAllKey, Boolean.FALSE);

    int newStart = -1;
    int newEnd = -1;

    if (valueParam != null)
    {
      String newStartString = valueParam.toString();

      // We get "all" if the user selected the "Show All" option.
      // If so, set showAll to true and set newStart and newEnd to
      // be the entire range.
      if (newStartString.equals(XhtmlConstants.VALUE_SHOW_ALL))
      {
        bean.setProperty(_showAllKey, Boolean.TRUE);
        newStart = 0;
        newEnd = rowCount;
      }
      else
      {
        try
        {
          newStart = Integer.parseInt(newStartString) - 1;
          newEnd = newStart + rows;
        }
        catch (NumberFormatException nfe)
        {
          // Shouldn't happen with a legit request
          _LOG.severe(nfe);
        }
      }
    }

    return new RangeChangeEvent(choiceBar, oldStart, oldEnd,
                                newStart, newEnd);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Always render an ID, needed for proper PPR.
   */
  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  protected int getRows(
    UIComponent component,
    FacesBean bean)
  {
    Object o = bean.getProperty(_rowsKey);
    if (o == null)
      o = _rowsKey.getDefault();

    return toInt(o);
  }

  protected int getFirst(
    UIComponent component,
    FacesBean bean)
  {
    Object o = bean.getProperty(_firstKey);
    if (o == null)
      o = _firstKey.getDefault();

    return toInt(o);
  }

  protected boolean getShowAll(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_showAllKey);
    if (o == null)
      o = _showAllKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getImmediate(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected String getVar(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_varKey));
  }

  protected UIComponent getRangeLabel(
    UIComponent component)
  {
    return getFacet(component, CoreSelectRangeChoiceBar.RANGE_LABEL_FACET);
  }

  //
  // HOOKS FOR SUBCLASSES
  // These methods exist entirely for subclasses to override behavior.
  //
  protected int getRowCount(
    UIComponent component)
  {
    return ((UIXSelectRange) component).getRowCount();
  }

  protected int getRowIndex(
    UIComponent component)
  {
    return ((UIXSelectRange) component).getRowIndex();
  }

  protected void setRowIndex(
    UIComponent component,
    int         index)
  {
    ((UIXSelectRange) component).setRowIndex(index);
  }

  protected boolean isRowAvailable(
    UIComponent component)
  {
    return ((UIXSelectRange) component).isRowAvailable();
  }

  protected boolean isRowAvailable(
    UIComponent component,
    int         rowIndex)
  {
    return ((UIXSelectRange) component).isRowAvailable(rowIndex);
  }

  protected Object getRowData(
    UIComponent component)
  {
    return ((UIXSelectRange) component).getRowData();
  }

  protected String getSource()
  {
    return null;
  }

  protected boolean showAllSupported()
  {
    return true;
  }

  //
  // END OF HOOKS
  //

  /**
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    int rowIndex = getRowIndex(component);
    try
    {
      int blockSize = getRows(component, bean);
      if (blockSize < 0)
       blockSize = toInt(_rowsKey.getDefault());

      // =-=AEW The old rendering code was written with "value" as one-indexed;
      // "first" is zero-indexed.  Rewrite the rendering code to deal.
      long currentValue = getFirst(component, bean) + 1;
      if (currentValue < 1)
        currentValue = 1;

      long minValue = 1;

      // @todo: =-=jmw ... get maxValue from the model. If no model, then use the
      // maximum attribute. Not sure we want to implement this feature.
      long maxValue = getRowCount(component);
      if (maxValue <= 0)
        maxValue = XhtmlConstants.MAX_VALUE_UNKNOWN;

      // get name
      String id = getClientId(context, component);
      // For the source, just pass the ID as long as this is being
      // used on its own - but give a hook for subclasses to their thing.
      String source = getSource();
      if (source == null)
        source = id;

      if (rc.getFormData() == null)
        return;

      String formName = rc.getFormData().getName();
      if (formName == null)
        return;

      int  nextRecords = 0;
      int  prevRecords = 0;
      long backValue = 0;
      long nextValue = 0;

      if (blockSize > 0)
      {
        // determine how many records user can go forward
        long lNextRecords = blockSize;

        if (maxValue != XhtmlConstants.MAX_VALUE_UNKNOWN)
        {
          // if we know the total records, align the current value to the
          // start of its block. This makes the choice-rendering style
          // not show extra back navigation records on the min block,
          // which it would if not aligned.

          // =-=AEW Revisit bug 3052637 for JSF
          /** no don't do this. see bug: 3052637
              currentValue -= minValue;
              currentValue /= blockSize;
              currentValue *= blockSize;
              currentValue += minValue;
          */

          lNextRecords = maxValue - (currentValue + blockSize - 1);
        }

        // determine how many records user can go back
        long lPrevRecords = currentValue - minValue;

        // trim
        nextRecords = (lNextRecords > blockSize)
          ? blockSize
          : (int) lNextRecords;

        prevRecords = (lPrevRecords > blockSize)
          ? blockSize
          : (int) lPrevRecords;

        backValue = currentValue - prevRecords;
        nextValue = currentValue + blockSize;
      }


      boolean validate = !getImmediate(component, bean);

      boolean showDisabledNavigation = disabledNavigationShown();
      boolean hasBackRecords = (prevRecords > 0);
      boolean hasNextRecords = (nextRecords > 0);

      if (hasNextRecords && (maxValue == XhtmlConstants.MAX_VALUE_UNKNOWN))
      {
        // make sure the next range exists in the data model.
        hasNextRecords = isRowAvailable(component, (int)nextValue-1);
      }

      boolean showBackButton = hasBackRecords || showDisabledNavigation;
      boolean showNextButton = hasNextRecords || showDisabledNavigation;
      if (!supportsNavigation(rc))
      {
        showBackButton = false;
        showNextButton = false;
      }

      boolean showAllActive = getShowAll(component, bean);

      if (showAllActive)
      {
        prevRecords = 0;
        nextRecords = 0;
      }

      String prevOnClick = null;
      String nextOnClick = null;


      if (hasBackRecords || hasNextRecords)
      {
        addHiddenFields(rc);
        // Render script submission code.
        ProcessUtils.renderNavSubmitScript(context, rc);
        ProcessUtils.renderNavChoiceSubmitScript(context, rc);
      }

      // use form submit
      if (supportsScripting(rc))
      {
        if (hasBackRecords && !showAllActive)
        {
          prevOnClick = ProcessUtils.getSubmitScriptCall(formName,
                                                         source,
                                                         backValue,
                                                         validate);
        }

        if (hasNextRecords && !showAllActive)
        {
          nextOnClick =  ProcessUtils.getSubmitScriptCall(formName,
                                                          source,
                                                          nextValue,
                                                          validate);
        }
      }

      // ready to render
      ResponseWriter writer = context.getResponseWriter();
      boolean renderAsTable = __renderAsTable(component);


      // The following strange code is part of the work around for
      // bug 2275703.  IE has problems re-laying out a TableBean
      // after a partial page replacement.  In particular, pieces
      // of the table's top navigation bar, such as the previous link
      // or icon, or sometimes the entire navigation bar,  may shift to
      // the left.  In some cases, pieces of the navigation bar (the previous
      // icon) may disappear during re-layout!  There doesn't seem to be
      // any clean way to avoid this apparent IE bug.  However, we explicitly
      // perform a partial replacement of the navigation bar's previous icon
      // *after* the entire table has been replaced, everything seems to lay
      // out just fine.
      //
      // So, if we are rendering a TableBean's navigation bar with
      // PPR enabled on IE, then we generate an ID for the nav bar's
      // previous icon, and we add this to the list of rendered partial
      // targets during the partial page render.  This forces the icon
      // to be replaced as part of the partial page update, and fixes
      // our layout problems.
      String iconID = null;
      if (PartialPageUtils.isPPRActive(context) &&
          isIE(rc))
      {
        iconID = id + "-i";
      }

      // we only want to render the baseID, if needed, once. Then we
      // render the subIDs. So we need to keep track of this.
      boolean renderedId = false;

      // If the request is from a desktop browser we don't need to wrap up
      // with a div tag
      boolean isDesktop = false;

      // if we need to render standalone, create a table and table row...
      if (renderAsTable)
      {
        isDesktop = (rc.getAgent().getType().equals(Agent.TYPE_DESKTOP));
        // Few mobile browsers doesn't support PPR for Table element
        // so lets wrap it up with a div tag
        if(!isDesktop )
        {
          writer.startElement("div", component);
          writer.writeAttribute("id", id, "id");
        }
        writer.startElement("table", component);
        OutputUtils.renderLayoutTableAttributes(context, rc, "0", null);

        // =-=AEW Where do these attrs get written out when
        // we're inside a PanelPageButton?
        renderAllAttributes(context, rc, component, bean);

        // We should always render the ID, but we particularly need
        // to make sure that the ID is rendered if the NavBar is being
        // used to navigate a TableBean, since we explicitly target
        // TableBean NavBars when using PPR to re-render TableBeans...
        if(isDesktop )
        {
          writer.writeAttribute("id", id, "id");
        }
        renderedId = true;

        writer.startElement("tr", null);
      }

      boolean narrowScreen = supportsNarrowScreen(rc);

      // skip rendering back button for narrow-screen PDAs to reduce the
      // overall width of selectRangeChoiceBar.
      if (showBackButton && !narrowScreen)
      {
        Icon prevIcon = getIcon(rc, false, (prevOnClick != null));

        if (!prevIcon.isNull())
        {
          // We assign an id to the left arrow so that we can target it as
          // a partial target to work around bug 2275703 - see note above.
          if (iconID != null)
          {
            writer.startElement("td", component);
            writer.writeAttribute("id", iconID, null);

            // If the navigation bar that we are currently rendering
            // is included in a partial page response, add the icon
            // id to the list of partial targets.
            // =-=AEW Not sure this is still necessary
            PartialPageContext pprContext = rc.getPartialPageContext();
            if ((pprContext != null) &&
                pprContext.isInsidePartialTarget())
            {
              pprContext.addRenderedPartialTarget(iconID);
            }
          }
          else
          {
            // not in PPR mode, so just render the td (and id if not in a table)
            _renderStartTableCell(writer, id, renderedId);
            renderedId = true;
          }


          writer.writeAttribute("valign", "middle", null);
          _renderArrow(context, rc, prevIcon, false, prevOnClick);
          writer.endElement("td");

          _renderSpacerCell(context, rc);
        }

        _renderStartTableCell(writer, id, renderedId);
        renderedId = true;
        writer.writeAttribute("valign", "middle", null);
        writer.writeAttribute("nowrap", Boolean.TRUE, null);

        _renderLink(context,
                    rc,
                    false,
                    prevOnClick,
                    prevRecords,
                    id,
                    source,
                    backValue);

        writer.endElement("td");
        _renderSpacerCell(context, rc);
      }

      _renderStartTableCell(writer, id, renderedId);
      renderedId = true;
      writer.writeAttribute("valign", "middle", null);
      writer.writeAttribute("nowrap", Boolean.TRUE, null);

      _renderChoice(context,
                    rc,
                    component,
                    id,
                    source,
                    formName,
                    minValue,
                    currentValue,
                    blockSize,
                    maxValue,
                    validate);

      writer.endElement("td");

      // skip rendering back button for narrow-screen PDAs to reduce the
      // overall width of selectRangeChoiceBar.
      if (showNextButton && !narrowScreen)
      {
        _renderSpacerCell(context, rc);

        _renderStartTableCell(writer, id, true);
        writer.writeAttribute("valign", "middle", null);
        writer.writeAttribute("nowrap", Boolean.TRUE, null);

        _renderLink(context,
                    rc,
                    true,
                    nextOnClick,
                    nextRecords,
                    id,
                    source,
                    nextValue);

        writer.endElement("td");

        Icon nextIcon = getIcon(rc, true, (nextOnClick != null));
        if (!nextIcon.isNull())
        {
          _renderSpacerCell(context, rc);

          _renderStartTableCell(writer, id, true);
          writer.writeAttribute("valign", "middle", null);
          _renderArrow(context, rc, nextIcon, true, nextOnClick);
          writer.endElement("td");
        }
      }

      if (renderAsTable)
      {
        writer.endElement("tr");
        writer.endElement("table");
      }

      if (renderAsTable && !isDesktop )
      {
        writer.endElement("div");
      }

    }
    // Make sure we always restore the row index correctly
    finally
    {
      setRowIndex(component, rowIndex);
    }
  }

  /**
   * render form value needed values and javascript code.
   */
  public static void addHiddenFields(
    RenderingContext rc)
  {
    FormData fData = rc.getFormData();
    fData.addNeededValue(XhtmlConstants.EVENT_PARAM);
    fData.addNeededValue(XhtmlConstants.SOURCE_PARAM);
    fData.addNeededValue(XhtmlConstants.PARTIAL_PARAM);
    fData.addNeededValue(XhtmlConstants.VALUE_PARAM);
  }

  private void _renderChoice(
   FacesContext     context,
   RenderingContext rc,
   UIComponent      component,
   String           id,
   String           source,
   String           form,
   long             minValue,
   long             currentValue,
   int              blockSize,
   long             maxValue,
   boolean          validate
    ) throws IOException
  {
    UIComponent rangeLabel = getRangeLabel(component);
    boolean firstRowAvailable = isRowAvailable(component, 0);

    ResponseWriter writer = context.getResponseWriter();
    // if there is no blockSize to step by, or there are no items in the
    // table, then we don't render a choice
    if ((blockSize <= 0) || (!firstRowAvailable) ||
          ((maxValue < minValue) &&
           (maxValue != XhtmlConstants.MAX_VALUE_UNKNOWN)))
    {
      writer.writeText(XhtmlConstants.NBSP_STRING, null);
    }
    else
    {
      List<SelectItem> items =
        new ArrayList<SelectItem>((int) _MAX_VISIBLE_OPTIONS);

      int selectedIndex = _getItems(context, rc, component, items,
                                    minValue, maxValue, currentValue,
                                    blockSize, rangeLabel);
      int count = items.size();
      if (count > 1)
      {
        String choiceTip = rc.getTranslatedString(_CHOICE_TIP_KEY);
        String choiceId = XhtmlUtils.getCompositeId(id, _CHOICE_ID_SUFFIX);
        String onChange = ProcessUtils.getChoiceOnChangeFormSubmitted(
                             form, source, validate);
        boolean javaScriptSupport = supportsScripting(rc);

        writer.startElement("select", null);
        writer.writeAttribute("title", choiceTip, null);
        renderStyleClass(context, rc,
                         SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS);


        if (onChange != null && javaScriptSupport)
        {
          // set the onchange handler
          writer.writeAttribute("onchange", onChange, null);
          // set the onfocus handler to save the initial value
          writer.writeAttribute("onfocus", _CHOICE_FORM_ON_FOCUS, null);
        }

        writer.writeAttribute("id", choiceId, null);

        // For Non-JavaScript browsers, render the name attribute thus it
        // would enable the browsers to include the name and value of this
        // element in its payLoad.

        if (!javaScriptSupport)
        {
          writer.writeAttribute("name", choiceId, null);
        }

        _writeSelectItems(context, items, selectedIndex);

        writer.endElement("select");

        if (HiddenLabelUtils.supportsHiddenLabels(rc))
        {
          HiddenLabelUtils.outputHiddenLabelIfNeeded(context,
                                             rc,
                                             choiceId,
                                             choiceTip,
                                             null);
        }


        // For Non-JavaScript browsers, render a input element(type= submit) to
        // submit the page. Encode the name attribute with the parameter name
        // and value thus it would enable the browsers to include the name of
        // this element in its payLoad if it submits the page.

        if (!javaScriptSupport)
        {
          String nameAttri =  XhtmlUtils.getEncodedParameter
                                          (XhtmlConstants.MULTIPLE_VALUE_PARAM)
                              + XhtmlUtils.getEncodedParameter(choiceId)
                              + XhtmlUtils.getEncodedParameter
                                          (XhtmlConstants.SOURCE_PARAM)
                              + XhtmlUtils.getEncodedParameter(source)
                              + XhtmlUtils.getEncodedParameter
                                          (XhtmlConstants.EVENT_PARAM)
                              + XhtmlConstants.GOTO_EVENT;

          renderSubmitButtonNonJSBrowser(context,
                                         rc,
                                         XhtmlConstants.
                                               NO_JS_PARAMETER_KEY_BUTTON,
                                         nameAttri);

        }
        else
        {
          writer.startElement("script", null);
          renderScriptDeferAttribute(context, rc);
          renderScriptTypeAttribute(context, rc);
          writer.writeText("_setSelectIndexById(\"", null);
          writer.writeText(choiceId, null);
          writer.writeText("\",", null);
          writer.writeText(IntegerUtils.getString(selectedIndex), null);
          writer.writeText(")", null);
          writer.endElement("script");
        }
      }
      else if (count == 1)
      {
        writer.startElement("span", null);
        renderStyleClass(context, rc,
                         SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS);
        writer.writeText(items.get(0).getLabel(), null);
        writer.endElement("span");
      }
    }
  }

  private void _writeSelectItems(
    FacesContext     context,
    List<SelectItem> items,
    int              selectedIndex
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    int count = items.size();
    for (int i = 0; i < count; i++)
    {
      SelectItem item = items.get(i);
      writer.startElement("option", null);
      writer.writeAttribute("value", item.getValue(), null);
      if (i == selectedIndex)
        writer.writeAttribute("selected", Boolean.TRUE, null);
      writer.writeText(item.getLabel(), null);
      writer.endElement("option");
    }
  }

  /**
   * create each of the choice options and add them onto the List.
   * @return the number of options added
   */
  private int _getItems(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    List<SelectItem> items,
    long             minValue,
    long             maxValue,
    long             value,
    int              blockSize,
    UIComponent      rangeLabel)
  {
    int selectedIndex = -1;

    boolean maxUnknown = (maxValue == XhtmlConstants.MAX_VALUE_UNKNOWN);

    // Zero-indexed block index.
    long blockIndex = (value - minValue + blockSize - 1L) / blockSize;

    // sometimes a record set won't start on a multiple of blockSize. So
    // remember to add any offset:
    // this can safely be an int because it is an index into the blockSize,
    // which is itself an int:
    int offset = (int) (value - (minValue + (blockIndex * blockSize)));
    if (offset < 0)
      offset = offset + blockSize;

    // Total number of blocks (again, zero-indexed)
    long maxBlockIndex;
    if (maxUnknown)
      maxBlockIndex = blockIndex + 1;
    else
    {
      maxBlockIndex = (maxValue - minValue - offset) / blockSize;
      if (offset > 0)
        maxBlockIndex++;
    }

    // Calculate the first block that should be shown.  The order goes:
    // Group 0:            0-28 + More
    // Group 1:Previous + 29-56 + More
    // Group 2:Previous + 57-84 + More
    // etc..
    long firstBlockIndex;

    // If everything is visible, or we're in the first group, start at zero.
    if ((maxBlockIndex <= (_MAX_VISIBLE_OPTIONS - 1L)) ||
        (blockIndex <= (_MAX_VISIBLE_OPTIONS - 2L)))
      firstBlockIndex = 0;
    else
      firstBlockIndex = ((blockIndex - 1L) / (_MAX_VISIBLE_OPTIONS - 2L)) *
                         (_MAX_VISIBLE_OPTIONS - 2L);

    // And we always show a total of 30 groups (or straight to the end)
    long lastBlockIndex = firstBlockIndex + (_MAX_VISIBLE_OPTIONS - 1L);
    if (lastBlockIndex > maxBlockIndex)
      lastBlockIndex = maxBlockIndex;

    boolean showAllActive = getShowAll(component, getFacesBean(component));

    // Add "Show All" option if showAll was set to true OR
    // when there are less than 30 groups (maxBlockIndex
    // start as zero, hence "29") and only allow it when there's
    // more than 1 visible item!
    if (showAllActive ||
        (!maxUnknown && (lastBlockIndex > firstBlockIndex) &&
         (maxBlockIndex <= (_MAX_VISIBLE_OPTIONS - 1L))
         ))
    {
      // Omit show all if it's not supported
      if (showAllSupported())
      {
        items.add(_createShowAllSelectItem(rc,
                                           maxValue));
        if (showAllActive)
          selectedIndex = 0;
      }
    }

    for (blockIndex = firstBlockIndex;
         blockIndex <= lastBlockIndex;
         blockIndex++)
    {
      long blockStart = minValue + (blockIndex * blockSize);

      // if there is an offset, then adjust accordingly. for example, if the
      // offset is 7 (and the blockSize is 10), then the new blockStarts are:
      // 1-7, 8-17, 18-27, etc ...
      if (offset > 0)
        blockStart += (offset - blockSize);

      final int currentRecordSize;
      // check to see if this is the very first record set in a table using an
      // offset:
      if (blockStart < minValue)
      {
        // treat this specially. this is the 1-7 case from the example above:
        blockStart = minValue;
        currentRecordSize = offset;
      }
      else
      {
        currentRecordSize = blockSize;
      }


      // return immediately if the start of the next range is not available.
      if (maxUnknown)
      {
        if (!isRowAvailable(component, (int)blockStart - 1))
          return selectedIndex;
      }

      String text;
      // Need "Previous..."
      if ((blockIndex == firstBlockIndex) &&
          (blockIndex != 0))
      {
        text = rc.getTranslatedString(_PREVIOUS_TEXT_KEY);
      }

      // Need "More..." (on the last block, either 'cause
      // the total number of blocks is unknown or we've shown enough blocks
      // However, don't show More... if the total number of blocks is unknown,
      // and we checked and found out that the start of the next block doesn't
      // exist.
      else if ((blockIndex == lastBlockIndex) &&
               (maxUnknown || (lastBlockIndex < maxBlockIndex)))
      {
        text = rc.getTranslatedString(_MORE_TEXT_KEY);
      }
      else
      {
        text = null;
      }

      // =-=AEW I don't understand this next line...
      long currValue = showAllActive ? minValue - 1 : value;// Don't select

      SelectItem item = _createNavigationItem(context,
                                              rc,
                                              component,
                                              blockStart,
                                              currentRecordSize,
                                              maxValue,
                                              text,
                                              rangeLabel);
      if ((currValue >= blockStart) &&
          (currValue <  (blockStart + currentRecordSize)))
      {
        selectedIndex = items.size();
      }

      items.add(item);
    }

    return selectedIndex;
  }

  private SelectItem _createShowAllSelectItem(
    RenderingContext arc,
    long                maxValue)
  {
    String[] parameters = new String[]{IntegerUtils.getString(maxValue)};
    String showAllText = XhtmlUtils.getFormattedString(
                                     arc.getTranslatedString(_SHOW_ALL_KEY),
                                     parameters);

    return new SelectItem(XhtmlConstants.VALUE_SHOW_ALL,
                          showAllText);
  }

  // create a choice option when max value is known
  private SelectItem _createNavigationItem(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    long             blockStart,
    int              blockSize,
    long             maxValue,
    String           text,
    UIComponent      rangeLabel
    )
  {
    // if text isn't null, it is More or Previous.
    if (text == null)
      text = _getRangeString(context,
                             rc,
                             component,
                             blockStart,
                             blockSize,
                             maxValue,
                             rangeLabel);

    return new SelectItem(IntegerUtils.getString(blockStart),
                          text);
  }

  /**
   * Returns true if disabled navigation items should be shown
   */
  protected boolean disabledNavigationShown()
  {
    return true;
  }

  // create one of the text links for navigation
  private void _renderLink(
    FacesContext     context,
    RenderingContext rc,
    boolean          isNext,
    String           onclick,
    int              records,
    String           id,
    String           source,
    long             value
    ) throws IOException
  {

    String text = getBlockString(rc, isNext, records);
    boolean isEnabled = ((onclick != null) && (records > 0));
    ResponseWriter writer = context.getResponseWriter();

    // if we have more than one record and browser is js capable then
    // render as a link
    if (isEnabled)
    {
      writer.startElement("a", null);
      writer.writeURIAttribute("href", "#", null);
      writer.writeAttribute("onclick", onclick, null);

      // The navBar needs its initial focus to be on the Next button,
      // according to the BLAF. Render a special id on the Next button
      // if this navBar is to have the initial focus. (unless it needs
      // initial focus, the Next button does not have an id on it)
      if (isNext)
      {
        String linkID = _getIDForFocus(rc, id);
        writer.writeAttribute("id", linkID, null);
      }

      renderStyleClass(context, rc, SkinSelectors.NAV_BAR_ALINK_STYLE_CLASS);
      writer.writeText(text, null);
      writer.endElement("a");
    }
    // if we don't have any record then just render as <span> element
    else if (records < 1)
    {
      writer.startElement("span", null);
      renderStyleClass(context, rc, SkinSelectors.NAV_BAR_ILINK_STYLE_CLASS);
      writer.writeText(text, null);
      writer.endElement("span");
    }

    // For Non-JavaScript browsers, render a submit element
    // (<input type = "submit"/> ). Encode the the name attribute with the
    // parameter name and value thus it would enable the browsers to
    // include the name of this element in its payLoad if it submits the

    else
    {
      String nameAttri = XhtmlUtils.getEncodedParameter
                                       (XhtmlConstants.SOURCE_PARAM)
                         + XhtmlUtils.getEncodedParameter(source)
                         + XhtmlUtils.getEncodedParameter
                                       (XhtmlConstants.EVENT_PARAM)
                         + XhtmlUtils.getEncodedParameter
                                       (XhtmlConstants.GOTO_EVENT)
                         + XhtmlUtils.getEncodedParameter
                                       (XhtmlConstants.VALUE_PARAM)
                         + IntegerUtils.getString(value);

      writer.startElement("input", null);
      writer.writeAttribute("type", "submit", null);
      writer.writeAttribute("name", nameAttri, null);
      writer.writeAttribute("value", text, "text");
      renderStyleClass(context, rc,
                  SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS);

      // This style makes a button to appear as a link
      writer.writeAttribute("style",
        "border:none;background:inherit;text-decoration:underline;",null);
      writer.endElement("input");
    }

 }


  /**
   */
  protected Icon getIcon(
    RenderingContext rc,
    boolean          isNext,
    boolean          isEnabled
    )
  {
    // get the image location
    String iconName;

    if (isNext)
    {
      if (isEnabled)
      {
        iconName   = SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME;
      }
      else
      {
        iconName   = SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME;
      }
    }
    else
    {
      if (isEnabled)
      {
        iconName   = SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME;
      }
      else
      {
        iconName   = SkinSelectors.AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME;
      }
    }

    return rc.getIcon(iconName);
  }

  protected String getIconTitleKey(
    boolean isNext,
    boolean isEnabled
    )
  {
    if (isNext)
    {
      return (isEnabled) ? _NEXT_DESC_KEY : _DISABLED_NEXT_DESC_KEY;
    }
    else
    {
      return (isEnabled) ? _PREVIOUS_DESC_KEY : _DISABLED_PREVIOUS_DESC_KEY;
    }
  }

  /**
   * @todo GENERIC FIX: need to use renderURIAttribute() in Icon
   *  code to output the Icon URL.  But that'll break a zillion
   *  renderkit tests.
   */
  private void _renderArrow(
    FacesContext     context,
    RenderingContext rc,
    Icon             icon,
    boolean          isNext,
    String           onclick
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    if (onclick != null)
    {
      writer.startElement("a", null);
      writer.writeURIAttribute("href", "#", null);
      writer.writeAttribute("onclick", onclick, null);
    }

    boolean isEnabled = (onclick != null);
    String titleKey = getIconTitleKey(isNext, isEnabled);
    String title = rc.getTranslatedString(titleKey);
    OutputUtils.renderIcon(context, rc, icon, title, null);

    if (onclick != null)
      writer.endElement("a");
  }


  /**
   * Gets the string to use for next/previous links
   * in a table navigation bar.
   */
  protected String getBlockString(
    RenderingContext rc,
    boolean          isNext,
    int              numRecords
    )
  {
    // check to make sure that we have some records in this direction:
    if (numRecords > 0)
    {
      String pattern = (isNext)
      ? rc.getTranslatedString("af_selectRangeChoiceBar.NEXT")
      : rc.getTranslatedString("af_selectRangeChoiceBar.PREVIOUS");
      String value = IntegerUtils.getString(numRecords);

      return XhtmlUtils.getFormattedString(pattern, new String[]{value});
    }
    else
    {
      // since we don't have any records, we are going to display some
      // disabled text. see bug 1740486.
      String text = (isNext)
        ? rc.getTranslatedString("af_selectRangeChoiceBar.DISABLED_NEXT")
        : rc.getTranslatedString("af_selectRangeChoiceBar.DISABLED_PREVIOUS");
      return text;
    }
  }

  //
  /**
   * get the string for the current range
   * @todo We probably shouldn't use the same substitution string
   *  when we know the max and when we don't.  We should have two:
   *   {0}-{1} of {2}
   *   {0}-{1}
   *  (and not bother with the "of" substitution)
   */
  @SuppressWarnings("unchecked")
  private String _getRangeString(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    long             start,
    int              visibleItemCount,
    long             total,
    UIComponent      rangeLabel
    )
  {

    // how many records do we really see now?
    long currVisible = (total == XhtmlConstants.MAX_VALUE_UNKNOWN)
                           ? visibleItemCount
                           : total - start + 1;

    if (currVisible > visibleItemCount)
      currVisible = visibleItemCount;


    // getItemLabel from selectItem in the facet if it exists.
    if ((rangeLabel != null) &&
        ((rangeLabel instanceof UISelectItem) ||
         (rangeLabel instanceof UIXSelectItem)))
    {
      Range range = new Range();
      // getting data is zero-indexed, whereas start is 1-indexed.
      setRowIndex(component, (int)start-1);
      Object startRow = getRowData(component);
      range.setStart(startRow);

      // get end row. If the end row doesn't exist, find the
      // last row that does exist. The row indices are zero-indexed.
      int endIndex = (int)(start + currVisible - 2);

      endIndex = _setToExistingEndRow(component, (int)start -1, endIndex);
      setRowIndex(component, endIndex);

      // ok, we are sure we have an existing end row set, so set the end
      // parameter on the range object.
      range.setEnd(getRowData(component));

      Object old = null;
      String var = getVar(component, getFacesBean(component));
      if (var != null)
      {
        Map<String, Object> requestMap =
          context.getExternalContext().getRequestMap();
        old = requestMap.put(var, range);
      }

      String label = (rangeLabel instanceof UISelectItem)
        ? ((UISelectItem) rangeLabel).getItemLabel()
        : toString(((UIXSelectItem) rangeLabel).getAttributes().get("label"));

      if (var != null)
      {
        Map<String, Object> requestMap =
          context.getExternalContext().getRequestMap();

        if (old == null)
          requestMap.remove(var);
        else
          requestMap.put(var, old);
      }


      return label;
    }
    else
    {
      // formatter for generating the page string
      String startParam = IntegerUtils.getString(start);


      int endIndex = (int)(start + currVisible - 2);
      endIndex = _setToExistingEndRow(component, (int)start -1, endIndex);
      String endParam = IntegerUtils.getString(endIndex+1);
      String   pattern = null;
      String[] parameters = null;

      if ((total == XhtmlConstants.MAX_VALUE_UNKNOWN))
      {
        pattern = rc.getTranslatedString(_MULTI_RANGE_NO_TOTAL_FORMAT_STRING);
        parameters = new String[]
        {
          startParam,
          endParam
        };

      }
      else
      {

        pattern = rc.getTranslatedString(_MULTI_RANGE_TOTAL_FORMAT_STRING);
        parameters = new String[]
        {
          startParam,
          endParam,
          IntegerUtils.getString(total)
        };

      }

      return XhtmlUtils.getFormattedString(pattern, parameters);
    }
  }

  /**
   * Find the highest end row in the range from startRowIndex to endRowIndex
   * inclusive that exists, and make that row current
   * (by calling selectRange.setRowIndex).
   * @param startRowIndex. the start index for the first row in this range.
   * @param endRowIndex the initial end row. that is, the row index for the
   * last row in this range.
   * @return the index of the highest end row that exists.
   */
  private int _setToExistingEndRow(
    UIComponent component,
    int         startRowIndex,
    int         endRowIndex )
  {
    boolean rowAvailable = isRowAvailable(component, endRowIndex);

    // make sure that the end row exists. If it doesn't, then loop
    // back from the end until we see that it does exist.
    while (!rowAvailable && endRowIndex >= startRowIndex)
    {
      endRowIndex--;
      rowAvailable = isRowAvailable(component, endRowIndex);
    }

    return endRowIndex;
  }


  // don't render as a table in certain locations like a page button bar
  static boolean __renderAsTable(
    UIComponent component
    )
  {
    UIComponent parent = XhtmlUtils.getStructuralParent(component);
    if ((parent instanceof UIXPanel) &&
        ("org.apache.myfaces.trinidad.ButtonBar".equals(parent.getRendererType()) ||
         "org.apache.myfaces.trinidad.rich.ButtonBar".equals(parent.getRendererType())))
    {
      return false;
    }

    return true;
  }


  /**
   * Writes the separator between two elements
   */
  protected void renderItemSpacer(
   FacesContext     context,
   RenderingContext rc
    ) throws IOException
  {
    if (isPDA(rc))
    {
      context.getResponseWriter().writeText(XhtmlConstants.NBSP_STRING, null);
    }
    else
    {
      renderSpacer(context, rc, "5", "1");
    }
  }


  /**
   * Writes the separator between two elements
   */
  private void _renderSpacerCell(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    renderItemSpacer(context, rc);
    writer.endElement("td");
  }

  /**
   * render the "td".
   */
  private void _renderStartTableCell(
    ResponseWriter writer,
    String         id,
    boolean        alreadyRenderedId
    ) throws IOException
  {
    writer.startElement("td", null);
    if (!alreadyRenderedId)
    {
      writer.writeAttribute("id", id, null);
    }
  }

  private String _getIDForFocus(
    RenderingContext rc,
    String           baseId
    )
  {
    // The navBar needs its initial focus to be on the Next button,
    // according to the BLAF. Render a special id on the Next button
    // if this navBar is to have the initial focus. (unless it needs
    // initial focus, the Next button does not have an id on it)
    // We get body's initialFocus attribute off of the rendering context.
    // If this is equal to the navBar's id, then we make up a new id
    // for the Next button.

    Object initialFocusID =
      rc.getProperties().get(XhtmlConstants.INITIAL_FOCUS_CONTEXT_PROPERTY);

    String id = null;
    if ((initialFocusID != null) && initialFocusID.equals(baseId))
    {
      // make up an id to use for the initial focus.
      String focus = "-focus";
      StringBuilder buffer = new StringBuilder(baseId.length()+
                                               focus.length());
      buffer.append(baseId);
      buffer.append(focus);
      id = buffer.toString();
      // set the new id on the rendering context so that the body
      // renderer can write it out to a script variable.
      // A side-effect is that the initialFocusID in subsequent calls will
      // never equal the navBar's id.
      rc.getProperties().put(XhtmlConstants.INITIAL_FOCUS_CONTEXT_PROPERTY, id);
    }


    return id;
  }

  //
  // Private variables
  //

  private PropertyKey _rowsKey;
  private PropertyKey _firstKey;
  private PropertyKey _showAllKey;
  private PropertyKey _immediateKey;
  private PropertyKey _varKey;

  // resource keys
  static private final String _PREVIOUS_DESC_KEY =
  "af_selectRangeChoiceBar.PREVIOUS_TIP";
  static private final String _NEXT_DESC_KEY =
  "af_selectRangeChoiceBar.NEXT_TIP";
  static private final String _DISABLED_PREVIOUS_DESC_KEY =
  "af_selectRangeChoiceBar.PREV_DISABLED_TIP";
  static private final String _DISABLED_NEXT_DESC_KEY =
  "af_selectRangeChoiceBar.NEXT_DISABLED_TIP";
  static private final String _CHOICE_TIP_KEY =
  "af_selectRangeChoiceBar.CHOICE_TIP";
  static private final String _MULTI_RANGE_NO_TOTAL_FORMAT_STRING =
  "af_selectRangeChoiceBar.CHOICE_FORMAT_NO_TOTAL";
  static private final String _MULTI_RANGE_TOTAL_FORMAT_STRING =
  "af_selectRangeChoiceBar.CHOICE_FORMAT_TOTAL";
  static private final String _PREVIOUS_TEXT_KEY =
  "af_selectRangeChoiceBar.PREVIOUS_OPTION";
  static private final String _MORE_TEXT_KEY     =
  "af_selectRangeChoiceBar.MORE_OPTION";
  static private final String _SHOW_ALL_KEY     =
  "af_selectRangeChoiceBar.SHOW_ALL";

  /**
   * @todo This should be pulled from a skin property
   */
  static private final long   _MAX_VISIBLE_OPTIONS = 30L;

  // on focus handler for the form case.  We save off the old value so that
  // we can restore it if the validation chokes
  private static final String _CHOICE_FORM_ON_FOCUS =
  "this._lastValue = this.selectedIndex";

  static private final String _CHOICE_ID_SUFFIX = "c";

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SelectRangeChoiceBarRenderer.class);
}
