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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelButtonBar;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Skin;

import org.apache.myfaces.trinidad.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.TextNode;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.data.DataObjectList;
import org.apache.myfaces.trinidadinternal.ui.data.bind.AccessKeyBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;

/**
 * Renderer for Navigation Bars showing either single or multiple records.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/NavigationBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:04 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class NavigationBarRenderer extends XhtmlLafRenderer
{

  protected MutableUINode createSingleItemURLButton(
    UIXRenderingContext context,
    boolean          isBack,
    Object           buttonText,
    Object           buttonAccessKey,
    String           destinationURL
    )
  {
    MarlinBean urlButton = new MarlinBean(BUTTON_NAME);

    urlButton.setAttributeValue(TEXT_ATTR, buttonText);
    urlButton.setAttributeValue(ACCESS_KEY_ATTR, buttonAccessKey);
    urlButton.setAttributeValue(DESTINATION_ATTR, destinationURL);

    return urlButton;
  }


  protected MutableUINode createSingleItemSubmitButton(
    UIXRenderingContext context,
    boolean          isBack,
    Object           buttonText,
    Object           buttonAccessKey,
    String           onClickJS
    )
  {
    MarlinBean submitButton = new MarlinBean(BUTTON_NAME);

    submitButton.setAttributeValue(TEXT_ATTR, buttonText);
    submitButton.setAttributeValue(ACCESS_KEY_ATTR, buttonAccessKey);

    submitButton.setOnClick(onClickJS);

    return submitButton;
  }


  // =-=bwa: like the TableRenderer, this class creates anonymous children
  //         at render-time. This is a _temporary_ solution, until we
  //         decide how to create anonymous children for UINodes that
  //         can persist across renderings.
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    URLEncoder encoder = context.getURLEncoder();

    String eventKey  = encoder.encodeParameter(EVENT_PARAM);
    String sourceKey = encoder.encodeParameter(SOURCE_PARAM);
    String valueKey  = encoder.encodeParameter(VALUE_PARAM);
    String sizeKey   = encoder.encodeParameter(SIZE_PARAM);
    String partialTargetsKey = encoder.encodeParameter(PARTIAL_TARGETS_PARAM);

    // Get any partial targets - encoded in String form
    String partialTargets = _getPartialTargets(context, node);

    // determine real values through attributes
    Number result =
      (Number)node.getAttributeValue(context, BLOCK_SIZE_ATTR);

    int visibleItemCount = (result != null)
                             ? result.intValue()
                             : SINGLE_STEP;

    if (visibleItemCount == SINGLE_STEP)
    {
      // we've got enough info, render as a single step
      _renderSingleItemNavigator(context,
                                 node,
                                 eventKey,
                                 sourceKey,
                                 valueKey,
                                 sizeKey,
                                 partialTargetsKey,
                                 partialTargets);
    }
    else
    {

      // render the multi-item navigator
      _renderMultiItemNavigator(context,
                                node,
                                visibleItemCount,
                                eventKey,
                                sourceKey,
                                valueKey,
                                sizeKey,
                                partialTargetsKey,
                                partialTargets);
    }

  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // do nothing
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Don't bother with renderContent - it's all done in the prerender
  }


  //
  // Private methods
  //

  // render a single-step navigator
  private void _renderSingleItemNavigator(
    UIXRenderingContext context,
    UINode           navBar,
    String           eventKey,
    String           sourceKey,
    String           valueKey,
    String           sizeKey,
    String           partialTargetsKey,
    String           partialTargets
    )
    throws IOException
  {
    Number result;

    // get current value
    result = (Number)navBar.getAttributeValue(context, VALUE_ATTR);
    long currentValue = (result != null)
                          ? result.longValue()
                          : 1;

    // get max value
    long totalItems;

    int childCount = navBar.getIndexedChildCount(context);
    if (childCount > 0)
    {
      totalItems = childCount;
    }
    else
    {
      result = (Number)navBar.getAttributeValue(context, MAX_VALUE_ATTR);
      totalItems = (result != null)
                     ? result.longValue()
                     : MAX_VALUE_UNKNOWN;
    }

    boolean showBackButton = (currentValue > 1);
    boolean showNextButton = ((totalItems == MAX_VALUE_UNKNOWN) ||
                               (currentValue < totalItems));

    // bail if no buttons shown or values are bogus
    if ((!showBackButton && !showNextButton) ||
        ((currentValue > totalItems) && (totalItems != MAX_VALUE_UNKNOWN)) ||
        (currentValue < MAX_VALUE_UNKNOWN))
      return;


    // get form name (#1308799)
    String formName = XhtmlLafUtils.getParentFormName(context);


    // If we don't support navigation (e.g., printable pages),
    // lie and claim we support scripting (even though we probably don't).
    // This will give us the highest fidelity output - that is,
    // we avoid creating submit buttons.
    boolean supportsScripting = (supportsScripting(context) ||
                                 !supportsNavigation(context));
    if ((formName != null) && supportsScripting)
    {
      // render hidden fields to hold the form data
      renderHiddenFields( context,
                          formName,
                          false,
                          eventKey,
                          sourceKey,
                          valueKey,
                          sizeKey,
                          partialTargetsKey,
                          partialTargets);
    }

    // get name
    String nameString = _getNameString(context, navBar);

    UINode backButton = null;
    UINode nextButton = null;

    // set up the back button
    if (showBackButton)
    {
      BoundValue buttonTextandAccessKeyBV =
                           new SkinTranslatedBoundValue( _SINGLE_BACK_TEXT_KEY);


      Object buttonTextBV = new AccessKeyBoundValue(buttonTextandAccessKeyBV,
                                                    false);

      Object buttonAccessKeyBV = new AccessKeyBoundValue(
                                                    buttonTextandAccessKeyBV,
                                                    true);
      // set the destination.
      if (formName == null)
      {
        backButton = createSingleItemURLButton(
                                    context,
                                    true,
                                    buttonTextBV,
                                    buttonAccessKeyBV,
                                    getSingleDestinationURL(context,
                                                            navBar,
                                                            eventKey,
                                                            sourceKey,
                                                            nameString,
                                                            valueKey,
                                                            currentValue-1));
      }
      else
      {
        if (supportsScripting)
        {
          backButton = createSingleItemSubmitButton(
                                  context,
                                  true,
                                  buttonTextBV,
                                  buttonAccessKeyBV,
                                  getSingleDestinationSubmit( context,
                                                              navBar,
                                                              formName,
                                                              eventKey,
                                                              sourceKey,
                                                              nameString,
                                                              valueKey,
                                                              currentValue - 1,
                                                              false));
        }
        else
        {
          backButton = createSubmitButton(context,
                                           buttonTextBV,
                                           buttonAccessKeyBV,
                                           null,
                                           formName,
                                           false,
                                           eventKey,
                                           sourceKey,
                                           nameString,
                                           valueKey,
                                           currentValue - 1,
                                           null,
                                           -1);
        }
      }
    }

    // set up the next button
    if (showNextButton)
    {

      String buttonTextKey = ((totalItems == 2)
                              ? _SINGLE_CONTINUE_TEXT_KEY
                              : _SINGLE_NEXT_TEXT_KEY);

      BoundValue buttonTextandAccessKeyBV =
                           new SkinTranslatedBoundValue(buttonTextKey);


      Object buttonTextBV = new AccessKeyBoundValue( buttonTextandAccessKeyBV,
                                                     false);

      Object buttonAccessKeyBV = new AccessKeyBoundValue(
                                                      buttonTextandAccessKeyBV,
                                                      true);

      // The navBar needs its initial focus to be on the Next button,
      // according to the BLAF. Render a special id on the Next button
      // if this navBar is to have the initial focus. (unless it needs
      // initial focus, the Next button does not have an id on it)
      String buttonID = _getIDForFocus(context, navBar);

      if (formName == null)
      {

        MutableUINode mutableNextButton =
            createSingleItemURLButton(
                                     context,
                                     false,
                                     buttonTextBV,
                                     buttonAccessKeyBV,
                                     getSingleDestinationURL(context,
                                                             navBar,
                                                             eventKey,
                                                             sourceKey,
                                                             nameString,
                                                             valueKey,
                                                             currentValue+1));
        if (buttonID != null)
        {
          mutableNextButton.setID(buttonID);
        }
        nextButton = mutableNextButton;
      }
      else
      {

        // set the destination
        if (supportsScripting)
        {
          MutableUINode mutableNextButton =
          createSingleItemSubmitButton(
                                       context,
                                       false,
                                       buttonTextBV,
                                       buttonAccessKeyBV,
                                       getSingleDestinationSubmit(
                                                  context,
                                                  navBar,
                                                  formName,
                                                  nameString,
                                                  currentValue + 1,
                                                  true));
          if (buttonID != null)
          {
            mutableNextButton.setID(buttonID);
          }
          nextButton = mutableNextButton;
        }
        else
        {

          nextButton = createSubmitButton(context,
                                           buttonTextBV,
                                           buttonAccessKeyBV,
                                           buttonID,
                                           formName,
                                           false,
                                           eventKey,
                                           sourceKey,
                                           nameString,
                                           valueKey,
                                           currentValue + 1,
                                           null,
                                           -1);
        }
      }
    }

    // start the rendering
    ResponseWriter writer = context.getResponseWriter();
    boolean renderAsTable = _renderAsTable(context, navBar);

    if (renderAsTable)
    {
      writer.startElement("table", navBar.getUIComponent());
      renderLayoutTableAttributes(context, "0", null);
      renderID(context, navBar);
      writer.startElement("tr", null);
    }
    // we only want to render the ID in the "td" if renderAsTable is false.
    // render the base ID the first time only, then we render the subIDs.
    _renderStartTableCell(context, navBar, writer, renderAsTable, true);

    // don't render back button on first step
    if (showBackButton)
    {
      backButton.render(context);
      writer.endElement("td");

      _renderSpacerCell(context);
      // we only want to render the ID in the "td" if renderAsTable is false.
      // render the subID.
      _renderStartTableCell(context, navBar, writer, renderAsTable, false);

    }

    //
    // create the label and render it
    //
    writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);

    // Now, we create one of two things.  Normally, we'll create
    // a simple range string.  But, when there are link children,
    // we create a ChoiceBean
    if ((childCount == 0) ||
        // No form when we require one means no choice
        ((formName == null) && requiresForm(context)) ||
        // No scripting also means no choice
        !supportsScripting(context))
    {
      // No "Step 1 of X" when there's 1 or two steps
      if (totalItems > 2)
      {
        // the string to be displayed between buttons
        String rangeString = _getRangeString(context,
                                             navBar,
                                             currentValue,
                                             SINGLE_STEP,
                                             totalItems,
                                             null);

        MarlinBean stepLabel = new MarlinBean(STYLED_TEXT_NAME);
        stepLabel.setAttributeValue(TEXT_ATTR, rangeString);
        stepLabel.setStyleClass(NAV_BAR_VIEW_STYLE_CLASS);
        stepLabel.render(context);
      }
    }
    else
    {
      // Gather the children
      DataObjectList links = LinkDataObject.getLinkDataList(context,
                                                            navBar);
      MarlinBean     choice = new MarlinBean(CHOICE_NAME);

      int maxVisited = (int) currentValue;
      Object maxVisitedObj = navBar.getAttributeValue(context,
                                                      MAX_VISITED_ATTR);
      if (maxVisitedObj instanceof Number)
        maxVisited = ((Number) maxVisitedObj).intValue();

      // Sanity check the "maxVisited" attribute
      maxVisited = Math.min(maxVisited,
                            links == null ? 0 : links.getLength());
      // Add one option per link, but only go up to the "maxVisited"
      // value.  (BTW, since maxVisited is 1-indexed, but i is 0-indexed,
      // this is an _inclusive_ range here)
      for (int i = 0; i < maxVisited; i++)
      {
        DataObject link = links.getItem(i);
        MarlinBean option = new MarlinBean(OPTION_NAME);
        option.setAttributeValue(TEXT_ATTR,
                                 link.selectValue(context, TEXT_ATTR));
        option.setAttributeValue(VALUE_ATTR, IntegerUtils.getString(i + 1));

        if (currentValue == i + 1)
          option.setAttributeValue(SELECTED_ATTR, Boolean.TRUE);
        choice.addIndexedChild(option);
      }

      if (choice.getIndexedChildCount(context) > 0)
      {
        String onChange;

        // get name
        String name = _getNameString(context, navBar);

        // Two options: formSubmitted mode and non-formSubmitted mode.
        // First, non-formSubmitted mode.
        if (formName == null)
        {
          onChange = _getChoiceOnChange(context,
                                        _getDestinationString(context, navBar),
                                        sourceKey,
                                        eventKey,
                                        name,
                                        null,
                                        null);
        }
        else
        {
          onChange = _getChoiceOnChangeFormSubmitted(context,
                                                     navBar,
                                                     formName,
                                                     eventKey,
                                                     sourceKey,
                                                     name,
                                                     partialTargetsKey,
                                                     null);
        }

        choice.setAttributeValue(ON_CHANGE_ATTR, onChange);
      }
      else
      {
        choice.setAttributeValue(READ_ONLY_ATTR, Boolean.TRUE);
      }

      choice.render(context);
    }

    // don't render the next button on last step
    if (showNextButton)
    {
      writer.endElement("td");

      _renderSpacerCell(context);

      _renderStartTableCell(context, navBar, writer, renderAsTable, false);

      nextButton.render(context);
    }

    writer.endElement("td");

    if (renderAsTable)
    {
      writer.endElement("tr");
      writer.endElement("table");
    }
  }

  private String _getChoiceOnChange(
    UIXRenderingContext context,
    String destination,
    String sourceKey,
    String eventKey,
    String name,
    String partialTargets,
    String partialTargetsKey)
  {

    int initialSize = destination.length() +
                        sourceKey.length() +
                        eventKey.length() +
                        name.length() +
                        8; // length of known keys/values   +
                           // separators for each key/value
    String[] keysAndValues;
    String   startScript;
    String   endScript;

    if (partialTargets != null)
    {
      // Add in partialTargets parameter
      keysAndValues = new String[] {
                        sourceKey, name,        // ? ?
                        eventKey,  GOTO_EVENT,   // ? 4
                        partialTargetsKey, partialTargets
                        };

      startScript = _FIRE_PARTIAL_CHANGE_START;
      endScript = _FIRE_PARTIAL_CHANGE_END;
      initialSize += (partialTargetsKey.length() +
                      partialTargets.length()    +
                      2);
    }
    else
    {
      keysAndValues = new String[] {
        sourceKey, name,        // ? ?
        eventKey,  GOTO_EVENT   // ? 4
      };

      startScript = _CHOICE_ON_CHANGE_START;
      endScript = _CHOICE_ON_CHANGE_END;
    }
    
    int bufferlength = initialSize + startScript.length() + endScript.length();
    StringBuffer buffer = new StringBuffer(bufferlength);
    StringBuffer urlBuffer = new StringBuffer(initialSize);

    buffer.append(startScript);
    
    appendURLArguments(urlBuffer, destination, keysAndValues);
    String url = urlBuffer.toString();
    FacesContext facesContext = context.getFacesContext();
    if(facesContext != null)
      url = facesContext.getExternalContext().encodeActionURL(url);
    
    buffer.append(url);
    buffer.append(endScript);

    return buffer.toString();
  }


  private String _getChoiceOnChangeFormSubmitted(
    UIXRenderingContext context,
    UINode           node,
    String           form,
    String           eventKey,
    String           sourceKey,
    String           name,
    String           partialTargetsKey,
    String           partialTargets
    )
  {
    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedGotoEvent =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form, eventKey, GOTO_EVENT);
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form, sourceKey, name);
    String encodedPartialTargets =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            partialTargetsKey,
                                            partialTargets);

    int initialSize = _CHOICE_ON_CHANGE_FORM_START.length() +
                          form.length()                         +
                          13                                    +
                          encodedSource.length()                         +
                          _CHOICE_ON_CHANGE_FORM_END.length();

    // Make room for partialTargets if we've got any
    if (encodedPartialTargets != null)
      initialSize += (encodedPartialTargets.length() + 2);

    StringBuffer buffer = new StringBuffer(initialSize);
    buffer.append(_CHOICE_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("','");
    buffer.append(encodedGotoEvent);
    buffer.append("','");
    buffer.append(encodedSource);
    if (_doValidate(context, node))
      buffer.append("',1");
    else
      buffer.append("',0");

    if (encodedPartialTargets != null)
    {
      buffer.append(",null,");
      buffer.append("'");
      buffer.append(encodedPartialTargets);
      buffer.append("'");
    }

    buffer.append(_CHOICE_ON_CHANGE_FORM_END);
    return buffer.toString();
  }


  // render a multi-item navigator
  private void _renderMultiItemNavigator(
    UIXRenderingContext context,
    UINode navBar,
    int    blockSize,
    String eventKey,
    String sourceKey,
    String valueKey,
    String sizeKey,
    String partialTargetsKey,
    String partialTargets
    )
    throws IOException
  {
    // get current value
    Number result = (Number)navBar.getAttributeValue(context,
                                                     VALUE_ATTR);
    long currentValue = (result != null)
                          ? result.longValue()
                          : 1;

    // get min value
    result = (Number)navBar.getAttributeValue(context, MIN_VALUE_ATTR);
    long minValue = (result != null)
                       ? result.longValue()
                       : 1;

    // get max value
    result = (Number)navBar.getAttributeValue(context, MAX_VALUE_ATTR);
    long maxValue = (result != null)
                       ? result.longValue()
                       : MAX_VALUE_UNKNOWN;

    // get destination
    String destinationString = _getDestinationString(context, navBar);

    // get name
    String nameString = _getNameString(context, navBar);


    // get form name (#1308799)
    String formName = XhtmlLafUtils.getParentFormName(context);

    int  nextRecords = 0;
    int  backRecords = 0;
    long backValue = 0;
    long nextValue = 0;

    if (blockSize > 0)
    {
      // determine how many records user can go forward
      long lNextRecords = blockSize;

      if (maxValue != MAX_VALUE_UNKNOWN)
      {
        // if we know the total records, align the current value to the
        // start of its block. This makes the choice-rendering style
        // not show extra back navigation records on the min block,
        // which it would if not aligned.

        /** no don't do this. see bug: 3052637
        currentValue -= minValue;
        currentValue /= blockSize;
        currentValue *= blockSize;
        currentValue += minValue;
        */

        lNextRecords = maxValue - (currentValue + blockSize - 1);
      }

      // determine how many records user can go back
      long lBackRecords = currentValue - minValue;

      // trim
      nextRecords = (lNextRecords > blockSize)
        ? blockSize
        : (int) lNextRecords;

      backRecords = (lBackRecords > blockSize)
        ? blockSize
        : (int) lBackRecords;

      backValue = currentValue - backRecords;
      nextValue = currentValue + blockSize;
    }

    // calculate destinations
    String prevDestination = null;
    String nextDestination = null;
    String prevOnClick     = null;
    String nextOnClick     = null;

    UINode leftArrow  = null;
    UINode rightArrow = null;
    UINode leftLink   = null;
    UINode rightLink  = null;

    boolean validate = false;
    boolean createSubmitButton = false;

    boolean showDisabledNavigation = disabledNavigationShown(context);
    boolean hasBackRecords = (backRecords > 0);
    boolean hasNextRecords = (nextRecords > 0);

    boolean showBackButton = hasBackRecords || showDisabledNavigation;
    boolean showNextButton = hasNextRecords || showDisabledNavigation;
    if (!supportsNavigation(context))
    {
      showBackButton = false;
      showNextButton = false;
    }

    Object showAll = navBar.getAttributeValue(context, SHOW_ALL_ATTR);
    boolean atShowAll = SHOW_ALL_ACTIVE.equals(showAll);
    if (atShowAll)
    {
      backRecords = 0;
      nextRecords = 0;
    }

    if (formName == null)
    {
      // use parameterized URLs
      if (hasBackRecords && !atShowAll)
      {
        prevDestination = _getMultiDestinationURL(destinationString,
                                                  eventKey,
                                                  sourceKey,
                                                  nameString,
                                                  valueKey,
                                                  backValue,
                                                  sizeKey,
                                                  partialTargetsKey,
                                                  backRecords,
                                                  partialTargets);

        if (partialTargets != null)
        {
          // If we're doing partial page rendering, we need to
          // generate the _firePartialChange() onclick handler
          prevOnClick = XhtmlLafUtils.getFirePartialChangeHandler(
                                        prevDestination);
          prevDestination = "#";
        }
      }

      if (hasNextRecords && !atShowAll)
      {
        nextDestination = _getMultiDestinationURL(destinationString,
                                                  eventKey,
                                                  sourceKey,
                                                  nameString,
                                                  valueKey,
                                                  nextValue,
                                                  sizeKey,
                                                  partialTargetsKey,
                                                  nextRecords,
                                                  partialTargets);

        if (partialTargets != null)
        {
          // If we're doing partial page rendering, we need to
          // generate the _firePartialChange() onclick handler
          nextOnClick = XhtmlLafUtils.getFirePartialChangeHandler(
                                        nextDestination);
          nextDestination = "#";
        }
      }
    }
    else
    {
      // determine whether we need to validate on submit
      validate = _doValidate(context, navBar);

      // use form submit
      if (supportsScripting(context))
      {
        if (hasBackRecords && !atShowAll)
        {
          prevDestination = "#";
          prevOnClick = _getMultiDestinationSubmit(context,
                                                   formName,
                                                   eventKey,
                                                   sourceKey,
                                                   nameString,
                                                   valueKey,
                                                   backValue,
                                                   sizeKey,
                                                   backRecords,
                                                   validate,
                                                   partialTargetsKey,
                                                   partialTargets);
        }

        if (hasNextRecords && !atShowAll)
        {
          nextDestination = "#";
          nextOnClick =  _getMultiDestinationSubmit(context,
                                                    formName,
                                                    eventKey,
                                                    sourceKey,
                                                    nameString,
                                                    valueKey,
                                                    nextValue,
                                                    sizeKey,
                                                    nextRecords,
                                                    validate,
                                                    partialTargetsKey,
                                                    partialTargets);
        }

        // render hidden fields to hold the form data
        if (hasBackRecords || hasNextRecords)
          renderHiddenFields( context,
                              formName,
                              true,
                              eventKey,
                              sourceKey,
                              valueKey,
                              sizeKey,
                              partialTargetsKey,
                              partialTargets);
      }
      else
      {
        // create submit buttons to handle submission
        createSubmitButton = true;
      }
    }

    //
    // create the anonymous link nodes
    //
    if (createSubmitButton)
    {
      if (showBackButton)
      {
        // create back submit button bean
        leftArrow = createSubmitButton(context,
                                        _getMultiSubmitButtonText(context,
                                                                  true,
                                                                  backRecords),
                                        null,
                                        null,
                                        formName,
                                        validate,
                                        eventKey,
                                        sourceKey,
                                        nameString,
                                        valueKey,
                                        backValue,
                                        sizeKey,
                                        backRecords);
      }

      // create next submit button bean
      if (showNextButton)
      {
        rightArrow = createSubmitButton(context,
                                         _getMultiSubmitButtonText(context,
                                                                  false,
                                                                  nextRecords),
                                         null,
                                         null,
                                         formName,
                                         validate,
                                         eventKey,
                                         sourceKey,
                                         nameString,
                                         valueKey,
                                         nextValue,
                                         sizeKey,
                                         nextRecords);
      }
    }
    else
    {
      if (showBackButton)
      {
        leftArrow  = _createArrowImage(context,
                                       true,
                                       prevDestination,
                                       prevOnClick);

        leftLink   = _createTextLink(context,
                                     navBar,
                                     true,
                                     prevDestination,
                                     prevOnClick,
                                     backRecords);
      }

      if (showNextButton)
      {
        rightArrow = _createArrowImage(context,
                                       false,
                                       nextDestination,
                                       nextOnClick);

        rightLink  = _createTextLink(context,
                                     navBar,
                                     false,
                                     nextDestination,
                                     nextOnClick,
                                     nextRecords);

      }
    }

    UINode rangeNode  = _createRangeNode(context,
                                         navBar,
                                         destinationString,
                                         nameString,
                                         formName,
                                         minValue,
                                         currentValue,
                                         blockSize,
                                         maxValue,
                                         eventKey,
                                         sourceKey,
                                         sizeKey,
                                         partialTargetsKey,
                                         partialTargets);

    // ready to render
    ResponseWriter writer = context.getResponseWriter();
    boolean renderAsTable = _renderAsTable(context, navBar);



    // The following strange code is part of the work around for
    // bug 2275703.  IE has problems re-laying out a TableBean
    // after a partial page replacement.  In particular, pieces
    // of the table's top navigation bar, such as the previous link
    // or icon, or sometimes the entire navigation bar,  may shift to
    // the left.  In some cases, pieces of the navigation bar (the previous
    // icon) may disappear during re-layout!  There doesn't seem to be
    // any clean way to avoid this apparent IE bug.  However, we explicitly
    // pareform a partial replacement of the navigation bar's previous icon
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
    Object id = (supportsID(context) ? getID(context, navBar) : null);
    TrinidadAgent agent = context.getAgent();

    if ((id != null) &&
        (partialTargets != null) &&
        (agent.getAgentApplication() == TrinidadAgent.Application.IEXPLORER))
    {
      iconID = id.toString() + "-i";
    }

    // if we need to render standalone, create a table and table row...
    if (renderAsTable)
    {
      writer.startElement("table", null);
      renderLayoutTableAttributes(context, "0", null);

      // We should always render the ID, but we particularly need
      // to make sure that the ID is rendered if the NavBar is being
      // used to navigate a TableBean, since we explicitly target
      // TableBean NavBars when using PPR to re-render TableBeans...
      renderID(context, navBar);
      writer.startElement("tr", null);
    }

    // we only want to render the baseID, if needed, once. Then we
    // render the subIDs. So we need to keep track of this.
    boolean isBaseID = true;
    if (leftArrow != null)
    {
      // We assign an id to the left arrow so that we can target it as
      // a partial target to work around bug 2275703 - see note above.
      if (iconID != null)
      {
        writer.startElement("td", null);
        renderAttribute(context, "id", iconID);

        // If the navigation bar that we are currently rendering
        // is included in a partial page response, add the icon
        // id to the list of partial targets.
        PartialPageContext pprContext = context.getPartialPageContext();
        if ((pprContext != null) &&
            pprContext.isInsidePartialTarget())
        {
          pprContext.addRenderedPartialTarget(iconID);
        }
      }
      else
      {
        // not in PPR mode, so just render the td (and id if not in a table
        // for the Visual Editor)
        _renderStartTableCell(context, navBar, writer, renderAsTable, isBaseID);
        isBaseID = false;
      }

      writer.writeAttribute(VALIGN_ATTRIBUTE, "middle", null);
      leftArrow.render(context);
      writer.endElement("td");
      _renderSpacerCell(context);
    }

    if (leftLink != null)
    {
      _renderStartTableCell(context, navBar, writer, renderAsTable, isBaseID);
      isBaseID = false;
      writer.writeAttribute(VALIGN_ATTRIBUTE, "middle", null);
      writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
      leftLink.render(context);
      writer.endElement("td");
      _renderSpacerCell(context);
    }

    _renderStartTableCell(context, navBar, writer, renderAsTable, isBaseID);
    isBaseID = false;
    writer.writeAttribute(VALIGN_ATTRIBUTE, "middle", null);
    writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
    rangeNode.render(context);
    writer.endElement("td");

    if (rightLink != null)
    {
      _renderSpacerCell(context);

      _renderStartTableCell(context, navBar, writer, renderAsTable, false);
      writer.writeAttribute(VALIGN_ATTRIBUTE, "middle", null);
      writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
      rightLink.render(context);
      writer.endElement("td");
    }

    if (rightArrow != null)
    {
      _renderSpacerCell(context);

      _renderStartTableCell(context, navBar, writer, renderAsTable, false);
      writer.writeAttribute(VALIGN_ATTRIBUTE, "middle", null);
      rightArrow.render(context);
      writer.endElement("td");
    }

    if (renderAsTable)
    {
      writer.endElement("tr");
      writer.endElement("table");
    }
  }


  /**
   * Returns the destination, defaulting if none is provided
   */
  private static String _getDestinationString(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object destination = node.getAttributeValue(context,
                                                DESTINATION_ATTR);
    String destinationString = null;

    if (destination == null)
    {
      destinationString = context.getURLEncoder().getDefaultURL();

      if (destinationString == null)
        destinationString = "";
    }
    else
    {
      destinationString = destination.toString();
    }

    return destinationString;
  }


  /**
   * Returns the name of the String, defaulting if none is provided
   */
  private String _getNameString(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object name = node.getAttributeValue(context, NAME_ATTR);

    if (name != null)
    {
      return name.toString();
    }
    else
    {
      return "";
    }
  }


  // render a hidden field value, initialized to show that
  // no navigation is taken place
  // Note: This public method is called by TrainRenderer.java. The train
  // uses the javascript submission code rendered here to do validation
  public static void renderHiddenFields(
    UIXRenderingContext context,
    String           formName,
    boolean          isMulti,
    String           eventKey,
    String           sourceKey,
    String           valueKey,
    String           sizeKey,
    String           partialTargetsKey,
    String           partialTargets
    ) throws IOException
  {
    assert (supportsScripting(context));

    // first time we get a form-based navbar, render the
    // necessary Javascript
    if ((formName != null) &&
        (context.getProperty(MARLIN_NAMESPACE,
                             _NAVBAR_SUBMIT_JAVASCRIPT_RENDERED) == null))
    {
      ResponseWriter writer = context.getResponseWriter();

      // write the submit function
      writer.startElement("script", null);
      renderScriptDeferAttribute(context);

      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      XhtmlLafRenderer.renderScriptTypeAttribute(context);

      // write generic navbar submission code
      writer.write(_getNavBarSubmitScript(eventKey,
                      sourceKey,
                      valueKey,
                      sizeKey,
                      partialTargetsKey));

      // write choice submission code
      writer.write(_CHOICE_SUBMIT_SCRIPT);
      writer.endElement("script");

      // push this property into the context so we only render once
      context.setProperty(MARLIN_NAMESPACE,
                          _NAVBAR_SUBMIT_JAVASCRIPT_RENDERED,
                          Boolean.TRUE);
    }

    //only multi-step bars need the size field
    FormValueRenderer.addNeededValue(context,
                                     formName,
                                     eventKey,
                                     sourceKey,
                                     valueKey,
                                     (isMulti) ? sizeKey : null);

    if (partialTargets != null)
    {
      URLEncoder encoder = context.getURLEncoder();

      FormValueRenderer.addNeededValue(context,
                                       formName,
                                       partialTargetsKey,
                                       encoder.encodeParameter(PARTIAL_PARAM),
                                       null,
                                       null);
    }
  }

  // create the anonymous node displayed between the links
  private UINode _createRangeNode(
    UIXRenderingContext context,
    UINode navBar,
    String destination,
    String name,
    String form,
    long   minValue,
    long   currentValue,
    int    blockSize,
    long    maxValue,
    String eventKey,
    String sourceKey,
    String sizeKey,
    String partialTargetsKey,
    String partialTargets
    )
  {
    UINode rangeNode = null;

    // if there is no blockSize to step by, or there are no items in the
    // table, then we don't render a choice
    if ((blockSize <= 0) ||
        ((maxValue < minValue) &&
         (maxValue != MAX_VALUE_UNKNOWN)))
    {
      rangeNode = _getEmptyRangeNode();
    }
    else if (/*(maxValue == MAX_VALUE_UNKNOWN) ||*/ !supportsScripting(context))
    {
      // we don't know the size, so use a label
      String rangeString = _getRangeString(context,
                                           navBar,
                                           currentValue,
                                           blockSize,
                                           maxValue,
                                           null);

      MarlinBean rangeLabel = new MarlinBean(STYLED_TEXT_NAME);
      
      rangeLabel.setAttributeValue(TEXT_ATTR, rangeString);
      rangeLabel.setAttributeValue(STYLE_CLASS_ATTR, NAV_BAR_VIEW_STYLE_CLASS);

      rangeNode = rangeLabel;
    }
    else
    {
      // we know the size, so use the choice control
      MarlinBean choice = new MarlinBean(CHOICE_NAME);

      choice.setAttributeValue(SHORT_DESC_ATTR,
                               getTranslatedValue(context, _CHOICE_TIP_KEY));

      String onChange = null;
      int selectedIndex;

      if (form == null)
      {
        // Bug #1765747: if we're trying to render a choice,
        // Netscape will get very unhappy if there isn't a form,
        // and render wacky output that pretty much trashes the
        // navbar.
        // If a client was _directly_ adding a choice, then this
        // would be their fault, and we wouldn't try to work around
        // it.  But the NavigationBar implicitly adds the choice
        // for them, and the Back/Next buttons are still useable
        // even without the choice.  So, the nice thing to do
        // is just make the choice read-only.
        if (requiresForm(context))
        {
          if (getParentFormName(context) == null)
          {
            // No form - turn on read-only
            choice.setAttributeValue(READ_ONLY_ATTR, Boolean.TRUE);
          }
        }

        // create each of the choice options for parameterized URLs
        selectedIndex = _addNavigationOptions(context, navBar, choice, false,
                                          minValue, maxValue, currentValue,
                                          blockSize, sizeKey);
        int count = choice.getIndexedChildCount(context);
        if (count > 1)
        {
          onChange = _getChoiceOnChange(context, 
                                        destination,
                                        sourceKey,
                                        eventKey,
                                        name,
                                        partialTargets,
                                        partialTargetsKey);
        }
        else
        {
          choice.setAttributeValue(READ_ONLY_ATTR, Boolean.TRUE);
        }
      }
      else
      {
        // create each of the choice options for form submit
        selectedIndex = _addNavigationOptions(context, navBar, choice, true,
                                          minValue, maxValue, currentValue,
                                          blockSize, sizeKey);
        int count = choice.getIndexedChildCount(context);
        if (count > 1)
        {
          onChange = _getChoiceOnChangeFormSubmitted(
                        context,
                        navBar,
                        form,
                        eventKey,
                        sourceKey,
                        name,
                        partialTargetsKey,
                        partialTargets);
        }
        else
        {
          choice.setAttributeValue(READ_ONLY_ATTR, Boolean.TRUE);
        }
      }

      if (onChange != null)
      {
        // set the onchange handler
        choice.setAttributeValue(ON_CHANGE_ATTR, onChange);

        // set the onfocus handler to save the initial value
        choice.setAttributeValue(ON_FOCUS_ATTR, _CHOICE_FORM_ON_FOCUS);
      }

      if (supportsID(context) &&
          (selectedIndex >= 0) &&
          !Boolean.TRUE.equals(choice.getAttributeValue(READ_ONLY_ATTR)))
      {
        String choiceId = XhtmlLafUtils.generateUniqueID(context);
        choice.setID(choiceId);

        StringBuffer text = new StringBuffer(26 + choiceId.length());
        text.append("_setSelectIndexById(\"");
        text.append(choiceId);
        text.append("\",");
        text.append(IntegerUtils.getString(selectedIndex));
        text.append(")");

        MarlinBean sb = new MarlinBean(SCRIPT_NAME);
        sb.setAttributeValue(TEXT_ATTR, text.toString());

        MarlinBean flb = new MarlinBean(FLOW_LAYOUT_NAME);
        flb.addIndexedChild(choice);
        flb.addIndexedChild(sb);

        rangeNode = flb;
      }
      else
      {
        rangeNode = choice;
      }
    }

    return rangeNode;
  }


  /**
   * returns true if the navigation bar requires a form in order to
   * submit
   */
  protected boolean requiresForm(
    UIXRenderingContext context
    )
  {
    return true;
  }


  /**
   * create each of the choice options and add them onto the choiceBean.
   * @return the number of options added
   */
  private int _addNavigationOptions(UIXRenderingContext context,
                                    UINode navBar,
                                    MarlinBean choice,
                                    boolean isForm,
                                    long minValue, long maxValue, long value,
                                    int blockSize,
                                    String sizeKey)
  {
    int selectedIndex = -1;

    boolean maxUnknown = (maxValue == MAX_VALUE_UNKNOWN);

    // Zero-indexed block index.
    long blockIndex = (value - minValue + blockSize - 1L) /
                         blockSize;

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

    Object showAll = navBar.getAttributeValue(context, SHOW_ALL_ATTR);
    boolean atShowAll = SHOW_ALL_ACTIVE.equals(showAll);

    // If they want "Show All" add an option for it - but ONLY allow
    // this option when there are less than 30 groups (maxBlockIndex
    // start as zero, hence "29"), and only allow it when there's
    // more than 1 visible item!
    if (!maxUnknown &&
        (lastBlockIndex > firstBlockIndex) &&
        (maxBlockIndex <= (_MAX_VISIBLE_OPTIONS - 1L)) &&
        (atShowAll || SHOW_ALL_YES.equals(showAll)))
    {
      choice.addIndexedChild(0,
                             _createShowAllOption(context,
                                                  maxValue,
                                                  atShowAll));
    }
    else
    {
      atShowAll = false;
    }

    DataObject indexNames = (DataObject) navBar.getAttributeValue(context,
                                                                  _INDEX_NAMES_ATTR);
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

      Object text;
      // Need "Previous..."
      if ((blockIndex == firstBlockIndex) &&
          (blockIndex != 0))
      {
        text = getTranslatedValue(context, _PREVIOUS_TEXT_KEY);
      }
      // Need "More..." (on the last block, either 'cause
      // the total number of blocks is unknown or we've shown enough blocks
      else if ((blockIndex == lastBlockIndex) &&
               (maxUnknown || (lastBlockIndex < maxBlockIndex)))
      {
        text = getTranslatedValue(context, _MORE_TEXT_KEY);
      }
      else
      {
        text = null;
      }

      MarlinBean currOption = _createNavigationOption(context,
                                                  navBar,
                                                  isForm,
                                                  blockStart,
                                                  currentRecordSize,
                                                  maxValue,
                                                  // Don't select
                                                  atShowAll ?
                                                    minValue - 1 : value,
                                                  sizeKey,
                                                  text,
                                                  indexNames);
      choice.addIndexedChild(currOption);
      if (Boolean.TRUE.equals(currOption.getAttributeValue(SELECTED_ATTR)))
        selectedIndex = choice.getIndexedChildCount(context) - 1;
    }

    return selectedIndex;
  }

  private UINode _createShowAllOption(
    UIXRenderingContext context,
    long             maxValue,
    boolean          atShowAll)
  {
    MarlinBean option = new MarlinBean(OPTION_NAME);
    option.setAttributeValue(VALUE_ATTR, VALUE_SHOW_ALL);
    String[] parameters = new String[]{IntegerUtils.getString(maxValue)};
    String showAllText = formatString(context,
                                      getTranslatedString(context,
                                                          _SHOW_ALL_KEY),
                                      parameters);

    option.setAttributeValue(TEXT_ATTR, showAllText);
    if (atShowAll)
      option.setAttributeValue(SELECTED_ATTR, Boolean.TRUE);

    return option;
  }

  // create a choice option when max value is known
  private MarlinBean _createNavigationOption(
    UIXRenderingContext context,
    UINode           navBar,
    boolean          isForm,
    long             blockStart,
    int              blockSize,
    long             maxValue,
    long             currValue,
    String           sizeKey,
    Object           text,
    DataObject       indexNames
    )
  {
    MarlinBean option = new MarlinBean(OPTION_NAME);

    if (text == null)
      text = _getRangeString(context,
                             navBar,
                             blockStart,
                             blockSize,
                             maxValue,
                             indexNames);
    option.setAttributeValue(TEXT_ATTR, text);

    int actualBlockSize;

    if (maxValue == MAX_VALUE_UNKNOWN)
    {
      actualBlockSize = blockSize;
    }
    else
    {
      actualBlockSize = (int)(maxValue - blockStart + 1);
      if (actualBlockSize > blockSize)
        actualBlockSize = blockSize;
    }

    // add the value for javascript
    option.setAttributeValue(VALUE_ATTR, _getMultiDestinationURLEnd(blockStart,
                                               sizeKey,
                                               actualBlockSize,
                                               isForm));

    // select the correct one
    if ((currValue >= blockStart) &&
        (currValue <  (blockStart + blockSize)))
      option.setAttributeValue(SELECTED_ATTR, Boolean.TRUE);

    return option;
  }


  /**
   * Returns true if disabled navigation items should be shown
   */
  protected boolean disabledNavigationShown(
    UIXRenderingContext context
    )
  {
    return true;
  }


  /**
   * Returns the correct text to use for a submit button on a
   * multi item submit navbar
   */
  private String _getMultiSubmitButtonText(
    UIXRenderingContext context,
    boolean          isBack,
    int              size
    )
  {
    //
    // Create correct rtl string
    //
    boolean isRTL = isRightToLeft(context);

    char arrowChar = (isBack ^ isRTL)
                          ? '<'
                          : '>';

    String blockText = getBlockString(context, isBack, size);

    StringBuffer buttonText = new StringBuffer(blockText.length() + 2);

    if (isBack)
    {
      buttonText.append(arrowChar);
      buttonText.append(NBSP_CHAR);
      buttonText.append(blockText);
    }
    else
    {
      buttonText.append(blockText);
      buttonText.append(NBSP_CHAR);
      buttonText.append(arrowChar);
    }

    return buttonText.toString();
  }


  /**
   * Creates the submit button bean
   */
  public static UINode createSubmitButton(
    UIXRenderingContext context,
    Object           buttonText,
    Object           buttonAccessKey,
    String           buttonID,
    String           formName,
    boolean          validate,
    String           eventKey,
    String           sourceKey,
    String           source,
    String           valueKey,
    long             value,
    String           sizeKey,
    int              size
    )
  {
    MarlinBean submitButton =  new MarlinBean(SUBMIT_BUTTON_NAME);
    submitButton.setID(buttonID);
    submitButton.setAttributeValue(FORM_NAME_ATTR, formName);
    submitButton.setAttributeValue(UNVALIDATED_ATTR, !validate);
    submitButton.setAttributeValue(TEXT_ATTR, buttonText);
    submitButton.setAttributeValue(ACCESS_KEY_ATTR, buttonAccessKey);
    submitButton.setAttributeValue(NAME_VALUES_ATTR,
                                   _createKeyValueArray(eventKey,
                                                        sourceKey,
                                                        source,
                                                        valueKey,
                                                        value,
                                                        sizeKey,
                                                        size,
                                                        null,
                                                        null));
    return submitButton;
  }


  // create one of the text links for navigation
  private UINode _createTextLink(
    UIXRenderingContext context,
    UINode           navBar,
    boolean          isBack,
    String           destination,
    String           onClick,
    int              records
    )
  {
    String text = getBlockString(context, isBack, records);

    MutableUINode link;

    if (records > 0)
    {
      link = new MarlinBean(LINK_NAME);
      link.setAttributeValue(DESTINATION_ATTR, destination);
      link.setAttributeValue(ON_CLICK_ATTR, onClick);
      link.setAttributeValue(STYLE_CLASS_ATTR, NAV_BAR_ALINK_STYLE_CLASS);
    }
    else
    {
      link = new MarlinBean(STYLED_TEXT_NAME);
      link.setAttributeValue(STYLE_CLASS_ATTR, NAV_BAR_ILINK_STYLE_CLASS);
    }

    link.setAttributeValue(TEXT_ATTR, text);

    // The navBar needs its initial focus to be on the Next button,
    // according to the BLAF. Render a special id on the Next button
    // if this navBar is to have the initial focus. (unless it needs
    // initial focus, the Next button does not have an id on it)
    if (!isBack)
    {
      String linkID = _getIDForFocus(context, navBar);
      if (linkID != null)
      {
        link.setID(linkID);
      }
    }

    return link;
  }

  protected String getIconURI(
    UIXRenderingContext context,
    boolean          isBack,
    boolean          isEnabled
  )
  {
    // get the image location
    String iconName;

    if (isBack)
    {
      if (isEnabled)
      {
        iconName   = AF_TABLE_NB_PREV_ICON_NAME;
      }
      else
      {
        iconName   = AF_TABLE_NB_PREV_DISABLED_ICON_NAME;
      }
    }
    else
    {
      if (isEnabled)
      {
        iconName   = AF_TABLE_NB_NEXT_ICON_NAME;
      }
      else
      {
        iconName   = AF_TABLE_NB_NEXT_DISABLED_ICON_NAME;
      }
    }
    Skin skin = context.getSkin();
    RenderingContext arc = RenderingContext.getCurrentInstance();
    FacesContext fContext = context.getFacesContext();

    String iconURI = (String)(skin.getIcon(
                                          iconName).getImageURI(fContext, arc));

    return iconURI;
  }


  // create the arrow image used in multi-item navigation
  private UINode _createArrowImage(
    UIXRenderingContext context,
    boolean          isBack,
    String           destination,
    String           onClick
    )
  {
    // get the image location
    boolean isEnabled = (destination != null);
    String iconURI = getIconURI(context, isBack, isEnabled);

    // If we don't have an icon to render, return null so that
    // we won't render anything
    if (iconURI == null)
      return null;

    String shortDesKey;



    if (isBack)
    {
      if (isEnabled)
      {
        shortDesKey = _PREVIOUS_DESC_KEY;
      }
      else
      {
        shortDesKey = _DISABLED_PREVIOUS_DESC_KEY;
      }
    }
    else
    {
      if (isEnabled)
      {
        shortDesKey = _NEXT_DESC_KEY;
      }
      else
      {
        shortDesKey = _DISABLED_NEXT_DESC_KEY;
      }
    }

    MarlinBean arrow = new MarlinBean(IMAGE_NAME);
    arrow.setAttributeValue(SOURCE_ATTR, iconURI);
    arrow.setAttributeValue(SHORT_DESC_ATTR,
                            getTranslatedValue(context, shortDesKey));

    // if not a link, we're done;
    if (destination != null)
    {
      arrow.setAttributeValue(DESTINATION_ATTR, destination);
      arrow.setOnClick(onClick);
    }

    return arrow;
  }

  /**
   * Gets the string to use for next/previous links
   * in a table navigation bar.
   */
  protected String getBlockString(
    UIXRenderingContext context,
    boolean          isBack,
    int              numRecords
    )
  {
    // check to make sure that we have some records in this direction:
    if (numRecords > 0)
    {
      String pattern = (isBack)
        ? getTranslatedString(context, _SELECT_RANGE_PREVIOUS_KEY)
        : getTranslatedString(context, _SELECT_RANGE_NEXT_KEY);
      String value = IntegerUtils.getString(numRecords);
      String[] parameters = new String[]
      {
        value
      };

      return formatString(context, pattern, parameters);
    }
    else
    {
      // since we don't have any records, we are going to display some
      // disabled text. see bug 1740486.
      String text = (isBack)
        ? getTranslatedString(context, _SELECT_RANGE_DISABLED_PREVIOUS_KEY)
        : getTranslatedString(context, _SELECT_RANGE_DISABLED_NEXT_KEY);
      return text;
    }
  }

  // get the string for the current range
  private String _getRangeString(
    UIXRenderingContext context,
    UINode node,
    long   start,
    int    visibleItemCount,
    long   total,
    DataObject indexNames
    )
  {
    // formatter for generating the page string
    String pattern;
    String[] parameters;

    if (visibleItemCount == SINGLE_STEP)
    {

      Object typeText = node.getAttributeValue(context, TYPE_TEXT_ATTR);

      if (typeText == null)
        typeText = getTranslatedString(context, _STEP_TEXT_KEY);

      if (total == MAX_VALUE_UNKNOWN)
      {
        // =-= right now you can't ever get here because we don't show the
        // step number if maxStep is unknown, but just in case...
        pattern = getTranslatedString(context, 
                                      _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING);
        parameters = new String[]
                     {
                       typeText.toString(),
                       IntegerUtils.getString(start)
                     };
        
      }
      else
      {
        pattern = getTranslatedString(context, _SINGLE_RANGE_FORMAT_TOTAL_STRING);
        parameters = new String[]
                     {
                       typeText.toString(),
                       IntegerUtils.getString(start),
                       IntegerUtils.getString(total)
                     };
      }
    }
    else
    {
      // how many records do we really see now?
      long currVisible = (total == MAX_VALUE_UNKNOWN)
                           ? visibleItemCount
                           : total - start + 1;

      if (currVisible > visibleItemCount)
        currVisible = visibleItemCount;

      String startParam;
      String endParam;
      if (indexNames != null)
      {
        Long startNum = Long.valueOf(start);
        Long endNum = Long.valueOf(start + currVisible - 1);
        Object o = indexNames.selectValue(context, startNum);
        if (o != null)
          startParam = o.toString();
        else
          startParam = startNum.toString();

        o = indexNames.selectValue(context, endNum);
        if (o != null)
          endParam = o.toString();
        else
          endParam = endNum.toString();

        pattern = getTranslatedString(context, _STRING_RANGE_FORMAT_STRING); 
        parameters = new String[]
                     {
                       startParam,
                       endParam
                     }; 
      }
      else
      {
        startParam = IntegerUtils.getString(start);
        endParam = IntegerUtils.getString(start + currVisible - 1);
 
        if (total == MAX_VALUE_UNKNOWN)
        {
          pattern = getTranslatedString(context, 
                                   _MULTI_RANGE_FORMAT_NO_TOTAL_STRING);   
          parameters = new String[]
                          {
                            startParam,
                            endParam
                          }; 
        }
        else
        {
          pattern = getTranslatedString(context, 
                                   _MULTI_RANGE_FORMAT_TOTAL_STRING); 
          parameters = new String[]
                     {
                       startParam,
                       endParam,
                       IntegerUtils.getString(total)
                     }; 
        }
      }
    }

    return formatString(context, pattern, parameters);
  }

  // generate a destination URL for single-step navigators
  // Note: UINode is not guaranteed to be a navBar node.
  // e.g., it could be a train node.
  public static String getSingleDestinationURL(
    UIXRenderingContext context,
    UINode node,
    String eventKey,
    String sourceKey,
    String name,
    String valueKey,
    long   value
    )
  {
    String baseURL = _getDestinationString(context, node);
    String[] keysAndValues = {
                               eventKey,   GOTO_EVENT,
                               sourceKey,  name,
                               valueKey,   IntegerUtils.getString(value)
                             };

    return appendURLArguments(baseURL, keysAndValues);
  }

  // Note: UINode is not guaranteed to be a navBar node.
  // e.g., it could be a train node.
  public static String getSingleDestinationSubmit(
    UIXRenderingContext context,
    UINode  node,
    String  form,
    String  name,
    long    value,
    boolean doValidate
    )
  {
    URLEncoder urlEncoder = context.getURLEncoder();

    // this code might be refactored to avoid recomputing the parameter
    // keys here, but the overhead is small anyway because the URLEncoder is
    // typically a passthrough without renaming the parameters
    String eventKey = urlEncoder.encodeParameter(UIConstants.EVENT_PARAM);
    String sourceKey = urlEncoder.encodeParameter(UIConstants.SOURCE_PARAM);
    String valueKey = urlEncoder.encodeParameter(UIConstants.VALUE_PARAM);

    return getSingleDestinationSubmit(context, node, form, eventKey, sourceKey,
                                      name, valueKey, value, doValidate);
  }

  // Note: UINode is not guaranteed to be a navBar node.
  // e.g., it could be a train node.
  public static String getSingleDestinationSubmit(
    UIXRenderingContext context,
    UINode  node,
    String  form,
    String  eventKey,
    String  sourceKey,
    String  name,
    String  valueKey,
    long    value,
    boolean doValidate
    )
  {
    String valueString = IntegerUtils.getString(value);

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedGotoEvent =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form, eventKey, GOTO_EVENT);
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            sourceKey, name);
    String encodedValue =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            valueKey, valueString);

    if(form == null) form = "";
    if(encodedSource == null) encodedSource = "";
    boolean validate = false;
    if(doValidate)
    {
        validate = _doValidate(context, node);
    }

    int bufferSize = _LINK_ON_CHANGE_FORM_START.length() +
                     form.length() +
                     25 +     // appended literals below
                     encodedGotoEvent.length() +
                     encodedSource.length() +
                     encodedValue.length();
    StringBuffer buffer = new StringBuffer(bufferSize);
    buffer.append(_LINK_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("','");
    buffer.append(encodedGotoEvent);
    buffer.append("','");
    buffer.append(encodedSource);
    if (validate)
      buffer.append("',1,'");
    else
      buffer.append("',0,'");
    buffer.append(encodedValue);
    buffer.append("');return false");

    return buffer.toString();
  }

  private String _getMultiDestinationSubmit(
    UIXRenderingContext context,
    String  form,
    String  eventKey,
    String  sourceKey,
    String  name,
    String  valueKey,
    long    value,
    String  sizeKey,
    int     size,
    boolean validate,
    String  partialTargetsKey,
    String  partialTargets
    )
  {
    String valueString = IntegerUtils.getString(value);
    String sizeString  = IntegerUtils.getString(size);

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedGotoEvent =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            eventKey, GOTO_EVENT);
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            sourceKey, name);
    String encodedValue =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            valueKey, valueString);
    String encodedSize =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            sizeKey, sizeString);
    String encodedPartialTargets =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, form,
                                            partialTargetsKey,
                                            partialTargets);

    int bufferSize = _LINK_ON_CHANGE_FORM_START.length() +
                     form.length() +
                     34 +
                     encodedGotoEvent.length() +
                     encodedSource.length() +
                     encodedValue.length() +
                     encodedSize.length();

    if (partialTargets != null)
      bufferSize += (partialTargets.length() + 2);

    StringBuffer buffer = new StringBuffer(bufferSize);
    buffer.append(_LINK_ON_CHANGE_FORM_START);
    buffer.append(form);
    buffer.append("', '");
    buffer.append(encodedGotoEvent);
    buffer.append("', '");
    buffer.append(encodedSource);
    if (validate)
      buffer.append("',1,'");
    else
      buffer.append("',0,'");
    buffer.append(encodedValue);
    buffer.append("', '");
    buffer.append(encodedSize);
    buffer.append("',");

    // Tack on the partial targets array
    if (encodedPartialTargets != null)
    {
      buffer.append("'");
      buffer.append(encodedPartialTargets);
      buffer.append("'");
    }
    else
    {
      buffer.append("null");
    }

    buffer.append(");return false");

    return buffer.toString();
  }


  private static String[] _createKeyValueArray(
    String eventKey,
    String sourceKey,
    String source,
    String valueKey,
    long   value,
    String sizeKey,
    int    size,
    String partialTargetsKey,
    String partialTargets
    )
  {
    int length = 8;

    // Make room for partial targets if we've got some
    if (partialTargets != null)
      length += 2;

    String[] keyValues = new String[length];
    keyValues[0] = eventKey;
    keyValues[1] = GOTO_EVENT;
    keyValues[2] = sourceKey;
    keyValues[3] = source;
    keyValues[4] = valueKey;
    keyValues[5] = IntegerUtils.getString(value);
    keyValues[6] = sizeKey;
    keyValues[7] = IntegerUtils.getString(size);

    if (partialTargets != null)
    {
      keyValues[8] = partialTargetsKey;
      keyValues[9] = partialTargets;
    }

    return keyValues;
  }

  /**
   * Creates a submit button that will submit all of the name value pairs
   * as a single
   */
  private String _getMultiDestinationURL(
    String baseURL,
    String eventKey,
    String sourceKey,
    String source,
    String valueKey,
    long   value,
    String sizeKey,
    String partialTargetsKey,
    int    size,
    String partialTargets
    )
  {
    return appendURLArguments(baseURL,
                              _createKeyValueArray(eventKey,
                                                   sourceKey,
                                                   source,
                                                   valueKey,
                                                   value,
                                                   sizeKey,
                                                   size,
                                                   partialTargetsKey,
                                                   partialTargets));
  }


  /**
   * generate the end URL for onselects
   */
  private String _getMultiDestinationURLEnd(
    long    value,
    String  sizeKey,
    int     size,
    boolean isForm
    )
  {
    String valueString = IntegerUtils.getString(value);
    String sizeString  = IntegerUtils.getString(size);

    int bufferSize = valueString.length() +
                     sizeKey.length()     +
                     2                    +
                     sizeString.length();

    StringBuffer buffer = new StringBuffer(bufferSize);

    buffer.append(valueString);

    if (isForm)
    {
      buffer.append(",");
    }
    else
    {
      buffer.append('&');
      buffer.append(sizeKey);
      buffer.append('=');
    }
    buffer.append(sizeString);

    return buffer.toString();
  }

  private UINode _getEmptyRangeNode()
  {
    if (_sEmptyRangeNode == null)
    {
      _sEmptyRangeNode = new TextNode(NBSP_STRING);
    }

    return _sEmptyRangeNode;
  }

  private static String _getNavBarSubmitScript(
    String eventKey,
    String sourceKey,
    String valueKey,
    String sizeKey,
    String partialTargetsKey
    )
  {
    int len = _NAVBAR_SUBMIT_SCRIPT_LENGTH +
      eventKey.length() +
      sourceKey.length() +
      valueKey.length() +
      sizeKey.length();
    StringBuffer buf = new StringBuffer(len);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[0]);
    buf.append(eventKey);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[1]);
    buf.append(sourceKey);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[2]);
    buf.append(valueKey);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[3]);
    buf.append(sizeKey);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[4]);
    buf.append(partialTargetsKey);
    buf.append(_NAVBAR_SUBMIT_SCRIPT[5]);

    return buf.toString();
  }

  // don't render as a table in certain locations like a page button bar
  private boolean _renderAsTable(
    UIXRenderingContext context,
    UINode              navBar
    )
  {
    UIComponent component = NodeUtils.getUIComponent(context, navBar);
    if (component.getParent() instanceof CorePanelButtonBar)
      return false;

    return true;
  }

  // Utility method which tests whether the nav bar should validate
  // prior to navigating
  private static boolean _doValidate(
    UIXRenderingContext context,
    UINode           navBar
    )
  {
    return !Boolean.TRUE.equals(navBar.getAttributeValue(context,
                                                         UNVALIDATED_ATTR));
  }


  /**
   * Writes the separator between two elements
   */
  protected void renderItemSpacer(
    UIXRenderingContext context
    ) throws IOException
  {
    char[] chars = new char[1];
  chars[0] = NBSP_CHAR;

  context.getResponseWriter().writeText(chars, 0, 1);
  }

  /**
   * Writes the separator between two elements
   */
  private void _renderSpacerCell(
    UIXRenderingContext context
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    renderItemSpacer(context);
    writer.endElement("td");
  }
  /*
   * render the "td".
   * we only want to render the ID in the "td" if renderAsTable is false.
   * we only render the baseID if isBaseID is true. Otherwise we render the
   * subID.
   */
  private void _renderStartTableCell(
    UIXRenderingContext context,
    UINode           node,
    ResponseWriter   writer,
    boolean          renderAsTable,
    boolean          isBaseID
    ) throws IOException
  {

    writer.startElement("td", null);
    if (!renderAsTable)
    {
      if (isBaseID)
      {
        renderID(context, node);
      }
    }
  }

  // Gets the encoded partial targets for the specified node
  private static String _getPartialTargets(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (!supportsPartialRendering(context))
      return null;

    String[] partialTargets = (String[])node.getAttributeValue(context,
                                               PARTIAL_TARGETS_ATTR);

    // Convert the partial targets to the encoded form
    return PartialPageRendererUtils.encodePartialTargets(partialTargets);
  }

  private String _getIDForFocus(
    UIXRenderingContext context,
    UINode           navBar
    )
  {
    String id = null;

    // The navBar needs its initial focus to be on the Next button,
    // according to the BLAF. Render a special id on the Next button
    // if this navBar is to have the initial focus. (unless it needs
    // initial focus, the Next button does not have an id on it)
    // We get body's initialFocus attribute off of the rendering context.
    // If this is equal to the navBar's id, then we make up a new id
    // for the Next button.

    Object initialFocusID =
      getRenderingProperty(context, INITIAL_FOCUS_CONTEXT_PROPERTY);

    if ((initialFocusID != null))
    {
      Object navBarID = (supportsID(context) ? getID(context, navBar) : null);
      if (initialFocusID.equals(navBarID))
      {
        // make up an id to use for the initial focus.
        String focus = "-focus";
        StringBuffer buffer = new StringBuffer(navBarID.toString().length()+
                                               focus.length());
        buffer.append(navBarID.toString());
        buffer.append(focus);
        id = buffer.toString();
        // set the new id on the rendering context so that the body
        // renderer can write it out to a script variable.
        // A side-effect is that the initialFocusID in subsequent calls will
        // never equal the navBar's id.
        setRenderingProperty(context, INITIAL_FOCUS_CONTEXT_PROPERTY, id);
      }
    }

    return id;
  }

  //
  // Private variables
  //


  static private UINode _sEmptyRangeNode;

  // resource keys
  static private final String _SINGLE_BACK_TEXT_KEY =
    "af_singleStepButtonBar.BACK";
  static private final String _SINGLE_NEXT_TEXT_KEY =
    "af_singleStepButtonBar.NEXT";
  static private final String _SINGLE_CONTINUE_TEXT_KEY =
    "af_singleStepButtonBar.CONTINUE";
  static private final String _SINGLE_RANGE_FORMAT_TOTAL_STRING =
    "af_singleStepButtonBar.FORMAT_TOTAL";
  static private final String _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING =
    "af_singleStepButtonBar.FORMAT_NO_TOTAL";
  static private final String _STEP_TEXT_KEY =
    "af_singleStepButtonBar.STEP";


  static private final String _PREVIOUS_DESC_KEY =
    "af_table.SELECT_RANGE_PREVIOUS_TIP";
  static private final String _NEXT_DESC_KEY =
    "af_table.SELECT_RANGE_NEXT_TIP";
  static private final String _DISABLED_PREVIOUS_DESC_KEY =
    "af_table.SELECT_RANGE_PREV_DISABLED_TIP";
  static private final String _DISABLED_NEXT_DESC_KEY =
    "af_table.SELECT_RANGE_NEXT_DISABLED_TIP";
  static private final String _MULTI_RANGE_FORMAT_TOTAL_STRING =
    "af_table.SELECT_RANGE_CHOICE_FORMAT_TOTAL";
  static private final String _MULTI_RANGE_FORMAT_NO_TOTAL_STRING =
    "af_table.SELECT_RANGE_CHOICE_FORMAT_NO_TOTAL";
  static private final String _CHOICE_TIP_KEY =
    "af_table.SELECT_RANGE_CHOICE_TIP";
  static private final String _SELECT_RANGE_PREVIOUS_KEY =
    "af_table.SELECT_RANGE_PREVIOUS";
  static private final String _SELECT_RANGE_NEXT_KEY =
    "af_table.SELECT_RANGE_NEXT";
  static private final String _SELECT_RANGE_DISABLED_PREVIOUS_KEY =
    "af_table.SELECT_RANGE_DISABLED_PREVIOUS";
  static private final String _SELECT_RANGE_DISABLED_NEXT_KEY =
    "af_table.SELECT_RANGE_DISABLED_NEXT";
  static private final String _SHOW_ALL_KEY     =
    "af_table.SELECT_RANGE_SHOW_ALL";
  static private final String _PREVIOUS_TEXT_KEY =
    "af_table.SELECT_RANGE_PREVIOUS_OPTION";
  static private final String _MORE_TEXT_KEY     =
    "af_table.SELECT_RANGE_MORE_OPTION";

  static private final String _STRING_RANGE_FORMAT_STRING =
    "NAVBAR_STRING_RANGE_FORMAT";

  static private final long   _MAX_VISIBLE_OPTIONS = 30L;


  static private final String _CHOICE_ON_CHANGE_START =
  "window.self.location.href = '";

  static private final String _CHOICE_ON_CHANGE_END =
  "&value=' + this.options[this.selectedIndex].value";


  static private final String _LINK_ON_CHANGE_FORM_START =
  "_navBarSubmit('";

  static private final String _CHOICE_ON_CHANGE_FORM_START =
  "_navChoiceSubmit(this, '";

  static private final String _CHOICE_ON_CHANGE_FORM_END =
  ")";

  // navbar submission function that submits the form if the validation
  // succeeds and returns whether the validation succeeded.
  static private final String[] _NAVBAR_SUBMIT_SCRIPT =
  {
     "function _navBarSubmit(formName, event, navBar, vld, val, sze, partialTargets)"
    +"{"
    +  "var i = val.indexOf(',');"

    +  "if (i >= 0)"
    +  "{"
    +    "sze = val.substring(i+1);"
    +    "val = val.substring(0, i);"
    +  "}"

    +  "var submitFunc = (partialTargets == (void 0)) ? submitForm : "
    +                               "_submitPartialChange;"
    +  "return submitFunc("
    +             "formName,"
    +             "vld,"
    +             "{",                        // followed by the event name
                    ":event,", // followed by the source
                    ":navBar,",               // followed by the value
                    ":val,",                  // followed by the size
                    ":sze,",                  // followed by partialTargets
                    ":partialTargets});}"};
  private static final int _NAVBAR_SUBMIT_SCRIPT_LENGTH =
    XhtmlLafUtils.getLength(_NAVBAR_SUBMIT_SCRIPT);

  // submits the form for the choice and rools back if validation fails
  private static final String _CHOICE_SUBMIT_SCRIPT =
    "function _navChoiceSubmit(choice, formName, event, navBar, vld, size, partialTargets)"
   +"{"
   +  "if (!_navBarSubmit(formName, event, navBar, vld, choice.options[choice.selectedIndex].value, size, partialTargets))"
   +  "{"
   +    "choice.selectedIndex = choice._lastValue;"
   +  "}"
   +"}";

  // on focus handler for the form case.  We save off the old value so that
  // we can restore it if the validation chokes
  private static final String _CHOICE_FORM_ON_FOCUS =
    "this._lastValue = this.selectedIndex";

  static private final String _NAVBAR_SUBMIT_JAVASCRIPT_RENDERED =
  "_NAVBAR_SUBMIT_JAVASCRIPT_RENDERED";

  // Partial page rendering scripts
  private static final String _FIRE_PARTIAL_CHANGE_START =
    "_firePartialChange(\'";
  private static final String _FIRE_PARTIAL_CHANGE_END =
    _CHOICE_ON_CHANGE_END + ");return false;";

  // Private, undocumented attribute.  We may expose this publicly,
  // but for now, it's some OHW magic.
  private static final AttributeKey _INDEX_NAMES_ATTR =
    AttributeKey.getAttributeKey("indexNames");

}
