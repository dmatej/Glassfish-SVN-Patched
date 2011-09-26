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
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXProcess;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelButtonBar;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.AccessKeyBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ProcessChoiceBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:08 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ProcessChoiceBarRenderer extends ChoiceRenderer
{

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
    Object focusPath = component.getFocusRowKey();
    component.setRowKey(focusPath);
    return true;


  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object newPath = context.getLocalProperty(0, _NEW_PATH_KEY, null);

    if (newPath != null)
    {
      UIXHierarchy component = getHierarchyBase(context, node);
      Object oldPath = component.getRowKey();
      component.setRowKey(newPath);
      UINode stamp = getStamp(context, node);
      int size = component.getRowCount();

      for (int i = 0; i < size; i++)
      {
        component.setRowIndex(i);
        renderChild(context, stamp);
      }

      component.setRowKey(oldPath);
    }

    super.renderContent(context, node);
  }

  @Override
  protected void selectItemsRenderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  }

  /**
   * Called to render the portion before the contents.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    UIXHierarchy component = getHierarchyBase(context, node);
    UINode stamp = getStamp(context, node);

    if(stamp != null)
    {
      Object oldPath = component.getRowKey();
      boolean isNewPath = setNewPath(context, node, component);
      if (isNewPath)
      {
        // The postrender will only close out the DOM is newPath is non-null so we should only
        // prerender if newPath is also non-null.
        Object newPath = component.getRowKey();
        if (newPath != null)
        {
          context.setLocalProperty(_NEW_PATH_KEY, newPath);
          component.setRowKey(oldPath);

          // add core.js
          XhtmlLafUtils.addLib(context, "_commandChoice()");
          renderPreChoice(context, node);
          CommandNavigationItemRenderer.setNavigationItemRendererType(context,
                                     CommandNavigationItemRenderer.OPTION_TYPE);

          // start drop-down rendering...
          super.prerender(context, node);
        }
      }
    }
  }

  /**
   * Called to render the portion before the contents.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void renderPreChoice(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    Object newPath = context.getLocalProperty(0, _NEW_PATH_KEY, null);
    UIXHierarchy component = getHierarchyBase(context, node);
    Object oldPath = component.getRowKey();  // Save the current key
    component.setRowKey(null);
    String nameString =
              BaseLafUtils.getStringAttributeValue(context, node, NAME_ATTR);
    String idString =
              BaseLafUtils.getStringAttributeValue(context, node, ID_ATTR);

    component.setRowKey(newPath);
    URLEncoder encoder = context.getURLEncoder();

    String eventKey  = encoder.encodeParameter(EVENT_PARAM);
    String sourceKey = encoder.encodeParameter(SOURCE_PARAM);
    String valueKey  = encoder.encodeParameter(VALUE_PARAM);
    String sizeKey   = encoder.encodeParameter(SIZE_PARAM);
    String partialTargetsKey = encoder.encodeParameter(PARTIAL_TARGETS_PARAM);

    // Get any partial targets - encoded in String form
    String partialTargets = _getPartialTargets(context, node);

    int totalItems = component.getRowCount();

    int selectedIndex = component.getRowIndex();

    if (totalItems <= 0)
    {
      component.setRowKey(oldPath);
      return;
    }

    int currentIndex = component.getRowIndex();
    UIComponent stamp = component.getFacet(UIXProcess.NODE_STAMP_FACET);
    int backButtonIndex = ProcessUtils.getBackIndex(component,
                                                    stamp,
                                                    currentIndex);
    int nextButtonIndex = ProcessUtils.getNextIndex(component,
                                                    stamp,
                                                    currentIndex);

    boolean showBackButton = backButtonIndex != ProcessUtils.NO_INDEX;
    boolean showNextButton = nextButtonIndex != ProcessUtils.NO_INDEX;
    
    boolean narrowScreenPDA = supportsNarrowScreen(context);

    // bail if no buttons shown
    if (!showBackButton && !showNextButton)
    {
      component.setRowKey(oldPath);
      return;
    }

    // get form name (#1308799)
    String formName = XhtmlLafUtils.getParentFormName(context);


    // If we don't support navigation (e.g., printable pages),
    // lie and claim we support scripting (even though we probably don't).
    // This will give us the highest fidelity output - that is,
    // we avoid creating submit buttons.
    boolean supportsScripting = (supportsScripting(context) ||
                                 !supportsNavigation(context));
    if (supportsScripting)
    {
      // render hidden fields to hold the form data
      _renderHiddenFields( context,
                          formName,
                          eventKey,
                          sourceKey,
                          valueKey,
                          sizeKey,
                          partialTargetsKey,
                          partialTargets);
    }

    UINode backButton = null;
    Object nonJSBackButtonText = null;
    
    // In the case of narrow-screen PDAs, the next and previous buttons
    // are not rendered to reduce the component's width.
    showBackButton = showBackButton && !narrowScreenPDA;
    showNextButton = showNextButton && !narrowScreenPDA; 

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

      if (supportsScripting)
      {
        component.setRowIndex(backButtonIndex);
        boolean immediate = Boolean.TRUE.equals(
                          stamp.getAttributes().get(UIXCommand.IMMEDIATE_KEY));
        Object destination = stamp.getAttributes().get("destination");
        component.setRowIndex(currentIndex);

        backButton = createSingleItemSubmitButton(
                                true,
                                buttonTextBV,
                                buttonAccessKeyBV,
                                destination,
                                ProcessUtils.getSubmitScriptCall(
                                    context,
                                    formName,
                                    eventKey,
                                    sourceKey,
                                    nameString,
                                    valueKey,
                                    selectedIndex - 1,
                                    sizeKey,
                                    0,
                                    !immediate,
                                    null,
                                    null));
      }
      else
      {
        nonJSBackButtonText = buttonTextBV;
      }
    }

    UINode nextButton = null;

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
      String buttonID = _getIDForFocus(context, node);


      // set the destination
      if (supportsScripting)
      {
        component.setRowIndex(nextButtonIndex);
        boolean immediate = Boolean.TRUE.equals(
                          stamp.getAttributes().get(UIXCommand.IMMEDIATE_KEY));
        Object destination = stamp.getAttributes().get("destination");
        component.setRowIndex(currentIndex);

        MutableUINode mutableNextButton =
        createSingleItemSubmitButton(false,
                                     buttonTextBV,
                                     buttonAccessKeyBV,
                                     destination,
                                     ProcessUtils.getSubmitScriptCall(
                                        context,
                                        formName,
                                        eventKey,
                                        sourceKey,
                                        nameString,
                                        valueKey,
                                        selectedIndex + 1,
                                        sizeKey,
                                        1,
                                        !immediate, null, null));
        if (buttonID != null)
        {
          mutableNextButton.setID(buttonID);
        }
        nextButton = mutableNextButton;
        context.setLocalProperty(_NEXT_BUTTON_KEY, nextButton);
      }
      else
      {
        // For Non-JavaScript browsers, encode the parameter name and value
        // pairs required for the next button's funtionality. This encoded  
        // value would be used as the name attribute of the element that    
        // would be rendered for the next button's funtionality.
        String nameAttri = XhtmlUtils.getEncodedNameAttribute (
                             // Array should be in the order of parameter 
                             // name and value pair
                                 new String[]{SOURCE_PARAM,
                                              nameString,
                                              EVENT_PARAM,
                                              UIConstants.GOTO_EVENT,
                                              VALUE_PARAM,
                                              Long.toString(selectedIndex + 1),
                                              SIZE_PARAM,
                                              Integer.toString(1)});
                                                          
        context.setLocalProperty(_NON_JS_NEXT_BUTTON_NAME_ATTR, nameAttri);
        context.setLocalProperty(_NON_JS_NEXT_BUTTON_TEXT, buttonTextBV);
      }
    }

    // start the rendering
    ResponseWriter writer = context.getResponseWriter();
    boolean renderAsTable = _renderAsTable(context, node);

    if (renderAsTable)
    {
      writer.startElement("table", NodeUtils.getUIComponent(context, node));
      renderLayoutTableAttributes(context, "0", null);
      String uniqueId = UniqueCompositeId.getId(idString, null);
      writer.writeAttribute("id", uniqueId, null);
      writer.startElement("tr", null);
    }
    // we only want to render the ID in the "td" if renderAsTable is false.
    // render the base ID the first time only, then we render the subIDs.
    _renderStartTableCell(context, node, writer, renderAsTable, true);

    // don't render back button on first step
    if (showBackButton)
    {

      if (supportsScripting)
      {
        backButton.render(context);
      } 
      else
      {
        // For Non-JavaScript browsers, render an input element(type= submit)
        // to submit the page. The name attribute of this element is encoded 
        // with parameter name and value pairs thus it would enable browsers
        // to include the name of this element in its payLoad if it submits the
        // page.
        String nameAttri = XhtmlUtils.getEncodedNameAttribute (
                            // Array should be in the order of parameter name 
                            // and value pair
                               new String[]{SOURCE_PARAM,
                                            nameString,
                                            EVENT_PARAM,
                                            UIConstants.GOTO_EVENT,
                                            VALUE_PARAM,
                                            Long.toString(selectedIndex - 1),
                                            SIZE_PARAM,
                                            Integer.toString(0)});

        String buttonText = ((AccessKeyBoundValue)nonJSBackButtonText)
                                              .getValue(context).toString();

        _renderSubmitButtonNonJSBrowser(context, buttonText, nameAttri);
      }
      writer.endElement("td");

      _renderSpacerCell(context);
      // we only want to render the ID in the "td" if renderAsTable is false.
      // render the subID.
      _renderStartTableCell(context, node, writer, renderAsTable, false);

    }

    //
    // create the label and render it
    //
    writer.writeAttribute(NOWRAP_ATTRIBUTE, Boolean.TRUE, null);
    component.setRowKey(oldPath);
  }

  /**
   * Called to render the portion after the contents.
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object hasChildren = context.getLocalProperty(0, _NEW_PATH_KEY, null);

    if (hasChildren != null)
    {
      // end drop-down rendering
      super.postrender(context, node);
      CommandNavigationItemRenderer.setNavigationItemRendererType(context, null);
      renderPostChoice(context, node);
      context.setLocalProperty(_NEW_PATH_KEY, null);
    }
  }

  protected void renderPostChoice(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // start rendering
    ResponseWriter writer = context.getResponseWriter();
    boolean renderAsTable = _renderAsTable(context, node);
    
    if (supportsScripting(context))
    {
      UINode nextButton =
                    (UINode)context.getLocalProperty(0,_NEXT_BUTTON_KEY, null);
      // don't render the next button on last step
      if (nextButton != null)
      {
        writer.endElement("td");

        _renderSpacerCell(context);

        _renderStartTableCell(context, node, writer, renderAsTable, false);

        nextButton.render(context);
        context.setLocalProperty(_NEXT_BUTTON_KEY, null);

      }
    }
    else  
    {
      writer.endElement("td");
      writer.startElement("td", null);
      
      // For Non-JavaScript browsers, render an input element(type= submit)
      // to submit the page. The name attribute of this element is encoded 
      // with parameter name and value pairs thus it would enable browsers
      // to include the name of this element in its payLoad if it submits the
      // page.
      String nameAttri = 
               XhtmlUtils.getEncodedNameAttribute (
               // Array should be in the order of parameter name and value pair
                      new String[]{ XhtmlConstants.MULTIPLE_VALUE_PARAM,
                                    BaseLafUtils.getStringAttributeValue
                                    (context, node, NAME_ATTR)});

      _renderSubmitButtonNonJSBrowser(
                           context, 
                           XhtmlConstants.NO_JS_PARAMETER_KEY_BUTTON, 
                           nameAttri);
      
      String nextButtonNameAttr =
                         (String)context.getLocalProperty(
                                 0, _NON_JS_NEXT_BUTTON_NAME_ATTR, null);
                                  
      if (nextButtonNameAttr != null)
      {
        
        writer.endElement("td");
       
        _renderSpacerCell(context);
       
        writer.startElement("td", null);
        
        Object nextButtonText = context.getLocalProperty(
                                        0, _NON_JS_NEXT_BUTTON_TEXT, null);
                                          
        String buttonText = ((AccessKeyBoundValue)nextButtonText)
                                              .getValue(context).toString();
       
        _renderSubmitButtonNonJSBrowser(context, buttonText, 
                                                 nextButtonNameAttr );
                                       
        context.setLocalProperty(_NON_JS_NEXT_BUTTON_NAME_ATTR, null);
        context.setLocalProperty(_NON_JS_NEXT_BUTTON_TEXT, null);
        
      }
    }
    
    writer.endElement("td");

    if (renderAsTable)
    {
      writer.endElement("tr");
      writer.endElement("table");
    }

  }

  @Override
  protected Object getOnChange(
    UIXRenderingContext context,
    UINode           node
  )throws IOException
  {
    StringBuffer handler = new StringBuffer();
    Object name = getTransformedName(context, node);

    String sourceValue = (name != null)
                        ? name.toString()
                        : null;

    // get the form name
    String formName = getParentFormName(context);


    URLEncoder encoder = context.getURLEncoder();
    String sourceKey = encoder.encodeParameter( SOURCE_PARAM );
    FormValueRenderer.addNeededValue(context, formName, sourceKey);

    // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
    FormEncoder formEncoder = context.getFormEncoder();
    String encodedSource =
      XhtmlLafUtils.getFormEncodedParameter(formEncoder, formName,
                                            sourceKey, sourceValue);

    handler.append("_commandChoice('");
    handler.append( formName ).append( "','");
    handler.append( encodedSource ).append("'");
    handler.append( ");return false;");

    String handlerString = handler.toString();
    Object superHandler = super.getOnChange(context, node);

    // Do we ever want to short circuit? Yes, see bug #3761794
    return XhtmlLafUtils.getChainedJS(superHandler, handlerString, true);
  }


  protected MutableUINode createSingleItemSubmitButton(
    boolean          isBack,
    Object           buttonText,
    Object           buttonAccessKey,
    Object           destination,
    String           onClickJS
    )
  {
    MarlinBean submitButton = new MarlinBean(BUTTON_NAME);
    submitButton.setAttributeValue(TEXT_ATTR, buttonText);
    submitButton.setAttributeValue(ACCESS_KEY_ATTR, buttonAccessKey);

    if (destination != null)
      submitButton.setAttributeValue(DESTINATION_ATTR, destination.toString());
    else
      submitButton.setOnClick(onClickJS);

    return submitButton;
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

  // don't render as a table in certain locations like a page button bar
  private boolean _renderAsTable(
    UIXRenderingContext context,
    UINode              node
    )
  {
    UIComponent component = NodeUtils.getUIComponent(context, node);
    if (component.getParent() instanceof CorePanelButtonBar)
      return false;

    return true;
  }




  /**
   * Writes the separator between two elements
   */
  protected void renderItemSpacer(
    UIXRenderingContext context
    ) throws IOException
  {
    char[] chars = new char[NBSP_CHAR];

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
   * This logic is so for the Visual Editor to know
   * the pieces are one UINode. See bug 2222541.
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
    UINode           node
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
      Object navBarID = (supportsID(context) ? getID(context, node) : null);
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

  /**
   * render form value needed values and javascript code.
   */
  private void _renderHiddenFields(
    UIXRenderingContext context,
    String           formName,
    String           eventKey,
    String           sourceKey,
    String           valueKey,
    String           sizeKey,
    String           partialTargetsKey,
    String           partialTargets
    ) throws IOException
  {

    assert (supportsScripting(context));

    if ((formName != null) && supportsScripting(context))
    {
      // render hidden fields to hold the form data
      FormValueRenderer.addNeededValue( context,
                                        formName,
                                        eventKey,
                                        sourceKey,
                                        valueKey,
                                        sizeKey);

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

      // Render script submission code.
      ProcessUtils.renderNavSubmitScript(context);
      ProcessUtils.renderNavChoiceSubmitScript(context);
    }

  }

  /**
    * @param context a <code>UIXRenderingContext</code>
    * @param valueAttri a <code>String</code> it is the value attribute  
    *  of the submit button 
    * @param nameAttri  a <code>String</code> it is the name attribute  
    *  of the submit button 
    *
    * This method renders an input element(type= submit) to submit the page.
    * The name attribute of this element is encoded with parameter name and
    * value pairs thus it would enable browsers to include the name of this 
    * element in its payLoad if it submits the page.
    *
    */
  private void _renderSubmitButtonNonJSBrowser(
    UIXRenderingContext context,
    String         valueAttri,
    String         nameAttri
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("input", null);
    renderAttribute(context, "type", "submit");
    renderAttribute(context, "value", valueAttri);
    renderAttribute(context, "name", nameAttri);
    renderStyleClassAttribute(context, 
          SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS);
    writer.endElement("input");
  }

  //
  // Private variables
  //

  // resource keys
  static private final String _SINGLE_BACK_TEXT_KEY =
    "af_processChoiceBar.BACK";
  static private final String _SINGLE_NEXT_TEXT_KEY =
    "af_processChoiceBar.NEXT";
  static private final String _SINGLE_CONTINUE_TEXT_KEY =
    "af_processChoiceBar.CONTINUE";

  static private final Object _NEXT_BUTTON_KEY = new Object();
  static private final Object _NEW_PATH_KEY = new Object();
  static private final Object _NON_JS_NEXT_BUTTON_NAME_ATTR = new Object();
  static private final Object _NON_JS_NEXT_BUTTON_TEXT = new Object();  

}
