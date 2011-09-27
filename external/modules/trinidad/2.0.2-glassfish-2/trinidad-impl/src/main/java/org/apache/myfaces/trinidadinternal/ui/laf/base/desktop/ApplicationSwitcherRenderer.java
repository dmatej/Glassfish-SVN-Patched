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

import java.util.Locale;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationLevel;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.url.FormEncoder;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.AccessKeyBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.ContextPropertyBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ChoiceRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.CommandNavigationItemRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FormValueRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ModelRendererUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

import org.apache.myfaces.trinidadinternal.uinode.bind.MenuSelectedValueBoundValue;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ApplicationSwitcherRenderer.java#0 $) $Date: 10-nov-2005.18:55:07 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ApplicationSwitcherRenderer extends
                                        ChoiceRenderer
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
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    return ModelRendererUtils.setNewPath(component, startDepth,
                                         component.getFocusRowKey());


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
      boolean isNewPath = setNewPath(context, node, component);
      if (isNewPath)
      {

        int size = component.getRowCount();
        for (int i = 0; i < size; i++)
        {
          component.setRowIndex(i);
          renderChild(context, stamp);

        }

        // Restore the old path
        component.setRowKey(oldPath);
      }
    }

    super.renderContent(context, node);
  }

  @Override
  protected Object getNodeName(
          UIXRenderingContext context,
          UINode           node
          )
  {
    return node.getAttributeValue(context, ID_ATTR);
  }

  /**
   * Returns the value associated with the selected value attribute
   */
  @Override
  protected String getSelectedValue(UIXRenderingContext context, UINode node)
  {
    BoundValue bv = new MenuChoiceSelectedValueBoundValue(
      NodeUtils.getUIComponent(context, node));
    return (String) bv.getValue(context);
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
     // add core.js
     XhtmlLafUtils.addLib(context, "_commandChoice()");

    CommandNavigationItemRenderer.setNavigationItemRendererType(context,
                                        CommandNavigationItemRenderer.OPTION_TYPE);
    renderPreChoice(context, node);

    // start drop-down rendering...
    super.prerender(context, node);
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
    ResponseWriter writer = context.getResponseWriter();



    // start table
    writer.startElement(TABLE_ELEMENT, node.getUIComponent());

    String language = context.getLocaleContext().getTranslationLocale().getLanguage();

    // Do not set width so words don't break for Chinese, Japanese, and Korean
    if ( Locale.JAPANESE.getLanguage().equals(language) ||
         Locale.CHINESE.getLanguage().equals(language) ||
         Locale.KOREAN.getLanguage().equals(language))
    {
      renderLayoutTableAttributes(context, "0", null);
    }
    else
    {
      // setting width to some small non-zero number so that the
      //  prompt wraps in the case where it's to the left of the choice.
      renderLayoutTableAttributes(context, "0", "5");
    }

    writer.startElement(TABLE_ROW_ELEMENT, null);

    Object title = getTitle( context, node );

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(ALIGN_ATTRIBUTE, RIGHT_ATTRIBUTE_VALUE, null);
    renderStyleClassAttribute(context, AF_MENU_CHOICE_LABEL_STYLE_CLASS );
    if (title != null)
      writer.writeText(title, "title");
    writer.endElement(TABLE_DATA_ELEMENT);

    // add cell with space in it
    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeText(NBSP_STRING, null);
    writer.endElement(TABLE_DATA_ELEMENT);

    // cell with drop down menu
    writer.startElement(TABLE_DATA_ELEMENT, null);

    if (  context.getAgent().getAgentApplication()
          != TrinidadAgent.Application.NETSCAPE)
      writer.writeAttribute("valign", "bottom", null);
  }


  protected Object getTitle(
    UIXRenderingContext context,
    UINode           node
    )
  {

    // get the text to go next to choice
    Object title = node.getAttributeValue(context, TITLE_ATTR);

    if (title == null)
      title = getTranslatedValue(context, _SWITCH_APP);

    return title;
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
    // end drop-down rendering
    super.postrender(context, node);

    renderPostChoice(context, node);

    CommandNavigationItemRenderer.setNavigationItemRendererType(context, null);

  }

  protected void renderPostChoice(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    // end drop down cell
    writer.endElement(TABLE_DATA_ELEMENT);


    // add cell with space in it
    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeText(NBSP_STRING, null);
    writer.endElement(TABLE_DATA_ELEMENT);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    if (  context.getAgent().getAgentApplication()
          != TrinidadAgent.Application.NETSCAPE)
      writer.writeAttribute("valign", "bottom", null);

    // render goto button
    renderButton(context, node);

    writer.endElement(TABLE_DATA_ELEMENT);
    writer.endElement(TABLE_ROW_ELEMENT);

    writer.endElement(TABLE_ELEMENT);
  }
  // render the button
  protected void renderButton(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    // get the onClick handler
    String onClickHandler =  _getOnClickHandler( context, node );
    context.setProperty(MARLIN_NAMESPACE, _ON_CLICK_PROPERTY, onClickHandler);

    _BUTTON.render(context);

    context.setProperty(MARLIN_NAMESPACE, _ON_CLICK_PROPERTY, null);
  }


  /**
   *
   * @param context the rendering context
   * @param node the current UINode
   */
  private String _getOnClickHandler(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // get the name
    Object name = getTransformedName(context, node);

    String sourceValue = (name != null)
                        ? name.toString()
                        : null;

    // get the form name
    String formName = getParentFormName(context);


    URLEncoder encoder = context.getURLEncoder();

    /*Object destination = node.getAttributeValue(context, DESTINATION_ATTR);*/


    // get the body of a function called when there is switchApp event
    // not publicly documented because don't want clients doing
    // this in general, so for now this attribute not in xsd
    Object onClickBody = node.getAttributeValue(context,
                                                ON_SWITCH_APP_ATTR);

    StringBuffer function =  new StringBuffer();

    // if no body of function buffer should be empty
    // otherwise create new function with parameter called 'type',
    // body should refer to 'type' to distinguish between which
    // button pushed
    if (onClickBody != null)
    {
      function.append( "var func = new Function( \"type\",\"");
      function.append( onClickBody ).append("\");return func('");
      function.append( SWITCH_APP_TYPE_GOTO ).append("');");
    }

    // encode parameters
    /*String eventKey  = encoder.encodeParameter( EVENT_PARAM );*/
    String sourceKey = encoder.encodeParameter( SOURCE_PARAM );
    /*String typeKey = encoder.encodeParameter( TYPE_PARAM );*/

    StringBuffer handler = new StringBuffer();

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

    // only chains if necessary
    return XhtmlLafUtils.getChainedJS( function.toString(),
                                       handler.toString(),
                                       true).toString();
  }

  // render the button
  private static final MarlinBean _createButton()
  {
    BoundValue buttonTextandAccessKeyBV =
      new SkinTranslatedBoundValue(_GO_BUTTON_LABEL_KEY);


    Object buttonTextBV = new AccessKeyBoundValue( buttonTextandAccessKeyBV,
                                                   false);

    Object buttonAccessKeyBV = new AccessKeyBoundValue(
                                                    buttonTextandAccessKeyBV,
                                                    true);
    BoundValue shortDescBoundValue =
      new SkinTranslatedBoundValue(_GO_BUTTON_TIP_KEY);
    BoundValue onClickBoundValue =
                       new ContextPropertyBoundValue( MARLIN_NAMESPACE,
                                                      _ON_CLICK_PROPERTY);
    MarlinBean button = new MarlinBean(BUTTON_NAME);
    button.setAttributeValue(TEXT_ATTR, buttonTextBV);
    button.setAttributeValue(ACCESS_KEY_ATTR, buttonAccessKeyBV);
    button.setAttributeValue(SHORT_DESC_ATTR, shortDescBoundValue);
    button.setAttributeValue(ON_CLICK_ATTR, onClickBoundValue);

    return button;
  }


  private static class MenuChoiceSelectedValueBoundValue
                 extends MenuSelectedValueBoundValue
  {
    MenuChoiceSelectedValueBoundValue(
      UIComponent component
      )
    {
      super(component);
    }

    @Override
    protected boolean setNewPath(
      UIXNavigationHierarchy  menuComponent
    )
    {
      int startDepth = getLevel(menuComponent);
      return ModelRendererUtils.setNewPath(menuComponent, startDepth,
                                           menuComponent.getFocusRowKey());
    }

    @Override
    protected UIComponent getStamp(
      UIXNavigationHierarchy   menuComponent
      )
    {
      return ((UIXNavigationLevel)menuComponent).getNodeStamp();
    }

    protected int getLevel(
      UIXNavigationHierarchy   menuComponent
      )
    {
      return ((UIXNavigationLevel)menuComponent).getLevel();
    }

  }

  // button constants
  private static final String _GO_BUTTON_TIP_KEY   =
                                         "af_menuChoice.GO_TIP";
  private static final String _GO_BUTTON_LABEL_KEY   =
                                         "af_menuChoice.GO";

  private static final Object _ON_CLICK_PROPERTY = new Object();
  private static MarlinBean _BUTTON = _createButton();

  // text under drop down
  protected static final String _SWITCH_APP = "af_menuChoice.LABEL";

}
