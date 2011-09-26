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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreMessages;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.util.LabeledFacesMessage;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;


/**
 * Renderer for org.apache.myfaces.trinidad.Messages, family org.apache.myfaces.trinidad.Messages.
 *
 */
public class MessageBoxRenderer extends XhtmlRenderer
{
  public MessageBoxRenderer()
  {
    this(CoreMessages.TYPE);
  }

  protected MessageBoxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey     = type.findKey("text");
    _messageKey     = type.findKey("message");
    _globalOnlyKey  = type.findKey("globalOnly");

    _headerRenderer = new HeaderRenderer(type);
    _boxRenderer = new BoxRenderer(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Force MessageBox to be re-rendered via PPR, since the set
    // of messages may have changed.
    RequestContext afContext = RequestContext.getCurrentInstance();
    if (afContext != null)
      afContext.addPartialTarget(component);

    ResponseWriter writer = context.getResponseWriter();

    Map<String, String> origSkinResourceMap = rc.getSkinResourceKeyMap();

    // Setup the rendering context, so that default skin selectors of
    // delegate renderers are mapped to those of this renderer
    rc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);

    // Check if INLINE validation mode is enabled
    boolean inlineValidation =
        RequestContext.ClientValidation.INLINE.equals(
            RequestContext.getCurrentInstance().getClientValidation());

    boolean isGlobalOnly = isGlobalOnly(component, bean);

    // Determine if we should render the MessageBox
    boolean renderMsgBox = (isGlobalOnly && context.getMessages(null).hasNext()) ||
      (!isGlobalOnly && inlineValidation) || (!isGlobalOnly && context.getMessages().hasNext());

    if (renderMsgBox)
    {

      if (!isGlobalOnly && inlineValidation && supportsScripting(rc))
      {
        writer.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
        renderScriptDeferAttribute(context, rc);
        renderScriptTypeAttribute(context, rc);

        // Output the styles required for client-side manipulation of the MessageBox

        // Output style for list of messages
        writer.writeText("TrPage.getInstance().addStyleClassMap( {'", null);
        writer.writeText(SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS + "':'", null);
        writer.writeText(rc.getStyleClass(SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS), null);

        // Single entry list uses two styles
        writer.writeText("','" + SkinSelectors.AF_MESSAGES_LIST_SINGLE_STYLE_CLASS + "':'", null);
        writer.writeText(rc.getStyleClass(SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS), null);
        writer.writeText(" " + rc.getStyleClass(SkinSelectors.AF_MESSAGES_LIST_SINGLE_STYLE_CLASS), null);

        // Output Style for MessageBox Anchors
        writer.writeText("','" + SkinSelectors.LINK_STYLE_CLASS + "':'", null);
        writer.writeText(rc.getStyleClass(SkinSelectors.LINK_STYLE_CLASS), null);
        writer.writeText("'} ); ", null);

        // Output the script that will register the MessageBox with
        // the TrMessageBox javascript class that handles client-side
        // add/remove of messages.
        writer.writeText("TrMessageBox._registerMessageBox(\"", null);
        writer.writeText(getClientId(context, component), null);
        writer.writeText("\");", null);
        writer.endElement("script");
      }

      // Delegate rendering of the outer shell to the BoxRenderer class
      // which will call back to this renderer to output the messages
      _boxRenderer.encodeAll(context, rc, component, bean);
    }
    else
    {
      // Always render an element, for update at PPR-time
      writer.startElement(XhtmlConstants.SPAN_ELEMENT, component);
      renderId(context, component);
      writer.endElement(XhtmlConstants.SPAN_ELEMENT);
    }

    // Reset the skin resource map
    rc.setSkinResourceKeyMap(origSkinResourceMap);
  }

  protected void _renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    boolean globalOnly = isGlobalOnly(component, bean);

    // TODO - Merge styles into AF_MESSAGES_STYLE_CLASS
    writer.startElement(XhtmlConstants.DIV_ELEMENT, component);
    renderStyleClass(context, rc, SkinSelectors.AF_MESSAGES_BODY_STYLE_CLASS);

    _renderHeader(context, rc, component, bean);

    // Render the 'message' attribute if specified
    String message = getMessage(component, bean);
    if (message != null)
    {
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      renderStyleClass(context, rc, SkinSelectors.AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS);
      writer.write(message);
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }

    // Render messages as a list
    writer.startElement("ol", null);

    // Output an id for the list so client-side validation can
    // easily access the element
    String listId = getClientId(context, component) + "__LIST__";
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, listId, null);

    // Switch list style depending if no. of messages is 1 or >1
    String[] styleClasses = null;
    if (MessageUtils.multipleMessagesQueued(context, globalOnly))
      styleClasses = new String[] {SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS};
    else
      styleClasses = new String[] {SkinSelectors.AF_MESSAGES_LIST_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_LIST_SINGLE_STYLE_CLASS};

    renderStyleClasses(context, rc, styleClasses);

    _renderGlobalMessages(context, rc, component, bean);

    if (!globalOnly)
      _renderComponentMessages(context, rc, component, bean);

    // End of list
    writer.endElement("ol");

    writer.endElement(XhtmlConstants.DIV_ELEMENT);
  }

  @SuppressWarnings("unchecked")
  protected void _renderGlobalMessages(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Get all messages without and id
    Iterator<FacesMessage> msgIter = context.getMessages(null);
    while (msgIter.hasNext())
    {
      FacesMessage msg = msgIter.next();

      writer.startElement("li", null);

      String text = MessageUtils.getGlobalMessage(rc, msg.getSummary(), msg.getDetail());
      renderPossiblyFormattedText(context, text);

      writer.endElement("li");
    }
  }

  @SuppressWarnings("unchecked")
  protected void _renderComponentMessages(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    Iterator<String> idIter = context.getClientIdsWithMessages();
    while (idIter.hasNext())
    {
      String id = idIter.next();

      // Skip global messages
      if (id == null)
        continue;

      Iterator<FacesMessage> msgIter = context.getMessages(id);
      while (msgIter.hasNext())
      {
        FacesMessage msg = msgIter.next();

        writer.startElement("li", null);

        _renderMessageAnchor(context, rc, msg, id);

        String text = MessageUtils.getClientMessage(rc, msg.getSummary(), msg.getDetail());

        // If the first two characters are "- ", assume it's due to af_messages.LIST_FORMAT_private;
        // alternatively, we could change the value of this key in CoreBundle.xrts, located in:
        // trinidad-impl\src\main\xrts\org\apache\myfaces\trinidadinternal\renderkit\core\resource
        // If the label is null, then we don't want to render the "- ".
        boolean isNullLabel = false;
        if (msg instanceof LabeledFacesMessage)
        {
          LabeledFacesMessage labeledMsg = (LabeledFacesMessage)msg;
          String labelString = labeledMsg.getLabelAsString(context);
          if (labelString == null || labelString.length() == 0)
            isNullLabel = true;
        }
        else
        {
          isNullLabel = true;
        }

        if (isNullLabel && text.charAt(0) == '-' && text.charAt(1) == ' ')
          text = text.substring(2);

        renderPossiblyFormattedText(context, text);

        writer.endElement("li");
      }
    }
  }

  protected void _renderHeader(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    delegateRenderer(context, rc, component, bean, _headerRenderer);
  }

  protected void _renderMessageAnchor(
    FacesContext     context,
    RenderingContext rc,
    FacesMessage     msg,
    String           componentId
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (componentId == null)
      return;

    // Anchor rendering currently only possible for messages that
    // contain a label, but we could use summary text in future
    if (msg instanceof LabeledFacesMessage)
    {
      LabeledFacesMessage labeledMsg = (LabeledFacesMessage)msg;
      String labelString = labeledMsg.getLabelAsString(context);
      if (labelString != null && labelString.length() > 0) // check for empty string
      {
        String anchor = MessageUtils.getAnchor(componentId);
        writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
        renderStyleClass(context, rc, SkinSelectors.LINK_STYLE_CLASS);
        writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#" + anchor, null);
        writer.write(labelString);
        writer.endElement(XhtmlConstants.LINK_ELEMENT);
        writer.write(' ');
      }
    }
  }

  // Rendering delegate to handle output of the header for the message box
  private class HeaderRenderer extends PanelHeaderRenderer
  {
    public HeaderRenderer(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      // Header will always be refreshed as sub-element of parent
      return false;
    }

    @Override
    protected void renderEventHandlers(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean
      ) throws IOException
    {
      // Prevent HeaderRenderer from re-rendering event handlers
    }

    @Override
    protected String getMessageType(
      UIComponent component,
      FacesBean   bean)
    {
      String messageType = null;

      FacesMessage.Severity maxSeverity =
        FacesContext.getCurrentInstance().getMaximumSeverity();

      // Map FacesMessage severity to levels expected by panelHeaderRenderer
      if (maxSeverity == null)
        messageType = XhtmlConstants.MESSAGE_TYPE_ERROR;
      else if (FacesMessage.SEVERITY_FATAL.equals(maxSeverity))
        messageType = XhtmlConstants.MESSAGE_TYPE_ERROR;
      else if (FacesMessage.SEVERITY_ERROR.equals(maxSeverity))
        messageType = XhtmlConstants.MESSAGE_TYPE_ERROR;
      else if (FacesMessage.SEVERITY_WARN.equals(maxSeverity))
        messageType = XhtmlConstants.MESSAGE_TYPE_WARNING;
      else if (FacesMessage.SEVERITY_INFO.equals(maxSeverity))
        messageType = XhtmlConstants.MESSAGE_TYPE_INFO;

      return messageType;
    }

    @Override
    protected String getText(
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean,
      String           messageType)
    {
      String text = MessageBoxRenderer.this.getText(component, bean);
      if (text != null)
        // Use Text attribute of this component for header text
        return text;

      // Otherwise parent will decide text & style based on messageType
      return super.getText(rc, component, bean, messageType);
    }

    @Override
    protected String getMessageIconName(String messageType)
    {
      String iconName = null;

      // Use the af|messages skin selectors instead of those
      // used by panelHeader
      if (XhtmlConstants.MESSAGE_TYPE_ERROR.equals(messageType))
        iconName = SkinSelectors.AF_MESSAGES_ERROR_ICON_NAME;
      else if (XhtmlConstants.MESSAGE_TYPE_WARNING.equals(messageType))
        iconName = SkinSelectors.AF_MESSAGES_WARNING_ICON_NAME;
      else if (XhtmlConstants.MESSAGE_TYPE_INFO.equals(messageType))
        iconName = SkinSelectors.AF_MESSAGES_INFO_ICON_NAME;
      else if (XhtmlConstants.MESSAGE_TYPE_CONFIRMATION.equals(messageType))
        iconName = SkinSelectors.AF_MESSAGES_CONFIRMATION_ICON_NAME;

      assert ((iconName != null) ||
              XhtmlConstants.MESSAGE_TYPE_NONE.equals(messageType));

      return iconName;
    }
  }

  // Delegate renderer, handles the outer element rendering and
  // provides option to wrap message box using rounded borders etc.
  private class BoxRenderer extends PanelBoxRenderer
  {
    public BoxRenderer(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      // As panelBox is handling the outer rendering, then it should render
      // the id of the MessageBox component
      return true;
    }

    @Override
    protected String getBackground(
      UIComponent component,
      FacesBean   bean)
    {
      // Force use of 'light' style, so we know which style
      // to re-map in _RESOURCE_KEY_MAP
      return "light";
    }

    @Override
    protected String getInlineStyle(
      UIComponent component,
      FacesBean   bean)
    {
      String inlineStyle = super.getInlineStyle(component, bean);

      boolean inlineValidation =
        RequestContext.ClientValidation.INLINE.equals(
            RequestContext.getCurrentInstance().getClientValidation());

      if (!inlineValidation)
        return inlineStyle;

      boolean hasMessages = FacesContext.getCurrentInstance().getMessages().hasNext();

      if (hasMessages)
        return inlineStyle;

      // Ensure the MessageBox is hidden for inline mode when there are no messages
      if (inlineStyle != null)
        return inlineStyle + ";display:none;";

      return "display:none;";
    }

    @Override
    protected boolean hasChildren(
      UIComponent component)
    {
      // Required to force panelBox to call render properly
      return true;
    }

    @Override
    protected void renderBody(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean,
      Object           icon,
      Object           text
      ) throws IOException
    {
      // Pass control back to MessageBoxRenderer to continue rendering
      // the content of the message box.
      MessageBoxRenderer.this._renderContent(context, rc, component, bean);
    }
  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    // Normally BoxRenderer will output the id for this component, but
    // if we're just outputting an empty element for PPR purposes, then
    // this renderer should output the id.
    return true;
  }

  protected String getText(
    UIComponent component,
    FacesBean   bean)
  {
    return (String)this.resolveProperty(bean, _textKey);
  }

  protected String getMessage(
    UIComponent component,
    FacesBean   bean)
  {
    return (String)this.resolveProperty(bean, _messageKey);
  }

  protected boolean isGlobalOnly(
    UIComponent component,
    FacesBean   bean)
  {
    return (Boolean)this.resolveProperty(bean, _globalOnlyKey, true);
  }

  private PropertyKey _textKey;
  private PropertyKey _messageKey;
  private PropertyKey _globalOnlyKey;
  private XhtmlRenderer _headerRenderer;
  private PanelBoxRenderer _boxRenderer;

  // Map panelHeader & panelBox Styles/Icons etc. to this renderer's selectors.
  private static final Map<String, String> _RESOURCE_KEY_MAP;

  static
  {
    _RESOURCE_KEY_MAP = new HashMap<String, String>();

    // translation keys
    _RESOURCE_KEY_MAP.put("af_panelHeader.INFORMATION",
                              "af_messages.INFORMATION");
    _RESOURCE_KEY_MAP.put("af_panelHeader.WARNING",
                              "af_messages.WARNING");
    _RESOURCE_KEY_MAP.put("af_panelHeader.ERROR",
                              "af_messages.ERROR");
    _RESOURCE_KEY_MAP.put("af_panelHeader.CONFIRMATION",
                              "af_messages.CONFIRMATION");
    // icons
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_ERROR_ICON_NAME,
        SkinSelectors.AF_MESSAGES_ERROR_ICON_NAME);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_WARNING_ICON_NAME,
        SkinSelectors.AF_MESSAGES_WARNING_ICON_NAME);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_INFO_ICON_NAME,
        SkinSelectors.AF_MESSAGES_INFO_ICON_NAME);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_CONFIRMATION_ICON_NAME,
        SkinSelectors.AF_MESSAGES_CONFIRMATION_ICON_NAME);

    // styles
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_ERROR_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_ERROR_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_HEADER_STYLE_CLASS);

    // We forced the use of 'light' style above, so now map it
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_STYLE_CLASS);

    // frame styles
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_TOP_START_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_TOP_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_TOP_END_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_START_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_END_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_BOTTOM_START_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_BOTTOM_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS,
        SkinSelectors.AF_MESSAGES_BOTTOM_END_STYLE_CLASS);
  }

}
