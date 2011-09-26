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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.core.output.CoreMessages;

import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.data.bind.ContextPropertyBoundValue;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;

/**
 * Renders a message box.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/MessageBoxRenderer.java#0 $) $Date: 10-nov-2005.18:55:26 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MessageBoxRenderer extends HtmlLafRenderer
{
  @Override
  public void render(UIXRenderingContext context, UINode node) throws IOException
  {
    Object globalOnlyAttr = getAttributeValue(context, node,
                                       GLOBAL_ONLY_ATTR, null);
    _allMessages = !Boolean.TRUE.equals(globalOnlyAttr);

    if (MessageBoxUtils.sIsRendered(context, node, _allMessages))
    {
      super.render(context, node);
    }
    else
    {
      // Even when we're not rendering the messagebox, the ID had
      // better show up.
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("span", node.getUIComponent());
      renderID(context, node);
      writer.endElement("span");
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT;
  }

  /**
   * Returns the messageBox's style class.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return AF_MESSAGES_STYLE_CLASS;
  }

  private final static Object _TEXT_PROPERTY = new Object();
  private final static Object _MESSAGE_TYPE_PROPERTY = new Object();
  private static MarlinBean  _sHeader;
  static
  {
    MarlinBean header = new MarlinBean(HEADER_NAME);

    header.setAttributeValue(TEXT_ATTR,
                     new ContextPropertyBoundValue(MARLIN_NAMESPACE,
                                                   _TEXT_PROPERTY));
    header.setAttributeValue(MESSAGE_TYPE_ATTR,
                     new ContextPropertyBoundValue(MARLIN_NAMESPACE,
                                                   _MESSAGE_TYPE_PROPERTY));

    _sHeader = header;
  }




  // based on oracle.desktop.MessageBoxRenderer.renderChildren
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String messageType = MessageBoxUtils.sGetMaxType(context);

    Object text = getAttributeValue(context, node, TEXT_ATTR, null);

    context.setProperty(MARLIN_NAMESPACE, _TEXT_PROPERTY, text);
    context.setProperty(MARLIN_NAMESPACE, _MESSAGE_TYPE_PROPERTY, messageType);
    // save whatever is on the resource key map
    // set the resource key map; we need to use af_messages keys in
    // the messageBox, even though it uses a HeaderBean.
    // map the resource keys that are used in HideShowBean to the
    // keys we need to use in this renderer.
    Map<String, String> originalResourceKeyMap = context.getSkinResourceKeyMap();
    try
    {
      context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      _sHeader.render(context);
    }
    finally
    {
      context.setSkinResourceKeyMap(originalResourceKeyMap);
    }
    // restore the translation value map
    context.setProperty(MARLIN_NAMESPACE, _TEXT_PROPERTY, null);
    context.setProperty(MARLIN_NAMESPACE, _MESSAGE_TYPE_PROPERTY, null);

    Object message = MessageBoxUtils.sGetMessage(context, node);

    boolean useList = MessageBoxUtils.sMultipleMessages(context, _allMessages);

    writer.startElement(DIV_ELEMENT, null);
    renderStyleClassAttribute(context, AF_MESSAGES_MESSAGE_TEXT_STYLE_CLASS);
    if (message != null)
      writer.writeText(message, CoreMessages.MESSAGE_KEY.getName());
    writer.endElement(DIV_ELEMENT);

    if (useList)
    {
      // render the message list
      writer.startElement("ol", null);
    }
    else
    {
      // render a div
      writer.startElement("div", null);
    }

    // oracle desktop's renderer allows different list styles depending
    // upon if it is a list or a div, and whether it is an error or not.
    // should we do the same here? For now, don't worry about it.
    renderStyleClassAttribute(context, AF_MESSAGES_LIST_STYLE_CLASS);

    // Do the global messages first
    _renderMessages(context, writer, true, useList);

    // Then client component messages
    if (_allMessages)
      _renderMessages(context, writer, false, useList);

    if (useList)
      writer.endElement("ol");
    else
    {
      writer.endElement("div");
    }
  }

  private void _renderMessages(
    UIXRenderingContext context,
    ResponseWriter writer,
    boolean isGlobal,
    boolean useList
    ) throws IOException
  {
    BaseMutableUINode currentChild = null;
    String summary;
    String detail;

    Iterator<MessageWrapper> itr = (isGlobal
                    ? MessageBoxUtils.sGetGlobalsIterator(context)
                    : MessageBoxUtils.sGetClientsIterator(context));

    while (itr.hasNext())
    {
      MessageWrapper msg = itr.next();

      if (useList)
        writer.startElement("li", null);

      summary = msg.getSummary();
      detail = msg.getDetail();

      if (isGlobal)
        _writeGlobalMsg(context, writer, summary, detail);
      else
        currentChild = _writeClientMsg(context, writer, summary,
                                       msg, currentChild);

      if (useList)
        writer.endElement("li");
    }
  }

  private void _writeGlobalMsg(
    UIXRenderingContext context,
    ResponseWriter writer,
    String summary,
    String detail
    ) throws IOException
  {
    String text = MessageUtils.getGlobalMessage(context, summary, detail);
    if (isTextFormatted(text))
      renderFormattedText(context, text);
    else if (text != null)
      writer.writeText(text, null);
  }


  private BaseMutableUINode _writeClientMsg(
    UIXRenderingContext context,
    ResponseWriter writer,
    String summary,
    MessageWrapper msg,
    BaseMutableUINode currentChild
    ) throws IOException
  {
    String description;

    if (summary != null)
    {
      String pattern;
      String[] parameters;

      parameters = new String[] {summary};
      pattern = getTranslatedString(context, _MESSAGE_BOX_LIST_FORMAT_KEY);
      description = formatString(context, pattern, parameters);
    }
    else
    {
      description = "";
    }

    // get (or generate) the current child
    currentChild = _generateChild(msg, currentChild);
    currentChild.render(context);

    if (isTextFormatted(summary))
      renderFormattedText(context, description);
    else if (description != null)
      writer.writeText(description, null);

    return currentChild;
  }

  private BaseMutableUINode _generateChild(
    MessageWrapper   msg,
    BaseMutableUINode cachedChild)
  {
    if (cachedChild == null)
      cachedChild = new BaseMutableUINode(MARLIN_NAMESPACE, LINK_NAME);

    // link text
    String label = msg.getLabel();
    cachedChild.setAttributeValue(TEXT_ATTR, label);

    String anchor = null;
    String summary = null;

    if (label != null)
    {
      // If the text is null, no need to actually collect these values
      anchor = MessageUtils.getAnchor(msg.getId().toString());
      if (anchor != null)
        anchor = "#"+anchor;

      summary = msg.getSummary();
    }

    // However, we always have to update the anchor and description because
    // they were possibly set the last time this link was used. Even if they
    // don't render in the page, they'll be in the HTML.
    cachedChild.setAttributeValue(DESTINATION_ATTR, anchor);
    cachedChild.setAttributeValue(LONG_DESC_ATTR, summary);
    return cachedChild;
  }

  //
  // Private variables
  //

  // for now we render global and per-component messages
  private boolean _allMessages = true;

  static private final String _MESSAGE_BOX_LIST_FORMAT_KEY =
    "af_messages.LIST_FORMAT_private";

  // we need a  value map since we are using a HeaderBean.
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
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_ERROR_ICON_NAME,
                              AF_MESSAGES_ERROR_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_WARNING_ICON_NAME,
                              AF_MESSAGES_WARNING_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_INFO_ICON_NAME,
                              AF_MESSAGES_INFO_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_CONFIRMATION_ICON_NAME,
                              AF_MESSAGES_CONFIRMATION_ICON_NAME);

    // styles
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_ERROR_STYLE_CLASS,
                              AF_MESSAGES_ERROR_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(AF_PANEL_HEADER_STYLE_CLASS,
                              AF_MESSAGES_HEADER_STYLE_CLASS);
  }
}
