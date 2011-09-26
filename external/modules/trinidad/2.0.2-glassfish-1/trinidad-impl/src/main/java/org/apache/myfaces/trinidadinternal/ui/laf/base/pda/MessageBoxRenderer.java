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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import java.util.Iterator;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.core.output.CoreMessages;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.MessageBoxUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.MessageWrapper;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;



/**
 * Renders a message box.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/MessageBoxRenderer.java#0 $) $Date: 10-nov-2005.18:54:58 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class MessageBoxRenderer extends XhtmlLafRenderer
{
  // check for number of links at pre-, post-, and content.
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    //PH:set whether or not only global messages be rendererd depending on
    //the globalOnly attribute's value
    
    if (MessageBoxUtils.sIsRendered(context, node, 
                        !Boolean.TRUE.equals(getAttributeValue(context, node,
                                                    GLOBAL_ONLY_ATTR, null))))
    {
      super.prerender(context, node);
      context.setLocalProperty( _MB_IS_RENDERED, Boolean.TRUE);
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object isRendered = context.getLocalProperty( 0, _MB_IS_RENDERED, null);
    if ( Boolean.TRUE.equals(isRendered) )
      super.postrender(context, node);
  }


  //
  // do the rendering work
  //
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object isRendered = context.getLocalProperty( 0, _MB_IS_RENDERED, null);
    if ( Boolean.TRUE.equals(isRendered) )
    {
      int messageType
        = _getMessageTypeBySeverity(MessageBoxUtils.sGetMaxSeverity(context));

      Icon icon  = _getIcon(context, messageType);

      String messageStyle = _getMessageTextStyle(messageType);
      Object message = node.getAttributeValue(context, MESSAGE_ATTR);

      _renderLine(context,
                  node,
                  messageStyle,
                  icon,
                  messageType,
                  message);
    }


  }

  //PH:No reason for _renderLine to be static
  private final void _renderLine(
    UIXRenderingContext context,
    UINode           node,
    String           messageStyle,
    Icon             icon,
    int              messageType,
    Object           message
    )throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();

    MarlinBean text = new MarlinBean(STYLED_TEXT_NAME);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    FacesContext fContext = context.getFacesContext();
    if (icon != null)
    {
      icon.renderIcon(fContext, arc, null);
      writer.writeText(NBSP_STRING, null);
    }

    String messageKey = _MESSAGE_TYPE_KEYS[messageType];

    text.setStyleClass(messageStyle);
    text.setAttributeValue(TEXT_ATTR, getTranslatedString(context, messageKey) );

    //PH: Create BODY Styling for the entire MessageBox.
    writer.startElement("table", null);
    renderStyleClassAttribute(context, "af|messages::body");
    writer.startElement("tr", null);
    writer.startElement("td", null);


    writer.startElement("b", node.getUIComponent());
    text.render(context);

    if (message != null)
    {
      writer.startElement("br", null);
      writer.endElement("br");

      writer.writeText( message, CoreMessages.MESSAGE_KEY.getName());
    }

    writer.endElement("b");
    //PH:Do not render a separator. User may not necessary place a message
    //component at the top of the page. Moreover, styling should not be 
    //hard-coded like this.Application developer should have all the control 
    //over styling. I am using a Background instead of a separator to make a 
    //message box distinct from the rest of the content in the application page.
    //sep.render(context, node);

    //PH:start rendering global and per-component messages
    boolean useList = MessageBoxUtils.sMultipleMessages(context, 
                           !Boolean.TRUE.equals(getAttributeValue(context, node,
                                                    GLOBAL_ONLY_ATTR, null))
    );

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

    // oracle pda's renderer allows different list styles depending
    // upon if it is a list or a div, and whether it is an error or not.
    // should we do the same here? For now, don't worry about it.
    renderStyleClassAttribute(context, "af|messages::list");

    // Do the global messages first
    _renderMessages(context, writer, true, useList);

    // Then client component messages
    if (!Boolean.TRUE.equals(getAttributeValue(context, node,
                                                    GLOBAL_ONLY_ATTR, null)))
      _renderMessages(context, writer, false, useList);

    if (useList)
      writer.endElement("ol");
    else
    {
      writer.endElement("div");
    }

    writer.endElement("td");
    writer.endElement("tr");
    writer.endElement("table");
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
      pattern = getTranslatedString(context, "af_messages.LIST_FORMAT_private");
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

  private String _getMessageTextStyle(
    int messageType
    )
  {
    return (messageType == _ERROR_TYPE)
             ? AF_MESSAGES_ERROR_STYLE_CLASS
             : AF_MESSAGES_HEADER_STYLE_CLASS;
  }


  //
  // Private methods
  //

  private int _getMessageTypeBySeverity(FacesMessage.Severity severity)
  {
    if (FacesMessage.SEVERITY_ERROR.compareTo(severity) == 0)
      return _ERROR_TYPE;
    else if (FacesMessage.SEVERITY_WARN.compareTo(severity) == 0)
      return _WARNING_TYPE;
    else if (FacesMessage.SEVERITY_INFO.compareTo(severity) == 0)
      return _INFORMATION_TYPE;
    else return _CONFIRMATION_TYPE;
  }

  private Icon _getIcon(
    UIXRenderingContext context,
    int              messageType)
  {
    return context.getIcon(_getIconName(messageType));
  }

  private String _getIconName(
    int messageType
    )
  {
    return _ICON_NAMES[messageType];
  }

  // images
  private final static String[] _ICON_NAMES =
  {
    AF_MESSAGES_INFO_ICON_NAME,
    AF_MESSAGES_WARNING_ICON_NAME,
    AF_MESSAGES_ERROR_ICON_NAME,
    AF_MESSAGES_CONFIRMATION_ICON_NAME,
  };

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }


  //
  // Private variables
  //

  // local message types
  static private final int _INFORMATION_TYPE  = 0;
  static private final int _WARNING_TYPE      = 1;
  static private final int _ERROR_TYPE        = 2;
  static private final int _CONFIRMATION_TYPE = 3;

  // text keys
  static private final String _INFORMATION_KEY  = "af_messages.INFORMATION";
  static private final String _WARNING_KEY      = "af_messages.WARNING";
  static private final String _ERROR_KEY        = "af_messages.ERROR";
  static private final String _CONFIRMATION_KEY = "af_messages.CONFIRMATION";

  // message type
  private final static String[] _MESSAGE_TYPE_KEYS =
  {
    _INFORMATION_KEY,
    _WARNING_KEY,
    _ERROR_KEY,
    _CONFIRMATION_KEY
  };


  private static final Object _MB_IS_RENDERED = new Object();

}
