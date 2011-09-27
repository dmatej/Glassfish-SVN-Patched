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

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreMessage;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;


/**
 * Renderer for org.apache.myfaces.trinidad.Message, family org.apache.myfaces.trinidad.Message.
 *
 */
public class MessageRenderer extends ValueRenderer
{
  public MessageRenderer()
  {
    this(CoreMessage.TYPE);
  }

  protected MessageRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _forKey         = type.findKey("for");
    _messageTypeKey = type.findKey("messageType");
    _messageKey     = type.findKey("message");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected String getInlineStyle(
    UIComponent component,
    FacesBean   bean)
  {
    String beanInlineStyle = super.getInlineStyle(component, bean);
    String inlineStyle = null;

    if (getIndented(component, bean))
    {
      inlineStyle = (RenderingContext.getCurrentInstance().isRightToLeft()
          ? _sRTL_INDENTED_STYLE
              : _sLTR_INDENTED_STYLE);
    }

    // if neither of these are null, then combine them and render
   if (inlineStyle != null && beanInlineStyle != null)
    {
      StringBuffer buffer = new StringBuffer(inlineStyle.length() +
                                             beanInlineStyle.length() +
                                             1);
      buffer.append(inlineStyle);
      if (!inlineStyle.endsWith(";"))
        buffer.append(";");
      buffer.append(beanInlineStyle);
       return buffer.toString();
    }
    else if (inlineStyle != null)
      return inlineStyle;
    else
      return beanInlineStyle;
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

    String message = getMessage(component, bean);
    String messageType = getMessageType(component, bean);

    String forId = getForId(context, component, bean);
    if ((message == null) || (messageType == null))
    {
      FacesMessage facesMessage = MessageUtils.getFacesMessage(context, forId);
      if (facesMessage != null)
      {
        if (message == null)
          message = facesMessage.getDetail();
        if (messageType == null)
          messageType = MessageUtils.getMessageTypeFromSeverity(
                        facesMessage.getSeverity());
      }
    }

    UIComponent help = getFacet(component, "help");

    boolean isError = true;
    if (messageType != null)
      isError = CoreMessage.MESSAGE_TYPE_ERROR.equals(messageType);

    boolean hasHelp = (help != null);
    boolean hasMessage = (message != null);

    RequestContext requestContext = RequestContext.getCurrentInstance();
    boolean isInline = (requestContext.getClientValidation() ==
                        RequestContext.ClientValidation.INLINE);

    // Handle rendering the help text
    if (hasHelp)
    {
      // Write the root level element for the help
      writer.startElement(XhtmlConstants.SPAN_ELEMENT, component);
      if (shouldRenderId(context, component))
        writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE,
            forId + "::help", null);

      renderAllAttributes(context, rc, component, bean, false);
      renderStyleAttributes(context, rc, component, bean,
        SkinSelectors.INLINE_INFO_TEXT_STYLE_CLASS);

      encodeChild(context, help);

      if (hasMessage || isInline)
      {
        // We'll need a break between the help and message
        // Do it here, so it gets inserted if hasHelp
        writer.startElement("br", null);
        writer.endElement("br");
      }

      writer.endElement(XhtmlConstants.SPAN_ELEMENT);

    }

    // Handle rendering the message text (or the empty placeholder)
    // Skip if there is no message for PDA
    if (hasMessage || (isInline && !isPDA(rc)))
    {

      // Write the root level element for the help
      writer.startElement(XhtmlConstants.SPAN_ELEMENT, component);

      if (shouldRenderId(context, component) || (isInline && forId != null))
        writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE,
            forId + "::msg", null);

      renderAllAttributes(context, rc, component, bean, false);

      if (hasMessage)
      {
        // Output the server-side message and it's style attributes
        renderStyleAttributes(context, rc, component, bean, isError ?
            SkinSelectors.INLINE_ERROR_TEXT_STYLE_CLASS :
                SkinSelectors.INLINE_INFO_TEXT_STYLE_CLASS);          
        renderPossiblyFormattedText(context, message);
      }
      else
      {
        // Hide element ready for client-side validation
        writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
            "display:none;", null);
      }

      writer.endElement(XhtmlConstants.SPAN_ELEMENT);
    }

  }

  //
  // NEW HOOKS
  //

  protected boolean getIndented(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  protected String getFor(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_forKey));
  }

  protected String getMessageType(
    UIComponent component,
    FacesBean   bean)
  {
    // We're used in some composite circumstances where
    // the message type is always derived from the presence
    // of a message, and cannot be overridden
    if (_messageTypeKey == null)
      return null;

    return toString(bean.getProperty(_messageTypeKey));
  }

  protected String getMessage(
    UIComponent component,
    FacesBean   bean)
  {
    // Ditto.
    if (_messageKey == null)
      return null;

    return toString(bean.getProperty(_messageKey));
  }


  protected String getForId(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    String forValue = getFor(component, bean);
    if (forValue == null)
      return null;

    return MessageUtils.getClientIdFor(context,
                                       component,
                                       forValue);
  }

  private PropertyKey _forKey;
  private PropertyKey _messageTypeKey;
  private PropertyKey _messageKey;

  static private final String _sLTR_INDENTED_STYLE = "margin-left:21px";
  static private final String _sRTL_INDENTED_STYLE = "margin-right:21px";
}
