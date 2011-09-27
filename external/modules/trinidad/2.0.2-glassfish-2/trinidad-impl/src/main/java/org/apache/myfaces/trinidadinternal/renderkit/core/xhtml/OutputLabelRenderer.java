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
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;


/**
 * Renderer for org.apache.myfaces.trinidad.Label, family org.apache.myfaces.trinidad.Output.
 *
 * @todo Support "anchor"
 * @todo Support messageDescUrl and targetFrame
 */
public class OutputLabelRenderer extends ValueRenderer
{
  public OutputLabelRenderer()
  {
    this(CoreOutputLabel.TYPE);
  }

  protected OutputLabelRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _accessKeyKey          = type.findKey("accessKey");
    _forKey                = type.findKey("for");
    _messageTypeKey        = type.findKey("messageType");
    _showRequiredKey       = type.findKey("showRequired");
  }

  /**
   * @todo Often, we can get by with just a single label, not
   * a span and a label
   * @todo If all that is set is "required", it seems that we
   *  *don't* render a span, but do render the icon.  This is strange.
   */
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    String value = getConvertedString(context, component, bean);

    String forId = getForId(context, component, bean);
    FormData fd = rc.getFormData();
    if (fd != null)
      fd.addLabel(forId, value);

    String messageType = _getMessageType(context, component, bean, forId);

    boolean noSpanNeeded =
      ((value == null) &&
       ((messageType == null) || "none".equals(messageType)));

    if (!noSpanNeeded)
    {
      rw.startElement("span", needComponentInStartElement() ? component : null);
      renderId(context, component);
      renderAllAttributes(context, rc, component, bean);
    }

    boolean encodedIcons = encodeIcons(context, rc, component,
                                       bean, messageType, forId);

    if (value != null)
    {
      if (encodedIcons)
      {
        rw.writeText(XhtmlConstants.NBSP_STRING, null);
      }

      char accessKey;
      if (supportsAccessKeys(rc))
      {
        accessKey = getAccessKey(component, bean);
      }
      else
      {
        accessKey = CHAR_UNDEFINED;
      }

      int accessKeyIndex = AccessKeyUtils.getAccessKeyIndex(value, accessKey);

      boolean needsLabel = isLabelTagNeeded(rc, component, bean, forId, accessKeyIndex);

      if (needsLabel)
      {
        rw.startElement("label", needComponentInStartElement() ? component : null);
        if (forId != null)
        {
          rw.writeAttribute("for", forId, "for");
          // Remember this label so we don't output it twice
          HiddenLabelUtils.rememberLabel(rc, forId);
        }

        if (accessKey != CHAR_UNDEFINED)
        {
          rw.writeAttribute("accesskey",
                            Character.valueOf(accessKey),
                            "accessKey");

          // BlackBerry browsers underline the entire text instead of just
          // the accessKey character in cases where the accessKey character
          // happens to be the first character in the text. Rendering an
          // empty span element before rendering the text fixes this problem.

          Agent agent = rc.getAgent();

          if ((accessKeyIndex == 0) &&
               Agent.AGENT_BLACKBERRY.equals(agent.getAgentName()))
          {
            rw.startElement("span", null);
            // Since an empty span element is not rendered, lets
            // include some attribute to the span element
            rw.writeAttribute("style", "display:inline", null);
            rw.endElement("span");
          }
        }
      }


      AccessKeyUtils.renderAccessKeyText(context,
                                         value,
                                         accessKey,
                                         SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);

      if (needsLabel)
      {
        rw.endElement("label");
      }
    }

    if (!noSpanNeeded)
    {
      rw.endElement("span");
    }
  }

  protected boolean needComponentInStartElement()
  {
    return true;
  }

  protected boolean encodeIcons(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           messageType,
    String           forId
    ) throws IOException
  {
    boolean encodedIcons = false;
    boolean isInline = (RequestContext.getCurrentInstance().getClientValidation() ==
        RequestContext.ClientValidation.INLINE);

    if (_shouldRenderMessageSymbol(rc, messageType, isInline, forId))
    {
      String vAlign = getDefaultValign(component, bean);
      String destination = getMessageDescUrl(component, bean);
      String targetFrame = getMessageTargetFrame(component, bean);
      String anchor = MessageUtils.getAnchor(forId);

      ResponseWriter rw = context.getResponseWriter();
      if(isInline)
      {
        rw.startElement(XhtmlConstants.SPAN_ELEMENT, component);
        rw.writeAttribute(XhtmlConstants.ID_ATTRIBUTE,
            forId + "::icon", null);

        if(null == messageType || "none".equals(messageType))
        {
          messageType = XhtmlConstants.MESSAGE_TYPE_ERROR;
          rw.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
		          "display:none;", null);
        }
      }

      encodedIcons = renderMessageSymbol(context, rc, messageType,
                                         destination, anchor,
                                         targetFrame, vAlign);

      if(isInline)
        rw.endElement(XhtmlConstants.SPAN_ELEMENT);
    }

    if (getShowRequired(component, bean))
    {
      // Get the required Icon from the context
      Icon icon = rc.getIcon(SkinSelectors.REQUIRED_ICON_ALIAS_NAME);
      if (icon != null)
      {
        String vAlign = getDefaultValign(component, bean);
        _renderIcon(context, rc, icon, null, null, null, "REQUIRED_TIP", vAlign);
      }

      // Render the required icon
      encodedIcons = true;
    }

    return encodedIcons;
  }

  protected boolean isLabelTagNeeded(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           forId,
    int              accessKeyIndex
  )
  {
    return (((forId != null) &&
            !isInaccessibleMode(rc)) ||
            (accessKeyIndex >= 0));
  }

  /**
   * @todo Support targetFrame???
   */
  protected boolean renderMessageSymbol(
    FacesContext     context,
    RenderingContext rc,
    Object           type,
    Object           destination,
    Object           anchor,
    Object           targetFrame,
    Object           vAlign
    ) throws IOException
  {
    // Get the name of the Icon
    String iconName = null;
    String altTextKey = null;

    if (XhtmlConstants.MESSAGE_TYPE_ERROR.equals(type))
    {
      iconName = (destination == null) ? SkinSelectors.ERROR_ICON_ALIAS_NAME :
                                         SkinSelectors.ERROR_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "ERROR_TIP";
    }
    else if (XhtmlConstants.MESSAGE_TYPE_INFO.equals(type))
    {
      iconName = (destination == null) ? SkinSelectors.INFO_ICON_ALIAS_NAME :
                                         SkinSelectors.INFO_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "INFO_TIP";
    }
    else if (XhtmlConstants.MESSAGE_TYPE_WARNING.equals(type))
    {
      iconName = (destination == null) ? SkinSelectors.WARNING_ICON_ALIAS_NAME :
                                         SkinSelectors.WARNING_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "WARNING_TIP";
    }

    if (iconName != null)
    {
      // Get the Icon to render from the skin
      Icon icon = rc.getIcon(iconName);

      if (icon != null)
      {
        _renderIcon(context,
                    rc,
                    icon,
                    destination,
                    anchor,
                    targetFrame,
                    altTextKey,
                    vAlign);

        return true;
      }
    }

    return false;
  }

  //
  // OVERRIDES
  //
  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|outputLabel";
  }

  private void _renderIcon(
    FacesContext     context,
    RenderingContext rc,
    Icon             icon,
    Object           destination,
    Object           anchor,
    Object           targetFrame,
    String           altTextKey,
    Object           vAlign
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    boolean renderedAnchor = false;

    if ((destination != null) || (anchor != null))
    {
      if (supportsNavigation(rc))
      {
        writer.startElement("a", null);
        renderEncodedActionURI(context, "href", destination);
        writer.writeAttribute("target", targetFrame, null);
        writer.writeAttribute("name", anchor, null);

        // Set renderedAnchor to true so that we know that
        // we need to close the anchor element.
        renderedAnchor = true;
      }
    }

    // Get ready to render the Icon.  We need to get the
    // alt text, and also check to see whether the Icon
    // should render the style class
    Object altText = rc.getTranslatedString(altTextKey);

    // Apply the default alignment
    if (vAlign == null)
      vAlign = OutputUtils.getMiddleIconAlignment(rc);

    // Render the icon, specifying embedded=renderedAnchor.
    // This allows text-based Icons to render their style class
    // and altText directly on the anchor itself
    OutputUtils.renderIcon(context,
                           rc,
                           icon,
                           altText,
                           vAlign,
                           renderedAnchor);

    // Close up the anchor if necessary
    if (renderedAnchor)
      writer.endElement("a");
  }

  private String _getMessageType(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean,
    String       forId
    ) throws IOException
  {
    // Derive the message type
    String messageType = getMessageType(component, bean);
    if (null == messageType)
    {
      FacesMessage message = MessageUtils.getFacesMessage(context, forId);
      if (message != null)
      {
        messageType = MessageUtils.getMessageTypeFromSeverity(
                        message.getSeverity());
      }
    }

    return messageType;
  }

  //
  // NEW HOOKS
  //

  protected boolean getShowRequired(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_showRequiredKey);
    if (o == null)
      o = _showRequiredKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  /**
   * we default the valign. the user can use skinning to override.
   */
  protected String getDefaultValign(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  protected char getAccessKey(
    UIComponent component,
    FacesBean   bean)
  {
    return toChar(bean.getProperty(_accessKeyKey));
  }

  protected String getFor(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_forKey));
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

  protected String getMessageDescUrl(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  protected String getMessageTargetFrame(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  private boolean _shouldRenderMessageSymbol(
    RenderingContext rc,
    String           messageType,
    boolean          isInline,
    String           forId)
  {
    // BlackBerry does not support inline style "display:none".
    // BlackBerry supports some inline styles and thus test by
    // calling XhtmlRenderer.supportsStyleAttributes() is not
    // sufficient.
    // If rendering for BlackBerry and the span has inline style
    // "display:none", do not render the span. If rendered, the
    // message icon is always visible.
    // If rendering for non-BlackBerry OR rendering BlackBerry
    // but the condition
    // (null == messageType || "none".equals(messageType))
    // does not meet, return true, so that caller renders the
    // span element.
    if ((null != messageType) &&
        !"none".equals(messageType) ||
        ((forId != null) &&
        isInline))
    {
      Agent agent = rc.getAgent();

      if ((agent != null) &&
            (Agent.AGENT_BLACKBERRY.equals(agent.getAgentName()) ||
             Agent.AGENT_GENERICPDA.equals(agent.getAgentName())) &&
          (null == messageType || "none".equals(messageType)))
      {
        return false;
      }
      return true;
    }
    return false;
  }

  private PropertyKey _accessKeyKey;
  private PropertyKey _forKey;
  private PropertyKey _messageTypeKey;
  private PropertyKey _showRequiredKey;
}
