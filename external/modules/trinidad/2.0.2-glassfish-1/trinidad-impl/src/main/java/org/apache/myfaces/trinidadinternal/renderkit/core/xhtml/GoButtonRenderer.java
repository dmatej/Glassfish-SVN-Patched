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

import java.util.Collections;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.nav.CoreGoButton;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.StringUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;


/**
 * FIXME: the inheritance hierarchy is a bit annoying:  should
 * we extend CommandButtonRenderer or GoLinkRenderer?  Either
 * way, there's a fair bit of duplicated code
 */
public class GoButtonRenderer extends GoLinkRenderer
{
  public GoButtonRenderer()
  {
    this(CoreGoButton.TYPE);
  }

  protected GoButtonRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _iconKey = type.findKey("icon");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String clientId = component.getClientId(context);
    if (canSkipRendering(rc, clientId))
      return;

    // Make sure we don't have anything to save
    assert(rc.getCurrentClientId() == null);
    rc.setCurrentClientId(clientId);

    String element;
    boolean useButton = false;
    boolean useInput = false;
    boolean supportScriptEvents = false;
    boolean imageLink = false;
    boolean iconAvailable = false;
    String icon = getIcon(component, bean);

    if (icon != null)
    {
      iconAvailable = true;
    }
    if ((supportsScripting(rc) && supportsIntrinsicEvents(rc)))
    {
      supportScriptEvents = true;
    }

    if (supportScriptEvents)
    {
      if (supportsAdvancedForms(rc))
      {
        element = XhtmlLafConstants.BUTTON_ELEMENT;
        useButton = true;
      }
      //if icon is set, render as an image element within a link element
      //since "buttons" html element is not supported and "input" element of
      //type=image does not support "onClick" JS handler.
      else if (iconAvailable && !supportsOnClickOnImgInput(rc))
      {
        element = XhtmlLafConstants.LINK_ELEMENT;
        imageLink = true;
      }
      else
      {
        element = XhtmlLafConstants.INPUT_ELEMENT;
        useInput = true;
      }
    }
    else
    {
      element = XhtmlLafConstants.LINK_ELEMENT;
    }

    ResponseWriter rw = context.getResponseWriter();
    boolean disabled = getDisabled(component, bean);
    rw.startElement(element, component);
    renderId(context, component);

    if (supportScriptEvents)
    {
      if (useInput && iconAvailable)
      {
        rw.writeAttribute("type", "image", null);
      }
      //For any element like <button> or <input> except <a> set type to "button"
      else if (!imageLink)
      {
        rw.writeAttribute("type", "button", null);
      }
      // If disabled, render "disable" only for <input> and <button> elements
      if (!imageLink && disabled)
      {
        rw.writeAttribute("disabled", Boolean.TRUE, "disabled");
      }
    }

    if (disabled || !supportsNavigation(rc))
    {
      // Skip over event attributes when disabled
      renderStyleAttributes(context, rc, component, bean);
    }
    else
    {
      renderAllAttributes(context, rc, component, bean);
      if (supportScriptEvents)
      {
        rw.writeAttribute("onclick", getButtonOnclick(component, bean), null);
        if (imageLink)
        {
          renderEncodedActionURI(context, XhtmlConstants.HREF_ATTRIBUTE, "#");
        }
      }
      else
      {
        renderEncodedActionURI(context, XhtmlConstants.HREF_ATTRIBUTE,
          getDestination(component, bean));

        if (supportsTarget(rc))
        {
          rw.writeAttribute("target", getTargetFrame(component, bean), null);
        }
      }
    }

    // Write the text and access key

    char accessKey;
    if (supportsAccessKeys(rc))
    {
      accessKey = getAccessKey(component, bean);
      if (accessKey != CHAR_UNDEFINED)
      {
        rw.writeAttribute("accesskey",
                          Character.valueOf(accessKey),
                          "accessKey");
      }
    }
    else
    {
      accessKey = CHAR_UNDEFINED;
    }

    String text = getText(component, bean);

    if (useButton)
    {

      AccessKeyUtils.renderAccessKeyText(context,
                                         text,
                                         accessKey,
                                         SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      if (icon != null)
        OutputUtils.renderImage(context, rc, icon, null, null, null,
                                  getShortDesc(component, bean));
    }
    // For PDAs, render only the image if icon is available
    else if (!supportScriptEvents)
    {
      if(iconAvailable)
      {

       OutputUtils.renderImage(context, rc, icon, null, null, null,
                                  getShortDesc(component, bean));
      }
      else
      {

       AccessKeyUtils.renderAccessKeyText(context,
                                         text,
                                         accessKey,
                                         SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      }
    }
    else
    {
      // Render an image tag inside the anchor tag
      if (imageLink)
      {
        OutputUtils.renderImage(context, rc, icon, null, null, null,
                                getShortDesc(component, bean));
      }
      // For input element render src attribute to the url of the icon
      else if (iconAvailable)
      {
        renderEncodedResourceURI(context, "src", icon);
      }
      else
      {
        rw.writeAttribute("value", text, "text");
      }
    }

    rw.endElement(element);
    rc.setCurrentClientId(null);
  }

  /**
   * Override to return any state-based (selected, disabled, etc.)
   * CSS style markers.  HINT: use an immutable, cached List&lt;String>
   * for better performance.
   */
  protected List<String> getStateStyleClasses(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    if (getDisabled(component, bean))
      return _DISABLED_STATE_LIST;
    return null;
  }

  // FIXME: move this implementation to XhtmlRenderer
  @Override
  protected void renderStyleAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           defaultStyleClass
    ) throws IOException
  {
    String styleClass = getStyleClass(component, bean);
    // -= Simon =-
    // FIXME: How come inlineStyle is never read
    //String inlineStyle = getInlineStyle(bean);
    List<String> stateStyleClasses = getStateStyleClasses(context, rc, component, bean);

    if ((styleClass==null) &&
        (defaultStyleClass != null) &&
        (stateStyleClasses == null))
    {
      renderStyleClass(context, rc, defaultStyleClass);
    }
    else
    {
      int numStates =   ((stateStyleClasses != null) ?
                         stateStyleClasses.size() : 0);
      int numClasses = ((styleClass != null) ? 1 : 0) +
                        ((defaultStyleClass != null) ? 1 : 0) +
                        numStates;
      if (numClasses > 0)
      {
        // set all the styleClasses in one array so we can pass it to
        // renderStyleClasses
        String[] styleClasses = new String[numClasses];

        int i=0;
        if (styleClass != null)
          styleClasses[i++] = styleClass;
        if (defaultStyleClass != null)
          styleClasses[i++] = defaultStyleClass;

        for (int j=0; j < numStates; j++, i++)
        {
          styleClasses[i] = stateStyleClasses.get(j);
        }

        renderStyleClasses(context, rc, styleClasses);
      }
    }

    String style = getInlineStyle(component, bean);
    if (style != null)
    {
      context.getResponseWriter().writeAttribute("style",
                                                 style,
                                                 "inlineStyle");
    }
  }

  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  protected String getButtonOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    String base = super.getOnclick(component, bean);
    String destination = getDestination(component, bean);
    if (destination == null)
      return base;

    destination = FacesContext.getCurrentInstance().
      getExternalContext().encodeActionURL(destination);
    String onclickJS = null;

    if ((destination.length()) > 11 &&
        "javascript:".equalsIgnoreCase(destination.substring(0,11)))
    {
      onclickJS = destination.substring(11);
    }
    else
    {
      // Escape the destination in case there's any quotes
      destination = StringUtils.replace(destination, "'", "\\'");

      String targetFrame = getTargetFrame(component, bean);
      // Look for target frames with well-known names, like
      // _self, _top, _parent, _blank, and _new.  (_new
      // is actually non-standard, but often used for _blank)
      if (targetFrame != null && !"_self".equals(targetFrame))
      {
        if ("_top".equals(targetFrame))
        {
          onclickJS = "top.location='" + destination + "'";
        }
        else if ("_parent".equals(targetFrame))
        {
          onclickJS = "parent.location='" + destination + "'";
        }
        else if ("_blank".equals(targetFrame) || "_new".equals(targetFrame))
        {
          onclickJS = "window.open('" + destination + "')";
        }
        else
        {
          onclickJS = "top["         +
            targetFrame    +
            "].location='" +
            destination    +
            "'";
        }
      }
      else
      {
        onclickJS = "document.location='" + destination + "'";
      }
    }

    return XhtmlUtils.getChainedJS(base, onclickJS, true);
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_GO_BUTTON_STYLE_CLASS;
  }

  protected String getIcon(
    UIComponent component,
    FacesBean   bean)
  {
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
  }

  private PropertyKey _iconKey;

  static private final List<String> _DISABLED_STATE_LIST =
    Collections.singletonList(SkinSelectors.STATE_DISABLED);
}
