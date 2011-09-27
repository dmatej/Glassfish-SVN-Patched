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

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreInputListOfValues;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.agent.AgentUtil;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;


/**
 */
public class SimpleInputListOfValuesRenderer extends SimpleInputTextRenderer
{
  public SimpleInputListOfValuesRenderer()
  {
    this(CoreInputListOfValues.TYPE);
  }

  public SimpleInputListOfValuesRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _searchDescKey = type.findKey("searchDesc");
    _iconKey = type.findKey("icon");
    _actionExpressionKey = type.findKey("actionExpression");
  }

  //
  // DECODE BEHAVIOR
  //

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    super.decode(facesContext, component, facesBean, clientId);

    RequestContext afContext = RequestContext.getCurrentInstance();
    // See if a ReturnEvent is waiting for us.  We don't deliver
    // the ReturnEvent - we just use its value
    ReturnEvent returnEvent =
      afContext.getDialogService().getReturnEvent(component);
    if (returnEvent != null)
    {
      afContext.addPartialTarget(component);
      queueReturnEvent(facesContext, component, returnEvent);
    }
    else
    {
      Map<String, String> parameterMap =
        facesContext.getExternalContext().getRequestParameterMap();

      Object source = parameterMap.get("source");
      clientId = clientId == null ? component.getClientId(facesContext) : clientId;
      if ((source != null) && source.equals(clientId))
      {
        Object part = parameterMap.get(_PART_PARAMETER);
        if (_BUTTON_PART.equals(part))
        {
          // Force partial rendering (if we're launching a window)
          // =-=AEW I don't believe this is necessary;  I believe
          // we've already got "partial" turned on
          TrinidadAgent agent = AgentUtil.getAgent(facesContext);
          if (XhtmlUtils.supportsSeparateWindow(agent))
            PartialPageUtils.forcePartialRendering(facesContext);

          queueActionEvent(facesContext, component);
        }
        // else ???
      }
    }
  }

  /**
   * Give subclasses a chance to override the ReturnEvent.
   */
  protected void queueReturnEvent(
    FacesContext context,
    UIComponent  component,
    ReturnEvent  event)
  {
    event.queue();
  }

  protected void queueActionEvent(
    FacesContext context,
    UIComponent  component)
  {
    (new ActionEvent(component)).queue();
  }

  //
  // Encode behavior
  //

  @Override
  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    boolean simple = getSimple(component, bean);
    if (simple)
    {
      rw.startElement("span", component);
      // put the outer style class here, like af_selectManyRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, rc, component, bean);
    }
    // =-=AEW Write out an ID???
    renderTextField(context, rc, component, bean);
    renderAfterTextField(context, rc, component, bean);
    if (simple)
      rw.endElement("span");
  }

  @Override
  protected void encodeAllAsNonElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    boolean simple = getSimple(component, bean);
    if (simple)
    {
      rw.startElement("span", component);
      // put the outer style class here, like af_selectManyRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, rc, component, bean);
    }
    super.encodeAllAsNonElement(context, rc, component, bean);
    if (simple)
      rw.endElement("span");
  }

  /*
   * This is called from our super class to determine if we need to render
   * the span and root dom element styles on the text field, which we
   * don't, since we do it ourselves on our root dom element.
   */
  @Override
  protected boolean isSimpleInputText(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  protected void renderTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    super.encodeAllAsElement(context, rc, component, bean);
  }

  protected void renderAfterTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!getDisabled(component, bean))
    {
      // =-=AEW TODO: Make spacer a property?
      renderSpacer(context, rc, "8", "1");

      renderIcon(context, rc, component, bean);
    }
  }

  protected void renderIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String iconUri = getIcon(component, bean);
    Icon icon;

    if (iconUri == null)
    {
      icon = rc.getIcon(getButtonIconName());
      if ((icon == null) || icon.isNull())
        return;
    }
    else
    {
      icon = null;
    }

    String onclick = getLaunchOnclick(context,
                                      rc,
                                      component,
                                      bean);


    String buttonOnclick = getButtonOnclick(component, bean);
    if (buttonOnclick != null)
    {
      onclick = XhtmlUtils.getChainedJS(buttonOnclick, onclick, true);
    }

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("a", component);
    rw.writeURIAttribute("href", "#", null);
    rw.writeAttribute("onclick", onclick, null);

    String align = OutputUtils.getMiddleIconAlignment(rc);
    String title = getSearchDesc(component, bean);
    if (iconUri != null)
    {
      rw.startElement("img", null);
      rw.writeAttribute("border", "0", null);
      renderEncodedResourceURI(context, "src", iconUri);
      OutputUtils.renderAltAndTooltipForImage(context, rc, title);
      rw.writeAttribute("align", align, null);
      rw.endElement("img");
    }
    else
    {
      OutputUtils.renderIcon(context, rc, icon,
                             title, align);
    }

    rw.endElement("a");

  }

  protected String getButtonIconName()
  {
    return SkinSelectors.AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME;
  }

  protected String getButtonOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    return super.getOnclick(component, bean);
  }

  /**
   * Must be called <em>before</em> starting an element!!!
   */
  protected String getLaunchOnclick(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    FormData fd = rc.getFormData();

    if (fd == null)
      return null;

    fd.addNeededValue("part");

    // this is added for bug 4482982; when the selectInputDate or
    // selectInputText icon is selected in PocketIE, the script requires
    // the source hidden element to exist
    fd.addNeededValue(XhtmlConstants.SOURCE_PARAM);

    // Use a PPR autosubmit to launch the dialog only if we know
    // that we can use a separate window;  otherwise, just
    // use an ordinary request (since we're going to have to
    // refresh the whole window anyway)
    if (supportsSeparateWindow(rc))
    {
      AutoSubmitUtils.writeDependencies(context, rc);
      return AutoSubmitUtils.getSubmitScript(
                  rc,
                  getClientId(context, component),
                  true,
                  false,
                  null, //event
                  _BUTTON_AND_PART_PARAMETER,
                  false);
    }
    else
    {
      return AutoSubmitUtils.getFullPageSubmitScript(
                  rc,
                  getClientId(context, component),
                  true,
                  null, //event
                  _BUTTON_AND_PART_PARAMETER,
                  false);
    }
  }

  @Override
  public boolean isTextArea(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected boolean getSecret(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  /**
   * We want onclick to move from the input field to the button;
   * @see #getButtonOnclick
   */
  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    if (shouldRenderInputOnclick())
    return null;
    else
      return super.getOnclick(component, bean);
  }

  protected boolean shouldRenderInputOnclick()
  {
    return true;
  }

  protected Object getActionExpression(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_actionExpressionKey);
  }

  protected String getSearchDesc(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_searchDescKey));
  }

  protected String getIcon(
    UIComponent component,
    FacesBean   bean)
  {
    // Support subclasses without support for overriding the icon
    if (_iconKey == null)
      return null;

    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputListOfValues";
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputListOfValues::content";
  }

  private PropertyKey _actionExpressionKey;
  private PropertyKey _iconKey;
  private PropertyKey _searchDescKey;

  static private final String _BUTTON_PART = "b";
  static private final String _PART_PARAMETER = "part";
  static private final String _BUTTON_AND_PART_PARAMETER =
    _PART_PARAMETER + ":'" + _BUTTON_PART + "'";
}