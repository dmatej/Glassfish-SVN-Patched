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
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RenderingContext;


abstract public class FormElementRenderer extends EditableValueRenderer
{
  protected FormElementRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _autoSubmitKey = type.findKey("autoSubmit");
    _onblurKey  = type.findKey("onblur");
    _onfocusKey = type.findKey("onfocus");
    _onchangeKey = type.findKey("onchange");
    _labelKey = type.findKey("label");
    _contentStyleKey = type.findKey("contentStyle");
  }


  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    if (_autoSubmitKey != null)
      detectAutoSubmit(context, component, clientId);

    return super.getSubmittedValue(context,
                                   component,
                                   clientId);
  }

  @SuppressWarnings("unchecked")
  protected final void detectAutoSubmit(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
      Map<String, String> parameterMap =
        context.getExternalContext().getRequestParameterMap();

      String source = parameterMap.get("source");
      if (clientId.equals(source))
      {
        String event = parameterMap.get("event");
        if (XhtmlConstants.AUTOSUBMIT_EVENT.equals(event) &&
            isAutoSubmit(component, getFacesBean(component)))
        {
          (new AutoSubmitEvent(component)).queue();
        }
      }
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!renderAsElement(context, rc, component, bean))
    {
      encodeAllAsNonElement(context, rc, component, bean);
    }
    else
    {
      encodeAllAsElement(context, rc, component, bean);
    }
  }

  protected boolean isHiddenLabelRequired(
    RenderingContext rc)
  {
    return true;
  }

  /**
   */
  protected void renderShortDescAsHiddenLabel(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (HiddenLabelUtils.supportsHiddenLabels(rc) &&
        isHiddenLabelRequired(rc))
    {
      String clientId = getClientId(context, component);
      if (HiddenLabelUtils.wantsHiddenLabel(rc, clientId))
      {
        String hiddenLabel = getHiddenLabel(component, bean);
        if (hiddenLabel != null)
        {
          HiddenLabelUtils.outputHiddenLabel(context,
                                             rc,
                                             clientId,
                                             hiddenLabel,
                                             component);
        }
      }
    }
  }

  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
  }

  /**
   * @todo Make abstract if always overriden
   */
  protected void encodeAllAsNonElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", component);

    renderId(context, component);
    rw.writeAttribute("title", getShortDesc(component, bean), "shortDesc");
    renderStyleAttributes(context, rc, component, bean);

    renderNonElementContent(context, rc, component, bean);
    rw.endElement("span");
  }

  /**
   * @todo Remove if never necessary
   */
  protected void renderNonElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
  }

  @Override
  protected void renderEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    super.renderEventHandlers(context, component, bean);
    renderFormEventHandlers(context, component, bean);
  }

  protected void renderFormEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.writeAttribute("onblur", getOnblur(component, bean),  "onblur");
    rw.writeAttribute("onfocus", getOnfocus(component, bean),  "onfocus");
    rw.writeAttribute("onchange", getOnchange(component, bean),  "onchange");
  }

  /**
   * Should this component render as a form element, or just
   * as some non-form content?
   */
  protected final boolean renderAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    if (getReadOnly(context, component, bean) || !supportsEditing(rc))
    {
      if (!renderReadOnlyAsElement(rc, component, bean))
        return false;
    }

    if (!supportsDisabledFormElements(rc) &&
        getDisabled(component, bean))
    {
      return false;
    }

    return true;
  }

  protected boolean renderReadOnlyAsElement(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    return false;
  }

  protected String getLabel(
    UIComponent component,
    FacesBean   bean)
  {
    // Not all FormElements necessarily have a label
    if (_labelKey == null)
      return null;

    return toString(bean.getProperty(_labelKey));
  }

  protected String getContentStyle(
    UIComponent component,
    FacesBean   bean)
  {
    if (_contentStyleKey == null)
      return null;

    return toString(bean.getProperty(_contentStyleKey));
  }

  protected String getOnblur(
    UIComponent component,
    FacesBean   bean)
  {
    if (_onblurKey == null)
      return null;

    return XhtmlUtils.getClientEventHandler(FacesContext.getCurrentInstance(), component,
             "blur", null, toString(bean.getProperty(_onblurKey)), null);
  }

  protected String getOnfocus(
    UIComponent component,
    FacesBean   bean)
  {
    if (_onfocusKey == null)
      return null;

    return XhtmlUtils.getClientEventHandler(FacesContext.getCurrentInstance(), component,
             "focus", null, toString(bean.getProperty(_onfocusKey)), null);
  }

  protected String getOnchange(
    UIComponent component,
    FacesBean   bean)
  {
    if (_onchangeKey == null)
      return null;

    return XhtmlUtils.getClientEventHandler(FacesContext.getCurrentInstance(), component,
             "change", "valueChange", toString(bean.getProperty(_onchangeKey)), null);
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS;
  }

  /* FIXME: this method is never called
  protected String getDefaultDisabledStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_FIELD_TEXT_DISABLED_STYLE_CLASS;
  }*/

  /**
   * Tells whether or not the autoSubmit attribute is set on the bean
   *
   * @param bean the bean
   */
  protected boolean isAutoSubmit(
    UIComponent component,
    FacesBean   bean)
  {
    if (_autoSubmitKey == null)
      return false;

    return Boolean.TRUE.equals(bean.getProperty(_autoSubmitKey));
  }

  /**
   * Return the text for a hidden label, using "shortDesc" if set,
   * "label" otherwise.
   */
  protected String getHiddenLabel(
    UIComponent component,
    FacesBean   bean)
  {
    String hiddenLabel = getShortDesc(component, bean);
    if (hiddenLabel == null)
      hiddenLabel = getLabel(component, bean);

    return hiddenLabel;
  }


  /**
   * Dummy class purely to get subforms to recognize that
   * an event has occurred
   */
  static private final class AutoSubmitEvent extends FacesEvent
  {
    public AutoSubmitEvent(
      UIComponent source)
    {
      super(source);
      setPhaseId(PhaseId.INVOKE_APPLICATION);
    }

    @Override
    public void processListener(
      FacesListener listener)
    {
    }

    @Override
    public boolean isAppropriateListener(
      FacesListener listener)
    {
      return false;
    }

    private static final long serialVersionUID = 1L;
  }

  private PropertyKey _autoSubmitKey;
  private PropertyKey _labelKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _onblurKey;
  private PropertyKey _onfocusKey;
  private PropertyKey _onchangeKey;
}
