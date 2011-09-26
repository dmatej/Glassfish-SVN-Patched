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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectBooleanRadio;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 */
public class SimpleSelectBooleanRadioRenderer extends SimpleSelectBooleanRenderer
{
  public SimpleSelectBooleanRadioRenderer()
  {
    this(CoreSelectBooleanRadio.TYPE);
  }

  public SimpleSelectBooleanRadioRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _groupKey = type.findKey("group");
  }

  //**********************
  //decode
  //**********************

  @Override
  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    // Since we override getSubmittedValue() entirely,
    // detect auto submit manually
    detectAutoSubmit(context, component, clientId);

    String group = getGroup(getFacesBean(component));
    if (group != null)
    {
      Object newValue = (context.getExternalContext().
                         getRequestParameterMap().get(group));
      if (clientId.equals(newValue))
        return Boolean.TRUE;
    }

    return Boolean.FALSE;
  }

  //**********************
  //encode
  //**********************

  @Override
  protected Object getValueAttr(
    RenderingContext rc)
  {
    return rc.getCurrentClientId();
  }

  @Override
  protected Object getType()
  {
    return "radio";
  }

  @Override
  protected String getIconAltTextName(
    boolean selected
  )
  {
    return (selected
      ? "af_selectBooleanRadio.READONLY_CHECKED_TIP"
      : "af_selectBooleanRadio.READONLY_NOT_CHECKED_TIP");
  }

  @Override
  protected String getIconName(
    boolean selected,
    boolean disabled
  )
  {
    final String iconName;

    if (disabled)
    {
      iconName = (selected ?
              SkinSelectors.AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME :
              SkinSelectors.AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME);
    }
    else
    {
      iconName = (selected ?
              SkinSelectors.AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME :
              SkinSelectors.AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME);
    }

    return iconName;
  }

  @Override
  protected void renderNameAttribute(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean
    )throws IOException
  {
    String group = getGroup(bean);
    if (group != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeAttribute("name", group, null);
    }
  }

  /**
   * Returns true if the component should render the ID as a name.
   * By default, don't if the component is readonly.
   */
  @Override
  protected boolean shouldRenderName(
    FacesContext context,
    UIComponent  component)
  {
    return false;
  }

  @Override
  protected boolean isRadio()
  {
    return true;
  }

  @Override
  protected String getCompositeId(
    String clientId)
  {
    return clientId + XhtmlConstants.COMPOSITE_ID_EXTENSION + "r";
  }

  @Override
  protected void renderSpanEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    // PH: This condition is needed to set onclick on radio rather than on
    // enclosing span in an IE Mobile and PIE since these browsers don't have
    // onclick support on a span.

    if(!isPDA(RenderingContext.getCurrentInstance()))
    {
      if ( isAutoSubmit(component, bean))
        rw.writeAttribute("onclick", getAutoSubmitScript(component, bean) , null);
    }
    rw.writeAttribute("ondblclick", getOndblclick(component, bean),  "ondblclick");
    rw.writeAttribute("onkeydown", getOnkeydown(component, bean),  "onkeydown");
    rw.writeAttribute("onkeyup", getOnkeyup(component, bean),  "onkeyup");
    rw.writeAttribute("onkeypress", getOnkeypress(component, bean),  "onkeypress");
    rw.writeAttribute("onmousedown", getOnmousedown(component, bean),  "onmousedown");
    rw.writeAttribute("onmousemove", getOnmousemove(component, bean),  "onmousemove");
    rw.writeAttribute("onmouseout", getOnmouseout(component, bean),  "onmouseout");
    rw.writeAttribute("onmouseover", getOnmouseover(component, bean),  "onmouseover");
    rw.writeAttribute("onmouseup", getOnmouseup(component, bean),  "onmouseup");
  }

  @Override
  protected void renderInputEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    String onClick = getOnclick(component, bean);

    //PH: this condition is needed to set onclick on radio rather than on
    // enclosing span in an IE Mobile and PIE since these browsers don't have
    // onclick support on a span.
    if(isPDA(RenderingContext.getCurrentInstance()))
    {
      if (isAutoSubmit(component, bean))
      {
        String auto = getAutoSubmitScript(component, bean);

        if (onClick == null)
        {
          onClick = auto;
        }
        else
        {
          // Since we have both onClick script and autosubmit script to execute,
          // we need to chain the execution of these scripts.
          onClick = XhtmlUtils.getChainedJS(onClick, auto, true);
        }
      }
    }

    writer.writeAttribute("onclick", onClick, "onclick");
    writer.writeAttribute("onblur", getOnblur(component, bean),  "onblur");
    writer.writeAttribute("onfocus", getOnfocus(component, bean),  "onfocus");
    writer.writeAttribute("onchange", getOnchange(component, bean),  "onchange");
  }

  protected String getGroup(FacesBean bean)
  {
    return toString(bean.getProperty(_groupKey));
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
   return "af|selectBooleanRadio::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
   return "af|selectBooleanRadio";
  }

  private PropertyKey _groupKey;
}
