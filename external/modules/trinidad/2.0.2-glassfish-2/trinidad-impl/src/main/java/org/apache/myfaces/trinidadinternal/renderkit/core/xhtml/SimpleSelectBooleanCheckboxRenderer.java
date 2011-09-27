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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectBooleanCheckbox;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 */
public class SimpleSelectBooleanCheckboxRenderer extends SimpleSelectBooleanRenderer
{
  public SimpleSelectBooleanCheckboxRenderer()
  {
    this(CoreSelectBooleanCheckbox.TYPE);
  }

  public SimpleSelectBooleanCheckboxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  //**********************
  //decode
  //**********************

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    if (super.getSubmittedValue(context, component, clientId) == null)
      return Boolean.FALSE;

    return Boolean.TRUE;
  }


  //**********************
  //encode
  //**********************

  @Override
  protected Object getValueAttr(
    RenderingContext rc)
  {
    // HTML 3.2 specification, default value for checkboxes
    return "t";
  }

  @Override
  protected Object getType()
  {
    return "checkbox";
  }

  @Override
  protected String getIconAltTextName(
    boolean selected
  )
  {
    return (selected
      ? "af_selectBooleanCheckbox.READONLY_CHECKED_TIP"
      : "af_selectBooleanCheckbox.READONLY_NOT_CHECKED_TIP");
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
              SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_CHECKED_ICON_NAME :
              SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_UNCHECKED_ICON_NAME);
    }
    else
    {
      iconName = (selected ?
              SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_CHECKED_ICON_NAME :
              SkinSelectors.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_UNCHECKED_ICON_NAME);
    }

    return iconName;
  }

  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    String onClick = super.getOnclick(component, bean);
    if (isAutoSubmit(component, bean))
    {
      String auto = getAutoSubmitScript(component, bean);
      if (onClick == null)
        onClick = auto;
      else if (auto != null)
        onClick = XhtmlUtils.getChainedJS(onClick, auto, true);
    }

    return onClick;
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
   return "af|selectBooleanCheckbox::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
   return "af|selectBooleanCheckbox";
  }
}