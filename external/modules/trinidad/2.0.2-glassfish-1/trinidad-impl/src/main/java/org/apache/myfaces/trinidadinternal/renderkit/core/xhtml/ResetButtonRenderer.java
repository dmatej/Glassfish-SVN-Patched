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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.input.CoreResetButton;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class ResetButtonRenderer extends CommandButtonRenderer
{
  public ResetButtonRenderer()
  {
    super(CoreResetButton.TYPE);
  }

  @Override
  protected String getButtonType()
  {
    return "reset";
  }

  @Override
  protected String getInputType()
  {
    return "reset";
  }

  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    String onclick = getComponentOnclick(component, bean);

    RenderingContext arc = RenderingContext.getCurrentInstance();
    if (arc.getFormData() != null)
    {
      String formName = arc.getFormData().getName();
      if (formName != null)
      {
        onclick = XhtmlUtils.getChainedJS(onclick,
                                          "resetForm('" + formName +
                                             "');return false;",
                                          true);
      }
    }

    return onclick;
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_RESET_BUTTON_STYLE_CLASS;
  }

  @Override
  protected String getIcon(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected boolean getPartialSubmit(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }
}
