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
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreInputColor;
import org.apache.myfaces.trinidad.context.RenderingContext;

public class InputColorRenderer extends InputLabelAndMessageRenderer
{
  public InputColorRenderer()
  {
    super(CoreInputColor.TYPE);
  }

  protected InputColorRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _simpleInputColor = new SimpleInputColorRenderer(type);
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputColor";
  }

  @Override
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleInputColor;
  }

  @Override
  protected String getLabelFor(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    if (_isCompact(component, bean))
      return null;
    return super.getLabelFor(context, rc, component, bean);
  }

  private boolean _isCompact(
    UIComponent component,
    FacesBean   bean)
  {
    FacesBean.Type type = CoreInputColor.TYPE;
    PropertyKey compactKey = type.findKey("compact");
    Object o = bean.getProperty(compactKey);
    if (o == null)
      o = compactKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  private SimpleInputColorRenderer _simpleInputColor;
}
