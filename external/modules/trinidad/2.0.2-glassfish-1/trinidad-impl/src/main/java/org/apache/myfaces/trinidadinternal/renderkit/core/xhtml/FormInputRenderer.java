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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RenderingContext;


abstract public class FormInputRenderer extends FormElementRenderer
{
  protected FormInputRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _showRequiredKey = type.findKey("showRequired");
    _simpleKey       = type.findKey("simple");
  }

  /**
   * Render the client ID as both an "id" and a "name"
   */
  @Override
  protected void renderId(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    String clientId = getClientId(context, component);
    context.getResponseWriter().writeAttribute("id", clientId, "id");

    if (shouldRenderName(context, component))
    // Don't render the name if it's read-only
      context.getResponseWriter().writeAttribute("name", clientId, "id");
  }

  /**
   * Returns true if the component should render the ID as a name.
   * By default, don't if the component is readonly.
   */
  protected boolean shouldRenderName(
    FacesContext context,
    UIComponent  component)
  {
    FacesBean bean = getFacesBean(component);
    return !getReadOnly(context, component, bean);
  }

  protected void renderDisabledAttribute(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {

    if (getDisabled(component, bean))
    {
      context.getResponseWriter().writeAttribute("disabled",
                                                 Boolean.TRUE,
                                                 "disabled");
    }
  }

  /**
   * used in the form input components for the 'content' piece.
   * @param context
   * @param rc
   * @param bean
   * @param renderStyleAttrs, whether to render the styleClass/inlineStyle
   * attribute values on the 'content' piece. This is usually false.
   * @throws IOException
   */
  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          renderStyleAttrs
    ) throws IOException
  {
    super.renderAllAttributes(context, rc, component, bean, renderStyleAttrs);
    renderDisabledAttribute(context, rc, component, bean);
    renderStyleClass(context, rc, getContentStyleClass(component, bean));
    renderInlineStyleAttribute(context, rc, component, getContentStyle(component, bean));
  }

  protected boolean getSimple(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_simpleKey);
    if (o == null)
      o = _simpleKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }

  /**
   * Render the styles and style classes that should go on the root dom element.
   * (called from LabelAndMessageRenderer, the superclass)
   * @param context
   * @param arc
   * @param component
   * @param bean
   * @throws IOException
   */
  protected void renderRootDomElementStyles(
    FacesContext     context,
    RenderingContext arc,
    UIComponent      component,
    FacesBean        bean) throws IOException
  {
    // get the style classes that I want to render on the root dom element here.
    String styleClass         = getStyleClass(component, bean);
    String contentStyleClass  = getRootStyleClass(component, bean);
    String disabledStyleClass = null;
    String readOnlyStyleClass = null;
    String requiredStyleClass = null;

    // readOnly takes precedence over disabled for the state.
    // -= Simon =- Why?
    if (getReadOnly(context, component, bean))
    {
      readOnlyStyleClass = SkinSelectors.STATE_READ_ONLY;
    }
    else if (getDisabled(component, bean))
    {
      disabledStyleClass = SkinSelectors.STATE_DISABLED;
    }

    if(_isConsideredRequired(component, bean))
    {
      requiredStyleClass = SkinSelectors.STATE_REQUIRED;
    }

    List<String> parsedStyleClasses =
      OutputUtils.parseStyleClassList(styleClass);
    int userStyleClassCount;
    if (parsedStyleClasses == null)
      userStyleClassCount = (styleClass == null) ? 0 : 1;
    else
      userStyleClassCount = parsedStyleClasses.size();

    String[] styleClasses = new String[userStyleClassCount + 4];
    int i=0;
    if (parsedStyleClasses != null)
    {
      while (i < userStyleClassCount)
      {
        styleClasses[i] = parsedStyleClasses.get(i);
        i++;
      }
    }
    else if (styleClass != null)
    {
      styleClasses[i++] = styleClass;
    }

    styleClasses[i++] = contentStyleClass;
    styleClasses[i++] = disabledStyleClass;
    styleClasses[i++] = readOnlyStyleClass;
    styleClasses[i++] = requiredStyleClass;

    renderStyleClasses(context, arc, styleClasses);
    renderInlineStyle(context, arc, component, bean);
  }

  /*
   * override to return the content style class, like af|inputText::content
   * if component is tr:inputText.
   */
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    String styleClass = getRootStyleClass(component, bean);
    if(styleClass != null)
    {
      styleClass = styleClass + _CONTENT_PSEUDO_ELEMENT;
    }

    return styleClass;
  }

  /*
   * override to return the root style class, like af|inputText
   * if component is tr:inputText.
   */
  abstract protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean);

  protected boolean getShowRequired(
    UIComponent component,
    FacesBean   bean)
  {
    if(_showRequiredKey == null)
    { // showRequired is not supporte on the element
      return false;
    }

    Object o = bean.getProperty(_showRequiredKey);
    if (o == null)
    {
      o = _showRequiredKey.getDefault();
    }

    return Boolean.TRUE.equals(o);
  }

  private boolean _isConsideredRequired(
    UIComponent component,
    FacesBean   bean)
  {
    return getRequired(component, bean) || getShowRequired(component, bean);
  }

  private static final String _CONTENT_PSEUDO_ELEMENT = "::content";

  private PropertyKey _showRequiredKey;
  private PropertyKey _simpleKey;
}
