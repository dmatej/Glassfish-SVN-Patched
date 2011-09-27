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
import org.apache.myfaces.trinidad.component.core.input.CoreInputNumberSpinbox;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;


/**
 * overrides SimpleInputListOfValuesRenderer because like that class,
 * we need an 'icon' after the text field. --
 * the 'icon' is really two icons: the up/down buttons
 */
public class SimpleInputNumberSpinboxRenderer extends SimpleInputListOfValuesRenderer
{
  public SimpleInputNumberSpinboxRenderer()
  {
    this(CoreInputNumberSpinbox.TYPE);
  }

  public SimpleInputNumberSpinboxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _minimumKey = type.findKey("minimum");
    _maximumKey = type.findKey("maximum");
    _stepSizeKey = type.findKey("stepSize");
  }

  //
  // ENCODE BEHAVIOR
  //

  //
  // Overrides disabling all the things you can't do on
  // an InputNumberSpinbox
  //

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

  @Override
  protected Number getMaximumLength(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected boolean shouldRenderInputOnclick()
  {
    // keep the onclick on the input. selectInputText moves it to the button.
    return false;
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputNumberSpinbox";
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputNumberSpinbox::content";
  }

  @Override
  protected Integer getDefaultColumns(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    return _DEFAULT_COLUMNS;
  }

  @Override
  protected void renderTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // render <table><tr><td>, then text field, </td>
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", "0", "0", null);
    rw.startElement("tr", component);
    rw.startElement("td", component);
    // this renders the inputText. This will call getContentStyleClass to put
    // our styleclass on it. (af|inputNumberSpinbox::content)
    super.renderTextField(context, rc, component, bean);
    rw.endElement("td");
  }

  /**
   * render the spinboxes after the text field. Render these even if
   * they are disabled.
   * @param context
   * @param rc
   * @param component
   * @param bean
   * @throws IOException
   */
  @Override
  protected void renderAfterTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", component);
    rw.writeAttribute("align", "center", null);
    rw.writeAttribute("valign", "middle", null);

    renderStyleClass(context, rc, "af|inputNumberSpinbox::spinbox-cell");
    // use css to put in a space.???
    renderIcon(context, rc, component, bean);
    rw.endElement("td");
    rw.endElement("tr");
    rw.endElement("table");
  }

  /**
   * render the spinbox icons. <div>img</div><div>img</div>
   */
  @Override
  protected void renderIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    // render increment spinbox image
    rw.startElement("div", component);
    _renderSpinboxIcon(context, rc, component, bean, rw, true);
    rw.endElement("div");

    // render decrement spinbox image
    rw.startElement("div", component);
    _renderSpinboxIcon(context, rc, component, bean, rw, false);
    rw.endElement("div");
  }

  /**
   * render one of the spinbox icons: either the increment or decrement icon.
   * The <img> renders within <a> tags with onmousedown and onmouseup event
   * handlers which will call javascript to increment/decrement the input
   * value. If disabled, then do not render the <a> tags.
   */
  private void _renderSpinboxIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    ResponseWriter   rw,
    boolean          increment
    ) throws IOException
  {
    boolean disabled = getDisabled(component, bean);

    String styleClass =
    	(increment) ?
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_INCREMENT_CELL :
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_DECREMENT_CELL;
    renderStyleClass(context, rc, styleClass);

    String iconName;

    if (!disabled)
    {
      iconName =
      	(increment) ?
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_INCREMENT_ICON_NAME :
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_DECREMENT_ICON_NAME;
    }
    else
    {
      iconName =
      	(increment) ?
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_INCREMENT_DISABLED_ICON_NAME :
        SkinSelectors.AF_INPUT_NUMBER_SPINBOX_DECREMENT_DISABLED_ICON_NAME;
    }

    Icon icon = rc.getIcon(iconName);
    if ((icon != null) && !icon.isNull())
    {
      // Render Link with onmousedown and onmouseup event handlers
      if (!disabled)
      {
        rw.startElement("a", component);
        rw.writeURIAttribute("href", "#", null);
        // this will keep the href from being executed.
        rw.writeAttribute("onclick", "return false;", null);

        String mouseDownScript =
           _getSpinboxScript(context, component, bean, increment);

        rw.writeAttribute("onmousedown", mouseDownScript, null);
        rw.writeAttribute("onmouseup",  _CLEAR_SPINBOX_JS, null);
        rw.writeAttribute("onmouseout",  _CLEAR_SPINBOX_JS, null);

      }

      // TODO resource bundle
      String altText;
      if (!disabled)
        altText = (increment) ? "increment" : "decrement";
      else
        altText = (increment) ? "increment disabled" : "decrement disabled";
      OutputUtils.renderIcon(context, rc, icon, altText, null);

      if (!disabled)
        rw.endElement("a");
    }
  }

  private String _getSpinboxScript(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean,
    boolean      increment)
  {
    StringBuffer js = new StringBuffer();
    js.append(_SPINBOX_REPEAT_JS);
    js.append("('");
    js.append(component.getClientId(context));
    js.append("',");
    js.append(increment);
    js.append(",");
    js.append(_getStepSizeOrDefault(component, bean));
    js.append(",");
    js.append(_getMinimumOrDefault(component, bean));
    js.append(",");
    js.append(_getMaximumOrDefault(component, bean));
    js.append(");");

    return js.toString();
  }

  private int _getMinimumOrDefault(
    UIComponent component,
    FacesBean   bean)
  {
    Number minimum = (Number) bean.getProperty(_minimumKey);
    if (minimum == null)
      minimum = (Number)_minimumKey.getDefault();
    assert(minimum != null);
    return minimum.intValue();
  }

  private int _getMaximumOrDefault(
    UIComponent component,
    FacesBean   bean)
  {
    Number maximum = (Number) bean.getProperty(_maximumKey);
    if (maximum == null)
      maximum = (Number)_maximumKey.getDefault();
    assert(maximum != null);
    return maximum.intValue();
  }

  private int _getStepSizeOrDefault(
    UIComponent component,
    FacesBean   bean)
  {
    Number stepSize = (Number) bean.getProperty(_stepSizeKey);
    if (stepSize == null)
      stepSize = (Number) _stepSizeKey.getDefault();
    assert(stepSize != null);
    return stepSize.intValue();
  }

  private PropertyKey _minimumKey;
  private PropertyKey _maximumKey;
  private PropertyKey _stepSizeKey;

  private static String _SPINBOX_REPEAT_JS = "_spinboxRepeat";
  private static String _CLEAR_SPINBOX_JS = "_clearSpinbox();";

  private static Integer _DEFAULT_COLUMNS = Integer.valueOf(1);
}
