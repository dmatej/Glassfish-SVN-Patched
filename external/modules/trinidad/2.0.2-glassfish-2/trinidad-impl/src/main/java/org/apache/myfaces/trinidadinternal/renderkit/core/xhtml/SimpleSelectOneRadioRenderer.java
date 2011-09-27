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
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneRadio;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;


/**
 */
public class SimpleSelectOneRadioRenderer extends SimpleSelectOneRenderer
{
  public SimpleSelectOneRadioRenderer()
  {
    this(CoreSelectOneRadio.TYPE);
  }

  public SimpleSelectOneRadioRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _accessKeyKey   = type.findKey("accessKey");
    _layoutKey = type.findKey("layout");
    _unselectedLabelKey = type.findKey("unselectedLabel");
  }

  //
  // ENCODE BEHAVIOR
  //
  @Override
  protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int              selectedIndex,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("span", component);
    renderId(context, component);
    // Render all generic attributes, except styles (they go on the item),
    // and onclick (also on the item, see below)
    renderAllAttributes(context, rc, component, bean, false /*no styles*/);

    boolean applyFieldSet = _applyFieldSetWrapper(rc);
    if (applyFieldSet)
    {
      String shortDesc = getShortDesc(component, bean);
      if (shortDesc == null)
      {
        applyFieldSet = false;
      }
      else
      {
        writer.startElement("fieldset", null);
        writer.writeAttribute("style", "border:none;margin:0px;padding:0px;", null);
        writer.startElement("legend", null);
        renderStyleClass(context, rc,
                         SkinSelectors.HIDDEN_LABEL_STYLE_CLASS);
        writer.writeText(shortDesc, "shortDesc");
        writer.endElement("legend");
      }
    }

    encodeSelectItems(context, rc, component, bean,
                      selectItems, selectedIndex, converter,
                      valuePassThru);
    if (applyFieldSet)
    {
      writer.endElement("fieldset");
    }

    writer.endElement("span");
  }

  protected void encodeSelectItems(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int              selectedIndex,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    int size = (selectItems == null) ? 0 : selectItems.size();

    boolean disabled = getDisabled(component, bean);
    boolean isVertical = !CoreSelectOneRadio.LAYOUT_HORIZONTAL.equals(
                           getLayout(component, bean));
    Object accessKey;
    if (supportsAccessKeys(rc))
    {
      accessKey = getAccessKey(component, bean);
    }
    else
    {
      accessKey = null;
    }

    String itemOnclick = getItemOnclick(rc, component, bean);

    boolean renderedOne = false;
    String unselectedLabel = getUnselectedLabel(component, bean);
    if (unselectedLabel != null)
    {
      SelectItem item = new SelectItem("", unselectedLabel, "", false);
      encodeSelectItem(context, rc, component, item, null, true,
                       accessKey, -1,  selectedIndex < 0, disabled, false,
                       itemOnclick);
      renderedOne = true;
    }

    for (int i = 0; i < size; i++)
    {
      SelectItem item = selectItems.get(i);
      if (encodeSelectItem(context, rc, component, item, converter,
                           valuePassThru, accessKey,
                           i, selectedIndex == i, disabled,
                           renderedOne && isVertical,
                           itemOnclick))
      {
        renderedOne = true;
      }
    }
  }

  protected boolean encodeSelectItem(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    SelectItem       item,
    Converter        converter,
    boolean          valuePassThru,
    Object           accessKey,
    int              index,
    boolean          isSelected,
    boolean          isDisabled,
    boolean          renderBreak,
    String           itemOnclick
    ) throws IOException
  {
    if (item == null)
      return false;

    String id = rc.getCurrentClientId();
    if (id == null)
      return false;

    // Create the per-item ID, necessary for generating the <label>
    // tag.  We use "parentid:_[index]"
    StringBuffer subidBuffer = new StringBuffer(id.length() + 4);
    subidBuffer.append(id);
    subidBuffer.append(":_");
    subidBuffer.append(IntegerUtils.getString(index));
    String subid = subidBuffer.toString();

    Object itemValue = getItemValue(context,
                                    component,
                                    item,
                                    converter,
                                    valuePassThru,
                                    index);

    FacesBean bean = getFacesBean(component);
    ResponseWriter rw = context.getResponseWriter();

    // Render a <br> if necessary (in "vertical" alignment)
    if (renderBreak)
    {
      rw.startElement("br", null);
      rw.endElement("br");
    }


    // OK, now render the input control
    rw.startElement("input", null);
    rw.writeAttribute("type", "radio", null);
    rw.writeAttribute("name", id, null);
    rw.writeAttribute("id", subid, null);
    rw.writeAttribute("value", itemValue, null);
    rw.writeAttribute("accesskey", accessKey, null);
    if (isSelected)
      rw.writeAttribute("checked", Boolean.TRUE, null);
    if (isDisabled || item.isDisabled())
      rw.writeAttribute("disabled", Boolean.TRUE, null);

    // =-=AEW Render all the Javascript needed on a per-item basis.
    // We could optimize SelectOneRadio a bit by gathering
    // up the "form event handlers" in one pass (seems to be about
    // 8% slower this way)
    // render the events only if the browser supports JavaScript
    if (supportsScripting(rc))
    {
      rw.writeAttribute("onclick", itemOnclick, null);
      renderItemFormEventHandlers(context, component, bean);
    }

    rw.endElement("input");

    // And render the label
    rw.startElement("label", null);
    rw.writeAttribute("for", subid, null);

    // For reasons that aren't especially clear to me, we're getting
    // passed the empty string for our title.
    String description = item.getDescription();
    if ((description != null) && !"".equals(description))
      rw.writeAttribute("title", description, null);

    rw.writeText(item.getLabel(), null);
    rw.endElement("label");

    return true;
  }

  @Override
  protected void renderId(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      context.getResponseWriter().writeAttribute("id", clientId, "id");
    }
  }

  /**
   * Disable rendering the form event handlers on the parent.
   * In Gecko, they bubble up, but in IE, they don't, so
   * they have to go on the items.
   */
  @Override
  protected void renderFormEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
  }

  /**
   * Disable rendering "onclick" on the parent;  it needs to
   * go on the individual radio buttons
   */
  @Override
  protected String getOnclick(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  /**
   * Render the per-item event handlers
   */
  protected void renderItemFormEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    super.renderFormEventHandlers(context, component, bean);
  }

  /**
   * Get the onclick for the individual radio buttons.
   */
  protected String getItemOnclick(
    RenderingContext rc,
    UIComponent component,
    FacesBean   bean)
  {
    // Get the overall onclick, and merge in any needed autosubmit script
    String onclick = super.getOnclick(component, bean);
    if (isAutoSubmit(component, bean))
    {
      String source = LabelAndMessageRenderer.__getCachedClientId(rc);
      boolean immediate = isImmediate(component, bean);
      String auto = AutoSubmitUtils.getSubmitScript(rc,
                                                    source,
                                                    XhtmlConstants.AUTOSUBMIT_EVENT,
                                                    immediate);
      onclick = XhtmlUtils.getChainedJS(onclick, auto, true);
    }

    return onclick;
  }

  protected Object getAccessKey(
    UIComponent component,
    FacesBean   bean)
  {
    return bean.getProperty(_accessKeyKey);
  }

  protected String getLayout(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_layoutKey));
  }

  @Override
  protected String getUnselectedLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_unselectedLabelKey));
  }

  // Never render the "hidden label";  labels entirely go on the individual
  // items
  @Override
  protected boolean isHiddenLabelRequired(
    RenderingContext rc)
  {
    return false;
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectOneRadio::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectOneRadio";
  }

  private static boolean _applyFieldSetWrapper(
    RenderingContext rc)
  {
    // Don't bother with the output in inaccessible mode
    if (isInaccessibleMode(rc))
      return false;

    // The fieldset trick doesn't work without support for hidden labels
    if (!HiddenLabelUtils.supportsHiddenLabels(rc))
      return true;

    return Boolean.TRUE.equals(
       rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_FIELDSET));
  }

  private PropertyKey _accessKeyKey;
  private PropertyKey _layoutKey;
  private PropertyKey _unselectedLabelKey;
}
