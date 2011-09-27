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
import javax.faces.model.SelectItemGroup;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectManyListbox;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;


/**
 * Renderer for SelectMany listboxes.
 * @todo Expose at least some of the decode behavior for access
 *   by other selectMany renderers
 */
public class SimpleSelectManyListboxRenderer extends SimpleSelectManyRenderer
{
  public SimpleSelectManyListboxRenderer()
  {
    this(CoreSelectManyListbox.TYPE);
  }

  public SimpleSelectManyListboxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _sizeKey = type.findKey("size");
  }

  @Override
  protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int[]            selectedIndices,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("select", component);
    writer.writeAttribute("multiple", Boolean.TRUE, null);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean, false);

    int count = (selectItems == null) ? 0 : selectItems.size();
    int size = SimpleSelectOneListboxRenderer.getListSize(getSize(component, bean),
                                                          count, false);

    writer.writeAttribute("size", IntegerUtils.getString(size), "size");

    int selectedCount = selectedIndices.length;
    int selectedEntry = 0;
    int counter = 0;
    for (int i = 0; i < count; i++)
    {
      boolean selected;

      SelectItem item = selectItems.get(i);

      if(item instanceof SelectItemGroup)
      {
        writer.startElement("optgroup", component);
        writer.writeAttribute("label", item.getLabel(), null);
        SelectItem[] items = ((SelectItemGroup)item).getSelectItems();

        for(int j = 0; j < items.length; j++)
        {
          selected = ((selectedEntry < selectedCount) &&
                     (counter == selectedIndices[selectedEntry]));
          if (selected)
            selectedEntry++;

          SimpleSelectOneRenderer.encodeOption(
               context, rc, component, items[j], converter,
               valuePassThru, counter++, selected);
        }
        writer.endElement("optgroup");
      }
      else
      {
        selected = ((selectedEntry < selectedCount) &&
                   (counter == selectedIndices[selectedEntry]));

        if (selected)
          selectedEntry++;

        SimpleSelectOneRenderer.encodeOption(
             context, rc, component, item, converter,
             valuePassThru, counter++, selected);
      }
    }

    writer.endElement("select");

  }

  /**
   * Add autosubmit script
   */
  @Override
  protected String getOnchange(
    UIComponent component,
    FacesBean   bean)
  {
    String onchange = super.getOnchange(component, bean);
    if (isAutoSubmit(component, bean))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      String source = LabelAndMessageRenderer.__getCachedClientId(arc);
      boolean immediate = isImmediate(component, bean);
      String auto = AutoSubmitUtils.getSubmitScript(arc, source,
               XhtmlConstants.AUTOSUBMIT_EVENT, immediate);
      onchange = XhtmlUtils.getChainedJS(onchange, auto, true);
    }

    return onchange;
  }

  protected int getSize(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_sizeKey);
    if (o == null)
      o = _sizeKey.getDefault();
    if (o == null)
      return -1;

    return toInt(o);
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectManyListbox::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectManyListbox";
  }

  private PropertyKey _sizeKey;
}