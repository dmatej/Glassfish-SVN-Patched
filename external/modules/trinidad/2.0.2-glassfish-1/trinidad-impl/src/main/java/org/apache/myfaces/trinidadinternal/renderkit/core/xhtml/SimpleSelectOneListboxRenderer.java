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
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneListbox;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;


/**
 */
public class SimpleSelectOneListboxRenderer extends SimpleSelectOneRenderer
{
  public SimpleSelectOneListboxRenderer()
  {
    this(CoreSelectOneListbox.TYPE);
  }

  public SimpleSelectOneListboxRenderer(
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
    _unselectedLabelKey = type.findKey("unselectedLabel");
  }

  static public int getListSize(
    int     sizeAttr,
    int     itemCount,
    boolean addOne)
  {

    // Must have size > 1 or we'd render a choice!
    if (sizeAttr < 2)
    {
      sizeAttr = Math.min(8, Math.max(2, itemCount));

      if (addOne)
        sizeAttr++;
    }

    return sizeAttr;
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
    writer.startElement("select", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean, false);

    int count = (selectItems == null) ? 0 : selectItems.size();
    String unselectedLabel = getUnselectedLabel(component, bean);
    boolean hasUnselectedLabel = (unselectedLabel != null);

    int size = getListSize(getSize(component, bean), count, hasUnselectedLabel);

    writer.writeAttribute("size", IntegerUtils.getString(size), "size");

    if (hasUnselectedLabel)
    {
      SelectItem item = new SelectItem("", unselectedLabel, "", false);
      // @todo Restore the logic below
      encodeOption(context, rc, component, item, null, true, -1,
                   (selectedIndex < 0));
    }

    int counter = 0;
    for (int i = 0; i < count; i++)
    {
       SelectItem item = selectItems.get(i);

       if(item instanceof SelectItemGroup)
       {
          writer.startElement("optgroup", component);
          writer.writeAttribute("label", item.getLabel(), null);
          SelectItem[] items = ((SelectItemGroup)item).getSelectItems();

          for(int j = 0; j < items.length; j++)
          {
             encodeOption(context, rc, component, items[j], converter,
                          valuePassThru, counter, selectedIndex == counter);
             counter++;

          }
          writer.endElement("optgroup");
       }
       else
       {
          encodeOption(context, rc, component, item, converter,
                       valuePassThru, counter, selectedIndex == counter);
          counter++;
       }
    }

    writer.endElement("select");
  }

  @Override
  protected String getUnselectedLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_unselectedLabelKey));
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
      RenderingContext rc = RenderingContext.getCurrentInstance();
      String auto = getAutoSubmitScript(rc, component, bean);
      return XhtmlUtils.getChainedJS(onchange, auto, true);
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
    return "af|selectOneListbox::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectOneListbox";
  }

  private PropertyKey _sizeKey;
  private PropertyKey _unselectedLabelKey;
}
