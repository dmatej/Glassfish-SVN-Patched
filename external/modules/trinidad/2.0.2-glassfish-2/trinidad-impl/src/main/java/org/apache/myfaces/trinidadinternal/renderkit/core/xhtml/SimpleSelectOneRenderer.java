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
import javax.faces.convert.ConverterException;
import javax.faces.model.SelectItem;
import javax.faces.model.SelectItemGroup;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXSelectOne;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.uix.SelectItemSupport;


/**
 */
abstract public class SimpleSelectOneRenderer extends FormInputRenderer
{
  public SimpleSelectOneRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _valuePassThruKey = type.findKey("valuePassThru");
  }

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    Object submittedValue = super.getSubmittedValue(context,
                                                    component,
                                                    clientId);
    boolean valuePassThru = getValuePassThru(component, getFacesBean(component));

    if (submittedValue == null && valuePassThru)
      submittedValue = "";

    return submittedValue;
  }

  /**
   * Return the value to output for an item.
   */
  static public Object getItemValue(
    FacesContext context,
    UIComponent  component,
    SelectItem   item,
    Converter    converter,
    boolean      valuePassThru,
    int          index)
  {
    if (!valuePassThru)
    {
      return IntegerUtils.getString(index);
    }
    else
    {
      Object itemValue = item.getValue();
      if ((itemValue != null) && (converter != null))
      {
        itemValue = converter.getAsString(context,
                                          component,
                                          itemValue);
      }

      return itemValue;
    }
  }

  /**
   * @todo Move to utility class?
   */
  static public boolean encodeOption(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    SelectItem       item,
    Converter        converter,
    boolean          valuePassThru,
    int              index,
    boolean          isSelected
    ) throws IOException
  {
    if (item == null)
      return false;

    if (item.isDisabled())
    {
      if (!Boolean.TRUE.equals(rc.getAgent().getCapabilities().get(
                          TrinidadAgent.CAP_SUPPORTS_DISABLED_OPTIONS)))
        return false;
    }

    Object itemValue = getItemValue(context,
                                    component,
                                    item,
                                    converter,
                                    valuePassThru,
                                    index);

    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("option", null);

    if (item.isDisabled())
      writer.writeAttribute("disabled", Boolean.TRUE, null);

    // Never write out null, because that will result in the label
    // getting submitted, instead of null.
    if (itemValue == null)
      itemValue="";
    writer.writeAttribute("value", itemValue, null);

    if (isSelected)
      writer.writeAttribute("selected", Boolean.TRUE, null);

    // For reasons that aren't especially clear to me, we're getting
    // passed the empty string for our title.
    String description = item.getDescription();
    if ((description != null) && !"".equals(description))
      writer.writeAttribute("title", description, null);

    writer.writeText(item.getLabel(), null);

    writer.endElement("option");

    return true;
  }

  //
  // DECODE BEHAVIOR
  //
  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue
    ) throws ConverterException
  {
    boolean valuePassThru = getValuePassThru(component, getFacesBean(component));

    if (!valuePassThru)
    {
      return _convertIndexedSubmittedValue(context, component, submittedValue);
    }
    else
    {
      return super.getConvertedValue(context, component, submittedValue);
    }
  }

  /**
   * Call this method only when the valuePassThru attribute on the selectOne
   * component is not set to true.
   * This indicates that the client-side value
   * is an index. We need to convert that index into its real value.
   * @param component
   * @param submittedValue the submittedValue. Since this method is only
   *  called when the valuePassThru attribute on the selectOne component is
   *  not true, then the submittedValue in this case is an index into a List.
   * @return the Object value at that index specified in submittedValue,
   *    or null.
   */
  private Object _convertIndexedSubmittedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue
    ) throws ConverterException
  {
    FacesBean bean = getFacesBean(component);
    Converter converter = getConverter(component, bean);
    if ( converter == null)
      converter = getDefaultConverter(context, component, bean);

    List<SelectItem> selectItems = getSelectItems(component, converter, true);

    int index = __getIndex(submittedValue, selectItems);
    if (index < 0)
      return null;

    SelectItem item = selectItems.get(index);
    if (item != null)
    {
      Object converted = item.getValue();
      if (converter != null && converted != null)
      {
        converted = converter.getAsObject(context, component, converted.toString());
      }
      return converted;
    }
    else
    {
      return null;
    }
  }

  //
  // ENCODE BEHAVIOR
  //
  /*
   */
  @Override
  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    Converter converter = getConverter(component, bean);
    if ( converter == null)
      converter = getDefaultConverter(context, component, bean);
    boolean valuePassThru = getValuePassThru(component, bean);

    if (isAutoSubmit(component, bean))
      AutoSubmitUtils.writeDependencies(context, rc);

    // Only add in validators and converters when we're in valuePassThru
    // mode; otherwise, there's not enough on the client to even consider
    FormData fData = rc.getFormData();
    if (fData != null)
    {
      ((CoreFormData) fData).addOnSubmitConverterValidators(component,
                      valuePassThru ? converter : null,
                      valuePassThru ? getValidators(component, bean) : null,
                      getClientId(context, component),
                      isImmediate(component, bean),
                      getRequired(component, bean),
                      getRequiredMessageKey());
    }

    List<SelectItem> selectItems = getSelectItems(component, converter, false);

    int selectedIndex = _getSelectedIndex(context,
                                          component,
                                          bean,
                                          selectItems,
                                          converter,
                                          valuePassThru);

    ResponseWriter writer = context.getResponseWriter();
    boolean simple = getSimple(component, bean);
    if (simple)
    {
      writer.startElement("span", component);
      // put the outer style class here, like af_selectOneRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, rc, component, bean);
    }

    encodeElementContent(context,
                         rc,
                         component,
                         bean,
                         selectItems,
                         selectedIndex,
                         converter,
                         valuePassThru);



    if (isHiddenLabelRequired(rc))
      renderShortDescAsHiddenLabel(context, rc, component, bean);

    if (simple)
    {
      writer.endElement("span");
    }
  }

  abstract protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int              selectedIndex,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException;

  @Override
  protected void renderNonElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // http://issues.apache.org/jira/browse/ADFFACES-151
    // Getting default converter for null value leads to exception but
    // if value of component is null than there is no need to perform
    // this method
    if (getValue(component, bean) == null)
      return;

    Converter converter = getConverter(component, bean);
    if ( converter == null)
      converter = getDefaultConverter(context, component, bean);
    boolean valuePassThru = getValuePassThru(component, bean);

    // =-=AEW If needed, this could be made more efficient
    // by iterating through the list instead of getting
    // all the items
    List<SelectItem> selectItems = getSelectItems(component, converter, false);

    int selectedIndex = _getSelectedIndex(context,
                                          component,
                                          bean,
                                          selectItems,
                                          converter,
                                          valuePassThru);

    // If an item is selected, get its label.
    String text;
    if (selectedIndex >= 0)
    {
      SelectItem item = selectItems.get(selectedIndex);
      text = item.getLabel();
    }
    else
    {
      text = getUnselectedLabel(component, bean);
    }

    context.getResponseWriter().writeText(text, null);
  }

  @Override
  protected String getRequiredMessageKey()
  {
    return UIXSelectOne.REQUIRED_MESSAGE_ID;
  }

  protected List<SelectItem> getSelectItems(
    UIComponent component,
    Converter   converter)
  {
    return getSelectItems(component, converter, false);
  }

  protected List<SelectItem> getSelectItems(
    UIComponent component,
    Converter   converter,
    boolean     filteredItems)
  {
    return SelectItemSupport.getSelectItems(component, converter, filteredItems);
  }

  protected boolean getValuePassThru(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_valuePassThruKey);
    if (o == null)
      o = _valuePassThruKey.getDefault();
    assert(o != null);
    return Boolean.TRUE.equals(o);
  }

  /**
   * @todo Move up to all SelectOnes?
   */
  protected String getUnselectedLabel(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  /**
   * Convert a stringified index into an index, with range-checking.
   */
  static int __getIndex(
    Object submittedValue,
    List<SelectItem> selectItems)
  {
    if ("".equals(submittedValue))
      return -1;

    try
    {
      int index = Integer.parseInt(submittedValue.toString());

      if (( -1 < index) && (countSelectItems(selectItems) > index))
      {
        return index;
      }
      else
      {
        // TODO Don't throw exception: message!
        throw new IndexOutOfBoundsException(_LOG.getMessage(
          "SELECTONE_SUBMITTEDVALUE_INDEX_OUTSIDE_BOUNDS", new Object[]{index, (selectItems.size() - 1)}));
      }
    }
    catch (NumberFormatException ne)
    {
      // TODO Don't throw exception: message!
      throw new NumberFormatException(_LOG.getMessage(
        "SELECTONE_CANNOT_CONVERT_SUBMITTEDVALUE_INDEX_INTO_INTEGER", new Object[]{submittedValue.toString(), ne}));
    }
  }

  private static int countSelectItems(
    List<SelectItem> selectItems)
  {
    int count = 0;
    for(SelectItem item : selectItems)
    {
      count += calcItems( item );
    }

    return count;
  }

  private static int calcItems(
    SelectItem item)
  {
    if(item instanceof SelectItemGroup)
    {
      int count = 0;
      SelectItem[] items;
      items = ((SelectItemGroup)item).getSelectItems();

      for(int i = 0; i < items.length; i++)
      {
        count += calcItems( items[i] );
      }

      return count;
    }

    return 1;
  }

  //
  // Find the selected item in the list
  //
  private int _findIndex(
    Object           value,
    List<SelectItem> selectItems)
  {
    int size = selectItems.size();
    int result;
    for (int i = 0; i < size; i++)
    {
      SelectItem item = selectItems.get(i);
      if (item == null)
        continue;

      result = resolveIndex(item, value, i);
      if(result >= 0)
      {
        return result;
      }
    }

    return -1;
  }

  private int resolveIndex(
    SelectItem item,
    Object     value,
    int        index)
  {
    if(item instanceof SelectItemGroup)
    {
      int result;
      for(SelectItem subItem : ((SelectItemGroup)item).getSelectItems())
      {
        result = resolveIndex( subItem, value, index++ );

        if(result >= 0)
        {
          return result;
        }
      }
    }
    else
    {
      if (value == null)
      {
        Object itemValue = item.getValue();
        // =-=AEW Treat the empty string as if it were null
        if ((itemValue == null) || "".equals(itemValue))
          return index;
      }
      else
      {
        if (value.equals(item.getValue()) || (value.getClass().isEnum() && item.getValue() != null && value.toString().equals( item.getValue().toString() )))
          return index;
      }
    }

    return -1;
  }

  protected String getAutoSubmitScript(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    String source = LabelAndMessageRenderer.__getCachedClientId(rc);
    boolean immediate = isImmediate(component, bean);
    return AutoSubmitUtils.getSubmitScript(rc, source, XhtmlConstants.AUTOSUBMIT_EVENT, immediate);
  }

  private int _getSelectedIndex(
    FacesContext     context,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    Converter        converter,
    boolean          valuePassThru)
  {
    Object submittedValue = getSubmittedValue(component, bean);
    // In passthru mode, if there's a submitted value, we just
    // have to turn it into an int and range-check it
    if ((submittedValue != null) && !valuePassThru)
    {
      return __getIndex(submittedValue, selectItems);
    }
    // Figure out the current value, whether it's submitted or not
    else
    {
      Object value;
      if (submittedValue == null)
      {
        value = getValue(component, bean);
      }
      else
      {
        // submittedValue: run it through the converter if there is one
        if (converter != null)
        {
          try
          {
            value = converter.getAsObject(context,
                                          component,
                                          submittedValue.toString());
          }
          // This shouldn't happen unless we got sent a bogus value;
          // log a warning and move on
          catch (ConverterException ce)
          {
            _LOG.warning(ce);
            value = null;
          }
        }
        else
          value = submittedValue;
      }

      int index = _findIndex(value, selectItems);
      if ((value != null) && (index < 0))
      {
        if (_LOG.isWarning())
          _LOG.warning("CANNOT_FIND_SELECTED_ITEM_MATCHING_VALUE", new Object[]{value, component});
      }

      return index;
    }
  }

  private PropertyKey _valuePassThruKey;

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SimpleSelectOneRenderer.class);
}
