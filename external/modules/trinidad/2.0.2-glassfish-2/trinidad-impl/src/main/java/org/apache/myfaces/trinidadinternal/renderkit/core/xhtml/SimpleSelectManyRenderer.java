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

import java.lang.reflect.Array;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.el.ValueExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.model.SelectItem;
import javax.faces.model.SelectItemGroup;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXSelectMany;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;
import org.apache.myfaces.trinidadinternal.renderkit.uix.SelectItemSupport;


/**
 * Renderer for SelectMany listboxes.
 * @todo Expose at least some of the decode behavior for access
 *   by other selectMany renderers
 */
abstract public class SimpleSelectManyRenderer extends FormInputRenderer
{
  public SimpleSelectManyRenderer(
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

  /**
   * Support the following types of values:
   * Object[] (that contains strings),
   *   String[], List (that contains strings). (selected values)
   * @todo This throws a ConverterException on the first unconvertable
   *  value;  it should wait
   */
  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue
    ) throws ConverterException
  {
    // If "submittedValue" is null, something's gone wrong;  that
    // should be caught by editable component
    assert(submittedValue != null);

    String[] values = (String[]) submittedValue;
    if (values.length == 0)
      return null;

    FacesBean bean = getFacesBean(component);
    Converter converter = getConverter(component, bean);
    if ( converter == null)
      converter = getDefaultConverter(context, component, bean);

    Class<?> modelClass = null;
    ValueExpression expression = getValueExpression(component, bean);
    if (expression != null)
    {
      modelClass = expression.getType(context.getELContext());
    }

    boolean valuePassThru = getValuePassThru(component, bean);
    if (!valuePassThru)
    {
      return _convertIndexedSubmittedValue(context,
                                           component,
                                           converter,
                                           values,
                                           modelClass);

    }
    else
    {
      return _convertSubmittedValue(context,
                                    component,
                                    converter,
                                    values,
                                    modelClass);
    }
  }

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    // Since we override getSubmittedValue() entirely,
    // detect auto submit manually
    detectAutoSubmit(context, component, clientId);

    Object o = context.getExternalContext().
       getRequestParameterValuesMap().get(clientId);
    // Never return null (here, anyway);  null means no
    // items were selected, not no value was submitted
    if (o == null)
      o = _EMPTY_ARRAY;

    return o;

  }

  /**
   * Override to return a Converter for the items in the value,
   * not the value itself.
   */
  @Override
  protected Converter getDefaultConverter(
    FacesContext context,
    UIComponent  copmonent,
    FacesBean    bean)
  {
    ValueExpression expression = getValueExpression(copmonent, bean);
    if (expression == null)
      return null;

    Class<?> type = expression.getType(context.getELContext());
    if ((type == null) || type.isAssignableFrom(List.class))
      return null;

    if (type.isArray())
    {
      Class<?> itemClass = type.getComponentType();
      return ConverterUtils.createConverter(context, itemClass);
    }

    _throwUnsupportedModelType(context, type, null);
    return null;
  }

  static private Object _convertSubmittedValue(
    FacesContext context,
    UIComponent  component,
    Converter    converter,
    String[]     values,
    Class<?>     modelClass
    ) throws ConverterException
  {
    // Handle lists
    if ((modelClass == null) || modelClass.isAssignableFrom(List.class))
    {
      if (converter == null)
        return Arrays.asList(values);

      ArrayList<Object> newList = new ArrayList<Object>(values.length);
      for (int i = 0; i < values.length; i++)
      {
        // Note - any error will result in an immediate ConverterException
        newList.add(converter.getAsObject(context, component, values[i]));
      }

      return newList;
    }
    // Handle arrays
    else if (modelClass.isArray())
    {
      if (converter == null)
        return values;

      Class<?> itemClass = modelClass.getComponentType();
      Object convertedArray = Array.newInstance(itemClass, values.length);
      for (int i = 0; i < values.length; i++)
      {
        Array.set(convertedArray, i,
                  converter.getAsObject(context, component, values[i]));
      }

      return convertedArray;
    }
    else
    {
      _throwUnsupportedModelType(context, modelClass, component);
      return null;
    }
  }

  /**
   * Call this method only when the valuePassThru attribute on the component
   * is not set to true. This indicates that the client-side value
   * is an index. We need to convert that index into its real value.
   * @param component the component
   * @param converter the converter to use on items
   * @param values the array of submitted values. Since this method is only
   *  called when the valuePassThru attribute on the selectMany component is
   *  not true, then each is an index into the list or array of select items.
   * @throws ConverterException if an index specified in the
   *  list of values is out of bounds.
   */
  private Object _convertIndexedSubmittedValue(
    FacesContext context,
    UIComponent  component,
    Converter    converter,
    String[]     values,
    Class<?>     modelClass
    ) throws ConverterException
  {
    List<SelectItem> selectItems = getSelectItems(component, converter, true);

    // No selectItems automatically means that we failed to convert
    if ((selectItems == null) || (selectItems.isEmpty()))
    {
      // =-=AEW Would be better to have a more-specific error.
      _throwConversionError(context, component);
    }

    // -= Simon Lessard =- Useless assertion: assert (values instanceof String[]);

    // OK, is this a List or an array?
    boolean isList = ((modelClass == null) ||
                      modelClass.isAssignableFrom(List.class));
    boolean isArray = (modelClass != null) && modelClass.isArray();
    // We only support lists and arrays;  if neither, quit.
    if (!isList && !isArray)
    {
      _throwUnsupportedModelType(context, modelClass, component);
    }


    // Create either a List or array
    List<Object> objectList;
    Object       objectArray;
    if (isList)
    {
      objectList = new ArrayList<Object>(values.length);
      objectArray = null;
    }
    else
    {
      objectList = null;
      Class<?> itemClass = modelClass.getComponentType();
      // Use Array API instead of Object[] to support primitive types
      objectArray = Array.newInstance(itemClass, values.length);
    }


    // Convert each index into a SelectItem, then get the value
    // from the SelectItem and stash it
    for (int i=0; i < values.length; i++)
    {
      try
      {
        int index =  Integer.parseInt(values[i]);

        if (( -1 < index) && (selectItems.size() > index))
        {
          SelectItem item = selectItems.get(index);
          if (item == null)
            continue;

          if (isList)
            objectList.add(item.getValue());
          else
            Array.set(objectArray, i, item.getValue());
        }
        else
        {
          // =-=AEW Would be better to have a more-specific error.
          _throwConversionError(context, component);
        }
      }
      catch (NumberFormatException ne)
      {
        // =-=AEW Would be better to have a more-specific error.
        _throwConversionError(context, component);
      }
    }

    if (isList)
      return objectList;
    else
      return objectArray;
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

    List<SelectItem> selectItems = getSelectItems(component, converter);
    int selectedIndices[] = getSelectedIndices(context,
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
      // put the outer style class here, like af_selectManyRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, rc, component, bean);
    }
    encodeElementContent(context,
                         rc,
                         component,
                         bean,
                         selectItems,
                         selectedIndices,
                         converter,
                         valuePassThru);


    if (isHiddenLabelRequired(rc))
      renderShortDescAsHiddenLabel(context, rc, component, bean);
    if (simple)
    {
      writer.endElement("span");
    }
  }

  /**
   * Encode the content of a SelectMany component.
   * @param context the FacesContext
   * @param rc  the AdfRenderingContext
   * @param component the UIComponent
   * @param bean the FacesBean
   * @param selectItems a List of SelectItem instances
   */
  abstract protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int[]            selectedIndices,
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
    List<SelectItem> selectItems =
      getSelectItems(component, converter);

    int selectedIndices[] = getSelectedIndices(context,
                                               component,
                                               bean,
                                               selectItems,
                                               converter,
                                               valuePassThru);

    ResponseWriter rw = context.getResponseWriter();
    for (int i = 0; i < selectedIndices.length; i++)
    {
      if (i > 0)
      {
        renderBetweenNonElements(context, rc, component, bean);
      }

      SelectItem item = selectItems.get(selectedIndices[i]);
      rw.writeText(item.getLabel(), null);
    }
  }

  protected void renderBetweenNonElements(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("br", component);
    rw.endElement("br");
  }

  @Override
  protected String getRequiredMessageKey()
  {
    return UIXSelectMany.REQUIRED_MESSAGE_ID;
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
   * Returns true if the renderer cares about order.
   */
  protected boolean isReorderable()
  {
    return false;
  }

  /**
   * Return all the selected indices.  (There
   * may be included -1's in case of an error)
   * The list will be sorted, unless isReorderable()
   * returns true.
   */
  @SuppressWarnings("unchecked")
  protected int[] getSelectedIndices(
    FacesContext     context,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    Converter        converter,
    boolean          valuePassThru)
  {
    List<SelectItem> selectItemList =  flatItemList(selectItems);
    Object submittedValue = getSubmittedValue(component, bean);
    // In passthru mode, if there's a submitted value, we just
    // have to turn it into an array of ints and range-check it
    if ((submittedValue != null) && !valuePassThru)
    {
      String[] values = (String[]) submittedValue;
      if (values.length == 0)
        return _EMPTY_INT_ARRAY;
      int[] indices = new int[values.length];
      for (int i = 0; i < values.length; i++)
      {
        indices[i] = SimpleSelectOneRenderer.__getIndex(values[i], selectItemList);
      }

      // And sort it, but only if it's not reorderable
      if (!isReorderable())
        Arrays.sort(indices);
      return indices;
    }

    // Figure out the current value, whether it's submitted or not
    // (valuePassThru is irrelevant in here, since it can only
    // affect submittedValue, and we already dealt with that.)
    Object value;
    if (submittedValue == null)
    {
      value = getValue(component, bean);
    }
    else
    {
      if (converter == null)
      {
        value = submittedValue;
      }
      else
      {
        String[] values = (String[]) submittedValue;
        List<Object> valuesList = new ArrayList<Object>(values.length);
        for (int i = 0; i < values.length; i++)
        {
          valuesList.add(converter.getAsObject(context,
                                               component,
                                               values[i]));
        }

        value = valuesList;
      }
    }

    if (value == null)
      return _EMPTY_INT_ARRAY;

    // Now, get the value looking like a list
    List<Object> valueList;
    if (value instanceof List)
    {
      // Make a copy of the list so we can mutate it safely
      valueList = new ArrayList<Object>((List<Object>) value);
    }
    else if (value.getClass().isArray())
    {
      int length = Array.getLength(value);
      valueList = new ArrayList<Object>(length);
      for (int i = 0; i < length; i++)
      {
        valueList.add(Array.get(value, i));
      }
    }
    else
    {
      // Let's just take the one value as a single element
      valueList = new ArrayList<Object>(1);
      valueList.add(value);
    }

    // Now figure out what's selected or not
    int valueListSize = valueList.size();
    int[] indices = new int[valueListSize];
    // Pre-mark each item as -1 to indicate it as
    // not-found
    for (int i = 0; i < valueListSize; i++)
    {
      indices[i] = -1;
    }

    int itemCount = selectItemList.size();
    int foundCount = 0;

    for (int i = 0; i < itemCount; i++)
    {
      SelectItem item = selectItemList.get(i);
      if (item == null)
        continue;

      int index = calcIndex(item, valueList);

      if (index >= 0)
      {
        // Remove it from the valueList so that if the same
        // value appears multiple times, that'll (more-or-less)
        // work - but remove it by replacing it with an object
        // that won't be .equals() anything else, so the
        // indices all match up
        valueList.set(index, _ALREADY_FOUND);
        // Remember that this item is selected
        indices[index] = i;
        foundCount++;
      }
    }

    // If all of our values matched up to something in the
    // selectItemList, then the valueList will now be empty.
    // Otherwise, there's some values in the List that didn't
    // appear anywhere among our selectItemList, so clear
    // out the remainder of the indices (which otherwise would
    // be zero) and log a warning
    if (foundCount < valueListSize)
    {
      if (_LOG.isWarning())
      {
        _LOG.warning("SOME_ENTRIES_NOT_FOUND_IN_SELECTITEMS", new Object[]{component, valueList});
      }
    }

    // And sort it, but only if it's not reorderable
    if (!isReorderable())
      Arrays.sort(indices);

    Integer[] indicesObj = new Integer[indices.length];
    for (int foo = 0; foo < indices.length; foo++)
      indicesObj[foo] = indices[foo];

    return indices;
  }

  private List<SelectItem> flatItemList(List<SelectItem> selectItems)
  {
     List<SelectItem> result = new ArrayList<SelectItem>();
     for(SelectItem item : selectItems)
     {
        List<SelectItem> subresult = collectItems( item );

        for(SelectItem subItem : subresult)
        {
           result.add( subItem );
        }
     }

     return result;
  }

  private List<SelectItem> collectItems(SelectItem item)
  {
     List<SelectItem> result = new ArrayList<SelectItem>();
     if(item instanceof SelectItemGroup)
     {
        for(SelectItem subitem : ((SelectItemGroup)item).getSelectItems())
        {
           List<SelectItem> subresult = collectItems( subitem );

           for(SelectItem subItem : subresult)
           {
              result.add( subItem );
           }
        }
     }
     else
     {
        result.add( item );
     }
     return result;
  }

  private int calcIndex(SelectItem item, List<Object> valueList)
  {
     if(item instanceof SelectItemGroup)
     {
        int index = -1;
        for(SelectItem subItem : ((SelectItemGroup)item).getSelectItems())
        {
           index = calcIndex( subItem, valueList );
           if(index >= 0 )
           {
              break;
           }
        }
        return index;
     }
     else
     {
        return valueList.indexOf(item.getValue());
     }
  }

  static private void _throwConversionError(
    FacesContext context, UIComponent component)
      throws ConverterException
  {
    throw new ConverterException(
          MessageFactory.getMessage(context,
                      UIXSelectMany.CONVERSION_MESSAGE_ID,
                      component));
  }

  static private void _throwUnsupportedModelType(
    FacesContext context, Class<?> type, UIComponent component)
      throws ConverterException
  {
    throw new ConverterException(
          MessageFactory.getMessage(context,
                      UIXSelectMany.UNSUPPORTED_MODEL_TYPE_MESSAGE_ID,
                      new Object[]{type}, component));
  }

  private PropertyKey _valuePassThruKey;

  static private final int[] _EMPTY_INT_ARRAY = new int[0];
  static private final String[] _EMPTY_ARRAY = new String[0];
  static private final Object _ALREADY_FOUND = new Object();
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    EditableValueRenderer.class);
}



