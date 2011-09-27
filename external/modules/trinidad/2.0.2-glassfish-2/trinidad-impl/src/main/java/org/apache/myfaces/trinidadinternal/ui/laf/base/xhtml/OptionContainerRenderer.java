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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import java.lang.reflect.Array;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.component.UIXSelectOne;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.path.Path;
import org.apache.myfaces.trinidadinternal.ui.state.BaseSelection;
import org.apache.myfaces.trinidadinternal.ui.state.Selection;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/OptionContainerRenderer.java#0 $) $Date: 10-nov-2005.18:54:04 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class OptionContainerRenderer extends FormElementRenderer
{
  @Override
  protected void renderAsNonElement(UIXRenderingContext context, UINode node)
    throws IOException
  {
    pushRenderingProperty(context, _OPTION_INFO_PROPERTY,
                          createOptionInfo(context, node));

    super.renderAsNonElement(context, node);

    popRenderingProperty(context, _OPTION_INFO_PROPERTY);
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

    //TODO - =gc= when you move this to faces major, figure out if it's
    // a selectOne or a selectMany and use the correct key
    addOnSubmitRequiredValidator(context, node, 
                                 UIXSelectOne.REQUIRED_MESSAGE_ID);
   // _addRequiredValidater(context,
   //                       node);


    OptionInfo info = createOptionInfo(context, node);

    // Register a postback form element name with the FormEncoder
    if (!Boolean.TRUE.equals(info.readOnly) &&
        !Boolean.TRUE.equals(info.disabled))
    {
      context.getFormEncoder().registerFormParameter(info.transformedName);
    }

    pushRenderingProperty(context, _OPTION_INFO_PROPERTY, info);
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    popRenderingProperty(context, _OPTION_INFO_PROPERTY);
    super.postrender(context, node);
  }

  /**
   * Renders event handlers for the node.
   */
  @Override
  protected void renderEventHandlers(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderEventHandlers(context, node);

    renderAttribute(context, "onchange",  getOnChange(context, node));
  }


  abstract protected Boolean isMultipleSelection(
    UIXRenderingContext context,
    UINode           node);


  /**
   * Returns the onChange handler.  This is in place to support
   * RadioSetRenderer, which does not have an onChange handler.
   */
  protected Object getOnChange(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    return node.getAttributeValue(context, ON_CHANGE_ATTR);
  }

  /**
   * Returns the value associated with the selected value attribute
   */
  protected String getSelectedValue(UIXRenderingContext context, UINode node)
  {
    return BaseLafUtils.getLocalTextAttribute(context, node,
                                                       SELECTED_VALUE_ATTR);
  }

  /**
   * Returns the value associated with the selected index attribute
   */
  protected Integer getSelectedIndex(UIXRenderingContext context, UINode node)
  {
    return (Integer) BaseLafUtils.getLocalAttribute(context, node,
                                                    SELECTED_INDEX_ATTR);
  }

  protected Selection getSelection(UIXRenderingContext context, UINode node)
  {
    return (BaseSelection) BaseLafUtils.getLocalAttribute(context, node,
                                                          SELECTION_ATTR);
  }

  /**
   * Subclasses can override this method to create an OptionInfo subclass,
   * populated with information, leveraging populateOptionInfo as needed.
   */
  protected OptionContainerRenderer.OptionInfo createOptionInfo(
    UIXRenderingContext context,
    UINode           node)
  {
    OptionContainerRenderer.OptionInfo info = new OptionContainerRenderer.OptionInfo();
    populateOptionInfo(context, node, info);
    return info;
  }

  final protected void populateOptionInfo(
    UIXRenderingContext context,
    UINode           node,
    OptionContainerRenderer.OptionInfo info)
  {
    info.name = getNodeName(context, node);
    info.transformedName = getTransformedName(context, node);

    info.multiple = isMultipleSelection(context, node);
    info.selectedCount = 0;

    // store attribute values for use by option renderer
    info.styleClass = getStyleClass(context, node);
    info.selection  = getSelection(context, node);
    info.selectedValue = getSelectedValue(context, node);
    info.selectedIndex = getSelectedIndex(context, node);
    info.disabled = getDisabled(context, node);
    info.readOnly = getReadOnly(context, node);
    info.renderer = getOptionRenderer(context);
  }

  /**
   * Returns the cached option information.
   */
  protected OptionContainerRenderer.OptionInfo getOptionInfo(
    UIXRenderingContext context)
  {
    return (OptionInfo) getRenderingProperty(context,
                                             _OPTION_INFO_PROPERTY);
  }

  /**
   * Subclasses should implement this method to return the specifc
   * option renderer to use.
   */
  abstract protected Renderer getOptionRenderer(UIXRenderingContext context);

  protected void renderSelectItemOptions(
    UIXRenderingContext context,
    UINode           node,
    UIComponent      component,
    List<SelectItem> items) throws IOException
  {
    if ((items == null) || items.isEmpty())
      return;

    Converter converter = ((ValueHolder) component).getConverter();
    List<Object> selectedValues = null;
    boolean isSubmitted = false;

    // First, look in "submitted values" - assume it's
    // an Object array! (really, a String array)
    if (component instanceof EditableValueHolder)
    {
      Object submittedValue =
        ((EditableValueHolder) component).getSubmittedValue();
      if (submittedValue != null)
      {
        if (submittedValue instanceof Object[])
        {
          selectedValues = Arrays.asList((Object[]) submittedValue);
        }
        else
        {
          selectedValues = new ArrayList<Object>(1);
          selectedValues.add(submittedValue);
        }

        isSubmitted = true;
      }
    }

    if (!isSubmitted)
    {
      // this calls component.getValue(). This will get the actual
      // values, not the index, even if valuePassThru is false. The reason
      // is because we converted this in SelectOne/SelectManyRenderer in
      // getConvertedValue.
      // this is null when nothing has been submitted yet.
      selectedValues = _getSelectedValues(component);
    }

    Object valuePassThruObj = 
      component.getAttributes().get("valuePassThru");
    boolean valuePassThru = Boolean.TRUE.equals(valuePassThruObj);


    // renderedOne is true if an item has already been rendered
    // This information is used in renderSelectItem
    boolean renderedOne = false;
    
    // loop through each SelectItem
    int size = items.size();    
    for (int i = 0; i < size; i++)
    {
      SelectItem item = items.get(i);

      Object valueObj = null;
      // if item is null, then this most likely means that rendered="false".
      if (item != null)
      {
        valueObj = item.getValue(); 
      }

      String value = null;
      // See if the item is selected
      boolean isSelected = false;
      if (valueObj != null)
      {
        // if valuePassThru is false, then use the INDEX of the SelectItem in 
        // the list, not the SelectItem's value. unless the value is null
        // or the empty string. In that case, use the value. We do this 
        // so that if the component is required, a client-side validation
        // will occur if the value is empty. It won't work if we always use
        // an index.
        if ("".equals(valueObj))
          value = valueObj.toString();
        else if (!valuePassThru)
          value = String.valueOf(i);
        else if (converter == null)
          value = valueObj.toString();
        else
          value = converter.getAsString(context.getFacesContext(),
                                        component,
                                        valueObj);

        // Check if this value is selected;  in "submitted value" mode,
        // use the string;  otherwise, use the underlying value
        isSelected = ((selectedValues != null) &&
                      selectedValues.contains(isSubmitted ? value : valueObj));

        // render the value. get the other info, like disabled, from
        // the item.
        boolean result = renderSelectItem(context, node, component,
                                          item, value, isSelected,
                                          renderedOne, i);
        renderedOne = renderedOne || result;
      }

    }
  }


  /**
   * Render a single select item.
   * @param renderedOne true if an item has already been rendered
   * @return false if nothing was rendered, true otherwise
   */
  protected boolean renderSelectItem(
    UIXRenderingContext context,
    UINode           node,
    UIComponent      component,
    SelectItem       item,
    String           value,
    boolean          isSelected,
    boolean          renderedOne,
    int              index) throws IOException
  {
    if (item == null)
      return false;
    boolean agentSupportsDisabledOptions = Boolean.TRUE
        .equals(getAgentCapability(context,
            TrinidadAgent.CAP_SUPPORTS_DISABLED_OPTIONS));
    boolean isParentDisabled = Boolean.TRUE.equals(component.getAttributes()
        .get(DISABLED_ATTR.getAttributeName()));  
    if (!isParentDisabled && item.isDisabled()
        && (!(agentSupportsDisabledOptions)))
      return false;

    //
    FacesContext fContext = context.getFacesContext();
    ResponseWriter out = fContext.getResponseWriter();

    if (!renderAsElement(context, node))
    {
      if (isSelected)
      {
        if (renderedOne)
        {
          out.startElement("br", null);
          out.endElement("br");
        }

        out.writeText(item.getLabel(), null);
        return true;
      }
    }
    else
    {
      out.startElement("option", null);

      if (item.isDisabled())
        out.writeAttribute("disabled", Boolean.TRUE, null);

      out.writeAttribute("value", value, null);
      if (isSelected)
        out.writeAttribute("selected", Boolean.TRUE, null);

      out.writeAttribute("title", item.getDescription(), null);

      out.writeText(item.getLabel(), null);

      out.endElement("option");
    }

    return false;
  }

  static UIComponent __getUIComponent(
    UIXRenderingContext context,
    UINode           node)
  {
    UIComponent component = node.getUIComponent();
    if (component == null)
    {
      UIXRenderingContext parentContext = context.getParentContext();
      if (parentContext != null)
      {
        UINode parentNode = parentContext.getAncestorNode(0);
        component = NodeUtils.getUIComponent(parentContext, parentNode);
      }
    }

    return component;
  }

  @SuppressWarnings("unchecked")
  static private List<Object> _getSelectedValues(UIComponent component)
  {
    // Assume the component is a value holder
    Object value = ((ValueHolder) component).getValue();
    if (value == null)
      return null;

    if (value instanceof List)
      return (List<Object>) value;

    // Object array
    if (value instanceof Object[])
      return Arrays.asList((Object[]) value);

    // Primitive array

    if (value.getClass().isArray())
    {
      int length = Array.getLength(value);
      List<Object> list = new ArrayList<Object>(length);
      for (int i = 0; i < length; i++)
        list.add(Array.get(value, i));
      return list;
    }

    // Single object
    ArrayList<Object> list = new ArrayList<Object>(1);
    list.add(value);
    return list;
  }
    
        
        
        

  /**
   * This inner class provides a base option renderer implementation,
   * complete with accessors that dereference the option information.
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  protected static class OptionRenderer extends XhtmlLafRenderer
  {
    protected OptionContainerRenderer.OptionInfo getOptionInfo(
      UIXRenderingContext context)
    {
      return (OptionInfo) getRenderingProperty(context,
                                               _OPTION_INFO_PROPERTY);
    }

    @Override
    protected void renderAttributes(
      UIXRenderingContext context,
      UINode           node
      ) throws IOException
    {
      super.renderAttributes(context, node);
      _renderDisabledAttribute(context,node);
      renderSelectedAttribute(context, node);

      // BUG 3557710 - FORM ENCODER AND POSTBACK HANDLING
      Object transName = getTransformedName(context, node);
      boolean readOnly = Boolean.TRUE.equals(getReadOnly(context, node));
      boolean disabled = Boolean.TRUE.equals(getDisabled(context, node));
      
      Object value = getValue(context, node);
      // Only encode value for non-readonly and non-disabled fields.
      if (!readOnly && !disabled)
      {
        value = XhtmlLafUtils.getFormEncodedValue(context, transName, value);
      }
      
      // In the case of Non-JavaScript browsers, skip the renderValue method 
      // call since it appends the index of option element to the value 
      // attribute.
      if (!supportsScripting(context)) 
      {
        FacesContext fContext = context.getFacesContext();
        ResponseWriter out = fContext.getResponseWriter();
        out.writeAttribute("value", value, null);
      }
      else
      {
        renderValue(context, node, value);
      }
    }
    
    protected void renderValue(
      UIXRenderingContext context,
      UINode           node,
      Object           value
    )throws IOException
    {
      renderAttribute(context, VALUE_ATTRIBUTE, value);
    }
    
    /**
     * @param context
     * @param node
     * @throws IOException
     */
    private void _renderDisabledAttribute(UIXRenderingContext context, UINode node)
        throws IOException
    {
      boolean isReadOnly = BaseLafUtils.getLocalBooleanAttribute(context, 
                                                  node, 
                                                  READ_ONLY_ATTR, 
                                                  false);
      boolean isDisabled = BaseLafUtils.getLocalBooleanAttribute(context, 
                                                  node, 
                                                  DISABLED_ATTR, 
                                                  false);
      if (isReadOnly || isDisabled)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.writeAttribute("disabled", Boolean.TRUE, null);
      }
    }
    
    protected void renderSelectedAttribute(
      UIXRenderingContext context,
      UINode           node
      ) throws IOException
    {
    }

    protected boolean renderAsElement(
      UIXRenderingContext context,
      UINode           node
      )
    {
      // Cache the value of "renderAsElement".
      Object o = context.getLocalProperty(0, _AS_ELEMENT_KEY, null);
      if (o == null)
      {
        boolean asElement =
          (!Boolean.TRUE.equals(getReadOnly(context, node)) &&
           (supportsDisabledFormElements(context) ||
            !Boolean.TRUE.equals(getDisabled(context, node))));
        context.setLocalProperty(_AS_ELEMENT_KEY,
                                 asElement ? Boolean.TRUE : Boolean.FALSE);
        return asElement;
      }
      else
      {
        return Boolean.TRUE == o;
      }
    }

    @Override
    protected Object getNodeName(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.name;
    }

    @Override
    protected Object getTransformedName(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.transformedName;
    }

    protected Boolean isMultipleSelection(
      UIXRenderingContext context,
      UINode           node)
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.multiple;
    }

    protected Boolean getDisabled(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.disabled;
    }

    protected Boolean getReadOnly(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.readOnly;
    }

    @Override
    protected Object getStyleClass(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.styleClass;
    }

    /**
     * Returns the value associated with the text attribute
     */
    @Override
    protected Object getText(
      UIXRenderingContext context,
      UINode  node)
    {
      return BaseLafUtils.getLocalTextAttribute(context, node, TEXT_ATTR);
    }

    /**
     * Returns the value associated with the value attribute
     */
    protected Object getValue(
      UIXRenderingContext context,
      UINode  node)
    {
      return BaseLafUtils.getLocalTextAttribute(context, node, VALUE_ATTR);
    }

    /**
     * Returns the value associated with the selected attribute
     */
    protected Boolean getSelected(
      UIXRenderingContext context,
      UINode  node)
    {
      return (Boolean)
        BaseLafUtils.getLocalAttribute(context, node, SELECTED_ATTR);
    }

    /**
     * Returns the value associated with the option value attribute
     */
    protected Object getOptionValue(
      UIXRenderingContext context,
      UINode  node)
    {
      Object value = getValue(context, node);

      if (value == null)
        value = getText(context, node);

      return value;
    }

    protected Selection getSelection(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.selection;
    }

    protected Number getSelectedIndex(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.selectedIndex;
    }

    protected String getSelectedValue(
      UIXRenderingContext context,
      UINode           node
      )
    {
      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);
      return info.selectedValue;
    }

    protected boolean isOptionSelected(
      UIXRenderingContext context,
      UINode           node
      )
    {
      Boolean isOptionSelected = (Boolean)
        context.getLocalProperty(0, _OPTION_SELECTED_PROPERTY, null);

      // return cached value for subsequent calls
      // on same option
      if (isOptionSelected != null)
        return isOptionSelected.booleanValue();

      OptionContainerRenderer.OptionInfo info = getOptionInfo(context);

      // return false for subsequent options after selectedCount is > 0
      // when multiple selection is not enabled
      if (!Boolean.TRUE.equals(info.multiple) &&
           info.selectedCount > 0)
        return false;

      Object value = getOptionValue(context, node);

      String valueString = (value != null)
                             ? value.toString()
                             : null;

      int childIndex = _getChildIndex(context);
      boolean optionSelected = false;

      // if option container selection is set, this takes precedence
      if (info.selection != null)
      {
        optionSelected = info.selection.isSelected(context,
                                                   valueString,
                                                   childIndex);
      }
      else
      {
        // if selected attribute is set, this comes next
        Boolean selected = getSelected(context, node);

        if (selected != null)
        {
          optionSelected = selected.booleanValue();
        }
        else
        {
          // if selectedValue attribute is set, this comes next
          if (info.selectedValue != null)
          {
            optionSelected = info.selectedValue.equals(valueString);
          }
          else
          {
            // if selectedIndex attribute is set, this comes next
            if (info.selectedIndex != null)
            {
              optionSelected = (info.selectedIndex.intValue() == childIndex);
            }
          }
        }
      }

      context.setLocalProperty(_OPTION_SELECTED_PROPERTY,
                               Boolean.valueOf(optionSelected));

      if (optionSelected)
      {
        info.selectedCount ++;
      }

      return optionSelected;
    }

    private int _getChildIndex(UIXRenderingContext context)
    {
      Path path = context.getPath();
      // If the path is empty, this means we're working with composite
      // rendering.  Jump up to the parent, and use the end of its Path.
      // Sadly, this isn't perfect - it only helps you if the <option>
      // is tops in the template/composite widget - but it helps out a lot.
      if (path.getElementCount() == 0)
      {
        UIXRenderingContext parent = context.getParentContext();
        if (parent == null)
          return -1;
        return _getChildIndex(parent);
      }

      return path.getElementIndex(-1);
    }
  }

  /**
   * The base option information class.
   */
  protected static class OptionInfo
  {
    public Renderer renderer;

    public Object name;
    public Object transformedName;

    public Boolean  multiple;
    public int      selectedCount;

    public Boolean   disabled;
    public Boolean   readOnly;
    public Object    styleClass;
    public Selection selection;
    public String    selectedValue;
    public Integer   selectedIndex;

    public OptionContainerRenderer.OptionInfo toOptionInfo()
    {
      return this;
    }
  }




  private static final Object _OPTION_INFO_PROPERTY      = new Object();
  private static final Object _OPTION_SELECTED_PROPERTY  = new Object();
  private static final Object _AS_ELEMENT_KEY            = new Object();
}
