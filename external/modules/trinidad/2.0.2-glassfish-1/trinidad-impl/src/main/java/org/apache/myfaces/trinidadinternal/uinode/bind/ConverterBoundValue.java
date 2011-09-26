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
package org.apache.myfaces.trinidadinternal.uinode.bind;

import javax.el.ValueExpression;

import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.component.UIXValue;

import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * BoundValue that returns the appropriate converter
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/ConverterBoundValue.java#0 $) $Date: 10-nov-2005.18:50:05 $
 * @todo Support by-type conversions
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ConverterBoundValue implements BoundValue
{
 public ConverterBoundValue(
    UIXComponent component)
  {
    if (component == null)
      throw new NullPointerException();

    _component = component;
  }

  public Object getValue(UIXRenderingContext context)
  {
    FacesBean bean = _component.getFacesBean();
    // Try to get an explicit Converter
    Converter converter = (Converter)
      bean.getProperty(UIXValue.CONVERTER_KEY);

    // OK, no explicit converter, look at the ValueExpression
    if (converter == null)
    {
      ValueExpression expression = bean.getValueExpression(UIXValue.VALUE_KEY);
      if (expression != null)
      {
        FacesContext fContext = (context == null) ? 
          FacesContext.getCurrentInstance() : context.getFacesContext();
        Class<?> type = expression.getType(fContext.getELContext());
        converter = ConverterUtils.createConverter(fContext, type);
      }
    }
    
    return converter;
  }
  

  private final UIXComponent _component;
}
